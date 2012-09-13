unit FMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, LResources, ExtCtrls, Buttons, ActnList, StdActns, IniPropStorage,
  AsyncProcess, UComPort, Windows, ShellApi, process, UMetadata;

type
  { TFrmMain }
  TFrmMain = class(TForm)
    ActionList1: TActionList;
    AvrDude: TAsyncProcess;
    BtnLoadFirmware: TButton;
    BtnFlashFirmware: TButton;
    Button1: TButton;
    BtnLoadConfiguration: TButton;
    Button3: TButton;
    BtnFlashEEPROM: TButton;
    Button5: TButton;
    Button6: TButton;
    BtnEditEEPROM: TButton;
    CmbPgmType: TComboBox;
    CmbConfigurations: TComboBox;
    CmbPorts: TComboBox;
    CmbBaud: TComboBox;
    CmbTarget: TComboBox;
    FileOpen1: TFileOpen;
    gbProgrammer: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    PermStorage: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MemLog: TMemo;
    BtnFirmwareInfo: TSpeedButton;
    BtnConfigurationInfo: TSpeedButton;
    Stb: TStatusBar;
    TmLoadDelay: TTimer;
    procedure AvrDudeReadData(Sender: TObject);
    procedure AvrDudeTerminate(Sender: TObject);
    procedure BtnConfigurationInfoClick(Sender: TObject);
    procedure BtnFlashFirmwareClick(Sender: TObject);
    procedure BtnLoadConfigurationClick(Sender: TObject);
    procedure BtnLoadFirmwareClick(Sender: TObject);
    procedure BtnEditEEPROMClick(Sender: TObject);
    procedure CmbPgmTypeChange(Sender: TObject);
    procedure GlobChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure BtnFirmwareInfoClick(Sender: TObject);
    procedure TmLoadDelayTimer(Sender: TObject);
  private
    FMetadata: TMetadata;
    FBusy: boolean;
    FWorkingPath: String;
    FHomePath: String;
    FFirmware: TMemoryStream;
    FConfiguration: TMemoryStream;
    FEEPROM: TMemoryStream;
    procedure AvrDudeReadConsole;
    function GetCurrentConfiguration: TMetadataConfiguration;
    function GetCurrentFirmware: TMetadataFirmware;
    procedure LoadConfiguration(AConfiguration: TMetadataConfiguration);
    procedure ConvertConfiguration;
    procedure LogMessage(const S: String);
    procedure LogRaw(const S: String);
    procedure ObjToControls;
    procedure UpdateControls;
  public
    procedure LoadMetadata;
    procedure LoadFirmware(AFirmware: TMetadataFirmware);
    procedure UnpackResources;
    property CurrentFirmware: TMetadataFirmware read GetCurrentFirmware;
    property CurrentConfiguration: TMetadataConfiguration read GetCurrentConfiguration;
  end; 

var
  FrmMain: TFrmMain;

implementation

uses IniFiles, FConfigEditor, FPHttpClient;

{$R *.lfm}

{ TFrmMain }
procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FMetadata := TMetadata.Create;
  FFirmware := TMemoryStream.Create;
  FEEPROM := TMemoryStream.Create;
  FConfiguration := TMemoryStream.Create;
  FWorkingPath := GetTempDir(False) + 'wii_esc_flash_tool' + DirectorySeparator;
  FHomePath := GetAppConfigDir(False);
  ForceDirectories(FWorkingPath);
  ForceDirectories(FHomePath);
  AvrDude.CurrentDirectory := FWorkingPath;
  PermStorage.IniFileName := GetAppConfigFile(False);
  PermStorage.Active := True;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FFirmware.Free;
  FEEPROM.Free;
  FConfiguration.Free;
  DeleteDirectory(FWorkingPath, False);
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  TmLoadDelay.Enabled := True;
end;

procedure TFrmMain.Label6Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://wiki.openpilot.org/display/Doc/RapidESC+Database', '', nil, SW_SHOW);
end;

procedure TFrmMain.BtnFirmwareInfoClick(Sender: TObject);
begin
  if Assigned(CurrentFirmware) then
    MessageDlg('Additional Information:', CurrentFirmware.Description, mtInformation, [mbOK], 0);
end;

procedure TFrmMain.BtnConfigurationInfoClick(Sender: TObject);
begin
  if Assigned(CurrentConfiguration) then
    MessageDlg('Additional Information:', CurrentConfiguration.Description, mtInformation, [mbOK], 0);
end;

procedure TFrmMain.AvrDudeReadConsole;
var
  Count : Integer;
  Buffer: Array[0..4096] of byte;
begin
  repeat
    Count := AvrDude.Output.Read(Buffer, SizeOf(Buffer));
    if (Count > 0) then
      with TStringStream.Create('') do
      try
        Position := 0;
        Write(Buffer, Count);
        if (Count > 0) then
          LogRaw(DataString);
      finally
        Free;
      end;
  until (Count = 0);
end;

procedure TFrmMain.AvrDudeReadData(Sender: TObject);
begin
  AvrDudeReadConsole;
end;

procedure TFrmMain.AvrDudeTerminate(Sender: TObject);
begin
  AvrDudeReadConsole;
end;

procedure TFrmMain.BtnFlashFirmwareClick(Sender: TObject);
begin
  AvrDude.CommandLine := SysUtils.GetEnvironmentVariable('COMSPEC') + ' /c avrdude -v -v -v -?';
  LogMessage(AvrDude.CommandLine);
  AvrDude.Execute;
end;

procedure TFrmMain.TmLoadDelayTimer(Sender: TObject);
begin
  TmLoadDelay.Enabled := False;
  try
    Screen.Cursor := crHourGlass;
    LogMessage('Unpacking resources...');
    UnpackResources;
    LogMessage('Loading metadata...');
    LoadMetadata; ObjToControls;
    PermStorage.Restore;
    LogMessage('Ready.');
    LogMessage('');
  finally
    Screen.Cursor := crDefault;
    UpdateControls;
  end;
end;

procedure TFrmMain.LogMessage(const S: String);
begin
  MemLog.Lines.Add(S);
  MemLog.SelStart := MemLog.GetTextLen;
  MemLog.SelLength := 0;
end;

procedure TFrmMain.LogRaw(const S: String);
begin
  MemLog.Lines.Text := MemLog.Lines.Text + S;
  MemLog.SelStart := MemLog.GetTextLen;
  MemLog.SelLength := 0;
end;

function TFrmMain.GetCurrentFirmware: TMetadataFirmware;
begin
  Result := nil;
  if (CmbTarget.ItemIndex >= 0) then
    Result := TMetadataFirmware(CmbTarget.Items.Objects[CmbTarget.ItemIndex]);
end;

function TFrmMain.GetCurrentConfiguration: TMetadataConfiguration;
begin
  Result := nil;
  if (CmbConfigurations.ItemIndex >= 0) then
    Result := TMetadataConfiguration(CmbConfigurations.Items.Objects[CmbConfigurations.ItemIndex]);
end;

procedure TFrmMain.ObjToControls;
begin
  FMetadata.GetProgrammers(CmbPgmType.Items);
  FMetadata.GetFirmwares(CmbTarget.Items);
  FMetadata.GetConfigurations(CmbConfigurations.Items);
  if (CmbPgmType.Items.Count > 0) then CmbPgmType.ItemIndex := 0;
  if (CmbTarget.Items.Count > 0) then CmbTarget.ItemIndex := 0;
  if (CmbConfigurations.Items.Count > 0) then CmbConfigurations.ItemIndex := 0;
  Stb.Panels[0].Text := FMetadata.Version;
end;

procedure TFrmMain.UpdateControls;
begin
  CmbPgmType.Enabled := (CmbPgmType.Items.Count > 0) and not FBusy;
  CmbTarget.Enabled := (CmbTarget.Items.Count > 0) and not FBusy;
  CmbPorts.Enabled := (CmbPgmType.ItemIndex >= 0) and not FBusy;
  CmbBaud.Enabled := (CmbBaud.Items.Count > 0) and not FBusy;
  CmbConfigurations.Enabled := (CmbConfigurations.Items.Count > 0) and not FBusy;
  BtnLoadFirmware.Enabled := Assigned(CurrentFirmware);
  BtnFlashFirmware.Enabled := FFirmware.Size > 0;
  BtnFirmwareInfo.Enabled := Assigned(CurrentFirmware);
  BtnLoadConfiguration.Enabled := Assigned(CurrentConfiguration);
  BtnConfigurationInfo.Enabled := Assigned(CurrentConfiguration);
  BtnFlashEEPROM.Enabled := FEEPROM.Size > 0;
  BtnEditEEPROM.Enabled := FEEPROM.Size > 0;
end;

procedure TFrmMain.LoadMetadata;
var
  lINI: TInifile = nil;
  lStream: TMemoryStream = nil;
  lHTTP: TFPHTTPClient = nil;
begin
  try
    lStream := TMemoryStream.Create;
    lHTTP := TFPHTTPClient.Create(nil);
    lHTTP.Get('http://wii-esc.googlecode.com/svn/wii-esc-flash/metadata/__metadata.ini', lStream);
    lStream.Position := 0;
    lINI := TInifile.Create(lStream);
    FMetadata.LoadFromIni(lINI);
    lHTTP.Get('http://wii-esc.googlecode.com/files/wii-esc-flash_tracker.dat');
  finally
    lHTTP.Free;
    lINI.Free;
    lStream.Free;
  end;
end;

procedure TFrmMain.LoadFirmware(AFirmware: TMetadataFirmware);
begin
  FFirmware.Clear;;
  with TFPHTTPClient.Create(nil) do
  try
    Get(AFirmware.URL, FFirmware);
  finally
    Free;
  end;
end;

procedure TFrmMain.LoadConfiguration(AConfiguration: TMetadataConfiguration);
begin
  FConfiguration.Clear;;
  with TFPHTTPClient.Create(nil) do
  try
    Get(AConfiguration.URL, FConfiguration);
  finally
    Free;
  end;
end;

procedure TFrmMain.ConvertConfiguration;
begin
  FConfiguration.SaveToFile(FWorkingPath + '___tmp___.hex');
  with TProcess.Create(nil) do
  try
    CurrentDirectory := FWorkingPath;
    CommandLine := SysUtils.GetEnvironmentVariable('COMSPEC') + ' /c hex2bin.exe ___tmp___.hex';
    Options := [poUsePipes, poStderrToOutPut];
    ShowWindow := swoHIDE;
    Execute;
    WaitOnExit;
  finally
    Free;
  end;
  FEEPROM.Clear;
  FEEPROM.LoadFromFile(FWorkingPath + '___tmp___.bin');
  SysUtils.DeleteFile(FWorkingPath + '___tmp___.hex');
  SysUtils.DeleteFile(FWorkingPath + '___tmp___.bin');
end;

procedure TFrmMain.UnpackResources;

  procedure ExtractFile(const AFileName: String);
  begin
    with TLazarusResourceStream.Create(ExtractFileNameWithoutExt(AFileName), PChar(UpperCase(Copy(ExtractFileExt(AFileName), 2, 1024)))) do
    try
      SaveToFile(FWorkingPath + AFileName);
    finally
      Free;
    end;
  end;

begin
  ExtractFile('avrdude.conf');
  ExtractFile('avrdude.exe');
  ExtractFile('libusb0.dll');
  ExtractFile('libusb0_x64.dll');
  ExtractFile('bin2hex.exe');
  ExtractFile('hex2bin.exe');
end;

procedure TFrmMain.BtnLoadFirmwareClick(Sender: TObject);
begin
  if Assigned(CurrentFirmware) and (CurrentFirmware.URL <> '') then
  try
    Screen.Cursor := crHourGlass;
    LogMessage(Format('Downloading file: "%s"', [ExtractFileName(CurrentFirmware.URL)]));
    LoadFirmware(CurrentFirmware);
    LogMessage(Format('%.0n byte(s) downloaded.', [Double(FFirmware.Size)]));
    LogMessage('');
  finally
    Screen.Cursor := crDefault;
  end;
  UpdateControls;
end;

procedure TFrmMain.BtnEditEEPROMClick(Sender: TObject);
begin
  EditConfiguration(FEEPROM.Memory);
end;

procedure TFrmMain.BtnLoadConfigurationClick(Sender: TObject);
begin
  if Assigned(CurrentConfiguration) and (CurrentConfiguration.URL <> '') then
  try
    Screen.Cursor := crHourGlass;
    LogMessage(Format('Downloading file: "%s"', [ExtractFileName(CurrentConfiguration.URL)]));
    LoadConfiguration(CurrentConfiguration);
    LogMessage(Format('%.0n byte(s) downloaded.', [Double(FConfiguration.Size)]));
    ConvertConfiguration;;
    LogMessage(Format('%.0n byte(s) binary.', [Double(FEEPROM.Size)]));
    LogMessage('');
  finally
    Screen.Cursor := crDefault;
  end;
  UpdateControls;
end;

procedure TFrmMain.CmbPgmTypeChange(Sender: TObject);
var
  lProgrammer: TMetadataProgrammer;
begin
  if (CmbPgmType.ItemIndex >= 0) and Assigned(CmbPgmType.Items.Objects[CmbPgmType.ItemIndex]) then
  begin
    lProgrammer := TMetadataProgrammer(CmbPgmType.Items.Objects[CmbPgmType.ItemIndex]);
    if lProgrammer.Port = '$com$' then
      GetSerialPortRegNames(CmbPorts.Items)
    else
      CmbPorts.Items.CommaText := lProgrammer.Port;
    if (CmbPorts.Items.IndexOf(CmbPorts.Text) < 0) then
      CmbPorts.Text := CmbPorts.Items[0];
    CmbBaud.Items.Assign(lProgrammer.BaudRates);
  end;
  UpdateControls;
end;

procedure TFrmMain.GlobChange(Sender: TObject);
begin
  UpdateControls;
end;


initialization
  {$I payload.inc}

end.

