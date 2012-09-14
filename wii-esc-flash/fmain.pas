unit FMain; 

{$mode objfpc}{$H+}

interface

uses
  Windows, ShellApi, Classes, SysUtils, FileUtil, SynMemo,
  synhighlighterunixshellscript, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, LResources, ExtCtrls,
  Buttons, ActnList, StdActns, IniPropStorage, AsyncProcess, UComPort, process, UMetadata;

type
  { TFrmMain }
  TFrmMain = class(TForm)
    ActionList1: TActionList;
    ActBackup: TFileSaveAs;
    BtnFirmwareWarn: TSpeedButton;
    Programmer: TAsyncProcess;
    BtnBackup: TButton;
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
    ActOpenFirmware: TFileOpen;
    ActSaveFirmware: TFileSaveAs;
    ActOpenConfiguration: TFileOpen;
    gbProgrammer: TGroupBox;
    GrpFirmware: TGroupBox;
    GrpConfiguration: TGroupBox;
    ImageList1: TImageList;
    PermStorage: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    BtnFirmwareInfo: TSpeedButton;
    BtnConfigurationInfo: TSpeedButton;
    Stb: TStatusBar;
    MemLog: TSynMemo;
    SynUNIXShellScriptSyn: TSynUNIXShellScriptSyn;
    TmLoadDelay: TTimer;
    procedure ActBackupAccept(Sender: TObject);
    procedure ActOpenConfigurationAccept(Sender: TObject);
    procedure ActOpenFirmwareAccept(Sender: TObject);
    procedure ActSaveFirmwareAccept(Sender: TObject);
    procedure BtnFirmwareWarnClick(Sender: TObject);
    procedure BtnFlashEEPROMClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ProgrammerReadData(Sender: TObject);
    procedure ProgrammerTerminate(Sender: TObject);
    procedure BtnConfigurationInfoClick(Sender: TObject);
    procedure BtnFlashFirmwareClick(Sender: TObject);
    procedure BtnLoadConfigurationClick(Sender: TObject);
    procedure BtnLoadFirmwareClick(Sender: TObject);
    procedure BtnEditEEPROMClick(Sender: TObject);
    procedure CmbPgmTypeChange(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
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
    function GetCurrentProgrammer: TMetadataProgrammer;
    procedure LoadConfiguration(AConfiguration: TMetadataConfiguration); overload;
    procedure LoadConfiguration(const AFileName: String); overload;
    procedure ConvertConfiguration;
    procedure LoadFirmware(AFirmware: TMetadataFirmware); overload;
    procedure LoadFirmware(const AFileName: String); overload;
    procedure SaveFirmware(const AFileName: String); overload;
    procedure LogMessage(const S: String);
    procedure LogRaw(const S: String);
    procedure ObjToControls;
    procedure UpdateControls;
    procedure UpdateProgrammerSetting;
    procedure StartProgrammer(const AProgrammer, AParams: String);
    procedure OpenURL(const AURL: String);
  public
    procedure LoadMetadata;
    procedure UnpackResources;
    property CurrentProgrammer: TMetadataProgrammer read GetCurrentProgrammer;
    property CurrentFirmware: TMetadataFirmware read GetCurrentFirmware;
    property CurrentConfiguration: TMetadataConfiguration read GetCurrentConfiguration;
  end;

var
  FrmMain: TFrmMain;

implementation

uses IniFiles, FConfigEditor, FPHttpClient;

{$R *.lfm}

resourcestring
  rsAdditionalIn = 'Additional Information:';
  rsUnpackingRes = 'Unpacking resources...';
  rsLoadingMetad = 'Loading metadata...';
  rsReady = 'Ready.';
  rsDownloadingFileS = 'Downloading file: "%s"';
  rs0nByteSDownloaded = '%.0n byte(s) downloaded.';
  rs0nByteSBinary = '%.0n byte(s) binary.';
  rsLoadingFileS = 'Loading file: "%s"';
  rs0nByteSLoaded = '%.0n byte(s) loaded.';
  rsSavingFileS = 'Saving file: "%s"';
  rs0nByteSSaved = '%.0n byte(s) saved.';

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
  Programmer.CurrentDirectory := FWorkingPath;
  PermStorage.IniFileName := GetAppConfigFile(False);
  PermStorage.Active := True;
  DoubleBuffered := True;
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

procedure TFrmMain.OpenURL(const AURL: String);
begin
  ShellExecute(0, 'open', PChar(AURL), '', nil, SW_SHOW);
end;

procedure TFrmMain.Label6Click(Sender: TObject);
begin
  OpenURL('http://wiki.openpilot.org/display/Doc/RapidESC+Database');
end;

procedure TFrmMain.BtnFirmwareInfoClick(Sender: TObject);
begin
  if Assigned(CurrentFirmware) then
    MessageDlg(rsAdditionalIn, CurrentFirmware.Description, mtInformation, [mbOK], 0);
end;

procedure TFrmMain.BtnConfigurationInfoClick(Sender: TObject);
begin
  if Assigned(CurrentConfiguration) then
    MessageDlg(rsAdditionalIn, CurrentConfiguration.Description, mtInformation, [mbOK], 0);
end;

procedure TFrmMain.AvrDudeReadConsole;
var
  Count : Integer;
  Buffer: Array[0..4096] of byte;
begin
  Count := Programmer.Output.Read(Buffer, SizeOf(Buffer));
  if (Count > 0) then
    with TStringStream.Create('') do
    try
      Position := 0;
      Write(Buffer, Count);
      if (Count > 0) then LogRaw(DataString);
    finally
      Free;
    end;
end;

procedure TFrmMain.ProgrammerReadData(Sender: TObject);
begin
  AvrDudeReadConsole;
end;

procedure TFrmMain.ActOpenFirmwareAccept(Sender: TObject);
begin
  LoadFirmware(ActOpenFirmware.Dialog.FileName);
  LogMessage('');
  UpdateControls;
end;

procedure TFrmMain.ActOpenConfigurationAccept(Sender: TObject);
begin
  LoadConfiguration(ActOpenConfiguration.Dialog.FileName);
  LogMessage('');
  UpdateControls;
end;

procedure TFrmMain.ActBackupAccept(Sender: TObject);
begin
  if ExtractFileExt(ActBackup.Dialog.FileName) = '' then
    ActBackup.Dialog.FileName := ChangeFileExt(ActBackup.Dialog.FileName, '.hex');
  StartProgrammer(CurrentProgrammer.CmdLine, FMetadata.PgmBackupCmd);
end;

procedure TFrmMain.ActSaveFirmwareAccept(Sender: TObject);
var
  lFile: String;
begin
  lFile := ActSaveFirmware.Dialog.FileName;
  if ExtractFileExt(lFile) = '' then
    lFile := ChangeFileExt(lFile, '.hex');
  SaveFirmware(lFile);
  LogMessage('');
  UpdateControls;
end;

procedure TFrmMain.BtnFirmwareWarnClick(Sender: TObject);
begin
  if Assigned(CurrentFirmware) and (CurrentFirmware.WarnURL <> '') then
    OpenURL(CurrentFirmware.WarnURL);
end;

procedure TFrmMain.BtnFlashEEPROMClick(Sender: TObject);
begin
  StartProgrammer(CurrentProgrammer.CmdLine, FMetadata.PgmWriteEEPROMCmd);
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Programmer.Running then
  begin
    CanClose := False;
    Programmer.Terminate(-1);
  end;
end;

procedure TFrmMain.ProgrammerTerminate(Sender: TObject);
begin
  AvrDudeReadConsole;
  FBusy := False;
  UpdateControls;
end;

procedure TFrmMain.BtnFlashFirmwareClick(Sender: TObject);
begin
  StartProgrammer(CurrentProgrammer.CmdLine, FMetadata.PgmWriteFlashCmd);
end;

procedure TFrmMain.TmLoadDelayTimer(Sender: TObject);
begin
  TmLoadDelay.Enabled := False;
  try
    Screen.Cursor := crHourGlass;
    LogMessage(rsUnpackingRes);
    UnpackResources;
    LogMessage(rsLoadingMetad);
    LoadMetadata;
    ObjToControls;
    UpdateProgrammerSetting;
    PermStorage.Restore;
    LogMessage(rsReady);
    LogMessage('');
  finally
    Screen.Cursor := crDefault;
    UpdateControls;
  end;
end;

procedure TFrmMain.LogMessage(const S: String);
var
  P: TPoint;
begin
  P := Point(Length(MemLog.Lines[MemLog.Lines.Count - 1]) + 1, MemLog.Lines.Count);
  MemLog.TextBetweenPoints[P, P] := S + #10;
  MemLog.CaretY := MemLog.Lines.Count; MemLog.CaretX := 0;
  MemLog.EnsureCursorPosVisible;
  MemLog.Update;
end;

procedure TFrmMain.LogRaw(const S: String);
var
  P: TPoint;
begin
  P := Point(Length(MemLog.Lines[MemLog.Lines.Count - 1]) + 1, MemLog.Lines.Count);
  MemLog.TextBetweenPoints[P, P] := S;
  MemLog.CaretY := MemLog.Lines.Count; MemLog.CaretX := 0;
  MemLog.EnsureCursorPosVisible;
end;

function TFrmMain.GetCurrentFirmware: TMetadataFirmware;
begin
  Result := nil;
  if (CmbTarget.ItemIndex >= 0) then
    Result := TMetadataFirmware(CmbTarget.Items.Objects[CmbTarget.ItemIndex]);
end;

function TFrmMain.GetCurrentProgrammer: TMetadataProgrammer;
begin
  Result := nil;
  if (CmbPgmType.ItemIndex >= 0) then
    Result := TMetadataProgrammer(CmbPgmType.Items.Objects[CmbPgmType.ItemIndex]);
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
  BtnLoadFirmware.Enabled := Assigned(CurrentFirmware) and not FBusy;
  BtnFlashFirmware.Enabled := (FFirmware.Size > 0) and not FBusy;
  BtnFirmwareInfo.Enabled := Assigned(CurrentFirmware);
  BtnLoadConfiguration.Enabled := Assigned(CurrentConfiguration) and not FBusy;
  BtnConfigurationInfo.Enabled := Assigned(CurrentConfiguration);
  BtnFlashEEPROM.Enabled := (FEEPROM.Size > 0) and not FBusy;
  BtnEditEEPROM.Enabled := (FEEPROM.Size > 0) and not FBusy;
  BtnBackup.Enabled := Assigned(CurrentProgrammer) and not FBusy;
  ActOpenFirmware.Enabled := not FBusy;
  ActSaveFirmware.Enabled := (FFirmware.Size > 0) and not FBusy;
  ActOpenConfiguration.Enabled := not FBusy;
  ActBackup.Enabled := Assigned(CurrentProgrammer) and not FBusy;
  BtnFirmwareWarn.Visible := Assigned(CurrentFirmware) and (CurrentFirmware.WarnURL <> '');
  BtnFirmwareWarn.Enabled := not FBusy;
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
  try
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
  finally
    SysUtils.DeleteFile(FWorkingPath + '___tmp___.hex');
    SysUtils.DeleteFile(FWorkingPath + '___tmp___.bin');
  end;
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
    LogMessage(Format(rsDownloadingFileS, [ExtractFileName(CurrentFirmware.URL)]));
    LoadFirmware(CurrentFirmware);
    LogMessage(Format(rs0nByteSDownloaded, [Double(FFirmware.Size)]));
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
    LogMessage(Format(rsDownloadingFileS, [ExtractFileName(CurrentConfiguration.URL)]));
    LoadConfiguration(CurrentConfiguration);
    LogMessage(Format(rs0nByteSDownloaded, [Double(FConfiguration.Size)]));
    ConvertConfiguration;;
    LogMessage(Format(rs0nByteSBinary, [Double(FEEPROM.Size)]));
    LogMessage('');
  finally
    Screen.Cursor := crDefault;
  end;
  UpdateControls;
end;

procedure TFrmMain.UpdateProgrammerSetting;
begin
  if Assigned(CurrentProgrammer) then
  begin
    if CurrentProgrammer.Port = '$(com)' then
      GetSerialPortRegNames(CmbPorts.Items)
    else
      CmbPorts.Items.CommaText := CurrentProgrammer.Port;
    if (CmbPorts.Items.Count > 0) and (CmbPorts.Items.IndexOf(CmbPorts.Text) < 0) then
      CmbPorts.Text := CmbPorts.Items[0];
    CmbBaud.Items.Assign(CurrentProgrammer.BaudRates);
  end;
end;

procedure TFrmMain.StartProgrammer(const AProgrammer, AParams: String);
var
  CmdLine: String;
begin
  CmdLine := AProgrammer;
  CmdLine := StringReplace(CmdLine, '$(command)', AParams, [rfIgnoreCase, rfReplaceAll]);
  CmdLine := StringReplace(CmdLine, '$(port)',    CmbPorts.Text, [rfIgnoreCase, rfReplaceAll]);
  CmdLine := StringReplace(CmdLine, '$(baud)',    CmbBaud.Text, [rfIgnoreCase, rfReplaceAll]);
  if (Pos('$(tmp_out_flash_hex)', CmdLine) > 0) then
  begin
    FFirmware.SaveToFile(FWorkingPath + '___tmp_out_flash_hex___.hex');
    CmdLine := StringReplace(CmdLine, '$(tmp_out_flash_hex)', '___tmp_out_flash_hex___.hex', [rfIgnoreCase, rfReplaceAll]);
  end;
  if (Pos('$(tmp_out_eeprom_bin)', CmdLine) > 0) then
  begin
    FEEPROM.SaveToFile(FWorkingPath + '___tmp_out_eeprom_bin___.bin');
    CmdLine := StringReplace(CmdLine, '$(tmp_out_eeprom_bin)', '___tmp_out_eeprom_bin___.bin', [rfIgnoreCase, rfReplaceAll]);
  end;
  CmdLine := StringReplace(CmdLine, '$(file)',        ActBackup.Dialog.FileName, [rfIgnoreCase, rfReplaceAll]);
  CmdLine := StringReplace(CmdLine, '$(file_no_ext)', ExtractFileNameWithoutExt(ActBackup.Dialog.FileName), [rfIgnoreCase, rfReplaceAll]);

  if GetKeyState(VK_SHIFT) < 0 then CmdLine := CmdLine + ' -v -v';
  Programmer.CommandLine := Format('%s /c %s', [SysUtils.GetEnvironmentVariable('COMSPEC'), CmdLine]);
  LogMessage(CmdLine);
  LogMessage('');
  FBusy := True;
  UpdateControls;
  Programmer.Execute;
end;

procedure TFrmMain.CmbPgmTypeChange(Sender: TObject);
begin
  UpdateProgrammerSetting;
  UpdateControls;
end;

procedure TFrmMain.LoadFirmware(const AFileName: String);
begin
  LogMessage(Format(rsLoadingFileS, [AFileName]));
  FFirmware.Clear;
  FFirmware.LoadFromFile(AFileName);
  LogMessage(Format(rs0nByteSLoaded, [Double(FFirmware.Size)]));
end;

procedure TFrmMain.SaveFirmware(const AFileName: String);
begin
  LogMessage(Format(rsSavingFileS, [AFileName]));
  FFirmware.SaveToFile(AFileName);
  LogMessage(Format(rs0nByteSSaved, [Double(FileSize(AFileName))]));
end;

procedure TFrmMain.LoadConfiguration(const AFileName: String);
begin
  LogMessage(Format(rsLoadingFileS, [AFileName]));
  FConfiguration.Clear;
  FConfiguration.LoadFromFile(AFileName);
  ConvertConfiguration;
  LogMessage(Format(rs0nByteSLoaded, [Double(FConfiguration.Size)]));
  LogMessage(Format(rs0nByteSBinary, [Double(FEEPROM.Size)]));
end;


procedure TFrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: integer;
  lFile: String;
begin
  for i := Low(FileNames) to High(FileNames) do
  begin
    lFile := FileNames[i];
    if (LowerCase(ExtractFileExt(lFile)) = '.hex') then
    begin
      LoadFirmware(lFile);
      LogMessage('');
    end;
    if (LowerCase(ExtractFileExt(lFile)) = '.eep') then
    begin
      LoadConfiguration(lFile);
      LogMessage('');
    end;

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

