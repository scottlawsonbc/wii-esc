unit FMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, LResources, ExtCtrls, UComPort, Windows, ShellApi, UMetadata;

type
  { TFrmMain }
  TFrmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CmbPgmType: TComboBox;
    CmbPorts: TComboBox;
    CmbBaud: TComboBox;
    CmbTarget: TComboBox;
    CmbPgmType4: TComboBox;
    gbProgrammer: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MemLog: TMemo;
    StatusBar1: TStatusBar;
    TmLoadDelay: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure CmbPgmTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TmLoadDelayTimer(Sender: TObject);
  private
    FMetadata: TMetadata;
    FBusy: boolean;
    FWorkingPath: String;
    procedure LogMessage(const S: String);
    procedure ObjToControls;
    procedure UpdateControls;
  public
    procedure LoadMetadata;
    procedure UnpackResources;
  end; 

var
  FrmMain: TFrmMain;

implementation

uses IniFiles, FPHttpClient;

{$R *.lfm}

{ TFrmMain }
procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FMetadata := TMetadata.Create;
  FWorkingPath := GetTempDir(False) + 'wii_esc_flash_tool' + DirectorySeparator;
  ForceDirectories(FWorkingPath);
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  DeleteDirectory(FWorkingPath, False);
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  TmLoadDelay.Enabled := True;
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
    LogMessage('Ready.');
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

procedure TFrmMain.ObjToControls;
begin
  FMetadata.GetProgrammers(CmbPgmType.Items);
  FMetadata.GetFirmwares(CmbTarget.Items);
  if (CmbPgmType.Items.Count > 0) then CmbPgmType.ItemIndex := 0;
  if (CmbTarget.Items.Count > 0) then CmbTarget.ItemIndex := 0;
end;

procedure TFrmMain.UpdateControls;
begin
  CmbPgmType.Enabled := (CmbPgmType.Items.Count > 0) and not FBusy;
  CmbTarget.Enabled := (CmbTarget.Items.Count > 0) and not FBusy;
  CmbPorts.Enabled := (CmbPgmType.ItemIndex >= 0);
  CmbBaud.Enabled := (CmbBaud.Items.Count > 0);
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
  finally
    lHTTP.Free;
    lINI.Free;
    lStream.Free;
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
end;

procedure TFrmMain.Button1Click(Sender: TObject);
var
  lFirmware: TMetadataFirmware;
begin
  if (CmbTarget.ItemIndex >= 0) then
  begin
    lFirmware := TMetadataFirmware(CmbTarget.Items.Objects[CmbTarget.ItemIndex]);
    if Assigned(lFirmware) and (lFirmware.InfoURL <> '') then
      ShellExecute(0, 'open', PChar(lFirmware.InfoURL), '', nil, SW_SHOW);
  end;
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


initialization
  {$I payload.inc}

end.

