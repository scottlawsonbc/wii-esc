unit UMetadata;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TMetadata = class;

  { TMetadataBase }

  TMetadataBase = class
  private
    FCmdLine: String;
    procedure SetCmdLine(AValue: String);
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); virtual;
  public
    constructor Create; virtual;
  published
    property CmdLine: String read FCmdLine write SetCmdLine;
  end;

  { TMetadataProgrammer }

  TMetadataProgrammer = class(TMetadataBase)
  private
    FBaudRates: TStringList;
    FName: String;
    FPort: String;
    FMetadata: TMetadata;
    FPgmBackupCmd: String;
    FPgmReadEEPROMCmd: String;
    FPgmReadFlashCmd: String;
    FPgmTestCmd: String;
    FPgmWriteEEPROMCmd: String;
    FPgmWriteFlashCmd: String;
    function GetPgmBackupCmd: String;
    function GetPgmReadEEPROMCmd: String;
    function GetPgmReadFlashCmd: String;
    function GetPgmTestCmd: String;
    function GetPgmWriteEEPROMCmd: String;
    function GetPgmWriteFlashCmd: String;
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); override;
  public
    constructor Create(AMetadata: TMetadata); reintroduce;
    destructor Destroy; override;
  published
    property Name: String read FName;
    property Port: String read FPort;
    property BaudRates: TStringList read FBaudRates;
    property PgmWriteFlashCmd: String read GetPgmWriteFlashCmd;
    property PgmWriteEEPROMCmd: String read GetPgmWriteEEPROMCmd;
    property PgmReadFlashCmd: String read GetPgmReadFlashCmd;
    property PgmReadEEPROMCmd: String read GetPgmReadEEPROMCmd;
    property PgmBackupCmd: String read GetPgmBackupCmd;
    property PgmTestCmd: String read GetPgmTestCmd;
  end;

  { TMetadataFirmware }

  TMetadataFirmware = class(TMetadataBase)
  private
    FDescription: String;
    FInfoURL: String;
    FName: String;
    FURL: String;
    FWarnURL: String;
    procedure SetDescription(AValue: String);
    procedure SetInfoURL(AValue: String);
    procedure SetName(AValue: String);
    procedure SetURL(AValue: String);
    procedure SetWarnURL(AValue: String);
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); override;
  published
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property URL: String read FURL write SetURL;
    property InfoURL: String read FInfoURL write SetInfoURL;
    property WarnURL: String read FWarnURL write SetWarnURL;
  end;

  { TMetadataHFuse }

  TMetadataHFuse = class(TMetadataBase)
  private
    FDescription: String;
    FInfoURL: String;
    FName: String;
    FURL: String;
    FWarnURL: String;
    procedure SetDescription(AValue: String);
    procedure SetInfoURL(AValue: String);
    procedure SetName(AValue: String);
    procedure SetURL(AValue: String);
    procedure SetWarnURL(AValue: String);
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); override;
  published
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property URL: String read FURL write SetURL;
    property InfoURL: String read FInfoURL write SetInfoURL;
    property WarnURL: String read FWarnURL write SetWarnURL;
  end;

  { TMetadataConfiguration }

  TMetadataConfiguration = class(TMetadataBase)
  private
    FDescription: String;
    FName: String;
    FURL: String;
    procedure SetDescription(AValue: String);
    procedure SetName(AValue: String);
    procedure SetURL(AValue: String);
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); override;
  published
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property URL: String read FURL write SetURL;
  end;

  { TMetadata }

  TMetadata = class(TMetadataBase)
  private
    FMetadataVersion: integer;
    FPgmBackupCmd: String;
    FPgmReadEEPROMCmd: String;
    FPgmReadFlashCmd: String;
    FPgmTestCmd: String;
    FPgmWriteEEPROMCmd: String;
    FPgmWriteFlashCmd: String;
    FProgrammers: TList;
    FFirmwares: TList;
    FConfigurations: TList;
    FFuses: TList;
    FVersion: String;
    procedure SetMetadataVersion(AValue: integer);
    procedure SetPgmBackupCmd(AValue: String);
    procedure SetPgmReadEEPROMCmd(AValue: String);
    procedure SetPgmReadFlashCmd(AValue: String);
    procedure SetPgmTestCmd(AValue: String);
    procedure SetPgmWriteEEPROMCmd(AValue: String);
    procedure SetPgmWriteFlashCmd(AValue: String);
    procedure SetVersion(AValue: String);
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); override; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromIni(Ini: TIniFile); overload;
    procedure GetProgrammers(SL: TStrings);
    procedure GetFirmwares(SL: TStrings);
    procedure GetConfigurations(SL: TStrings);
    procedure GetFuses(SL: TStrings);
  published
    property MetadataVersion: integer read FMetadataVersion write SetMetadataVersion;
    property Version: String read FVersion write SetVersion;
  end;


implementation

function _ReadString(AIni: TIniFile; const Section, Ident, Default: String): String;
var
  SL: TStringList;
  i: integer;
begin
  SL := nil;
  Result := AIni.ReadString(Section, Ident, Default);
  if (Pos('@', Result) = 1) and AIni.SectionExists(Copy(Result, 2, 10240)) then
  try
    SL := TStringList.Create;
    AIni.ReadSectionValues(Copy(Result, 2, 10240), SL);
    Result := '';
    for i := 0 to SL.Count - 1 do
    begin
      if i = 0 then
        Result := SL.ValueFromIndex[i]
      else
        Result := Result + #10 + SL.ValueFromIndex[i];
    end;
  finally
    SL.Free;
  end;
end;

{ TMetadataHFuse }

procedure TMetadataHFuse.SetDescription(AValue: String);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
end;

procedure TMetadataHFuse.SetInfoURL(AValue: String);
begin
  if FInfoURL=AValue then Exit;
  FInfoURL:=AValue;
end;

procedure TMetadataHFuse.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TMetadataHFuse.SetURL(AValue: String);
begin
  if FURL=AValue then Exit;
  FURL:=AValue;
end;

procedure TMetadataHFuse.SetWarnURL(AValue: String);
begin
  if FWarnURL=AValue then Exit;
  FWarnURL:=AValue;
end;

procedure TMetadataHFuse.LoadFromIni(Ini: TIniFile; const ASection: String);
begin
  inherited LoadFromIni(Ini, ASection);
  with Ini do
  begin
    FName := ReadString(ASection, 'Name', ASection);
    FURL := ReadString(ASection, 'URL', '');
    FInfoURL := ReadString(ASection, 'Info', '');
    FDescription := _ReadString(Ini, ASection, 'Description', '');
    FWarnURL := ReadString(ASection, 'Warning', '');
  end;
end;

{ TMetadataConfiguration }

procedure TMetadataConfiguration.SetDescription(AValue: String);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
end;

procedure TMetadataConfiguration.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TMetadataConfiguration.SetURL(AValue: String);
begin
  if FURL=AValue then Exit;
  FURL:=AValue;
end;

procedure TMetadataConfiguration.LoadFromIni(Ini: TIniFile; const ASection: String);
begin
  inherited;
  with Ini do
  begin
    FName := ReadString(ASection, 'Name', ASection);
    FURL := ReadString(ASection, 'URL', '');
    FDescription := _ReadString(Ini, ASection, 'Description', '')
  end;
end;

{ TMetadataBase }
procedure TMetadataBase.SetCmdLine(AValue: String);
begin
  if FCmdLine=AValue then Exit;
  FCmdLine:=AValue;
end;

procedure TMetadataBase.LoadFromIni(Ini: TIniFile; const ASection: String);
begin
  FCmdLine := Ini.ReadString(ASection, 'CmdLine', '');
end;

constructor TMetadataBase.Create;
begin

end;

{ TMetadataProgrammer }
function TMetadataProgrammer.GetPgmBackupCmd: String;
begin
  Result := FPgmBackupCmd;
  if (Result = '') then
    Result := FMetadata.FPgmBackupCmd;
end;

function TMetadataProgrammer.GetPgmReadEEPROMCmd: String;
begin
  Result := FPgmReadEEPROMCmd;
  if (Result = '') then
    Result := FMetadata.FPgmReadEEPROMCmd;
end;

function TMetadataProgrammer.GetPgmReadFlashCmd: String;
begin
  Result := FPgmReadFlashCmd;
  if (Result = '') then
    Result := FMetadata.FPgmReadFlashCmd;
end;

function TMetadataProgrammer.GetPgmTestCmd: String;
begin
  Result := FPgmTestCmd;
  if (Result = '') then
    Result := FMetadata.FPgmTestCmd;
end;

function TMetadataProgrammer.GetPgmWriteEEPROMCmd: String;
begin
  Result := FPgmWriteEEPROMCmd;
  if (Result = '') then
    Result := FMetadata.FPgmWriteEEPROMCmd;
end;

function TMetadataProgrammer.GetPgmWriteFlashCmd: String;
begin
  Result := FPgmWriteFlashCmd;
  if (Result = '') then
    Result := FMetadata.FPgmWriteFlashCmd;
end;


procedure TMetadataProgrammer.LoadFromIni(Ini: TIniFile; const ASection: String);
begin
  inherited;
  with Ini do
  begin
    FBaudRates.CommaText := ReadString(ASection, 'Speed', '');
    FPort := ReadString(ASection, 'Port', '');
    FName := ReadString(ASection, 'Name', ASection);
    FPgmWriteFlashCmd := ReadString(ASection, 'PgmWriteFlashCmd', '');
    FPgmWriteEEPROMCmd := ReadString(ASection, 'PgmWriteEEPROMCmd', '');
    FPgmReadFlashCmd := ReadString(ASection, 'PgmReadFlashCmd', '');
    FPgmReadEEPROMCmd := ReadString(ASection, 'PgmReadEEPROMCmd', '');
    FPgmBackupCmd := ReadString(ASection, 'PgmBackupCmd', '');
    FPgmTestCmd := ReadString(ASection, 'PgmTestCmd', '');
  end;
end;

constructor TMetadataProgrammer.Create(AMetadata: TMetadata);
begin
  inherited Create;
  FBaudRates := TStringList.Create;
  FMetadata := AMetadata;
end;

destructor TMetadataProgrammer.Destroy;
begin
  FBaudRates.Free;
  inherited Destroy;
end;

{ TMetadataFirmware }
procedure TMetadataFirmware.SetDescription(AValue: String);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
end;

procedure TMetadataFirmware.SetInfoURL(AValue: String);
begin
  if FInfoURL=AValue then Exit;
  FInfoURL:=AValue;
end;

procedure TMetadataFirmware.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TMetadataFirmware.SetURL(AValue: String);
begin
  if FURL=AValue then Exit;
  FURL:=AValue;
end;

procedure TMetadataFirmware.SetWarnURL(AValue: String);
begin
  if FWarnURL=AValue then Exit;
  FWarnURL:=AValue;
end;

procedure TMetadataFirmware.LoadFromIni(Ini: TIniFile; const ASection: String);
begin
  inherited;
  with Ini do
  begin
    FName := ReadString(ASection, 'Name', ASection);
    FURL := ReadString(ASection, 'URL', '');
    FInfoURL := ReadString(ASection, 'Info', '');
    FDescription := _ReadString(Ini, ASection, 'Description', '');
    FWarnURL := ReadString(ASection, 'Warning', '');
  end;
end;

{ TMetadata }
constructor TMetadata.Create;
begin
  inherited Create;
  FProgrammers := TList.Create;
  FFirmwares := TList.Create;
  FConfigurations := TList.Create;
  FFuses := TList.Create;
end;

destructor TMetadata.Destroy;
begin
  Clear;
  FProgrammers.Free;
  FFirmwares.Free;
  FConfigurations.Free;
  inherited Destroy;
end;

procedure TMetadata.Clear;
var
  i: integer;
begin
  with FProgrammers do
  begin
    for i := 0 to Count - 1 do
      TObject(Items[i]).Free;
    Clear;
  end;
  with FFirmwares do
  begin
    for i := 0 to Count - 1 do
      TObject(Items[i]).Free;
    Clear;
  end;
  with FConfigurations do
  begin
    for i := 0 to Count - 1 do
      TObject(Items[i]).Free;
    Clear;
  end;
  with FFuses do
  begin
    for i := 0 to Count - 1 do
      TObject(Items[i]).Free;
    Clear;
  end;
end;

procedure TMetadata.SetMetadataVersion(AValue: integer);
begin
  if FMetadataVersion=AValue then Exit;
  FMetadataVersion:=AValue;
end;

procedure TMetadata.SetPgmBackupCmd(AValue: String);
begin
  if FPgmBackupCmd=AValue then Exit;
  FPgmBackupCmd:=AValue;
end;

procedure TMetadata.SetPgmReadEEPROMCmd(AValue: String);
begin
  if FPgmReadEEPROMCmd=AValue then Exit;
  FPgmReadEEPROMCmd:=AValue;
end;

procedure TMetadata.SetPgmReadFlashCmd(AValue: String);
begin
  if FPgmReadFlashCmd=AValue then Exit;
  FPgmReadFlashCmd:=AValue;
end;

procedure TMetadata.SetPgmTestCmd(AValue: String);
begin
  if FPgmTestCmd=AValue then Exit;
  FPgmTestCmd:=AValue;
end;

procedure TMetadata.SetPgmWriteEEPROMCmd(AValue: String);
begin
  if FPgmWriteEEPROMCmd=AValue then Exit;
  FPgmWriteEEPROMCmd:=AValue;
end;

procedure TMetadata.SetPgmWriteFlashCmd(AValue: String);
begin
  if FPgmWriteFlashCmd=AValue then Exit;
  FPgmWriteFlashCmd:=AValue;
end;

procedure TMetadata.SetVersion(AValue: String);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
end;

procedure TMetadata.LoadFromIni(Ini: TIniFile; const ASection: String);
var
  i: integer;
  lProg: TMetadataProgrammer;
  lFirm: TMetadataFirmware;
  lConf: TMetadataConfiguration;
  lFuse: TMetadataHFuse;
begin
  inherited;
  with Ini do
  begin
    FMetadataVersion := ReadInteger(ASection, 'MetadataVersion', 999);
    FVersion := ReadString(ASection, 'Version', '');
    FPgmWriteFlashCmd := ReadString(ASection, 'PgmWriteFlashCmd', '');
    FPgmWriteEEPROMCmd := ReadString(ASection, 'PgmWriteEEPROMCmd', '');
    FPgmReadFlashCmd := ReadString(ASection, 'PgmReadFlashCmd', '');
    FPgmReadEEPROMCmd := ReadString(ASection, 'PgmReadEEPROMCmd', '');
    FPgmBackupCmd := ReadString(ASection, 'PgmBackupCmd', '');
    FPgmTestCmd := ReadString(ASection, 'PgmTestCmd', '');
  end;
  with TStringList.Create do
  try
    CommaText := Ini.ReadString(ASection, 'Programmers', '');
    for i := 0 to Count - 1 do
    begin
      lProg := TMetadataProgrammer.Create(Self);
      FProgrammers.Add(lProg);
      lProg.LoadFromIni(Ini, Strings[i]);
    end;
    CommaText := Ini.ReadString(ASection, 'Targets', '');
    for i := 0 to Count - 1 do
    begin
      lFirm := TMetadataFirmware.Create;
      FFirmwares.Add(lFirm);
      lFirm.LoadFromIni(Ini, Strings[i]);
    end;
    CommaText := Ini.ReadString(ASection, 'Configurations', '');
    for i := 0 to Count - 1 do
    begin
      lConf := TMetadataConfiguration.Create;
      FConfigurations.Add(lConf);
      lConf.LoadFromIni(Ini, Strings[i]);
    end;
    CommaText := Ini.ReadString(ASection, 'FuseSettings', '');
    for i := 0 to Count - 1 do
    begin
      lFuse := TMetadataHFuse.Create;
      FFuses.Add(lFuse);
      lFuse.LoadFromIni(Ini, Strings[i]);
    end;
  finally
    Free;
  end;
end;

procedure TMetadata.LoadFromIni(Ini: TIniFile);
begin
  LoadFromIni(Ini, 'General');
end;

procedure TMetadata.GetProgrammers(SL: TStrings);
var
  i: integer;
begin
  SL.Clear;
  for i := 0 to FProgrammers.Count - 1 do
    SL.AddObject(TMetadataProgrammer(FProgrammers[i]).Name, FProgrammers[i]);
end;

procedure TMetadata.GetFirmwares(SL: TStrings);
var
  i: integer;
begin
  SL.Clear;
  for i := 0 to FFirmwares.Count - 1 do
    SL.AddObject(TMetadataFirmware(FFirmwares[i]).Name, FFirmwares[i]);
end;

procedure TMetadata.GetConfigurations(SL: TStrings);
var
  i: integer;
begin
  SL.Clear;
  for i := 0 to FConfigurations.Count - 1 do
    SL.AddObject(TMetadataConfiguration(FConfigurations[i]).Name, FConfigurations[i]);
end;

procedure TMetadata.GetFuses(SL: TStrings);
var
  i: integer;
begin
  SL.Clear;
  for i := 0 to FFuses.Count - 1 do
    SL.AddObject(TMetadataHFuse(FFuses[i]).Name, FFuses[i]);
end;


end.

