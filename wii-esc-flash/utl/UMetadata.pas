unit UMetadata;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles;

type

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
    procedure SetName(AValue: String);
    procedure SetPort(AValue: String);
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Name: String read FName write SetName;
    property Port: String read FPort write SetPort;
    property BaudRates: TStringList read FBaudRates;
  end;

  { TMetadataFirmware }

  TMetadataFirmware = class(TMetadataBase)
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
    FProgrammers: TList;
    FFirmwares: TList;
  protected
    procedure LoadFromIni(Ini: TIniFile; const ASection: String); override; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromIni(Ini: TIniFile); overload;
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
procedure TMetadataProgrammer.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TMetadataProgrammer.SetPort(AValue: String);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
end;

procedure TMetadataProgrammer.LoadFromIni(Ini: TIniFile; const ASection: String);
begin
  inherited;
  with Ini do
  begin
    FBaudRates.CommaText := ReadString(ASection, 'Speed', '');
    FPort := ReadString(ASection, 'Port', '');
    FName := ASection;
  end;
end;

constructor TMetadataProgrammer.Create;
begin
  inherited Create;
  FBaudRates := TStringList.Create;
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

procedure TMetadataFirmware.LoadFromIni(Ini: TIniFile; const ASection: String);
begin
  inherited;
  with Ini do
  begin
    FName := ASection;
    FURL := ReadString(ASection, 'URL', '');
    FDescription := _ReadString(Ini, ASection, 'Description', '')
  end;
end;

{ TMetadata }
constructor TMetadata.Create;
begin
  inherited Create;
  FProgrammers := TList.Create;
  FFirmwares := TList.Create;
end;

destructor TMetadata.Destroy;
begin
  Clear;
  FProgrammers.Free;
  FFirmwares.Free;
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
end;

procedure TMetadata.LoadFromIni(Ini: TIniFile; const ASection: String);
var
  i: integer;
  lProg: TMetadataProgrammer;
  lFirm: TMetadataFirmware;
begin
  inherited;
  with TStringList.Create do
  try
    CommaText := Ini.ReadString(ASection, 'Programmers', '');
    for i := 0 to Count - 1 do
    begin
      lProg := TMetadataProgrammer.Create;
      FProgrammers.Add(lProg);
      lProg.LoadFromIni(Ini, Strings[i]);
    end;
    CommaText := Ini.ReadString(ASection, 'Targets', '');
    for i := 0 to Count - 1 do
    begin
      lFirm := TMetadataFirmware.Create;
      FProgrammers.Add(lFirm);
      lFirm.LoadFromIni(Ini, Strings[i]);
    end;
  finally
    Free;
  end;
end;

procedure TMetadata.LoadFromIni(Ini: TIniFile);
begin
  LoadFromIni(Ini, 'General');
end;


end.

