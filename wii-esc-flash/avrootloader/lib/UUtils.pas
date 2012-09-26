unit UUtils;


interface

uses
  Classes, SysUtils, StrUtils, Windows, Variants;

function IncSeparatedString(Container, NewString: String; Separator: String = ','): String;
function ClassIsChildOf(AClass: TObject; AParentClassName: String): Boolean;
function ClassTypeIsChildOf(AClass: TPersistentClass; AParentClassName: String): Boolean;
function GetComponentPath(AComponent: TComponent): String;

function ParameterExists(Parameter: String): boolean;
function ParameterValue(const Parameter: String): String;

function CommandLineParams: TStringList;

function ModuleFileName: string;
function ModuleName: string;
function ComputerName: string;
function GetWindowsVersion: string;

function MakeStr(C: Char; N: Integer): string;
function MS(C: Char; N: Integer): string;
function NPos(const C: string; S: string; N: Integer): Integer;
function AddChar(C: Char; const S: string; N: Integer): string;
function AddCharR(C: Char; const S: string; N: Integer): string;
function LeftStr(const S: string; N: Integer): string;
function RightStr(const S: string; N: Integer): string;
function CompStr(const S1, S2: string): Integer;
function Csv(const AText: String; const APos: Integer; const ASeparators: TSysCharSet = ['\']): String;
function Incs(const Container, NewString: String; const Separator: String): String;


implementation

var
  CmdLine: TStringList;

function Incs(const Container, NewString: String; const Separator: String): String;
begin
  if Trim(Container) <> '' then
    if Trim(NewString) <> '' then
      Result := Container + Separator + NewString
    else
      Result := Container
  else
    Result := Trim(NewString);
end;

function Csv(const AText: String; const APos: Integer; const ASeparators: TSysCharSet = ['\']): String;
var
  SL: TStringList;
begin
  Result := '';
  SL := TSTringList.Create;
  try
    ExtractStrings(ASeparators, [#32], PChar(AText), SL);
    if APos < SL.Count then
      Result := SL[APos];
  finally
    SL.Free;
  end;
end;

function ParameterExists(Parameter: String): boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to CommandLineParams.Count-1 do
    if SameText(CommandLineParams[I], Parameter) then
    begin
      Result := True;
      Break;
    end;
end;

function ParameterValue(const Parameter: String): String;
begin
  Result := CommandLineParams.Values[Parameter];
end;

function IncSeparatedString;
begin
  if Trim(Container) <> '' then
    if Trim(NewString) <> '' then
      Result := Container + Separator + NewString
    else
      Result := Container
  else
    Result := Trim(NewString);
end;

function GetComponentPath;
begin
  Result := '';
  try
    if not Assigned(AComponent) then
      Exit;
    while AComponent <> nil do
    begin
      Result := IncSeparatedString(Format('[%s]%s', [AComponent.ClassName, AComponent.Name]), Result, '.');
      AComponent := AComponent.Owner;
    end;
  except
  end;  
end;

function ClassIsChildOf;
var
  ClassRef: TClass;
begin
  Result := False;

  if not Assigned(AClass) then
    Exit;

  ClassRef := AClass.ClassType;
  while ClassRef <> nil do
  begin
    if SameText(AParentClassName, ClassRef.ClassName) then
    begin
      Result := True;
      break;
    end;
    ClassRef := ClassRef.ClassParent;
  end;
end;

function ClassTypeIsChildOf;
var
  ClassRef: TClass;
begin
  Result := False;

  ClassRef := AClass;
  while ClassRef <> nil do
  begin
    if SameText(AParentClassName, ClassRef.ClassName) then
    begin
      Result := True;
      break;
    end;
    ClassRef := ClassRef.ClassParent;
  end;
end;

function CommandLineParams: TStringList;
var
  I: Integer;
begin
  if not Assigned(CmdLine) then
  begin
    CmdLine := TStringList.Create;
    for I := CmdLine.Count-1 downto 0 do
      if Trim(CommandLineParams[I]) = '' then
        CmdLine.Delete(I);
    for I := 1 to ParamCount do
      CmdLine.Add(ParamStr(I));
  end;
  Result := CmdLine;
end;

function ModuleFileName: String;
begin
  SetLength(Result, 666);
  SetLength(Result, Windows.GetModuleFileName(HInstance, PChar(Result), Length(Result)));
end;

function ModuleName: String;
begin
  Result := ExtractFileName(ChangeFileExt(ModuleFileName,''));
  Result := ModuleFileName;
end;

function ComputerName: String;
var
  nSize: Cardinal;
begin
  nSize := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, nSize);
  Windows.GetComputerName(PChar(Result), nSize);
  SetLength(Result, nSize);
end;

function GetWindowsVersion: string;
{$IFDEF WIN32}
const
  sWindowsVersion = 'Windows %s %d.%.2d.%.3d %s';
var
  Ver: TOsVersionInfo;
  Platform: string[4];
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s: Platform := '32s';
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          dwBuildNumber := dwBuildNumber and $0000FFFF;
          if (dwMajorVersion > 4) or ((dwMajorVersion = 4) and
            (dwMinorVersion >= 10)) then Platform := '98'
          else Platform := '95';
        end;
      VER_PLATFORM_WIN32_NT: Platform := 'NT';
    end;
    Result := Trim(Format(sWindowsVersion, [Platform, dwMajorVersion,
      dwMinorVersion, dwBuildNumber, szCSDVersion]));
  end;
end;
{$ELSE}
const
  sWindowsVersion = 'Windows%s %d.%d';
  sNT: array[Boolean] of string[3] = ('', ' NT');
var
  Ver: Longint;
begin
  Ver := GetVersion;
  Result := Format(sWindowsVersion, [sNT[not Boolean(HiByte(LoWord(Ver)))],
    LoByte(LoWord(Ver)), HiByte(LoWord(Ver))]);
end;
{$ENDIF WIN32}

function MakeStr(C: Char; N: Integer): string;
begin
  if N < 1 then Result := ''
  else begin
{$IFNDEF WIN32}
    if N > 255 then N := 255;
{$ENDIF WIN32}
    SetLength(Result, N);
    FillChar(Result[1], Length(Result), C);
  end;
end;

function MS(C: Char; N: Integer): string;
begin
  Result := MakeStr(C, N);
end;

function NPos(const C: string; S: string; N: Integer): Integer;
var
  I, P, K: Integer;
begin
  Result := 0;
  K := 0;
  for I := 1 to N do begin
    P := Pos(C, S);
    Inc(K, P);
    if (I = N) and (P > 0) then begin
      Result := K;
      Exit;
    end;
    if P > 0 then Delete(S, 1, P)
    else Exit;
  end;
end;

function AddChar(C: Char; const S: string; N: Integer): string;
begin
  if Length(S) < N then
    Result := MakeStr(C, N - Length(S)) + S
  else Result := S;
end;

function AddCharR(C: Char; const S: string; N: Integer): string;
begin
  if Length(S) < N then
    Result := S + MakeStr(C, N - Length(S))
  else Result := S;
end;

function LeftStr(const S: string; N: Integer): string;
begin
  Result := AddCharR(' ', S, N);
end;

function RightStr(const S: string; N: Integer): string;
begin
  Result := AddChar(' ', S, N);
end;

function CompStr(const S1, S2: string): Integer;
begin
{$IFDEF WIN32}
  Result := CompareString(GetThreadLocale, SORT_STRINGSORT, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) - 2;
{$ELSE}
  Result := CompareStr(S1, S2);
{$ENDIF}
end;


end.

