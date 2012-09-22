unit UUtils;

interface

uses
  Classes, SysUtils, StrUtils, Windows, Variants;

function IncSeparatedString(Container, NewString: String; Separator: String = ','): String;
function ClassIsChildOf(AClass: TObject; AParentClassName: String): Boolean;
function ClassTypeIsChildOf(AClass: TPersistentClass; AParentClassName: String): Boolean;
function GetComponentPath(AComponent: TComponent): String;

function ComponentToString(Component: TComponent): string;
function StringToComponent(Value: string; Component: TComponent = nil): TComponent;
procedure ReadVariant(Reader: TReader; var Value: Variant);
procedure WriteVariant(Writer: TWriter; Value: Variant);
procedure StreamToVariant(Stream : TStream; var V: Variant);
procedure VariantToStream(V: Variant; Stream : TStream);

function ParameterExists(Parameter: String): boolean;
function ParameterValue(const Parameter: String): String;

function VarArrayToString(V: Variant): String;
function StringToVarArray(S: String): Variant;

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

procedure ComponentToFile(Component: TComponent; aFile: string);
function FileToComponent(aFile: string; Component: TComponent = nil): TComponent;


implementation

var
  CmdLine: TStringList;

const
  GeneralSection = 'General';
  CommandLineParamsIdent = 'CommandLineParams';

resourcestring
  SStreamReadError = 'Stream read error';
  SStreamWriteError = 'Stream write error';

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

procedure _ObjectBinaryToText(Input, Output: TStream);
var
  NestingLevel: Integer;
  SaveSeparator: Char;
  Reader: TReader;
  Writer: TWriter;

  procedure WriteIndent;
  const
    Blanks: array[0..1] of Char = '  ';
  var
    I: Integer;
  begin
    for I := 1 to NestingLevel do Writer.Write(Blanks, SizeOf(Blanks));
  end;

  procedure WriteStr(const S: string);
  begin
    Writer.Write(S[1], Length(S));
  end;

  procedure NewLine;
  begin
    WriteStr(#13#10);
    WriteIndent;
  end;

  procedure ConvertValue; forward;

  procedure ConvertHeader;
  var
    ClassName, ObjectName: string;
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Reader.ReadPrefix(Flags, Position);
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;
    WriteIndent;
    if ffInherited in Flags then
      WriteStr('inherited ')
    else if ffInline in Flags then
      WriteStr('inline ')
    else
      WriteStr('object ');
    if ObjectName <> '' then
    begin
      WriteStr(ObjectName);
      WriteStr(': ');
    end;
    WriteStr(ClassName);
    if ffChildPos in Flags then
    begin
      WriteStr(' [');
      WriteStr(IntToStr(Position));
      WriteStr(']');
    end;
    WriteStr(#13#10);
  end;

  procedure ConvertBinary;
  const
    BytesPerLine = 32;
  var
    MultiLine: Boolean;
    I: Integer;
    Count: Longint;
    Buffer: array[0..BytesPerLine - 1] of Char;
    Text: array[0..BytesPerLine * 2 - 1] of Char;
  begin
    Reader.ReadValue;
    WriteStr('{');
    Inc(NestingLevel);
    Reader.Read(Count, SizeOf(Count));
    MultiLine := Count >= BytesPerLine;
    while Count > 0 do
    begin
      if MultiLine then NewLine;
      if Count >= 32 then I := 32 else I := Count;
      Reader.Read(Buffer, I);
      BinToHex(Buffer, Text, I);
      Writer.Write(Text, I * 2);
      Dec(Count, I);
    end;
    Dec(NestingLevel);
    WriteStr('}');
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue;
  const
    LineLength = 64;
  var
    I, J, K, L: Integer;
    S: string;
    W: WideString;
    LineBreak: Boolean;
  begin
    case Reader.NextValue of
      vaList:
        begin
          Reader.ReadValue;
          WriteStr('(');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            ConvertValue;
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteStr(')');
        end;
      vaInt8, vaInt16, vaInt32:
        WriteStr(IntToStr(Reader.ReadInteger));
      vaExtended:
        WriteStr(FloatToStrF(Reader.ReadFloat, ffFixed, 15, 18));
      vaSingle:
        WriteStr(FloatToStr(Reader.ReadSingle) + 's');
      vaCurrency:
        WriteStr(FloatToStr(Reader.ReadCurrency * 10000) + 'c');
      vaDate:
        WriteStr(FloatToStr(Reader.ReadDate) + 'd');
      vaWString:
        begin
          W := Reader.ReadWideString;
          L := Length(W);
          if L = 0 then WriteStr('''''') else
          begin
            I := 1;
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (W[I] >= ' ') and (W[I] <> '''') and (Ord(W[i]) <= 255) then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (W[I] < ' ') or (W[I] = '''') or
                    ((I - K) >= LineLength) or (Ord(W[i]) > 255);
                  if ((I - K) >= LineLength) then
                  begin
                    LineBreak := True;
                    if ByteType(W, I) = mbTrailByte then Dec(I);
                  end;
                  WriteStr('''');
                  while J < I do
                  begin
                    WriteStr(Char(W[J]));
                    Inc(J);
                  end;
                  WriteStr('''');
                end else
                begin
                  WriteStr('#');
                  WriteStr(IntToStr(Ord(W[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaString, vaLString:
        begin
          S := Reader.ReadString;
          L := Length(S);
          if L = 0 then WriteStr('''''') else
          begin
            I := 1;
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (S[I] >= ' ') and (S[I] <> '''') then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (S[I] < ' ') or (S[I] = '''') or
                    ((I - K) >= LineLength);
                  if ((I - K) >= LineLength) then
                  begin
                    LIneBreak := True;
                    if ByteType(S, I) = mbTrailByte then Dec(I);
                  end;
                  WriteStr('''');
                  Writer.Write(S[J], I - J);
                  WriteStr('''');
                end else
                begin
                  WriteStr('#');
                  WriteStr(IntToStr(Ord(S[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        WriteStr(Reader.ReadIdent);
      vaBinary:
        ConvertBinary;
      vaSet:
        begin
          Reader.ReadValue;
          WriteStr('[');
          I := 0;
          while True do
          begin
            S := Reader.ReadStr;
            if S = '' then Break;
            if I > 0 then WriteStr(', ');
            WriteStr(S);
            Inc(I);
          end;
          WriteStr(']');
        end;
      vaCollection:
        begin
          Reader.ReadValue;
          WriteStr('<');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            WriteStr('item');
            if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
            begin
              WriteStr(' [');
              ConvertValue;
              WriteStr(']');
            end;
            WriteStr(#13#10);
            Reader.CheckValue(vaList);
            Inc(NestingLevel);
            while not Reader.EndOfList do ConvertProperty;
            Reader.ReadListEnd;
            Dec(NestingLevel);
            WriteIndent;
            WriteStr('end');
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteStr('>');
        end;
      vaInt64:
        WriteStr(IntToStr(Reader.ReadInt64));
    end;
  end;

  procedure ConvertProperty;
  begin
    WriteIndent;
    WriteStr(Reader.ReadStr);
    WriteStr(' = ');
    ConvertValue;
    WriteStr(#13#10);
  end;

  procedure ConvertObject;
  begin
    ConvertHeader;
    Inc(NestingLevel);
    while not Reader.EndOfList do ConvertProperty;
    Reader.ReadListEnd;
    while not Reader.EndOfList do ConvertObject;
    Reader.ReadListEnd;
    Dec(NestingLevel);
    WriteIndent;
    WriteStr('end'#13#10);
  end;

begin
  NestingLevel := 0;
  Reader := TReader.Create(Input, 4096);
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Writer := TWriter.Create(Output, 4096);
    try
      Reader.ReadSignature;
      ConvertObject;
    finally
      Writer.Free;
    end;
  finally
    DecimalSeparator := SaveSeparator;
    Reader.Free;
  end;
end;

procedure ComponentToFile(Component: TComponent; aFile: string);
var
  BinStream: TFileStream;
begin
  BinStream := TFileStream.Create(aFile, fmOpenWrite or fmCreate);
  try
    BinStream.WriteComponent(Component);
  finally
    BinStream.Free
  end;
end;

function FileToComponent(aFile: string; Component: TComponent = nil): TComponent;
var
  BinStream: TFileStream;
begin
  BinStream := TFileStream.Create(aFile, fmOpenRead);
  try
    Result := BinStream.ReadComponent(Component);
  finally
    BinStream.Free
  end;
end;

function ComponentToString(Component: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

function StringToComponent(Value: string; Component: TComponent = nil): TComponent;
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result := BinStream.ReadComponent(Component);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure VariantToStream(V: Variant; Stream : TStream);
var
   i, Dim, Min, Max, Size, DimCount: integer;
   Buff: TVarData;
   StrStream: TStringStream;
begin
  DimCount := VarArrayDimCount(V);
  DimCount := 1; //!!! Any others not supportred
  Stream.Write(DimCount, SizeOf(DimCount));
  for Dim := 1 to DimCount do
  begin
   Min := VarArrayLowBound(V, Dim);
   Stream.Write(Min, SizeOf(Min));
   Max := VarArrayHighBound(V, Dim);
   Stream.Write(Max, SizeOf(Max));
  end;
  for Dim := 1 to DimCount do
  begin
   for i := Min to Max do
   begin
     StrStream := nil;
     Buff := TVarData(V[i]);
     Stream.Write(Buff, SizeOf(TVarData));
     if Buff.VType = varString then
     try
       StrStream := TStringStream.Create(String(Buff.VString));
       Size := StrStream.Size;
       Stream.Write(Size, SizeOf(Size));
       Stream.CopyFrom(StrStream, 0);
     finally
       StrStream.Free;
     end;
     if Buff.VType = varOleStr then
     try
       StrStream := TStringStream.Create(Buff.VOleStr);
       Size := StrStream.Size;
       Stream.Write(Size, SizeOf(Size));
       Stream.CopyFrom(StrStream, 0);
     finally
       StrStream.Free;
     end;
   end;
  end;
end;

procedure StreamToVariant(Stream : TStream; var V: Variant);
var
   i, Dim, Min, Max, Size, DimCount: integer;
   Buff: TVarData;
   StrStream: TStringStream;
   S: String;
   Bounds: array of Integer;
begin
  Stream.Read(DimCount, SizeOf(DimCount));
  SetLength(Bounds, DimCount shl 1);
  for Dim := 0 to DimCount - 1  do
  begin
   Stream.Read(Min, SizeOf(Min));
   Stream.Read(Max, SizeOf(Max));
   Bounds[(Dim shl 1)] := Min;
   Bounds[(Dim shl 1)+1] := Max;
  end;
  V := VarArrayCreate(Bounds, varVariant);
  for Dim := 1 to DimCount do
  begin
   for i := Min to Max do
   begin
     StrStream := nil;
     V[i] := Unassigned;
     Stream.Read(Buff, SizeOf(TVarData));
     case Buff.VType of
        varNull     : V[i] := NULL;
        varSmallint : V[i] := Buff.VSmallint;
        varInteger  : V[i] := Buff.VInteger;
        varSingle   : V[i] := Buff.VSingle;
        varDouble   : V[i] := Buff.VDouble;
        varCurrency : V[i] := Buff.VCurrency;
        varDate     : V[i] := Buff.VDate;
        varBoolean  : V[i] := Buff.VBoolean;
        varOleStr, varString:
           try
             StrStream := TStringStream.Create(S);
             Stream.Read(Size, SizeOf(Size));
             StrStream.CopyFrom(Stream, Size);
             StrStream.Seek(0, soFromBeginning);
             V[i] := StrStream.DataString;
           finally
             StrStream.Free;
           end;
     end;
   end;
  end;
end;

type
  TWriter_ = class(TWriter);
  TReader_ = class(TReader);

procedure WriteVariant(Writer: TWriter; Value: Variant);
var
  VType: Integer;
begin
  with TWriter_(Writer) do
  begin
    if VarIsArray(Value) then raise EWriteError.Create(SStreamWriteError);
    VType := VarType(Value);
    case VType and varTypeMask of
      varEmpty: WriteValue(vaNil);
      varNull: WriteValue(vaNull);
      varOleStr: WriteWideString(Value);
      varString: WriteString(Value);
      varByte, varSmallInt, varInteger: WriteInteger(Value);
      varSingle: WriteSingle(Value);
      varDouble: WriteFloat(Value);
      varCurrency: WriteCurrency(Value);
      varDate: WriteDate(Value);
      varBoolean:
        if Value then
          WriteValue(vaTrue) else
          WriteValue(vaFalse);
    else
      try
        WriteString(Value);
      except
        raise EWriteError.Create(SStreamWriteError);
      end;
    end;
  end;
end;


procedure ReadVariant(Reader: TReader; var Value: Variant);
const
  ValTtoVarT: array [TValueType] of TVarType =
    (varError,    // vaNull        { set via NULL }
     varError,    // vaList        { NOT SUPPORTED }
     varByte,     // vaInt8
     varSmallInt, // vaInt16
     varInteger,  // vaInt32
     varDouble,   // vaExtended
     varError,    // vaString      { set via ReadString }
     varError,    // vaIdent       { NOT SUPPORTED }
     varBoolean,  // vaFalse
     varBoolean,  // vaTrue
     varError,    // vaBinary      { set via ReadCustomVariant }
     varError,    // vaSet         { NOT SUPPORTED }
     varError,    // vaLString     { set via ReadString }
     varError,    // vaNil         { set via initial clear }
     varError,    // vaCollection  { NOT SUPPORTED }
     varSingle,   // vaSingle
     varCurrency, // vaCurrency
     varDate,     // vaDate
     varError,    // vaWString     { set via ReadWideString }
     varInt64,    // vaInt64
     varError);   // vaUTF8String  { set via Read(Wide)String }
var
  ValType: TValueType;
begin
  with TReader_(Reader) do
  begin
    ValType := NextValue;
    case ValType of
      vaNil, vaNull:
      begin
        if ReadValue = vaNil then
          VarClear(Value) else
          Value := NULL;
      end;
      vaInt8: TVarData(Value).VByte := Byte(ReadInteger);
      vaInt16: TVarData(Value).VSmallint := Smallint(ReadInteger);
      vaInt32: TVarData(Value).VInteger := ReadInteger;
      vaExtended: TVarData(Value).VDouble := ReadFloat;
      vaSingle: TVarData(Value).VSingle := ReadSingle;
      vaCurrency: TVarData(Value).VCurrency := ReadCurrency;
      vaDate: TVarData(Value).VDate := ReadDate;
      vaString, vaLString: Value := ReadString;
      vaWString: Value := ReadWideString;
      vaFalse, vaTrue: TVarData(Value).VBoolean := ReadValue = vaTrue;
    else
      raise EReadError.Create(SStreamReadError);
    end;
    TVarData(Value).VType := ValTtoVarT[ValType];
  end;
end;

function VarArrayToString(V: Variant): String;
var
  i: integer;
begin
  Result := '';
  if VarIsEmpty(V) or VarIsNull(V) then
    Exit;
  if VarIsArray(V) then
    for i := 0 to VarArrayHighBound(V, 1) do
      if (not VarIsNull(V[i])) and (not VarIsEmpty(V[i])) then
      begin
        if VarType(V[i]) <> varDate then
          Result := IncSeparatedString(Result, V[i])
        else
          Result := IncSeparatedString(Result, QuotedStr(DateTimeToStr(VarToDateTime(V[i]))))
      end
      else  
  else
    Result := V;
end;

function StringToVarArray(S: String): Variant;
var
  I: integer;
begin
  VarClear(Result);
  with TStringList.Create do
  try
    CommaText := S;
    if Count = 0 then
      exit;
    Result := VarArrayCreate([0, Count-1], varVariant);
    for I := 0 to Count-1 do
      Result[I] := Strings[I];
  finally
    Free;
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

