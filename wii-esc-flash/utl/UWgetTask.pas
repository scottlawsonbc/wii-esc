unit UWgetTask;

interface

uses
  Classes, SysUtils, regex, process, ExtCtrls;

const
  WGET_CMD_LINE_PASS           =  '$PATH$wget.exe -c --tries=3 --progress=dot --http-user="$USER$" --http-passwd="$PASS$" "$URL$"';
  WGET_CMD_LINE_CERT           =  '$PATH$wget.exe -c --tries=3 --progress=dot --no-check-certificate --certificate="$CERT$" --private-key="$KEY$" "$URL$"';
  WGET_OUT_PARSE          = '(\d*)([KM])(\s[.]{10}){5}\s*(\d*)\%\s*([0-9.]*)([KM])';
  WGET_OUT_PARSE_SIZE     = 1;
  WGET_OUT_PARSE_SIZEM    = 2;
  WGET_OUT_PARSE_PERCENT  = 4;
  WGET_OUT_PARSE_SPEED    = 5;
  WGET_OUT_PARSE_SPEEDM   = 6;

type
  TWgetTask = class(TComponent)
  private
    FDownLoad: TProcess;
    FReadTimer: TTimer;
    FURL: String;
    FUsername: String;
    FPassword: String;
    FDestFolder: String;
    FDownloading: boolean;
    FUseCertificate: boolean;
    FOnFinish: TNotifyEvent;
    FSuccess: boolean;
    FErrorMessage: String;
    FTotalSize: integer;
    FDownloadedSize: integer;
    FSpeed: Double;
    FSpeedTotal: Double;
    FSpeedCnt: integer;
    FPercent: integer;
    FOnProgress: TNotifyEvent;
    FWGetPath: String;
    FParser: TRegexEngine;
    FOutFileName: String;
    FCertificate: String;
    FPrivateKey: String;
    function ExpandMacro(const AStr: String): String;
    procedure SetURL(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetUsername(const Value: String);
    procedure SetDestFolder(const Value: String);
    procedure TaskRead(Sender: TObject; const S: String;  const StartsOnNewLine: Boolean);
    procedure TaskTerminate(Sender: TObject; ExitCode: Cardinal);
    procedure SetOnFinish(const Value: TNotifyEvent);
    procedure DoFinish;
    procedure SetOnProgress(const Value: TNotifyEvent);
    procedure DoProgress;
    procedure ClearInfo;
    procedure SetWGetPath(const Value: String);
    function GetFileName: String;
    procedure SetOutFileName(const Value: String);
    procedure SetupProxy;
  protected
    property DownLoad: TProcess read FDownLoad;
    property OutFileName: String read FOutFileName write SetOutFileName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property WGetPath: String read FWGetPath write SetWGetPath;
    property URL: String read FURL write SetURL;
    property Username: String read FUsername write SetUsername;
    property Password: String read FPassword write SetPassword;
    property DestFolder: String read FDestFolder write SetDestFolder;
    property Success: boolean read FSuccess;
    property ErrorMessage: String read FErrorMessage;
    property TotalSize: integer read FTotalSize;
    property DownloadedSize: integer read FDownloadedSize;
    property Speed: Double read FSpeed;
    property Percent: integer read FPercent;
    property FileName: String read GetFileName;
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;
    property OnProgress: TNotifyEvent read FOnProgress write SetOnProgress;
    property UseCertificate: Boolean read FUseCertificate write FUseCertificate;
    property Certificate: String read FCertificate write FCertificate;
    property PrivateKey: String read FPrivateKey write FPrivateKey;
  end;

  TWgetTaskSlot = class(TComponent)
  private
    FList: TList;
    FUsername: String;
    FPassword: String;
    FDestFolder: String;
    FOnProgress: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FWGetPath: String;
    function GetCount: integer;
    function GetItems(AIndex: integer): TWgetTask;
    procedure SetPassword(const Value: String);
    procedure SetUsername(const Value: String);
    procedure SetDestFolder(const Value: String);
    function GetDownloadedSize: integer;
    function GetSpeed: Double;
    procedure DoProgress(Sender: TObject);
    procedure DoFinish(Sender: TObject);
    procedure SetOnFinish(const Value: TNotifyEvent);
    procedure SetOnProgress(const Value: TNotifyEvent);
    function GetDownloading: boolean;
    procedure SetWGetPath(const Value: String);
  public
    constructor Create(AOwner: TComponent; ACnt: integer); reintroduce;
    destructor Destroy; override;
    function GetFreeTask: TWgetTask;
    procedure Stop;
    property Count: integer read GetCount;
    property Items[AIndex: integer]: TWgetTask read GetItems; default;
    property Username: String read FUsername write SetUsername;
    property Password: String read FPassword write SetPassword;
    property DestFolder: String read FDestFolder write SetDestFolder;
    property DownloadedSize: integer read GetDownloadedSize;
    property Speed: Double read GetSpeed;
    property Downloading: boolean read GetDownloading;
    property WGetPath: String read FWGetPath write SetWGetPath;
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;
    property OnProgress: TNotifyEvent read FOnProgress write SetOnProgress;
  end;

implementation

constructor TWgetTask.Create(AOwner: TComponent);
begin
  inherited;
  FReadTimer := TTimer.Create(Self);
  FReadTimer.Interval := 1000;
  FDownLoad := TProcess.Create(Self);
  FUseCertificate := False;
  with FDownload do
  begin
    Priority := ppIdle;
    Options := [poUsePipes, poStderrToOutPut];
    //StartupInfo.DefaultSize := False;
    //StartupInfo.ShowWindow := swHide;
    //StartupInfo.DefaultWindowState := False;
    //ConsoleOptions := [coOwnerData, coRedirect];
    //OnTerminate := TaskTerminate;
    //OnRead := TaskRead;
  end;
  FParser := TRegexEngine.Create(WGET_OUT_PARSE);
end;

destructor TWgetTask.Destroy;
begin
  if FDownloading then
    Stop;
  FDownLoad.Free;
  FParser.Free;;
  inherited;
end;

procedure TWgetTask.ClearInfo;
begin
  FPercent := 0;
  FTotalSize := 0;
  FDownloadedSize := 0;
  FSpeed := 0;
  FErrorMessage := '';
  FSpeedTotal := 0;
  FSpeedCnt := 0;
end;

procedure TWgetTask.SetupProxy;
begin
end;

procedure TWgetTask.Start;
var
  i: integer;
begin
  if not FDownloading then
  begin
    with FDownload do
    begin
      if UseCertificate then
        CommandLine := ExpandMacro(WGET_CMD_LINE_CERT)
      else
        CommandLine := ExpandMacro(WGET_CMD_LINE_PASS);
        
      if FDestFolder <> '' then
      begin
        if not DirectoryExists(FDestFolder) then
          ForceDirectories(FDestFolder);
        CurrentDirectory := FDestFolder;
      end
      else
        CurrentDirectory := ExtractFilePath(ParamStr(0));
      if FOutFileName <> '' then
        CommandLine := CommandLine + Format(' -O "%s"', [FOutFileName]);
      Environment.Clear;
      FDownLoad.Environment.Clear;
      // Load all variables to string list
      for i := 0 to GetEnvironmentVariableCount - 1 do
        FDownload.Environment.Add(GetEnvironmentString(i));
      SetupProxy;
      Execute;
      ClearInfo;
      FDownloading := True;
    end;
  end;
end;

procedure TWgetTask.TaskRead(Sender: TObject; const S: String; const StartsOnNewLine: Boolean);
var
  TmpS: String;
  Tmp: Double;
begin
{
  if Pos('Bad port number.', S) <> 0 then
    FErrorMessage := Copy(S, Pos('Bad port number.', S), 10240);
  if Pos('Unsupported scheme.', S) <> 0 then
    FErrorMessage := Copy(S, Pos('Unsupported scheme.', S), 10240);
  if Pos('ERROR', S) <> 0 then
    FErrorMessage := Copy(S, Pos('ERROR', S), 10240) + ' (File cannot be found.)';
  if Pos('failed:', S) <> 0 then
    FErrorMessage := Copy(S, Pos('failed:', S), 10240);
  if Pos('Authorization failed.', S) <> 0 then
    FErrorMessage := 'Authorization failed.';
  if Pos('HTTP request sent, awaiting response...', S) <> 0 then
    FErrorMessage := 'No response.';
  if Pos('OpenSSL: error', S) <> 0 then
    FErrorMessage := S;
  if Pos('Length:', S) <> 0 then
  begin
    TmpS := Copy(S, Pos('Length:', S) + 8, 10240);
    TmpS := Copy(TmpS, 1, Pos(' ', TmpS) - 1);
    if (TmpS <> '') and (TmpS <> 'unspecified')  then
    begin
      TmpS := StringReplace(TmpS, ',', '', [rfReplaceAll]);
      TmpS := StringReplace(TmpS, ' ', '', [rfReplaceAll]);
      TmpS := StringReplace(TmpS, ThousandSeparator, '', [rfReplaceAll]);
      FTotalSize := StrToIntDef(TmpS, 0);
    end  
    else
      FTotalSize := 0;
  end;
  FParser.Subject := S;
  if FParser.Match then
  begin
    FDownloadedSize := StrToIntDef(FParser.SubExpressions[WGET_OUT_PARSE_SIZE], 0);
    if FParser.SubExpressions[WGET_OUT_PARSE_SIZEM] = 'K' then
      FDownloadedSize := FDownloadedSize * 1024
    else
    if FParser.SubExpressions[WGET_OUT_PARSE_SIZEM] = 'M' then
      FDownloadedSize := FDownloadedSize * 1024 * 1024;
    FPercent := StrToIntDef(FParser.SubExpressions[WGET_OUT_PARSE_PERCENT], 0);
    Tmp := StrToFloatDef(StringReplace(FParser.SubExpressions[WGET_OUT_PARSE_SPEED], '.', DecimalSeparator, [rfReplaceAll]), 0);
    if FParser.SubExpressions[WGET_OUT_PARSE_SPEEDM] = 'K' then
      Tmp := Tmp / 1024;
    FSpeedTotal := FSpeedTotal +  Tmp;
    Inc(FSpeedCnt);
    FSpeed := FSpeedTotal / FSpeedCnt;
  end;
  DoProgress;
}
end;

procedure TWgetTask.TaskTerminate(Sender: TObject; ExitCode: Cardinal);
begin
  FSuccess := ExitCode = 0;
  FDownloading := False;
  DoFinish;
end;

function TWgetTask.ExpandMacro(const AStr: String): String;
begin
  Result := AStr;
  //By Password
  Result := StringReplace(Result, '$USER$', FUsername, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$PASS$', FPassword, [rfReplaceAll, rfIgnoreCase]);
  //By Certificate
  Result := StringReplace(Result, '$CERT$', FCertificate, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$KEY$', FPrivateKey, [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '$URL$',  FURL,      [rfReplaceAll, rfIgnoreCase]);


  if FWGetPath <> '' then
    Result := StringReplace(Result, '$PATH$',  IncludeTrailingPathDelimiter(FWGetPath),      [rfReplaceAll, rfIgnoreCase])
  else
    Result := StringReplace(Result, '$PATH$',  '',     [rfReplaceAll, rfIgnoreCase])
end;

procedure TWgetTask.SetURL(const Value: String);
begin
  FURL := Value;
end;

procedure TWgetTask.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TWgetTask.SetUsername(const Value: String);
begin
  FUsername := Value;
end;

procedure TWgetTask.SetDestFolder(const Value: String);
begin
  FDestFolder := Value;
end;

procedure TWgetTask.SetOnFinish(const Value: TNotifyEvent);
begin
  FOnFinish := Value;
end;

procedure TWgetTask.DoFinish;
begin
  if Assigned(FOnFinish) and (not (csDestroying in ComponentState)) then
    FOnFinish(Self);
end;

procedure TWgetTask.DoProgress;
begin
  if Assigned(FOnProgress) and (not (csDestroying in ComponentState)) then
    FOnProgress(Self);
end;

procedure TWgetTask.SetOnProgress(const Value: TNotifyEvent);
begin
  FOnProgress := Value;
end;

procedure TWgetTask.Stop;
begin
  if FDownloading then
  begin
    FDownLoad.Terminate(-1);
    FDownloading := False;
    DoProgress;
  end;
end;

{ TWgetTaskSlot }

constructor TWgetTaskSlot.Create(AOwner: TComponent; ACnt: integer);
var
  i: integer;
  lTask: TWgetTask;
begin
  inherited Create(AOwner);
  FList := TList.Create;
  for i := 0 to ACnt - 1 do
  begin
    lTask := TWgetTask.Create(Self);
    lTask.OnFinish := DoFinish;
    lTask.OnProgress := DoProgress;
    FList.Add(lTask);
  end;
end;

destructor TWgetTaskSlot.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TWgetTaskSlot.DoFinish(Sender: TObject);
begin
  if Assigned(FOnFinish) then
    FOnFinish(Sender);
end;

procedure TWgetTaskSlot.DoProgress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

function TWgetTaskSlot.GetCount: integer;
begin
  Result := FList.Count;
end;

function TWgetTaskSlot.GetDownloadedSize: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
   if Items[i].FDownloading then
     Inc(Result, Items[i].DownloadedSize);
end;

function TWgetTaskSlot.GetDownloading: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
   if Items[i].FDownloading then
   begin
     Result := True;
     Break;
   end;
end;

function TWgetTaskSlot.GetFreeTask: TWgetTask;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
   if not Items[i].FDownloading then
   begin
     Result := Items[i];
     Break;
   end;
end;

function TWgetTaskSlot.GetItems(AIndex: integer): TWgetTask;
begin
  Result := TWgetTask(FList[AIndex]);
end;

function TWgetTaskSlot.GetSpeed: Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
   if Items[i].FDownloading then
     Result := Result +  Items[i].Speed;
end;

procedure TWgetTaskSlot.SetDestFolder(const Value: String);
var
  i: integer;
begin
  FDestFolder := Value;
  for i := 0 to Count - 1 do
    Items[i].DestFolder := FDestFolder;
end;

procedure TWgetTaskSlot.SetOnFinish(const Value: TNotifyEvent);
begin
  FOnFinish := Value;
end;

procedure TWgetTaskSlot.SetOnProgress(const Value: TNotifyEvent);
begin
  FOnProgress := Value;
end;

procedure TWgetTaskSlot.SetPassword(const Value: String);
var
  i: integer;
begin
  FPassword := Value;
  for i := 0 to Count - 1 do
    Items[i].Password := FPassword;
end;

procedure TWgetTaskSlot.SetUsername(const Value: String);
var
  i: integer;
begin
  FUsername := Value;
  for i := 0 to Count - 1 do
    Items[i].Username := FUsername;
end;

procedure TWgetTaskSlot.SetWGetPath(const Value: String);
var
  i: integer;
begin
  FWGetPath := Value;
  for i := 0 to Count - 1 do
    Items[i].WGetPath := FWGetPath;
end;

procedure TWgetTaskSlot.Stop;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Stop;
end;

procedure TWgetTask.SetWGetPath(const Value: String);
begin
  FWGetPath := Value;
end;

function TWgetTask.GetFileName: String;
var
  Idx: integer;
begin
  Result := '';
  Idx := Length(FUrl);
  while (Idx > 0) and (not (FUrl[Idx] in ['/', '\'])) do
    Dec(Idx);
  if Idx > 0 then
   Result := Copy(FUrl, Succ(Idx), 10240);
end;

procedure TWgetTask.SetOutFileName(const Value: String);
begin
  FOutFileName := Value;
end;

end.
