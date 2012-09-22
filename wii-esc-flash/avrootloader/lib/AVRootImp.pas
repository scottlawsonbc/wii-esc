unit AVRootImp;

interface

uses SysUtils, Classes, AVRootIntf, Windows;

type
  TAVRootApplication = class(TComponent, IApplication)
  private
    FFlashFileName: TFileName;
    FEEPROMFileName: TFileName;
    FBootSign: String;
    FPassword: String;
    FBaudRate: integer;
    FPort: String;
    FLoader: IAVRootloader;
    FVerbose: boolean;

    function ProcessMessages: Bool; stdcall;
    procedure Changed; stdcall;
    procedure Output(const Msg: WideString; Code: Integer); stdcall;

    function GetFLASHFileName: WideString; stdcall;
    function GetEEPROMFileName: WideString; stdcall;
    function GetACYFileName: WideString; stdcall;
    function GetPassword: WideString; stdcall;
    function GetBootSign: WideString; stdcall;
    function GetTimeouts: TTimeouts; stdcall;
    function GetAppCmd: WideString; stdcall;
    function GetAppCmdResponse: WideString; stdcall;
    function GetAppVersion(Masked: Bool = False): Integer; stdcall;
    function GetACYInfo: WideString; stdcall;

    function OpenCommunication(Index: Integer): ICOM; stdcall;
    procedure SetFlashFileName(const Value: TFileName);
    procedure SetEEPROMFileName(const Value: TFileName);
    procedure SetBootSign(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetBaudRate(const Value: integer);
    procedure SetPort(const Value: String);
    procedure SetVerbose(const Value: boolean);
  public
    procedure Init;
    procedure Connect;
    procedure Disconnect;
    procedure Flash;
    procedure EraseEEPROM;
    procedure EraseFlash;
    property FlashFileName: TFileName read FFlashFileName write SetFlashFileName;
    property EEPROMFileName: TFileName read FEEPROMFileName write SetEEPROMFileName;
    property BootSign: String read FBootSign write SetBootSign;
    property Password: String read FPassword write SetPassword;
    property Port: String read FPort write SetPort;
    property BaudRate: integer read FBaudRate write SetBaudRate;
    property Verbose: boolean read FVerbose write SetVerbose;
  end;

var
  App: TAVRootApplication;


implementation


{ TAVRootApplication }

procedure TAVRootApplication.Changed;
begin

end;

procedure TAVRootApplication.Connect;
begin
  FLoader.DoConnect;
end;

procedure TAVRootApplication.Disconnect;
begin
  FLoader.DoDisconnect;
end;

procedure TAVRootApplication.EraseEEPROM;
begin
  FLoader.DoEraseEeprom;
end;

procedure TAVRootApplication.EraseFlash;
begin
  FLoader.DoEraseFlash;
end;

procedure TAVRootApplication.Flash;
begin
  FLoader.DoProgram(True, True);
end;

function TAVRootApplication.GetACYFileName: WideString;
begin
  Result := '';
end;

function TAVRootApplication.GetACYInfo: WideString;
begin
  Result := '';
end;

function TAVRootApplication.GetAppCmd: WideString;
begin
  Result := '';
end;

function TAVRootApplication.GetAppCmdResponse: WideString;
begin
  Result := '';
end;

function TAVRootApplication.GetAppVersion(Masked: Bool): Integer;
begin
  Result := 1;
end;

function TAVRootApplication.GetBootSign: WideString;
begin
  Result := FBootSign;
end;

function TAVRootApplication.GetEEPROMFileName: WideString;
begin
  Result := FEEPROMFileName;
end;

function TAVRootApplication.GetFLASHFileName: WideString;
begin
  Result := FFlashFileName;
end;

function TAVRootApplication.GetPassword: WideString;
begin
  Result := FPassword;
end;

function TAVRootApplication.GetTimeouts: TTimeouts;
begin
  with Result do
  begin
    Baudrate := FBaudRate;
    Connect := 100;
    Base := 25;
    Erase := 100;
    Flash := 150;
    Eeprom := 10;
    Buffer := 1;
    AppCmd := 0;
    KeepAlive := 250;
    MaxPacketSize := 0;
    RTSPulse := 0;
    RTSInterval := 0;
    ConnectTrials := 0;
    Options := integer(FVerbose);
  end;
end;

procedure TAVRootApplication.Init;
begin
  FLoader := AVRootIntf.OpenAVRootloader(Self);
end;

function TAVRootApplication.OpenCommunication(Index: Integer): ICOM;
begin
  Result := AVRootIntf.OpenCOM(FPort, Self);
  IF Assigned(Result) then
    with Result do
    begin
      SetParams(FBaudRate);
      SetDTR(False);
      Sleep(500);
      SetDTR(True);
    end;
end;

procedure TAVRootApplication.Output(const Msg: WideString; Code: Integer);
begin
  Writeln('AVRootLoader:  ', Msg);
  System.Flush(System.Output);
end;

function TAVRootApplication.ProcessMessages: Bool;
begin
  Result := False;
end;

procedure TAVRootApplication.SetBaudRate(const Value: integer);
begin
  FBaudRate := Value;
end;

procedure TAVRootApplication.SetBootSign(const Value: String);
begin
  FBootSign := Value;
end;

procedure TAVRootApplication.SetEEPROMFileName(const Value: TFileName);
begin
  FEEPROMFileName := Value;
end;

procedure TAVRootApplication.SetFlashFileName(const Value: TFileName);
begin
  FFlashFileName := Value;
end;

procedure TAVRootApplication.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TAVRootApplication.SetPort(const Value: String);
begin
  FPort := Value;
end;

procedure TAVRootApplication.SetVerbose(const Value: boolean);
begin
  FVerbose := Value;
end;

end.
