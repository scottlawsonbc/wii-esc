program avrootloader_cli;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  AVRootIntf in 'lib\AVRootIntf.pas',
  UUtils in 'lib\UUtils.pas',
  AVRootImp in 'lib\AVRootImp.pas';

var
  HelpReq: boolean;
  EEPRomEraseReq: boolean;
  FlashEraseReq: boolean;

procedure Init;
begin
  App := TAVRootApplication.Create(nil);
  App.Init;
end;

procedure PrintGeeting;
begin
  Writeln;
  Writeln(ExtractFileName(ParamStr(0)),': Version 6.0');
  Writeln('             Copyright (c) 2000-2008 Hagen Reddmanns');
  Writeln('             Copyright (c) 2012      Ziss_dm');
  Writeln;
end;


procedure ParseCommandLine;
begin
  with App do
  begin
    Port := ParameterValue('-P');
    BaudRate := StrToIntDef(ParameterValue('-b'), 115200);
    BootSign := ParameterValue('--boot_sign');
    if (BootSign = '') then BootSign := 'ESCBL1';
    EEPROMFileName := ParameterValue('--eeprom');
    FlashFileName := ParameterValue('--flash');
    Verbose := ParameterExists('-v');
    HelpReq := ParameterExists('-h');
    EEPRomEraseReq := ParameterExists('--erase-eeprom');
    FlashEraseReq := ParameterExists('--erase-flash');
  end;
end;


procedure DoRun;
begin
  ParseCommandLine;
  if HelpReq then
  begin
    Exit;
  end;
  PrintGeeting;
  Writeln('Waiting for device...');
  App.Connect;
  if EEPRomEraseReq then
    App.EraseEEPROM;
  if FlashEraseReq then
    App.EraseFlash;
  if (App.EEPROMFileName <> '') or (App.FlashFileName <> '') then
    App.Flash;
  App.Disconnect;
  Writeln;
end;


begin
  try
    Init;
    DoRun;
  except
    on E: Exception do
    begin
      Writeln('Fatal: ', E.Message);
      Halt(1);
    end;
  end;
end.
