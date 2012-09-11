unit UComPort;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure GetSerialPortRegNames(const v: TStrings);
function GetFriendlyName(const Port: string): string;


implementation

uses Registry;

function __GetFriendlyName(const port: string; const key: string = '\System\CurrentControlSet\Enum\'): string;
var
  r : TRegistry;
  k : TStringList;
  i : Integer;
  ck: string;
  rs: string;
begin
  r := TRegistry.Create;
  k := TStringList.Create;
  r.RootKey := HKEY_LOCAL_MACHINE;
  r.OpenKeyReadOnly(key);
  r.GetKeyNames(k);
  r.CloseKey;
  try
    for i := 0 to k.Count - 1 do
    begin
      ck := key + k[i] + '\'; // current key
      // looking for "PortName" stringvalue in "Device Parameters" subkey
      if r.OpenKeyReadOnly(ck + 'Device Parameters') then
      begin
        if r.ReadString('PortName') = port then
        begin
          r.CloseKey;
          r.OpenKeyReadOnly(ck);
          rs := r.ReadString('FriendlyName');
          Break;
        end
      end
      // keep looking on subkeys for "PortName"
      else
      begin
        if r.OpenKeyReadOnly(ck) and r.HasSubKeys then
        begin
          rs := __GetFriendlyName(port, ck);
          if rs <> '' then Break;
        end;
      end;
    end;
    result := rs;
  finally
    r.Free;
    k.Free;
  end;
end;

function GetFriendlyName(const Port: string): string;
begin
  Result := __GetFriendlyName(Port);
end;

procedure GetSerialPortRegNames(const v: TStrings);
var
  reg: TRegistry;
  l: TStringList;
  i: integer;
begin
  reg := TRegistry.Create;
  l := TStringList.Create;
  try
{$IFNDEF VER100}
    reg.Access := KEY_READ;
{$ENDIF}
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM');
    reg.GetValueNames(l);
    for i := 0 to l.Count - 1 do
      v.Add(reg.ReadString(l[i]));
  finally
    reg.Free;
    l.Free;;
  end;
end;


end.

