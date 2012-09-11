unit FMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, LResources, UComPort, Windows, ShellApi, UMetadata;

type
  { TFrmMain }
  TFrmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CmbPgmType: TComboBox;
    CmbPorts: TComboBox;
    CmbPgmType2: TComboBox;
    CmbPgmType3: TComboBox;
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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMetadata: TMetadata;
  public
    { public declarations }
  end; 

var
  FrmMain: TFrmMain;

implementation

uses IniFiles;

{$R *.lfm}

{ TFrmMain }
procedure TFrmMain.FormCreate(Sender: TObject);
var
  lini: TInifile;
begin
  FMetadata := TMetadata.Create;
  lini := TInifile.Create('E:\!Work\Unsorted\wii-esc\wii-esc-flash\metadata\__metadata.ini');
  FMetadata.LoadFromIni(lini);
  lini.Free;
  GetSerialPortRegNames(CmbPorts.Items);
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.google.com', '', nil, SW_SHOW);
end;

initialization
  {$I payload.inc}

end.

