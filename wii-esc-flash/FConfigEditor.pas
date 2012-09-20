unit FConfigEditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin;

type
  { TFrmConfigEditor }
  TFrmConfigEditor = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    CmbBraking: TCheckBox;
    CmbTimingAdv: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EdtRcpMin: TSpinEdit;
    EdtRcpMax: TSpinEdit;
    EdtRcpStart: TSpinEdit;
    EdtRcpFull: TSpinEdit;
    EdtRcpCal: TSpinEdit;
    EdtRcpDB: TSpinEdit;
    Label7: TLabel;
  private
    FData: Pointer;
  protected
    procedure DataToControls;
    procedure ControlsToData;
  public
  end;

function EditConfiguration(AData: Pointer): boolean;

implementation

{$R *.lfm}

type
  TConfigData = packed record
     rcp_min_us: Word;
     rcp_max_us: Word;
     rcp_start_us: Word;
     rcp_full_us: Word;
     rcp_cal_us: Word;
     rcp_deadband_us: Byte;
     braking: Byte;
     timing_adv: Shortint;
  end;
  TEEPROMLayout = packed record
    __ver_magic: Byte;
    cfg: TConfigData;
  end;
  PEEPROMLayout = ^TEEPROMLayout;

function EditConfiguration(AData: Pointer): boolean;
begin
  with TFrmConfigEditor.Create(nil) do
  try
    FData := AData;
    DataToControls;
    Result := ShowModal = mrOK;
    if Result then ControlsToData;
  finally
    Free;
  end;
end;

{ TFrmConfigEditor }

procedure TFrmConfigEditor.DataToControls;
begin
  with PEEPROMLayout(FData)^.cfg do
  begin
    EdtRcpMin.Value := rcp_min_us;
    EdtRcpMax.Value := rcp_max_us;
    EdtRcpStart.Value := rcp_start_us;
    EdtRcpFull.Value := rcp_full_us;
    EdtRcpCal.Value := rcp_cal_us;
    EdtRcpDB.Value := rcp_deadband_us;
    CmbBraking.Checked := braking <> 0;
    if (timing_adv < 0) or (timing_adv > 3) then timing_adv := 0;
    CmbTimingAdv.ItemIndex := timing_adv;
  end;
end;

procedure TFrmConfigEditor.ControlsToData;
begin
  with PEEPROMLayout(FData)^.cfg do
  begin
    rcp_min_us := EdtRcpMin.Value;
    rcp_max_us := EdtRcpMax.Value;
    rcp_start_us := EdtRcpStart.Value;
    rcp_full_us := EdtRcpFull.Value;
    rcp_cal_us := EdtRcpCal.Value;
    rcp_deadband_us := EdtRcpDB.Value;
    braking := Byte(CmbBraking.Checked);
    timing_adv := CmbTimingAdv.ItemIndex;
  end;
end;


end.

