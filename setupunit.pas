unit SetupUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Grids, ValEdit, StdCtrls, ComCtrls;

type

  { TSetupForm }

  TSetupForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CutWidthEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    PiecesGrid: TStringGrid;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CutWidthEditEditingDone(Sender: TObject);
    procedure PiecesGridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SetupForm: TSetupForm;

implementation

{$R *.lfm}

{ TSetupForm }

procedure TSetupForm.Button1Click(Sender: TObject);
begin
  PiecesGrid.InsertColRow(false, PiecesGrid.RowCount);
end;

procedure TSetupForm.Button2Click(Sender: TObject);
begin
  PiecesGrid.DeleteRow(PiecesGrid.Row);
end;

procedure TSetupForm.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then begin
    PiecesGrid.SaveToCSVFile(SaveDialog1.FileName);
  end;
end;

procedure TSetupForm.Button4Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    PiecesGrid.LoadFromCSVFile(OpenDialog1.FileName);
  end;
end;


procedure TSetupForm.CutWidthEditEditingDone(Sender: TObject);
var value : real;
begin
    value := StrToFloatDef(CutWidthEdit.Text, 0);
    if value < 0 then value := 0;
    CutWidthEdit.Text := FloatToStr(value);
end;


procedure TSetupForm.PiecesGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var floatvalue : real;
    intvalue : integer;
begin
    if aCol = 0 then begin
        if (NewValue <> 'piece') and (NewValue <> 'space') then begin
            NewValue:='piece';
        end;
    end else if aCol = 1 then begin
        // Don't check
    end else if (aCol > 1) and (aCol < 6) then begin
        floatvalue := StrToFloatDef(NewValue, 0);
        if ((aCol = 3) or (aCol = 4)) and (floatvalue < 0) then floatvalue := 0;
        NewValue := FloatToStr(floatvalue);
    end else if aCol = 6 then begin
        if (NewValue <> 'true') and (NewValue <> 'false') and (NewValue <> '') then begin
            NewValue:='';
        end;
    end else if aCol = 7 then begin
        Newvalue := IntToStr(StrToIntDef(NewValue, 1));
    end;
end;

end.

