unit SetupUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Grids, ValEdit, StdCtrls, ComCtrls;

type

  { TSetupForm }

  TSetupForm = class(TForm)
    AddPieceButton: TButton;
    RemovePieceButton: TButton;
    SavePieceListButton: TButton;
    LoadPieceListButton: TButton;
    CutWidthEdit: TEdit;
    CutWidthLabel: TLabel;
    PiecesLabel: TLabel;
    OpenDialog1: TOpenDialog;
    PiecesGrid: TStringGrid;
    SaveDialog1: TSaveDialog;
    procedure AddPieceButtonClick(Sender: TObject);
    procedure RemovePieceButtonClick(Sender: TObject);
    procedure SavePieceListButtonClick(Sender: TObject);
    procedure LoadPieceListButtonClick(Sender: TObject);
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

procedure TSetupForm.AddPieceButtonClick(Sender: TObject);
begin
  PiecesGrid.InsertColRow(false, PiecesGrid.RowCount);
end;

procedure TSetupForm.RemovePieceButtonClick(Sender: TObject);
begin
  PiecesGrid.DeleteRow(PiecesGrid.Row);
end;

procedure TSetupForm.SavePieceListButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then begin
    PiecesGrid.SaveToCSVFile(SaveDialog1.FileName);
  end;
end;

procedure TSetupForm.LoadPieceListButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    PiecesGrid.LoadFromCSVFile(OpenDialog1.FileName);
  end;
end;


procedure TSetupForm.CutWidthEditEditingDone(Sender: TObject);
var value : integer;
begin
    value := StrToIntDef(CutWidthEdit.Text, 0);
    if value < 0 then value := 0;
    CutWidthEdit.Text := IntToStr(value);
end;


procedure TSetupForm.PiecesGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var intvalue : integer;
begin
    if aCol = 0 then begin
        if (NewValue <> 'piece') and (NewValue <> 'space') then begin
            NewValue:='piece';
        end;
    end else if aCol = 1 then begin
        // Don't check
    end else if (aCol > 1) and (aCol < 6) then begin
        intvalue := StrToIntDef(NewValue, 0);
        if ((aCol = 3) or (aCol = 4)) and (intvalue < 0) then intvalue := 0;
        NewValue := IntToStr(intvalue);
    end else if aCol = 6 then begin
        if (NewValue <> 'true') and (NewValue <> 'false') and (NewValue <> '') then begin
            NewValue:='';
        end;
    end else if aCol = 7 then begin
        Newvalue := IntToStr(StrToIntDef(NewValue, 1));
    end;
end;

end.

