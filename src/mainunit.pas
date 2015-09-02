unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, SetupUnit, SolutionUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    SetupButton: TButton;
    SolverButton: TButton;
    QuitButton: TButton;
    Image1: TImage;
    procedure SetupButtonClick(Sender: TObject);
    procedure SolverButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.SetupButtonClick(Sender: TObject);
begin
    SetupForm.Visible := not SetupForm.Visible;
end;

procedure TMainForm.SolverButtonClick(Sender: TObject);
begin
     SolutionForm.Visible := not SolutionForm.Visible;
end;

procedure TMainForm.QuitButtonClick(Sender: TObject);
begin
     Close;
end;

end.

