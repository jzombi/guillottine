unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, SetupUnit, SolutionUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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


procedure TMainForm.Button1Click(Sender: TObject);
begin
    SetupForm.Visible := not SetupForm.Visible;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
     SolutionForm.Visible := not SolutionForm.Visible;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
     Close;
end;

end.

