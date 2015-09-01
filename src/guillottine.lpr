program guillottine;

{$mode objfpc}{$H+}

{$IFDEF UNIX}
{$DEFINE UseCThreads}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SetupUnit, undoarray, stack, sortedarray, solver, simplesolver, rectangle,
  glist, MainUnit, solutionunit;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSetupForm, SetupForm);
  Application.CreateForm(TSolutionForm, SolutionForm);
  Application.Run;
end.

