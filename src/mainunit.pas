unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LResources, Translations, LCLTranslator, INIFiles,
  SetupUnit, SolutionUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    LanguageLabel: TLabel;
    MsgLabel: TLabel;
    LanguageComboBox: TComboBox;
    SetupButton: TButton;
    SolverButton: TButton;
    QuitButton: TButton;
    procedure FormActivate(Sender: TObject);
    procedure LanguageComboBoxChange(Sender: TObject);
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

resourcestring
  rsSavedTo = 'Saved to: ';

var Activated : boolean;
    ConfigFile : string;


function Translate(Language: string; update: boolean): boolean;
var
  Res: TResourceStream;
  PoStringStream: TStringStream;
  LocalTranslator: TPoTranslator;
  PoFile: TPOFile;
  FileName: string;
  ii : integer;
begin
  FileName := 'guillottine' + '.' + Language;
  Res := TResourceStream.Create(HInstance, UpCase(FileName), RT_RCDATA);
  PoStringStream := TStringStream.Create('');
  Res.SaveToStream(PoStringStream);
  Res.Free;

  PoFile := TPOFile.Create(False);
  PoFile.ReadPOText(PoStringStream.DataString);
  PoStringStream.Free;

  Result := TranslateResourceStrings(PoFile);

  LocalTranslator := nil;
  if result then begin
    LocalTranslator := TPoTranslator.Create(PoFile);
    if LocalTranslator <> nil then begin
      if Assigned(LRSTranslator) then
          LRSTranslator.Free;
      LRSTranslator := LocalTranslator;
      if update then begin
        for ii := 0 to Screen.CustomFormCount-1 do
          LocalTranslator.UpdateTranslation(Screen.CustomForms[ii]);
      end;
    end;
  end;
  //PoFile is freed in the destructor of TPoTranslator
  //PoFile.Free;
end;

procedure TMainForm.SetupButtonClick(Sender: TObject);
begin
    SetupForm.Visible := not SetupForm.Visible;
end;

procedure TMainForm.LanguageComboBoxChange(Sender: TObject);
var ini: TINIFile;
begin
  Translate(LanguageComboBox.Text, true);
  ini := TINIFile.Create(ConfigFile);
  ini.WriteString('config', 'language', LanguageComboBox.Text);
  ini.Free;
  MsgLabel.Caption := rsSavedTo + ConfigFile;
end;

procedure TMainForm.FormActivate(Sender: TObject);
var ini: TINIFile;
    language : string;
begin
  if not Activated then begin
    ini := TINIFile.Create(ConfigFile);
    language := ini.ReadString('config', 'language', '');
    if language <> '' then begin
      LanguageComboBox.Text := language;
      Translate(LanguageComboBox.Text, true);
    end;
    ini.Free;
    Activated := True;
  end;
end;

procedure TMainForm.SolverButtonClick(Sender: TObject);
begin
     SolutionForm.Visible := not SolutionForm.Visible;
end;

procedure TMainForm.QuitButtonClick(Sender: TObject);
begin
     Close;
end;

initialization
  Activated := False;
  ConfigFile := GetAppConfigFile(False);

end.

