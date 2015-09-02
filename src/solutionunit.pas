unit solutionunit;

{$mode objfpc}{$H+}

interface



uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Grids, math, SetupUnit, solver, simplesolver, rectangle;

type

  { TTransformation }

  TTransformation = object
      offsetX, offsetY, scaleX, scaleY : real;
      constructor Init;
      procedure AutoFit(solution: TSolution; width, height: real);
      procedure CenterStretchKeepAspect(tx1, ty1, tx2, ty2, sx1, sy1, sx2, sy2 : real);
      function transform(rectangle: TRectangle):TRectangle;
  end;

type

  { TSolverThread }

  TSolverThread = class(TThread)
     private
         Solver : TSimpleSolver;
         FSolution : TSolution;
         SpaceList: TSheetList;
         PieceList: TPieceList;
         CutWidth: integer;
         FStopped: boolean;
     public
         procedure stop;
         procedure Execute; override;
         property Solution: TSolution read FSolution;
         constructor Create(ASpaceList: TSheetList; APieceList: TPieceList; ACutWidth: integer);
         property Stopped: boolean read FStopped;
         //destructor Destroy; override;
  end;

type

  { TSolutionForm }

  TSolutionForm = class(TForm)
    SolverStartStopButton: TButton;
    SaveImageButton: TButton;
    Image1: TImage;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure SolverStartStopButtonClick(Sender: TObject);
    procedure SaveImageButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SolverTerminated(Sender: TObject);
    procedure ShowSolution(Solution: TSolution);
    procedure Timer1Timer(Sender: TObject);
    procedure UpdateStep;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SolutionForm: TSolutionForm;

implementation

var SolverThread: TSolverThread;

{$R *.lfm}

{ TSolverThread }

constructor TSolverThread.Create(ASpaceList: TSheetList; APieceList: TPieceList; ACutWidth: integer);
begin
    CutWidth := ACutWidth;
    PieceList := APieceList;
    SpaceList := ASpaceList;
    FSolution.Valid := false;
    solver := nil;
    inherited Create(false);
end;

procedure TSolverThread.stop;
begin
     if Assigned(solver) then
          solver.StopSolver;
end;

procedure TSolverThread.Execute;
begin
     FStopped := False;
     if not Assigned(solver) then begin
          solver := TSimpleSolver.Create(SpaceList, PieceList, CutWidth);
          FSolution := solver.Search;
          FreeAndNil(solver);
     end;
     FStopped := True;
end;

{destructor TSolverThread.Destroy;
begin
    stop;
     while not stopped do Sleep(100);
     inherited Destroy;
end;}

{ TSolutionForm }

function GetCount(RectType: string; input: TStringGrid):integer;
var ii : integer;
begin
     GetCount:=0;
     for ii:=1 to input.RowCount-1 do begin
         if UpCase(input.Cells[0, ii]) = UpCase(RectType) then begin
             Inc(GetCount);
         end;
     end;
end;

function GetSpaceList(input: TStringGrid):TSheetList;
var ii, kk : integer;
    count : integer;
    sheet : TSheet;
    name, namecount : string;
    xx, yy, width, height : integer;
    elemcount : integer;
begin
     count := GetCount('space', input);
     GetSpaceList.Init(count);
     for ii:=1 to input.RowCount-1 do begin
         if UpCase(input.Cells[0, ii]) = 'SPACE' then begin
             elemcount := StrToIntDef(input.Cells[7, ii], 1);
             name := input.Cells[1, ii];
             xx := StrToIntDef(input.Cells[2, ii], 0);
             yy := StrToIntDef(input.Cells[3, ii], 0);
             width := StrToIntDef(input.Cells[4, ii], 0);
             height := StrToIntDef(input.Cells[5, ii], 0);
             for kk:=1 to elemcount do begin
                 namecount := name;
                 if elemcount > 1 then namecount := namecount + '_' + IntToStr(kk);
                 sheet.Init(namecount, xx, yy, width, height);
                 GetSpaceList.Append(sheet);
             end;
         end;
     end;
end;

function GetPieceList(input: TStringGrid):TPieceList;
var ii, kk : integer;
    count : integer;
    piece : TPiece;
    name, namecount : string;
    width, height : integer;
    rotatable : boolean;
    elemcount : integer;
begin
     count := GetCount('piece', input);
     GetPieceList.Init(count);
     for ii:=1 to input.RowCount-1 do begin
         if UpCase(input.Cells[0, ii]) = 'PIECE' then begin
             elemcount := StrToIntDef(input.Cells[7, ii], 1);
             name := input.Cells[1, ii];
             width := StrToIntDef(input.Cells[4, ii], 0);
             height := StrToIntDef(input.Cells[5, ii], 0);
             rotatable := false;
             if UpCase(input.Cells[6, ii])='TRUE' then
                rotatable:=true;
             for kk:=1 to elemcount do begin
                 namecount := name;
                 if elemcount > 1 then namecount := namecount + '_' + IntToStr(kk);
                 piece.Init(namecount, width, height, 0, rotatable, false);
                 GetPieceList.Append(piece);
             end;
         end;
     end;
end;


procedure TSolutionForm.SolverStartStopButtonClick(Sender: TObject);
var piecelist : TPieceList;
    spacelist : TSheetList;
    cutwidth : integer;
begin
     if Assigned(SolverThread) and SolverThread.Stopped then begin
         SolverThread.stop;
         SolverThread.WaitFor;
         FreeAndNil(SolverThread);
     end;
     if Assigned(SolverThread) then begin
         SolverThread.stop;
         SolverThread.WaitFor;
         FreeAndNil(SolverThread);
         SolverStartStopButton.Caption:='Start';
     end else begin
         piecelist := GetPieceList(SetupForm.PiecesGrid);
         spacelist := GetSpaceList(SetupForm.PiecesGrid);
         cutwidth := StrToIntDef(SetupForm.CutWidthEdit.Text, 0);
         SolverThread := TSolverThread.Create(spacelist, piecelist, cutwidth);
         SolverThread.OnTerminate := @self.SolverTerminated;
         SolverThread.Start;
         Label1.Caption := 'Step: 0';
         SolverStartStopButton.Caption:='Stop';
     end;
end;

procedure TSolutionForm.SaveImageButtonClick(Sender: TObject);
begin
     If SaveDialog1.Execute then begin
          Image1.Picture.SaveToFile(SaveDialog1.FileName);
     end;
end;

procedure TSolutionForm.FormCreate(Sender: TObject);
begin
    SolverThread := nil;
    Image1.Canvas.Clear;
    Image1.Canvas.Brush.Color:=clWhite;
    Image1.Canvas.FillRect(0, 0, Image1.Canvas.Width, Image1.Canvas.Height);
end;

procedure TSolutionForm.FormDestroy(Sender: TObject);
begin
    if Assigned(SolverThread) then begin
        SolverThread.stop;
        SolverThread.WaitFor;
        FreeAndNil(SolverThread);
    end;
end;

procedure TSolutionForm.SolverTerminated(Sender: TObject);
begin
    SolverStartStopButton.Caption:='Start';
    ShowSolution(SolverThread.Solution);
    UpdateStep;
end;

procedure TSolutionForm.Timer1Timer(Sender: TObject);
begin
    UpdateStep;
end;

procedure TSolutionForm.UpdateStep;
begin
    if Assigned(SolverThread) and Assigned(SolverThread.Solver) then begin
        Label1.Caption := 'Step: ' + IntToStr(SolverThread.Solver.Step);
    end else if Assigned(SolverThread) then begin
        Label1.Caption := 'Step: ' + IntToStr(SolverThread.Solution.Step);
    end;
end;

function GetTRect(rectangle: TRectangle; transformation: TTransformation):TRect;
begin
    rectangle := transformation.transform(rectangle);
    GetTRect.Left:=Round(rectangle.Left);
    GetTRect.Top:=Round(rectangle.Top);
    GetTRect.Bottom:=Round(rectangle.Top+rectangle.Height);
    GetTRect.Right:=Round(rectangle.Left+rectangle.Width);
end;

procedure TSolutionForm.ShowSolution(Solution: TSolution);
var ii : integer;
    cut : TCut;
    space : TSheet;
    placedbox : TPlacedPiece;
    rect : TRect;
    transformation : TTransformation;
    tw, th : integer;
    placedname : string;
begin
     Image1.Canvas.Brush.Color:=clWhite;
     Image1.Canvas.FillRect(0, 0, Image1.Canvas.Width, Image1.Canvas.Height);
     if Solution.Valid then begin
         transformation.Init;
         transformation.AutoFit(Solution, Image1.Canvas.Width, Image1.Canvas.Height);
         Image1.Canvas.Pen.Color:=clBlack;
         for ii:=0 to Solution.OrigSheetList.size-1 do begin
             space:=Solution.OrigSheetList.data[ii];
             rect := GetTRect(space, transformation);
             Image1.Canvas.Frame(rect);
         end;
         for ii:=0 to Solution.PlacedList.size-1 do begin
             placedbox:=Solution.PlacedList.data[ii];
             rect := GetTRect(placedbox, transformation);
             Image1.Canvas.Frame(rect);
             Image1.Canvas.Brush.Color:=clGray;
             Inc(rect.Left);
             Inc(rect.Top);
             Dec(rect.Right);
             Dec(Rect.Bottom);
             Image1.Canvas.FillRect(rect);
             placedname := placedbox.Name;
             if placedbox.Rotated then
                placedname := placedname + '*';
             th := Image1.Canvas.TextHeight(placedbox.Name);
             tw := Image1.Canvas.TextWidth(placedbox.Name);
             Image1.Canvas.TextOut((rect.Right - rect.Left - tw) div 2 + rect.Left,
                                   (rect.Bottom - rect.Top - th) div 2 + rect.Top,
                                   placedname);
         end;
         Image1.Canvas.Pen.Color:=clRed;
         for ii:=0 to Solution.CutList.size-1 do begin
             cut:=Solution.CutList.data[ii];
             rect := GetTRect(cut, transformation);
             if cut.Direction = dirHorizontal then begin
                 rect.Top := Round(rect.Top+(rect.Bottom-rect.Top)/2);
                 rect.Bottom := rect.Top;
             end else begin
                 rect.Left := Round(rect.Left+(rect.Right-rect.Left)/2);
                 rect.Right := rect.Left;
             end;
             Image1.Canvas.Line(rect);
         end;
         Image1.Canvas.Refresh;
     end;
end;


{ TTransformaton }

constructor TTransformation.Init;
begin
     offsetX := 0;
     offsetY := 0;
     scaleX := 1;
     scaleY := 1;
end;

procedure TTransformation.CenterStretchKeepAspect(tx1, ty1, tx2, ty2 : real;
                                                  sx1, sy1, sx2, sy2 : real);
begin
     {Assume that x2>x1, y2>y1}
     if (tx2-tx1)*(sy2-sy1) > (ty2-ty1)*(sx2-sx1) then begin
         scaleY := (ty2-ty1)/(sy2-sy1);
         offsetY := ty1/scaleY-sy1;
         scaleX := scaleY;
         offsetX := (tx1 + 0.5*(tx2-tx1))/scaleX - (sx1 + 0.5*(sx2-sx1));
     end else begin
         scaleX := (tx2-tx1)/(sx2-sx1);
         offsetX := tx1/scaleX-sx1;
         scaleY := scaleX;
         offsetY := (ty1 + 0.5*(ty2-ty1))/scaleY - (sy1 + 0.5*(sy2-sy1));
     end;
end;

procedure TTransformation.AutoFit(solution: TSolution; width, height: real);
var sheet : TSheet;
    minx, miny, maxx, maxy : real;
    ii : integer;
    margin : real;
begin
     minx := 0;
     miny := 0;
     maxx := width;
     maxy := height;
     for ii:=0 to Solution.OrigSheetList.size-1 do begin
         sheet:=Solution.OrigSheetList.data[ii];
         if (ii = 0) or (sheet.Left < minx) then
            minx := sheet.Left;
         if (ii = 0) or (sheet.Top < miny) then
            miny := sheet.Top;
         if (ii = 0) or (sheet.Left + sheet.Width > maxx) then
            maxx := sheet.Left + sheet.Width;
         if (ii = 0) or (sheet.Top + sheet.Height > maxy) then
            maxy := sheet.Top + sheet.Height;
     end;
     margin := 0.025*min(width, height);
     CenterStretchKeepAspect(margin, margin, width-margin, height-margin,
                             minx, miny, maxx, maxy);
end;

function TTransformation.transform(rectangle: TRectangle):TRectangle;
begin
     transform.Init(rectangle.Name,
                   Round((rectangle.Left + offsetX) * scaleX),
                   Round((rectangle.Top + offsetY) * scaleY),
                   Round(rectangle.Width * scaleX),
                   Round(rectangle.Height * scaleY));
end;

end.

