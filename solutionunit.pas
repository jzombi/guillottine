unit solutionunit;

{$mode objfpc}{$H+}

interface



uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Grids, SetupUnit, solver, simplesolver, rectangle;

type

  { TTransformation }

  TTransformation = object
      offsetX, offsetY, scaleX, scaleY : real;
      constructor Init;
      procedure AutoFit(solution: TSolution; width, height: real);
      function transform(rectangle: TRectangle):TRectangle;
  end;

type

  { TSolverThread }

  TSolverThread = class(TThread)
     private
         Solver : TSimpleSolver;
         FSolution : TSolution;
         SpaceList: TSpaceList;
         PieceList: TPieceList;
         CutWidth: real;
         FStopped: boolean;
     public
         procedure stop;
         procedure Execute; override;
         property Solution: TSolution read FSolution;
         constructor Create(ASpaceList: TSpaceList; APieceList: TPieceList; ACutWidth: real);
         property Stopped: boolean read FStopped;
         //destructor Destroy; override;
  end;

type

  { TSolutionForm }

  TSolutionForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

constructor TSolverThread.Create(ASpaceList: TSpaceList; APieceList: TPieceList; ACutWidth: real);
begin
    CutWidth := ACutWidth;
    PieceList := APieceList;
    SpaceList := ASpaceList;
    FSolution.valid := false;
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

function GetSpaceList(input: TStringGrid):TSpaceList;
var ii, kk : integer;
    count : integer;
    space : TSpace;
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
                 space.Init(namecount, xx, yy, width, height);
                 GetSpaceList.Append(space);
             end;
         end;
     end;
end;

function GetPieceList(input: TStringGrid):TPieceList;
var ii, kk : integer;
    count : integer;
    box : TBox;
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
                 box.Init(namecount, width, height, rotatable, false);
                 GetPieceList.Append(box);
             end;
         end;
     end;
end;


procedure TSolutionForm.Button1Click(Sender: TObject);
var piecelist : TPieceList;
    spacelist : TSpaceList;
    cutwidth : real;
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
         Button1.Caption:='Start';
     end else begin
         piecelist := GetPieceList(SetupForm.PiecesGrid);
         spacelist := GetSpaceList(SetupForm.PiecesGrid);
         cutwidth := StrToFloatDef(SetupForm.CutWidthEdit.Text, 0);
         SolverThread := TSolverThread.Create(spacelist, piecelist, cutwidth);
         SolverThread.OnTerminate := @self.SolverTerminated;
         SolverThread.Start;
         Label1.Caption := 'Step: 0';
         Button1.Caption:='Stop';
     end;
end;

procedure TSolutionForm.Button2Click(Sender: TObject);
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
    Button1.Caption:='Start';
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
    GetTRect.Left:=Round(rectangle.left_);
    GetTRect.Top:=Round(rectangle.top_);
    GetTRect.Bottom:=Round(rectangle.top_+rectangle.height_);
    GetTRect.Right:=Round(rectangle.left_+rectangle.width_);
end;

procedure TSolutionForm.ShowSolution(Solution: TSolution);
var ii : integer;
    cut : TCut;
    space : TSpace;
    placedbox : TPlacedBox;
    rect : TRect;
    transformation : TTransformation;
    tw, th : integer;
    placedname : string;
begin
     if Solution.valid then begin
         Image1.Canvas.Brush.Color:=clWhite;
         Image1.Canvas.FillRect(0, 0, Image1.Canvas.Width, Image1.Canvas.Height);
         transformation.Init;
         transformation.AutoFit(Solution, Image1.Canvas.Width, Image1.Canvas.Height);
         Image1.Canvas.Pen.Color:=clBlack;
         for ii:=0 to Solution.origspacelist.size-1 do begin
             space:=Solution.origspacelist.data[ii];
             rect := GetTRect(space, transformation);
             Image1.Canvas.Frame(rect);
         end;
         for ii:=0 to Solution.placedlist.size-1 do begin
             placedbox:=Solution.placedlist.data[ii];
             rect := GetTRect(placedbox, transformation);
             Image1.Canvas.Frame(rect);
             Image1.Canvas.Brush.Color:=clGray;
             Inc(rect.Left);
             Inc(rect.Top);
             Dec(rect.Right);
             Dec(Rect.Bottom);
             Image1.Canvas.FillRect(rect);
             placedname := placedbox.name_;
             if placedbox.rotated_ then
                placedname := placedname + '*';
             th := Image1.Canvas.TextHeight(placedbox.name_);
             tw := Image1.Canvas.TextWidth(placedbox.name_);
             Image1.Canvas.TextOut((rect.Right - rect.Left - tw) div 2 + rect.Left,
                                   (rect.Bottom - rect.Top - th) div 2 + rect.Top,
                                   placedname);
         end;
         Image1.Canvas.Pen.Color:=clRed;
         for ii:=0 to Solution.cutlist.size-1 do begin
             cut:=Solution.cutlist.data[ii];
             rect := GetTRect(cut, transformation);
             if cut.direction_ = horizontal then begin
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

procedure TTransformation.AutoFit(solution: TSolution; width, height: real);
var space : TSpace;
    minx, miny, maxx, maxy : real;
    ii : integer;
begin
     minx := 0;
     miny := 0;
     maxx := width;
     maxy := height;
     for ii:=0 to Solution.origspacelist.size-1 do begin
         space:=Solution.origspacelist.data[ii];
         if (ii = 0) or (space.left_ < minx) then
            minx := space.left_;
         if (ii = 0) or (space.top_ < miny) then
            miny := space.top_;
         if (ii = 0) or (space.left_ + space.width_ > maxx) then
            maxx := space.left_ + space.width_;
         if (ii = 0) or (space.top_ + space.height_ > maxy) then
           maxy := space.top_ + space.height_;
     end;
     offsetX := -minx;
     offsetY := -minY;
     scaleX := width / (maxx + offsetX);
     scaleY := height / (maxy + offsetY);
end;

function TTransformation.transform(rectangle: TRectangle):TRectangle;
begin
     transform.Init(rectangle.name_,
                   (rectangle.left_ + offsetX) * scaleX,
                   (rectangle.top_ + offsetY) * scaleY,
                   rectangle.width_ * scaleX,
                   rectangle.height_ * scaleY);
end;

end.

