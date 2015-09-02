{
The basic built-in guillottine solver.
@author(Matyas Jani)
}
{$mode objfpc}
unit simplesolver;

interface

uses solver, rectangle, undoarray, sysutils;

{ @abstract(Piece type with sortvalue for basic search heuristics, and @link(TUndoArray) support.) }
type TSSPiece = object(TPiece)
    public
        SortValue : integer;
        constructor Init(AName: string; AWidth, AHeight: integer; AGain: integer; ARotatable: boolean; ARotated: boolean);
        constructor Init(Base: TPiece);
end;

{ @abstract(Sheet type with sortvalue for basic search heuristics, and @link(TUndoArray) support.) }
type TSSSheet = object(TSheet)
    public
        SortValue : integer;
        constructor Init(AName: string; ALeft, ATop, AWidth, AHeight: integer);
        constructor Init(Base: TSheet);
end;

{ @abstract(Cut type with sortvalue to make it compatible with @link(TUndoArray).) }
type TSSCut = object(TCut)
    public
        SortValue : integer;
        constructor Init(Base: TCut);
end;

{ @abstract(PlacedPiece type with sortvalue to make it compatible with @link(TUndoArray).) }
type TSSPlacedPiece = object(TPlacedPiece)
    public
        SortValue : integer;
        constructor Init(Base: TPlacedPiece);
end;

{ @abstract(Type which stores the search index (position in the arrays).) }
type TIndexState = object
    { True before first run. }
    First : boolean;

    { Just to make it compatible with @link(TUndoArray).}
    SortValue : integer;

    { Sizes of the respective arrays. }
    sCutDir, sRotation, sSpace, sPiece : integer;

    { Current search position in the respective arrays. }
    iCutDir, iRotation, iSpace, iPiece : integer;

    { Initialize search index with array sizes. }
    constructor Init(AsCutDir, AsRotation, AsSpace, AsPiece: integer);

    { Increases the search index, returns false after last element. }
    function Next:boolean;

    function toString:string;
end;

type TPlacedArray = specialize TUndoArray<TSSPlacedPiece>;
     TPiecesArray = specialize TUndoArray<TSSPiece>;
     TCutsArray = specialize TUndoArray<TSSCut>;
     TSheetsArray = specialize TUndoArray<TSSSheet>;
     TIndexesArray = specialize TUndoArray<TIndexState>;

{ @abstract(Type for storing the state of the search.) }
type TSimpleSolverState = object
    public
        { Stores placed pieces. }
        Placed : TPlacedArray;

        { Remaining pieces to be placed. }
        Pieces : TPiecesArray;

        { Cuts made to place the placed pieces. }
        Cuts : TCutsArray;

        { Remaining sheet parts. }
        Sheets : TSheetsArray;

        { Indexes of the search space. }
        Indexes : TIndexesArray;

        { Calls commit on all arrays. }
        procedure Commit;

        { Calls undo on all arrays.}
        procedure Undo;
end;

{ @abstract(Record to store the solution.) }
type TSolution = record
    { List of cuts. }
    CutList: TCutList;

    { List of remaining sheet parts. }
    SheetList: TSheetList;

    { List of original sheets for displaying purposes. }
    OrigSheetList: TSheetList;

    { List of placed pieces. }
    PlacedList: TPlacedList;

    { True if it is initalized with a valid solution. }
    Valid: boolean;

    { Number of steps done before reaching this solution. }
    Step: integer;

    { The parameter with which the cuts were made. }
    CutWidth: integer;
end;

{ @abstract(Exception class thrown by @link(TSimpleSolver) class.) }
type ESimpleSolver = class(Exception);

{ @abstract(Class which implements the basic guillottine solver.) }
type TSimpleSolver = class(TSolver)
    private
        { Current number of steps. }
        FStep : integer;
        Stop : boolean;
        CutWidth : integer;
        State : TSimpleSolverState;
        IndexState : TIndexState;
        CutDirections : array[0..1] of TDirection;
        Rotations : array[0..1] of boolean;
        Solution : TSolution;
        procedure SetSolution;
    public
        { Create and initialize the solver with the parameters. }
        constructor Create(SheetList: TSheetList; PieceList: TPieceList; ACutWidth: integer);

        { Start the search. All magic happens here.
        @longcode(#
          The solver stores a list of pieces, sheet parts, placed pieces and cuts.
          The pieces are sorted in inverse order by edge length, the sheets in the order of edge length.
          (Try to fit the largest piece on the smallest possible sheet.)
          What basically happens here in the loop:
            1. Check if the list of pieces is empty. If yes: solution, quit.
            2. Increment the index. Backtrack on failure (no more options to check in this branch).
            3. If we skipped a piece (could not fit it on any of the sheet parts) then backtrack (no possible solution on this branch).
            4. Try to place the piece on the sheet part with rotation and cut direction all referred by the current index.
            5. On success: make the cut, put the cut into the list of cuts, put the piece in the list of placed pieces,
               put the falling sheet parts into the list of sheets.
            6. On failure: continue
               (If piece with largest edge has larger edge than sheet part with largest edge then backtrack.)
          #)
        }
        function Search:TSolution;

        { Signal the search to exit the loop at the next condition check. }
        procedure StopSolver;

        { Returns the number of steps done in search. }
        property Step: integer read FStep;
end;

implementation

constructor TSSPiece.Init(AName: string; AWidth, AHeight: integer; AGain: integer; ARotatable: boolean; ARotated: boolean);
begin
    TPiece.Init(AName, AWidth, AHeight, AGain, ARotatable, ARotated);
    SortValue := -Edge;
end;

constructor TSSPiece.Init(Base: TPiece);
begin
    TPiece.Init(Base.Name, Base.Width, Base.Height, Base.Gain, Base.Rotatable, Base.Rotated);
    SortValue := -Edge;
end;

constructor TSSSheet.Init(AName: string; ALeft, ATop, AWidth, AHeight: integer);
begin
    TSheet.Init(AName, ALeft, ATop, AWidth, AHeight);
    SortValue := Edge;
end;

constructor TSSSheet.Init(Base: TSheet);
begin
    TSheet.Init(Base.Name, Base.Left, Base.Top, Base.Width, Base.Height);
    SortValue := Edge;
end;

constructor TSSCut.Init(Base: TCut);
begin
    TCut.Init(Base.Name, Base.Left, Base.Top, Base.Width, Base.Height, Base.Direction);
    SortValue := 0;
end;

constructor TSSPlacedPiece.Init(Base: TPlacedPiece);
begin
    TPlacedPiece.Init(Base.Name, Base.Left, Base.Top, Base.Width, Base.Height, Base.Gain, Base.Rotated);
    SortValue := 0;
end;

constructor TIndexState.Init(AsCutDir, AsRotation, AsSpace, AsPiece: integer);
begin
    sCutDir := AsCutDir;
    sRotation := AsRotation;
    sSpace := AsSpace;
    sPiece := AsPiece;
    iCutDir := 0;
    iRotation := 0;
    iSpace := 0;
    iPiece := 0;
    SortValue := 0;
    First := true;
end;

function TIndexState.Next:boolean;
begin
    if First then begin
        First := false;
        if (sCutDir = 0) or (sRotation = 0) or (sSpace = 0) or (sPiece = 0) then exit(false);
        exit(true);
    end;
    Next := true;
    Inc(iCutDir);
    if iCutDir = sCutDir then begin
        iCutDir := 0;
        Inc(iRotation);
        if iRotation = sRotation then begin
            iRotation := 0;
            Inc(iSpace);
            if iSpace = sSpace then begin
                iSpace := 0;
                Inc(iPiece);
                if iPiece = sPiece then Next := false;
            end;
        end;
    end;
//    Writeln('updated');
end;

function TIndexState.toString:string;
begin
    toString := 'IndexState(' + FloatToStr(sCutDir) + ', ' + FloatToStr(sRotation) + ', ' + FloatToStr(sSpace) + ', ' + FloatToStr(sPiece) + ', ' +
                 FloatToStr(iCutDir) + ', ' + FloatToStr(iRotation) + ', ' + FloatToStr(iSpace) + ', ' + FloatToStr(iPiece) + ', ' +
                 BoolToStr(First, 'true', 'false') + ')';
end;


procedure TSimpleSolverState.Commit;
begin
    Placed.commit;
    Pieces.commit;
    Cuts.commit;
    Sheets.commit;
    Indexes.commit;
end;

procedure TSimpleSolverState.Undo;
begin
    Placed.undo;
    Pieces.undo;
    Cuts.undo;
    Sheets.undo;
    Indexes.undo;
end;


constructor TSimpleSolver.Create(SheetList: TSheetList; PieceList: TPieceList; ACutWidth: integer);
var sheet : TSSSheet;
    piece : TSSPiece;
    ii : integer;
begin
    Stop := false;
    CutDirections[0] := dirHorizontal;
    CutDirections[1] := dirVertical;
    Rotations[0] := false;
    Rotations[1] := true;
    CutWidth := ACutWidth;
    // TODO: calculate exact array sizes...
    State.Pieces.Init(PieceList.size, 2*PieceList.size);
    State.Placed.Init(PieceList.size, 2*PieceList.size);
    State.Cuts.Init(2*PieceList.size, 4*PieceList.size);
    State.Indexes.Init(PieceList.size+1, PieceList.size+1);
    State.Sheets.Init(SheetList.size + PieceList.size, SheetList.size + 3*PieceList.size);
    for ii:=0 to PieceList.size-1 do begin
        piece.Init(PieceList.data[ii]);
        State.Pieces.Insert(piece);
    end;
    for ii:=0 to SheetList.size-1 do begin
        sheet.Init(SheetList.data[ii]);
        State.Sheets.Insert(sheet);
    end;
    Solution.OrigSheetList:=SheetList;
    Solution.CutWidth:=ACutWidth;
    Solution.Valid:=false;
    IndexState.Init(2, 2, State.Sheets.current.size, State.Pieces.current.size);
    State.Indexes.Append(IndexState);
    State.commit;
end;

procedure  TSimpleSolver.StopSolver;
begin
    Stop := true;
end;

function TSimpleSolver.Search:TSolution;
var leaf : boolean;
    LastPieceIndex : integer;
    CutDirection : TDirection;
    rotation : boolean;
    sheet : TSSSheet;
    piece : TSSPiece;
    placement : TPlacement;
    ii : integer;
    tmpcut : TSSCut;
    tmpsheet : TSSSheet;
    tmpplaced : TSSPlacedPiece;
begin
    FStep := 0;
    leaf := true;
    LastPieceIndex := -1;
    repeat
        Inc(FStep);
//        Writeln('step: ', Solution.Step);
//        Writeln('placed: ', State.Placed.toString);
//        Writeln('pieces: ', State.Pieces.toString);
//        Writeln('spaces: ', State.Sheets.toString);
//        Writeln('index: ', State.Indexes.toString);
        if State.Pieces.current.size = 0 then begin
            // Solution! append to Solution
            //Writeln('solution');
            //Writeln('placed: ', State.Placed.toString);
            SetSolution;
            break;
            //Writeln(State.Pieces.current.size);
            State.undo();
            leaf := False;
            if State.Indexes.current.size > 0 then begin
                IndexState := State.Indexes.current.data[State.Indexes.current.size-1];
            end else begin
                break;
                //quit := true;
            end;
            continue;
        end;
        if not IndexState.Next then begin
            State.undo();
            leaf := False;
            if State.Indexes.current.size > 0 then begin
                IndexState := State.Indexes.current.data[State.Indexes.current.size-1];
            end else begin
                break;
                //quit := true;
            end;
//            Writeln('index_state.next false');
            continue;
        end;
        State.Indexes.current.data[State.Indexes.current.size-1] := IndexState;
//        Writeln('Cur index: ', IndexState.toString);
        CutDirection := CutDirections[IndexState.iCutDir];
        rotation := Rotations[IndexState.iRotation];
        sheet := State.Sheets.current.data[IndexState.iSpace];
        piece := State.Pieces.current.data[IndexState.iPiece];
        
        if LastPieceIndex < 0 then begin
            LastPieceIndex := IndexState.iPiece;
        end;
        if leaf and (IndexState.iPiece <> LastPieceIndex) then begin
            // backtrack
            State.undo();
            leaf := False;
            if State.Indexes.current.size > 0 then begin
                IndexState := State.Indexes.current.data[State.Indexes.current.size-1];
            end else begin
                break;
            end;
            continue;
        end else begin
            if rotation then begin
                if piece.Rotatable then begin
                    piece.Init(piece.rotate);
                end else begin
//                    Writeln('non-rotatable');
                    continue;
                end;
            end;
            if sheet.fit(piece) then begin
                State.Sheets.pop(IndexState.iSpace);
                State.Pieces.pop(IndexState.iPiece);
                placement := PutPieceOpt(sheet, piece, CutDirection = dirHorizontal, CutWidth);
                for ii:=0 to placement.ncuts-1 do begin
                    tmpcut.Init(placement.cuts[ii]);
                    State.Cuts.Append(tmpcut);
                end;
                for ii:=0 to placement.nspaces-1 do begin
                    tmpsheet.Init(placement.sheets[ii]);
                    State.Sheets.Insert(tmpsheet);
                end;
                tmpplaced.Init(placement.placedpiece);
                State.Placed.Append(tmpplaced);
                IndexState.Init(2, 2, State.Sheets.current.size, State.Pieces.current.size);
                State.Indexes.append(IndexState);
//                Writeln(State.Indexes.toString);
                State.commit;
                LastPieceIndex := IndexState.iPiece;
                leaf := true;
            end else begin
                //if State.Sheets.current.data[State.Sheets.current.size-1].edge < State.Pieces.current.data[State.Pieces.current.size-1].edge then begin
                if State.Sheets.current.data[State.Sheets.current.size-1].edge < State.Pieces.current.data[0].edge then begin
                    // edge of largest sheet is smaller than largest piece
                    State.undo();
                    leaf := false;
                    if State.Indexes.current.size > 0 then begin
                        IndexState := State.Indexes.current.data[State.Indexes.current.size-1];
                    end else begin
                        break;
                    end;
//                    Writeln('largest space smaller than smallest box');
                    continue;
                end;
            end;
            //
        end;
    until Stop;
    Solution.Step := FStep;
    Search:=Solution;
end;


procedure TSimpleSolver.SetSolution;
var ii : integer;
    cut : TCut;
    placed : TPlacedPiece;
    space : TSheet;
begin
    Solution.CutList.Init();
    Solution.SheetList.Init();
    Solution.PlacedList.Init();
    Solution.Valid:=true;
    for ii:=0 to State.Cuts.current.size-1 do begin
        cut := State.Cuts.current.data[ii];
        Solution.CutList.Append(cut);
    end;
    for ii:=0 to State.Placed.current.size-1 do begin
        placed := State.Placed.current.data[ii];
        Solution.PlacedList.Append(placed);
    end;
    for ii:=0 to State.Sheets.current.size-1 do begin
        space := State.Sheets.current.data[ii];
        Solution.SheetList.Append(space);
    end;
end;

end.
