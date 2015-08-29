{$mode objfpc}
unit simplesolver;

interface

uses solver, rectangle, undoarray, sysutils;

type TSSBox = object(TBox)
    public
        sortvalue : real;
        constructor Init(name: string; width, height: real; rotatable: boolean; rotated: boolean);
        constructor Init(base: TBox);
end;

type TSSSpace = object(TSpace)
    public
        sortvalue : real;
        constructor Init(name: string; left, top, width, height: real);
        constructor Init(base: TSpace);
end;

type TSSCut = object(TCut)
    public
        sortvalue : integer;
        constructor Init(base: TCut);
end;

type TSSPlacedBox = object(TPlacedBox)
    public
        sortvalue : integer;
        constructor Init(base: TPlacedBox);
end;


type TIndexState = object
    first : boolean;
    sortvalue : integer;
    sCutDir_, sRotation_, sSpace_, sPiece_ : integer;
    iCutDir, iRotation, iSpace, iPiece : integer;
    constructor Init(sCutDir, sRotation, sSpace, sPiece: integer);
    function next:boolean;
    function toString:string;
end;

type TPlacedArray = specialize TUndoArray<TSSPlacedBox>;
     TBoxesArray = specialize TUndoArray<TSSBox>;
     TCutsArray = specialize TUndoArray<TSSCut>;
     TSpacesArray = specialize TUndoArray<TSSSpace>;
     TIndexesArray = specialize TUndoArray<TIndexState>;

type TSimpleSolverState = object
    public
        placed : TPlacedArray;
        boxes : TBoxesArray;
        cuts : TCutsArray;
        spaces : TSpacesArray;
        indexes : TIndexesArray;
        procedure Commit;
        procedure Undo;
end;

type TSolution = record
    cutlist: TCutList;
    spacelist: TSpaceList;
    origspacelist: TSpaceList;
    placedlist: TPlacedList;
    valid: boolean;
    step: integer;
    cutwidth: real;
end;

type ESimpleSolver = class(Exception);

type TSimpleSolver = class(TSolver)
    private
        FStep : integer;
        stop : boolean;
        cutwidth_ : real;
        state : TSimpleSolverState;
        index_state : TIndexState;
        cut_directions : array[0..1] of TDirection;
        rotations : array[0..1] of boolean;
        solution : TSolution;
        procedure SetSolution;
    public
        constructor Create(spacelist: TSpaceList; piecelist: TPieceList; cutwidth: real);
        function Search:TSolution;
        procedure StopSolver;
        property Step: integer read FStep;
end;

implementation

constructor TSSBox.Init(name: string; width, height: real; rotatable: boolean; rotated: boolean);
begin
    TBox.Init(name, width, height, rotatable, rotated);
    Writeln(toString, edge);
    sortvalue := -edge;
end;

constructor TSSBox.Init(base: TBox);
begin
    TBox.Init(base.name_, base.width_, base.height_, base.rotatable_, base.rotated_);
    sortvalue := -edge;
end;

constructor TSSSpace.Init(name: string; left, top, width, height: real);
begin
    TSpace.Init(name, left, top, width, height);
    sortvalue := edge;
end;

constructor TSSSpace.Init(base: TSpace);
begin
    TSpace.Init(base.name_, base.left_, base.top_, base.width_, base.height_);
    sortvalue := edge;
end;

constructor TSSCut.Init(base: TCut);
begin
    TCut.Init(base.name_, base.left_, base.top_, base.width_, base.height_, base.direction_);
    sortvalue := 0;
end;

constructor TSSPlacedBox.Init(base: TPlacedBox);
begin
    TPlacedBox.Init(base.name_, base.left_, base.top_, base.width_, base.height_, base.rotated_);
    sortvalue := 0;
end;

constructor TIndexState.Init(sCutDir, sRotation, sSpace, sPiece: integer);
begin
    sCutDir_ := sCutDir;
    sRotation_ := sRotation;
    sSpace_ := sSpace;
    sPiece_ := sPiece;
    iCutDir := 0;
    iRotation := 0;
    iSpace := 0;
    iPiece := 0;
    sortvalue := 0;
    first := true;
end;

function TIndexState.next:boolean;
begin
    if first then begin
        first := false;
        if (sCutDir_ = 0) or (sRotation_ = 0) or (sSpace_ = 0) or (sPiece_ = 0) then exit(false);
        exit(true);
    end;
    next := true;
    Inc(iCutDir);
    if iCutDir = sCutDir_ then begin
        iCutDir := 0;
        Inc(iRotation);
        if iRotation = sRotation_ then begin
            iRotation := 0;
            Inc(iSpace);
            if iSpace = sSpace_ then begin
                iSpace := 0;
                Inc(iPiece);
                if iPiece = sPiece_ then next := false;
            end;
        end;
    end;
//    Writeln('updated');
end;

function TIndexState.toString:string;
begin
    toString := 'IndexState(' + FloatToStr(sCutDir_) + ', ' + FloatToStr(sRotation_) + ', ' + FloatToStr(sSpace_) + ', ' + FloatToStr(sPiece_) + ', ' +
                 FloatToStr(iCutDir) + ', ' + FloatToStr(iRotation) + ', ' + FloatToStr(iSpace) + ', ' + FloatToStr(iPiece) + ', ' +
                 BoolToStr(first, 'true', 'false') + ')';
end;


procedure TSimpleSolverState.Commit;
begin
    placed.commit;
    boxes.commit;
    cuts.commit;
    spaces.commit;
    indexes.commit;
end;

procedure TSimpleSolverState.Undo;
begin
    placed.undo;
    boxes.undo;
    cuts.undo;
    spaces.undo;
    indexes.undo;
end;


constructor TSimpleSolver.Create(spacelist: TSpaceList; piecelist: TPieceList; cutwidth: real);
var space : TSSSpace;
    box : TSSBox;
    ii : integer;
begin
    stop := false;
    cut_directions[0] := horizontal;
    cut_directions[1] := vertical;
    rotations[0] := false;
    rotations[1] := true;
    cutwidth_ := cutwidth;
    // TODO: calculate exact array sizes...
    state.boxes.Init(piecelist.size, 2*piecelist.size);
    state.placed.Init(piecelist.size, 2*piecelist.size);
    state.cuts.Init(2*piecelist.size, 4*piecelist.size);
    state.indexes.Init(piecelist.size+1, piecelist.size+1);
    state.spaces.Init(spacelist.size + piecelist.size, spacelist.size + 3*piecelist.size);
    for ii:=0 to piecelist.size-1 do begin
        box.Init(piecelist.data[ii]);
        state.boxes.Insert(box);
    end;
    for ii:=0 to spacelist.size-1 do begin
        space.Init(spacelist.data[ii]);
        state.spaces.Insert(space);
    end;
    solution.origspacelist:=spacelist;
    solution.cutwidth:=cutwidth;
    solution.valid:=false;
    index_state.Init(2, 2, state.spaces.current.size, state.boxes.current.size);
    state.indexes.Append(index_state);
    state.commit;
end;

procedure  TSimpleSolver.StopSolver;
begin
    stop := true;
end;

function TSimpleSolver.Search:TSolution;
var leaf : boolean;
    last_piece_index : integer;
    cut_direction : TDirection;
    rotation : boolean;
    space : TSSSpace;
    piece : TSSBox;
    placement : TPlacement;
    ii : integer;
    tmpcut : TSSCut;
    tmpspace : TSSSpace;
    tmpplaced : TSSPlacedBox;
begin
    FStep := 0;
    leaf := true;
    last_piece_index := -1;
    repeat
        Inc(FStep);
        Writeln('step: ', solution.step);
//        Writeln('placed: ', state.placed.toString);
//        Writeln('pieces: ', state.boxes.toString);
//        Writeln('spaces: ', state.spaces.toString);
//        Writeln('index: ', state.indexes.toString);
        if state.boxes.current.size = 0 then begin
            // solution! append to solution
            Writeln('solution');
            Writeln('placed: ', state.placed.toString);
            SetSolution;
            break;
            Writeln(state.boxes.current.size);
            state.undo();
            leaf := False;
            if state.indexes.current.size > 0 then begin
                index_state := state.indexes.current.data[state.indexes.current.size-1];
            end else begin
                break;
                //quit := true;
            end;
            continue;
        end;
        if not index_state.next then begin
            state.undo();
            leaf := False;
            if state.indexes.current.size > 0 then begin
                index_state := state.indexes.current.data[state.indexes.current.size-1];
            end else begin
                break;
                //quit := true;
            end;
//            Writeln('index_state.next false');
            continue;
        end;
        state.indexes.current.data[state.indexes.current.size-1] := index_state;
//        Writeln('Cur index: ', index_state.toString);
        cut_direction := cut_directions[index_state.iCutDir];
        rotation := rotations[index_state.iRotation];
        space := state.spaces.current.data[index_state.iSpace];
        piece := state.boxes.current.data[index_state.iPiece];
        
        if last_piece_index < 0 then begin
            last_piece_index := index_state.iPiece;
        end;
        if leaf and (index_state.iPiece <> last_piece_index) then begin
            // backtrack
            state.undo();
            leaf := False;
            if state.indexes.current.size > 0 then begin
                index_state := state.indexes.current.data[state.indexes.current.size-1];
            end else begin
                break;
            end;
            continue;
        end else begin
            if rotation then begin
                if piece.rotatable_ then begin
                    piece.Init(piece.rotate);
                end else begin
//                    Writeln('non-rotatable');
                    continue;
                end;
            end;
            if space.fit(piece) then begin
                state.spaces.pop(index_state.iSpace);
                state.boxes.pop(index_state.iPiece);
                placement := PutBoxOpt(space, piece, cut_direction = horizontal, cutwidth_);
                for ii:=0 to placement.ncuts-1 do begin
                    tmpcut.Init(placement.cuts[ii]);
                    state.cuts.Append(tmpcut);
                end;
                for ii:=0 to placement.nspaces-1 do begin
                    tmpspace.Init(placement.spaces[ii]);
                    state.spaces.Insert(tmpspace);
                end;
                tmpplaced.Init(placement.placedbox);
                state.placed.Append(tmpplaced);
                index_state.Init(2, 2, state.spaces.current.size, state.boxes.current.size);
                state.indexes.append(index_state);
//                Writeln(state.indexes.toString);
                state.commit;
                last_piece_index := index_state.iPiece;
                leaf := true;
            end else begin
                if state.spaces.current.data[state.spaces.current.size-1].edge < state.boxes.current.data[state.boxes.current.size-1].edge then begin
                    // edge of largest space is smaller than smallest piece
                    state.undo();
                    leaf := false;
                    if state.indexes.current.size > 0 then begin
                        index_state := state.indexes.current.data[state.indexes.current.size-1];
                    end else begin
                        break;
                    end;
//                    Writeln('largest space smaller than smallest box');
                    continue;
                end;
            end;
            //
        end;
    until stop;
    solution.step := FStep;
    Search:=solution;
end;


procedure TSimpleSolver.SetSolution;
var ii : integer;
    cut : TCut;
    placed : TPlacedBox;
    space : TSpace;
begin
    solution.cutlist.Init();
    solution.spacelist.Init();
    solution.placedlist.Init();
    solution.valid:=true;
    for ii:=0 to state.cuts.current.size-1 do begin
        cut := state.cuts.current.data[ii];
        solution.cutlist.Append(cut);
    end;
    for ii:=0 to state.placed.current.size-1 do begin
        placed := state.placed.current.data[ii];
        solution.placedlist.Append(placed);
    end;
    for ii:=0 to state.spaces.current.size-1 do begin
        space := state.spaces.current.data[ii];
        solution.spacelist.Append(space);
    end;
end;

end.
