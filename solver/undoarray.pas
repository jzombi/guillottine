{$mode objfpc}
unit undoarray;

interface

uses sortedarray, stack;

type generic TUndoArray<T> = object
    private
        type TTSortedArray = specialize TSortedArray<T>;
        type TUndoElem = record
            elem : T;
            index : integer;
            insert : boolean;
            revision : integer;
        end;
        type TUndoElemStack = specialize TStack<TUndoElem>;
    public
        committed : boolean;
        current : TTSortedArray;
        revision : integer;
        undostack : TUndoElemStack;
        constructor Init(maxsize, undosize: integer);
        function Pop(index: integer):T;
        procedure Insert(elem: T);
        procedure Append(elem: T);
        procedure Commit;
        function Undo:boolean;
        function Revert:boolean;
        function toString:ansistring;
end;

implementation

constructor TUndoArray.Init(maxsize, undosize: integer);
begin
    current.Init(maxsize);
    undostack.Init(undosize);
    revision := 0;
    committed := false;
end;

function TUndoArray.Pop(index: integer):T;
var undoelem : TUndoElem;
begin
    undoelem.elem := current.data[index];
    undoelem.index := index;
    undoelem.revision := revision;
    undoelem.insert := false;
    undostack.Push(undoelem);
    current.Delete(index);
    committed := false;
    Pop := undoelem.elem;
end;

procedure TUndoArray.Insert(elem: T);
var undoelem : TUndoElem;
begin
    undoelem.index := current.BisectLeft(elem);
    undoelem.revision := revision;
    undoelem.insert := true;
    undostack.Push(undoelem);
    committed := false;
    current.Insert(elem, undoelem.index);
end;

procedure TUndoArray.Append(elem: T);
var undoelem : TUndoElem;
begin
    undoelem.index := current.size;
    undoelem.revision := revision;
    undoelem.insert := true;
    undostack.Push(undoelem);
    committed := false;
    current.Insert(elem, undoelem.index);
end;


procedure TUndoArray.Commit;
begin
    if not committed then begin
        committed := true;
        Inc(revision);
    end;
end;

function TUndoArray.Undo:boolean;
begin
    Undo := false;
    if committed then begin
        committed := false;
        Dec(revision);
        if Revert then begin
            Undo := true;
        end;
    end;
end;

function TUndoArray.Revert:boolean;
var top : TUndoElem;
begin
    if committed then exit(false);
    if undostack.size = 0 then exit(true);
    top := undostack.top;
    while (undostack.size > 0) and (top.revision = revision) do begin
        undostack.pop;
        if top.insert = true then begin
            current.Delete(top.index);
        end else begin
            current.Insert(top.elem, top.index);
        end;
        if undostack.size > 0 then top := undostack.top;
    end;
    committed := true;
    Revert := true;
end;

function TUndoArray.toString:ansistring;
begin
    toString := current.toString;
end;

end.