{
Generic preallocated sorted list with commit/revert/undo actions.
@author(Matyas Jani)
}
{$mode objfpc}
unit undoarray;

interface

uses sortedarray, stack;

{ @abstract(Generic preallocated sorted list type with undo action.) }
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
        { True iff the current revision is committed. }
        committed : boolean;

        { The array containing the current data. }
        current : TTSortedArray;

        { The id of tha current revision (incremental, decreases on undo). }
        revision : integer;

        { Stack to store the actions for undo/revert. }
        undostack : TUndoElemStack;

        { Initialize the list.
        @param(maxsize Number of elements for the array (@link(current)).)
        @param(undosize Number of elements for the undostack (@link(undostack)).)
        }
        constructor Init(maxsize, undosize: integer);

        { Return and remove an element.
        @param(index Index of the element to return (0 based).)
        }
        function Pop(index: integer):T;

        { Insert element by keeping the sort order.
        @param(elem Element to insert.)
        }
        procedure Insert(elem: T);

        { Append element to the end of the list, regardless of the sort order.
        @param(elem Element to insert.)
        }
        procedure Append(elem: T);

        { Increase the revision number, and set @link(committed). }
        procedure Commit;

        { Undo the actions of the last committed revision.
        @return(True on success (if @link(committed) was true).)
        }
        function Undo:boolean;

        { Undo the actions of the current, uncommitted revision.
        @return(True on success (if @link(committed) was false))
        }
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
