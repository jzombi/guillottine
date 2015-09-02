{
Generic stack implementation on a preallocated array.
@author(Matyas Jani)
}

{$mode objfpc}
unit stack;

interface

uses sysutils;

{ @abstract(Exception type for @link(TStack).) }
type EStack = class(Exception);

{ @abstract(TStack is a generic stack on a preallocated array.) }
type generic TStack<T> = object
    public
        { The current size of the stack. }
        size : integer;

        { Stores the elements of the stack. }
        data : array of T;

        { Initializes the array for the stack
          @param(maxsize Number of elements for the array.)
        }
        constructor Init(maxsize: integer);

        { Push element to the top of the stack.
          @param(elem Element to push on the stack.)
          @raises(Estack  on overflow.)
        }
        procedure Push(elem: T);

        { Return and remove top element of the stack.
          @raises(EStack on underflow.)
        }
        function Pop:T;

        { Return top element of the stack. }
        function Top:T;
end;

implementation

constructor TStack.Init(maxsize: integer);
begin
    SetLength(data, maxsize);
    size := 0;
end;

procedure TStack.Push(elem: T);
begin
    if size = Length(data) then raise EStack.Create('Can not push into full stack.');
    data[size] := elem;
    Inc(size);
end;

function TStack.Pop:T;
begin
    if size = 0 then raise EStack.Create('Can not pop from empty stack.');
    Dec(size);
    Pop := data[size];
end;

function TStack.Top:T;
begin
    if size = 0 then raise EStack.Create('Empty stack has no top element.');
    Top := data[size-1];
end;

end.
