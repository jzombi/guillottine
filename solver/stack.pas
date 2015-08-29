{$mode objfpc}
unit stack;

interface

uses sysutils;

type EStack = class(Exception);

type generic TStack<T> = object
    public
        size : integer;
        data : array of T;
        constructor Init(maxsize: integer);
        procedure Push(elem: T);
        function Pop:T;
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