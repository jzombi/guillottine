{$mode objfpc}
unit glist;

interface

uses sysutils;

const increment = 10;

type EGList = class(Exception);

type generic TGList<T> = object
  public
    data : array of T;
    size : integer;
    procedure Insert(elem: T; index: integer);
    procedure Append(elem: T);
    function Pop(index: integer):T;
    constructor Init(initsize: integer);
    constructor Init;
end;

implementation

procedure TGList.Insert(elem: T; index: integer);
begin
    if (index < 0) or (index > size) then raise EGList.Create('Index out of range.');
    if size = Length(data) then SetLength(data, Length(data) + increment);
    Move(data[index], data[index+1], size-index);
    data[index]:=elem;
    Inc(size);
end;

procedure TGList.Append(elem: T);
begin
    if size = Length(data) then SetLength(data, Length(data) + increment);
    data[size]:=elem;
    Inc(size);
end;

function TGList.Pop(index: integer):T;
begin
    if size < 0 then raise EGList.Create('Pop attempt from empty list.');
    if (index < 0) or (index >= size) then raise EGList.Create('Index out of range.');
    Pop:=data[index];
    Move(data[index+1], data[index], size-index-1);
    Dec(size);
end;

constructor TGList.Init(initsize: integer);
begin
  SetLength(data, initsize);
  size := 0;
end;

constructor TGList.Init;
begin
  SetLength(data, increment);
  size := 0;
end;

end.
