{
Generic list with incremental allocation and array backend.
@author(Matyas Jani)
}
{$mode objfpc}
unit glist;

interface

uses sysutils;

{ Increment size for reallocation. }
const increment = 10;

{ @abstract(Exception class for @link(TGList).) }
type EGList = class(Exception);

{ @abstract(Generic list based on array and incremental allocation.) }
type generic TGList<T> = object
  public
    { Array storing the elements. }
    data : array of T;

    { Number of elements currently in the list. }
    size : integer;

    { Insert element at a given position (0 based). }
    procedure Insert(elem: T; index: integer);

    { Append element to the end of list. }
    procedure Append(elem: T);

    { Remove and return element from the given position (0 based). }
    function Pop(index: integer):T;

    { Allocate list with given initial size. }
    constructor Init(initsize: integer);

    { Allocate list with default @link(increment) size. }
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
