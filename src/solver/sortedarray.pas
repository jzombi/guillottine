{
Generic preallocated array supporting sorted inserts.
@author(Matyas Jani)
}

{$mode objfpc}
unit sortedarray;

interface

uses sysutils;

{ @abstract(Exception class for @link(TSortedArray).) }
type ESortedArray = class(Exception);

{ @abstract(Generic preallocated array supporting sorted inserts.) }
type generic TSortedArray<T> = object
    public
        { Current number of elements in the array. }
        size : integer;

        { Array containing the elements. }
        data : array of T;

        { Allocate @link(data).
        @param(max Maximal number of elements in the array (allocation size for @link(data)).)
        }
        constructor Init(max: integer);

        { Find the insert index of the element to keep sort order. }
        function BisectLeft(const elem: T):integer;

        { Append element to the end of the array, without keeping sort order.
        @raises(ESortedArray on overflow.)
        }
        procedure Append(elem: T);

        { Insert element with keeping sort order.
        @raises(ESortedArray on overflow.)
        }
        procedure Insert(elem: T);

        { Insert element at position, without keeping sort order.
        @raises(ESortedArray on overflow or invalid index.)
        }
        procedure Insert(elem: T; index: integer);

        { Delete element
        @raises(ESortedArray on invalid index.)
        }
        procedure Delete(index: integer);

        function toString:ansistring;
end;


implementation

constructor TSortedArray.Init(max: integer);
begin
    SetLength(data, max);
    size := 0;
end;

function TSortedArray.BisectLeft(const elem: T):integer;
var first, last, mid : integer;
begin
    if size = 0 then exit(0);
    if data[0].sortvalue >= elem.sortvalue then exit(0);
    if data[size-1].sortvalue < elem.sortvalue then exit(size);
    first := 0;
    last := size;
    mid := last div 2;
    while last-first > 1 do begin
        if data[mid].sortvalue < elem.sortvalue then begin
            first := mid;
        end else begin
            last := mid;
        end;
        mid := first + (last - first) div 2;
    end;
    Inc(first);
    BisectLeft := first;
end;

procedure TSortedArray.Insert(elem: T);
var index : integer;
begin
    if size = Length(data) then raise ESortedArray.Create('Can not insert element into a full array.');
    index := BisectLeft(elem);
    Move(data[index], data[index + 1], (size-index) * SizeOf(T));
    data[index] := elem;
    Inc(size);
end;

procedure TSortedArray.Append(elem: T);
begin
    if size = Length(data) then raise ESortedArray.Create('Can not append element into a full array.');
    data[size] := elem;
    Inc(size);
end;


procedure TSortedArray.Insert(elem: T; index: integer);
begin
    if size = Length(data) then raise ESortedArray.Create('Can not insert element into a full array.');
    if (index < 0) or (index > size) then raise ESortedArray.Create('Index out of range.');
    Move(data[index], data[index + 1], (size-index) * SizeOf(T));
    data[index] := elem;
    Inc(size);
end;


procedure TSortedArray.Delete(index : integer);
begin
    if (index < 0) or (index >= size) then raise ESortedArray.Create('Index out of range');
    Move(data[index + 1], data[index], (size-index-1) * SizeOf(T));
    Dec(size);
end;

function TSortedArray.toString:ansistring;
var ii : integer;
begin
    toString := '';
    for ii:=0 to size-1 do begin
        toString := toString + data[ii].toString + ';';
    end;
end;

end.
