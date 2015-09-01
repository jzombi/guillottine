{$mode objfpc}
uses sysutils, sortedarray, test;


Type TElem = object
    name : string;
    sortvalue : real;
    function toString:ansistring;
end;

function TElem.toString:ansistring;
begin
    toString := name;
end;

function CreateTElem(sortvalue: real):TElem;
begin
    CreateTElem.name := FloatToStr(sortvalue);
    CreateTElem.sortvalue := sortvalue;
end;

Type TElemSortedArray = specialize TSortedArray<TElem>;

var myarr : TElemSortedArray;

begin
    myarr.Init(11);
    myarr.insert(CreateTElem(1));
    myarr.insert(CreateTElem(2));
    myarr.insert(CreateTElem(10));
    myarr.insert(CreateTElem(6));
    myarr.insert(CreateTElem(8));
    myarr.insert(CreateTElem(7));
    myarr.insert(CreateTElem(3));
    myarr.insert(CreateTElem(5));
    myarr.insert(CreateTElem(9));
    myarr.insert(CreateTElem(7));
    myarr.insert(CreateTElem(4));
    teststring('TSortedArray', '1;2;3;4;5;6;7;7;8;9;10;', myarr.toString);
    myarr.delete(7);
    myarr.delete(2);
    teststring('TSortedArray', '1;2;4;5;6;7;8;9;10;', myarr.toString);
    endtests;
end.