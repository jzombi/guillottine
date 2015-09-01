{$mode objfpc}
uses sysutils, undoarray, test;


Type TElem = object
    name : string;
    sortvalue : real;
    function toString:string;
end;

function TElem.toString:string;
begin
    toString := name;
end;

function CreateTElem(sortvalue: real):TElem;
begin
    CreateTElem.name := FloatToStr(sortvalue);
    CreateTElem.sortvalue := sortvalue;
end;

Type TElemUndoArray = specialize TUndoArray<TElem>;

var myarr : TElemUndoArray;

begin
    myarr.Init(11, 22);
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
    teststring('TUndoArray', '1;2;3;4;5;6;7;7;8;9;10;', myarr.toString);
    myarr.pop(7);
    myarr.pop(2);
    teststring('TUndoArray', '1;2;4;5;6;7;8;9;10;', myarr.toString);
    myarr.commit;
    myarr.undo;
    teststring('TUndoArray', '', myarr.toString);
    myarr.insert(CreateTElem(1));
    myarr.insert(CreateTElem(2));
    myarr.commit; // 1;2;
    myarr.pop(0);
    myarr.insert(CreateTElem(10));
    myarr.insert(CreateTElem(6));
    myarr.insert(CreateTElem(8));
    myarr.commit; // 2;6;8;10;
    myarr.insert(CreateTElem(7));
    myarr.insert(CreateTElem(3));
    myarr.insert(CreateTElem(5));
    myarr.insert(CreateTElem(9));
    myarr.pop(2);
    myarr.pop(3);
    myarr.commit; // 2;3;6;8;9;10;
    myarr.insert(CreateTElem(7));
    myarr.insert(CreateTElem(4));
    myarr.commit; // 2;3;4;6;7;8;9;10;
    teststring('TUndoArray', '2;3;4;6;7;8;9;10;', myarr.toString);
    myarr.undo;
    teststring('TUndoArray', '2;3;6;8;9;10;', myarr.toString);
    myarr.undo;
    teststring('TUndoArray', '2;6;8;10;', myarr.toString);
    myarr.undo;
    teststring('TUndoArray', '1;2;', myarr.toString);
    myarr.insert(CreateTElem(3));
    teststring('TUndoArray', '1;2;3;', myarr.toString);
    myarr.revert;
    teststring('TUndoArray', '1;2;', myarr.toString);
    myarr.undo;
    teststring('TUndoArray', '', myarr.toString);
    myarr.undo;
    teststring('TUndoArray', '', myarr.toString);
    endtests;
end.