{$mode objfpc}
uses sysutils, rectangle, test;


var space : TSpace;
    box : TBox;
    placement : TPlacement;

begin
    space.Init('Space', 1, 2, 10, 20);
    box.Init('Box', 4, 3, true, false);
    teststring('TSpace toString', 'TSpace(''Space'', 1, 2, 10, 20)', space.toString);
    teststring('TBox toString', 'TBox(''Box'', 4, 3, true, false)', box.toString);
    placement := PutBox(space, box, horizontal, 0);
    testint('TPlacement.ncuts', 2, placement.ncuts);
    testint('TPlacement.nspaces', 2, placement.nspaces);
    teststring('TCut.toString', 'TCut('''', 1, 5, 10, 0, horizontal)', placement.cuts[0].toString);
    teststring('TCut.toString', 'TCut('''', 5, 2, 0, 3, vertical)', placement.cuts[1].toString);
    teststring('TSpace.toString', 'TSpace(''Space'', 1, 5, 10, 17)', placement.spaces[0].toString);
    teststring('TSpace.toString', 'TSpace(''Space'', 5, 2, 6, 3)', placement.spaces[1].toString);
    teststring('TPlacedBox.toString', 'TPlacedBox(''Box'', 1, 2, 4, 3, false)', placement.placedbox.toString);

//    myarr.delete(7);
//    myarr.delete(2);
//    teststring('TSortedArray', '1;2;4;5;6;7;8;9;10;', myarr.toString);
//    endtests;
end.