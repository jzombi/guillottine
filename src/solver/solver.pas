{
Base class for solvers. Currently just a stub.
@author(Matyas Jani)
}
{$mode objfpc}
unit solver;

interface

uses rectangle, glist;

type TPieceList = specialize TGList<TPiece>;
     TPlacedList = specialize TGList<TPlacedPiece>;
     TSheetList = specialize TGList<TSheet>;
     TCutList = specialize TGList<TCut>;

{ Base class for solvers. Stub. }
type TSolver = class
end;

implementation


end.
