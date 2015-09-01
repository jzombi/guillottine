{$mode objfpc}
unit solver;

interface

uses rectangle, sortedarray, glist;

type TPieceList = specialize TGList<TBox>;
     TPlacedList = specialize TGList<TPlacedBox>;
     TSpaceList = specialize TGList<TSpace>;
     TCutList = specialize TGList<TCut>;

type TSolver = class
end;

implementation


end.
