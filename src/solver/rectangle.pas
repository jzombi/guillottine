{
Unit containing base classes for different rectangle types.
@author(Matyas Jani)
}
{$mode objfpc}
unit rectangle;

interface

uses sysutils;

{ @abstract(Cut direction.) }
type TDirection = (dirVertical, dirHorizontal);

{ @abstract(Base type for rectangles.) }
type TRectangle = object
    public
        { Placement / dimension of rectangle. }
        Left, Top, Width, Height : integer;

        { Name of the rectangle. }
        Name : string;

        { Area and edge are calculated at @link(Init). }
        Area, Edge : integer;

        { Initializes the rectangle. }
        constructor Init(AName: string; ALeft, ATop, AWidth, AHeight: integer);

        { Checks if the parameter rectangle fits self.
        @param(rect The other rectangle.)
        @return(True iff dimensions of rect are <= dimensions of self.)
        }
        function Fit(Rect: TRectangle):boolean;

        function toString:ansistring;
end;

{ @abstract(Piece to be placed on a @link(TSheet).) }
type TPiece = object(TRectangle)
    public
        { Stores if rectangle can be rotated. }
        Rotatable : boolean;

        { Stores if rectangle is already rotated. }
        Rotated   : boolean;

        { Benefit for placing this piece. }
        Gain : integer;

        { Initialize the piece with parameters. }
        constructor Init(AName: string; AWidth, AHeight : integer;
                         AGain: integer; ARotatable: boolean; ARotated: boolean);

        { Rotate the piece. }
        function Rotate:TPiece;

        function toString:ansistring;
end;

{ @abstract(Exception class for @link(TPiece).) }
type EPiece = class(Exception);

{ @abstract(Piece placed on a @link(TSheet).) }
type TPlacedPiece = object(TRectangle)
    public
        { Store if the piece was placed in rotated orientation. }
        Rotated : boolean;

        { Benefit for placing this piece. }
        Gain : integer;

        { Initialize placed piece with parameters. }
        constructor Init(AName: string; ALeft, ATop, AWidth, AHeight : integer;
                         AGain: integer; ARotated: boolean);

        function toString:ansistring;
end;


{ @abstract(Object for storing the parameters of a cut.) }
type TCut = object(TRectangle)
    public
        { Direction of cut. }
        Direction : TDirection;

        { Initialize cut with parameters. }
        constructor Init(AName: string; ALeft, ATop, AWidth, AHeight: integer; ADirection: TDirection);

        function toString:ansistring;
end;


{ @abstract(Sheet on which @link(TPiece)s are placed.) }
type TSheet = object(TRectangle)
    public
        function toString:ansistring;
end;

{ @abstract(Exception class for @link(TSheet).) }
type ESheet = class(Exception);

{ @abstract(Stores the result of placing a piece on a sheet.) }
type TPlacement = record
    sheets : array[0..1] of TSheet;
    nspaces : integer;
    cuts : array[0..1] of TCut;
    ncuts : integer;
    PlacedPiece : TPlacedPiece;
end;

{ Place a piece on a sheet, by cutting the sheet two times.
The piece is placed in the top left corner of the sheet.
@param(sheet the sheet on which the piece is placed)
@param(piece the piece which should be placed)
@param(firstcut direction of the first cut)
@param(cutwidth the width of the desposed material when cutting the sheet)
@return(placement @link(TPlacement))
}
function PutPiece(const sheet: TSheet; const rect: TPiece; const firstcut: TDirection; const cutwidth: integer):TPlacement;

{ Place a piece on a sheet, by cutting the sheet two times.
Calls @link(PutPiece).
@param(edgemax if true PutPiece is called with firstcut set to have maximal edges for the remaining sheets.)
}
function PutPieceOpt(const sheet: TSheet; const rect: TPiece; const edgemax: boolean; const cutwidth: integer):TPlacement;



implementation

constructor TRectangle.Init(AName: string; ALeft, ATop, AWidth, AHeight: integer);
begin
    Left := ALeft;
    Top := ATop;
    Width := AWidth;
    Height := AHeight;
    Name := AName;
    Area := Width*Height;
    Edge := 2*(Width + Height);
end;

function TRectangle.Fit(Rect: TRectangle):boolean;
begin
    if (Width >= Rect.Width) and (Height >= Rect.Height) then begin
        Fit := true;
    end else begin
        Fit := false;
    end;
end;

function TRectangle.toString:ansistring;
begin
    toString := 'TRectangle(''' + Name + ''', ' + FloatToStr(Left) + ', ' + FloatToStr(Top) + ', ' + FloatToStr(Width) + ', ' + FloatToStr(Height) + ')';
end;

constructor TPiece.Init(AName: string; AWidth, AHeight : integer; AGain: integer; ARotatable: boolean; ARotated: boolean);
begin
    TRectangle.Init(AName, 0, 0, AWidth, AHeight);
    Rotatable := ARotatable;
    Rotated := ARotated;
end;

function TPiece.Rotate:TPiece;
begin
    if Rotatable then begin
        Rotate.Init(Name, Height, Width, Gain, Rotatable, not Rotated);
    end else begin
        raise EPiece.Create('Rotation attempt on non-rotatable box.');
    end;
end;

function TPiece.toString:ansistring;
begin
    toString := 'TPiece(''' + Name + ''', ' + FloatToStr(Width) + ', ' + FloatToStr(Height) + ', ' + BoolToStr(Rotatable, 'true', 'false') +
                ', ' + BoolToStr(Rotated, 'true', 'false') + ')';
end;


constructor TPlacedPiece.Init(AName: string; ALeft, ATop, AWidth, AHeight : integer; AGain: integer; ARotated: boolean);
begin
    TRectangle.Init(AName, ALeft, ATop, AWidth, AHeight);
    Rotated := ARotated;
    Gain := AGain;
end;

function TPlacedPiece.toString:ansistring;
begin
    toString := 'TPlacedPiece(''' + Name + ''', ' + FloatToStr(Left) + ', ' + FloatToStr(Top) + ', ' +
                FloatToStr(Width) + ', ' + FloatToStr(Height) +', ' + BoolToStr(Rotated, 'true', 'false') + ')';
end;


constructor TCut.Init(AName: string; ALeft, ATop, AWidth, AHeight : integer; ADirection: TDirection);
begin
    TRectangle.Init(AName, ALeft, ATop, AWidth, AHeight);
    Direction := ADirection;
end;

function TCut.toString:ansistring;
var dirname : string;
begin
    if Direction = dirHorizontal then begin
        dirname := 'horizontal';
    end else if Direction = dirVertical then begin
        dirname := 'vertical';
    end;
    toString := 'TCut(''' + Name + ''', ' + FloatToStr(Left) + ', ' + FloatToStr(Top) + ', ' +
                FloatToStr(Width) + ', ' + FloatToStr(Height) + ', ' + dirname + ')';
end;


function PutPiece(const sheet: TSheet; const rect: TPiece; const firstcut: TDirection; const cutwidth: integer):TPlacement;
{
        Places rect on self.
        firstcut:
        dirHorizontal:
            +------+--+
            | rect |  |
            +------+--+
            |         |
            +---------+

        dirVertical:
            +------+--+
            | rect |  |
            +------+  |
            |      |  |
            +------+--+
}
begin
    if not sheet.Fit(rect) then raise ESheet.Create('Could not place rectangle, it should be checked with Fit first.');
    PutPiece.placedpiece.Init(rect.Name, sheet.Left, sheet.Top, rect.Width, rect.Height, rect.Gain, rect.Rotated);
    PutPiece.nspaces := 0;
    PutPiece.ncuts := 0;
    if firstcut = dirHorizontal then begin
        if rect.Height < sheet.Height then begin
            PutPiece.cuts[PutPiece.ncuts].Init('', sheet.Left, sheet.Top+rect.Height, sheet.Width, cutwidth, dirHorizontal);
            Inc(PutPiece.ncuts);
            if rect.Height + cutwidth < sheet.Height then begin
                PutPiece.sheets[PutPiece.nspaces].Init(sheet.Name, sheet.Left, sheet.Top+rect.Height+cutwidth, sheet.Width, sheet.Height-rect.Height-cutwidth);
                Inc(PutPiece.nspaces);
            end;
        end;
        if rect.Width < sheet.Width then begin
            PutPiece.cuts[PutPiece.ncuts].Init('', sheet.Left+rect.Width, sheet.Top, cutwidth, rect.Height, dirVertical);
            Inc(PutPiece.ncuts);
            if rect.Width + cutwidth < sheet.Width then begin
                PutPiece.sheets[PutPiece.nspaces].Init(sheet.Name, sheet.Left+rect.Width+cutwidth, sheet.Top, sheet.Width-rect.Width-cutwidth, rect.Height);
                Inc(PutPiece.nspaces);
            end;
        end;
    end else if firstcut = dirVertical then begin
        if rect.Width < sheet.Width then begin
            PutPiece.cuts[PutPiece.ncuts].Init('', sheet.Left+rect.Width, sheet.Top, cutwidth, sheet.Height, dirVertical);
            Inc(PutPiece.ncuts);
            if rect.Width + cutwidth < sheet.Width then begin
                PutPiece.sheets[PutPiece.nspaces].Init(sheet.Name, sheet.Left+rect.Width+cutwidth, sheet.Top, sheet.Width-rect.Width-cutwidth, sheet.Height);
                Inc(PutPiece.nspaces);
            end;
        end;
        if rect.Height < sheet.Height then begin
            PutPiece.cuts[PutPiece.ncuts].Init('', sheet.Left, sheet.Top+rect.Height, rect.Width, cutwidth, dirHorizontal);
            Inc(PutPiece.ncuts);
            if rect.Height + cutwidth < sheet.Height then begin
                PutPiece.sheets[PutPiece.nspaces].Init(sheet.Name, sheet.Left, sheet.Top+rect.Height+cutwidth, rect.Width, sheet.Height-rect.Height-cutwidth);
                Inc(PutPiece.nspaces);
            end;
        end;
    end;
end;

function PutPieceOpt(const sheet: TSheet; const rect: TPiece; const edgemax: boolean; const cutwidth: integer):TPlacement;
var horizedgemax : boolean;
begin
    horizedgemax := (2*sheet.Width - rect.Width + sheet.Height) > (2*sheet.Height - rect.Height + sheet.Width);
    if horizedgemax xor edgemax then begin
        PutPieceOpt := PutPiece(sheet, rect, dirVertical, cutwidth);
    end else begin
        PutPieceOpt := PutPiece(sheet, rect, dirHorizontal, cutwidth);
    end;
end;

function TSheet.toString:ansistring;
begin
    toString := 'TSheet(''' + Name + ''', ' + FloatToStr(Left) + ', ' + FloatToStr(Top) + ', ' + FloatToStr(Width) + ', ' + FloatToStr(Height) + ')';
end;


end.
