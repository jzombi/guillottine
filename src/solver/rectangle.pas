{$mode objfpc}
unit rectangle;

interface

uses sysutils;

type TDirection = (vertical, horizontal);

type TRectangle = object
    public
        left_, top_, width_, height_ : real;
        name_ : string;
        area, edge : real;
        constructor Init(name: string; left, top, width, height: real);
        function Fit(rect: TRectangle):boolean;
        function toString:ansistring;
end;


type TBox = object(TRectangle)
    public
        rotatable_ : boolean;
        rotated_   : boolean;
        constructor Init(name: string; width, height : real; rotatable: boolean; rotated: boolean);
        function Rotate:TBox;
        function toString:ansistring;
end;

type EBox = class(Exception);


type TPlacedBox = object(TRectangle)
    public
        rotated_ : boolean;
        constructor Init(name: string; left, top, width, height : real; rotated: boolean);
        function toString:ansistring;
end;



type TCut = object(TRectangle)
    public
        direction_ : TDirection;
        constructor Init(name: string; left, top, width, height: real; direction: TDirection);
        function toString:ansistring;
end;




type TSpace = object(TRectangle)
    public
        function toString:ansistring;
end;

type ESpace = class(Exception);

type TPlacement = record
    spaces : array[0..1] of TSpace;
    nspaces : integer;
    cuts : array[0..1] of TCut;
    ncuts : integer;
    placedbox : TPlacedBox;
end;

function PutBox(const space: TSpace; const rect: TBox; const firstcut: TDirection; const cutwidth: real):TPlacement;
function PutBoxOpt(const space: TSpace; const rect: TBox; const edgemax: boolean; const cutwidth: real):TPlacement;



implementation

constructor TRectangle.Init(name: string; left, top, width, height: real);
begin
    left_ := left;
    top_ := top;
    width_ := width;
    height_ := height;
    name_ := name;
    area := width_*height_;
    edge := 2*(width_ + height_);
end;

function TRectangle.Fit(rect: TRectangle):boolean;
begin
    if (width_ >= rect.width_) and (height_ >= rect.height_) then begin
        Fit := true;
    end else begin
        Fit := false;
    end;
end;

function TRectangle.toString:ansistring;
begin
    toString := 'TRectangle(''' + name_ + ''', ' + FloatToStr(left_) + ', ' + FloatToStr(top_) + ', ' + FloatToStr(width_) + ', ' + FloatToStr(height_) + ')';
end;

constructor TBox.Init(name: string; width, height : real; rotatable: boolean; rotated: boolean);
begin
    TRectangle.Init(name, 0, 0, width, height);
    rotatable_ := rotatable;
    rotated_ := rotated;
end;

function TBox.Rotate:TBox;
begin
    if rotatable_ then begin
        Rotate.Init(name_, height_, width_, rotatable_, not rotated_);
    end else begin
        raise EBox.Create('Rotation attempt on non-rotatable box.');
    end;
end;

function TBox.toString:ansistring;
begin
    toString := 'TBox(''' + name_ + ''', ' + FloatToStr(width_) + ', ' + FloatToStr(height_) + ', ' + BoolToStr(rotatable_, 'true', 'false') +
                ', ' + BoolToStr(rotated_, 'true', 'false') + ')';
end;


constructor TPlacedBox.Init(name: string; left, top, width, height : real; rotated: boolean);
begin
    TRectangle.Init(name, left, top, width, height);
    rotated_ := rotated;
end;

function TPlacedBox.toString:ansistring;
begin
    toString := 'TPlacedBox(''' + name_ + ''', ' + FloatToStr(left_) + ', ' + FloatToStr(top_) + ', ' +
                FloatToStr(width_) + ', ' + FloatToStr(height_) +', ' + BoolToStr(rotated_, 'true', 'false') + ')';
end;


constructor TCut.Init(name: string; left, top, width, height : real; direction: TDirection);
begin
    TRectangle.Init(name, left, top, width, height);
    direction_ := direction;
end;

function TCut.toString:ansistring;
var dirname : string;
begin
    if direction_ = horizontal then begin
        dirname := 'horizontal';
    end else if direction_ = vertical then begin
        dirname := 'vertical';
    end;
    toString := 'TCut(''' + name_ + ''', ' + FloatToStr(left_) + ', ' + FloatToStr(top_) + ', ' +
                FloatToStr(width_) + ', ' + FloatToStr(height_) + ', ' + dirname + ')';
end;


function PutBox(const space: TSpace; const rect: TBox; const firstcut: TDirection; const cutwidth: real):TPlacement;
{
        Places rect on self.
        firstcut:
        horizontal:
            +------+--+
            | rect |  |
            +------+--+
            |         |
            +---------+

        vertical:
            +------+--+
            | rect |  |
            +------+  |
            |      |  |
            +------+--+
}
begin
    if not space.Fit(rect) then raise ESpace.Create('Could not place rectangle, it should be checked with Fit first.');
    PutBox.placedbox.Init(rect.name_, space.left_, space.top_, rect.width_, rect.height_, rect.rotated_);
    PutBox.nspaces := 0;
    PutBox.ncuts := 0;
    if firstcut = horizontal then begin
        if rect.height_ < space.height_ then begin
            PutBox.cuts[PutBox.ncuts].Init('', space.left_, space.top_+rect.height_, space.width_, cutwidth, horizontal);
            Inc(PutBox.ncuts);
            if rect.height_ + cutwidth < space.height_ then begin
                PutBox.spaces[PutBox.nspaces].Init(space.name_, space.left_, space.top_+rect.height_+cutwidth, space.width_, space.height_-rect.height_-cutwidth);
                Inc(PutBox.nspaces);
            end;
        end;
        if rect.width_ < space.width_ then begin
            PutBox.cuts[PutBox.ncuts].Init('', space.left_+rect.width_, space.top_, cutwidth, rect.height_, vertical);
            Inc(PutBox.ncuts);
            if rect.width_ + cutwidth < space.width_ then begin
                PutBox.spaces[PutBox.nspaces].Init(space.name_, space.left_+rect.width_+cutwidth, space.top_, space.width_-rect.width_-cutwidth, rect.height_);
                Inc(PutBox.nspaces);
            end;
        end;
    end else if firstcut = vertical then begin
        if rect.width_ < space.width_ then begin
            PutBox.cuts[PutBox.ncuts].Init('', space.left_+rect.width_, space.top_, cutwidth, space.height_, vertical);
            Inc(PutBox.ncuts);
            if rect.width_ + cutwidth < space.width_ then begin
                PutBox.spaces[PutBox.nspaces].Init(space.name_, space.left_+rect.width_+cutwidth, space.top_, space.width_-rect.width_-cutwidth, space.height_);
                Inc(PutBox.nspaces);
            end;
        end;
        if rect.height_ < space.height_ then begin
            PutBox.cuts[PutBox.ncuts].Init('', space.left_, space.top_+rect.height_, rect.width_, cutwidth, horizontal);
            Inc(PutBox.ncuts);
            if rect.height_ + cutwidth < space.height_ then begin
                PutBox.spaces[PutBox.nspaces].Init(space.name_, space.left_, space.top_+rect.height_+cutwidth, rect.width_, space.height_-rect.height_-cutwidth);
                Inc(PutBox.nspaces);
            end;
        end;
    end;
end;

function PutBoxOpt(const space: TSpace; const rect: TBox; const edgemax: boolean; const cutwidth: real):TPlacement;
var horizedgemax : boolean;
begin
    horizedgemax := (2*space.width_ - rect.width_ + space.height_) > (2*space.height_ - rect.height_ + space.width_);
    if horizedgemax xor edgemax then begin
        PutBoxOpt := PutBox(space, rect, vertical, cutwidth);
    end else begin
        PutBoxOpt := PutBox(space, rect, horizontal, cutwidth);
    end;
end;

function TSpace.toString:ansistring;
begin
    toString := 'TSpace(''' + name_ + ''', ' + FloatToStr(left_) + ', ' + FloatToStr(top_) + ', ' + FloatToStr(width_) + ', ' + FloatToStr(height_) + ')';
end;


end.