unit test;

interface

uses crt, sysutils;

procedure teststring(name: string; expected: string; got: string);
procedure testint(name: string; expected: integer; got: integer);
procedure endtests;

implementation

var numfailed : integer;

procedure test(name: string; passed: boolean);
var numdots, ii : word;
    dots : string;
begin
    numdots := ScreenWidth - 10 - 2 - Length(name);
    dots := '';
    for ii:= 1 to numdots do begin
        dots := dots + '.';
    end;
    Write(name + ': ' + dots + ' [');
    if passed then begin
        TextColor(Green);
        Write('ok');
        NormVideo;
        Writeln(']');
    end else begin
        Inc(numfailed);
        TextColor(Red);
        Write('fail');
        NormVideo;
        Writeln(']');
    end;
end;

procedure testint(name: string; expected: integer; got: integer);
var passed : boolean;
begin
    passed := (expected = got);
    test(name, passed);
    if not passed then begin
        Writeln('    expected: ', expected);
        Writeln('         got: ', got);
    end;
end;


procedure teststring(name: string; expected: string; got: string);
var passed : boolean;
begin
    passed := (expected = got);
    test(name, passed);
    if not passed then begin
        Writeln('    expected: ', expected);
        Writeln('         got: ', got);
    end;
end;

procedure endtests;
begin
    exitcode:=numfailed;
end;

begin
    numfailed := 0;
end.