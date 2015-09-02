{
Very basic unit testing tool.
@author(Matyas Jani)
}
unit test;

interface

uses crt, sysutils;

{ Tests string output, prints whether it matches or not.
@param(name Name of the test, will be printed to output.)
@param(expected The expected string output.)
@param(got The string output of the function to be compared to @link(expected).)
}
procedure teststring(name: string; expected: string; got: string);

{ Tests integer output, prints whether it matches or not.
@param(name Name of the test, will be printed to output.)
@param(expected The expected integer output.)
@param(got The integer output of the function to be compared to @link(expected).)
}
procedure testint(name: string; expected: integer; got: integer);

{ Sets the exit code to be equal to the number of failed tests (thus zero on success). }
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
