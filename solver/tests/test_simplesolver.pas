uses simplesolver;

var ss : TSimpleSolver;

begin
    ss := TSimpleSolver.Create;
    ss.Search;
    ss.Free;
end.