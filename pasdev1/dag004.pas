program dag004;

var
    a: array [1..20] of integer;
    i, j, k: integer;

begin
  a [i] := j;
  k := a [i];
  a [j] := i;
  k := a [i];
  k := a [j];

  a [i-1] := 5;
  a [i]   := 10;
  a [i+1] := 15;
  k := a [i-1];
  k := a [i];
  k := a [i+1];

  a [1] := 3;
  a [2] := 6;
  k := a [i];
  k := a [1];
  k := a [2];
end.
   