program test;
var i, j, k, l: integer;
    a: array [1..10] of integer;
    p, q: ^ array [1..10] of integer;
begin
  i := j * k + l;
  j := j * k + i;
  a[i] := j;
  a[j] := i;
  write (a[i], a[j]);
  if not eof then begin
    read (i, j, k, l);
    i := ord (input^);
    j := ord (input^);
    k := cursor (input);
    l := cursor (input);
    writeln (i, j, k, l);
    i := ord (input^);
    j := cursor (input);
  end;
  p^ [1] := 1;
  p^ [2] := 2;
  q^ [1] := 3;
  q^ [2] := p^ [2];
  i := p^ [1];
  j := p^ [2];
  k := q^ [1];
  l := q^ [2];
end.
  