program leebug;

var a: array [1..100] of packed record
    v: char;
    s: set of 1..4;
  end;
    i: integer;
    x: string;

begin
  x := '';
  for i := 1 to 100 do
    x := x || a[i].v;
end.
 