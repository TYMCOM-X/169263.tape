program ftest1;

var a, b, c, d: integer;

begin
  if true
    then a := b
    else b := a;
  while false do
    c := d;
  if false
    then a := b
    else b := a;
  case 5 of
    1..3: a := 1;
    4..6: a := 2;
    7..9: a := 3;
    others: a := 0;
  end;
  case 9 of
    1..8: a := 4;
    10..18: a := 5;
  end;
  while true do
    a := 0;
end.
 