program prog;
public var
  i, j, k: integer;
  r1, r2, r3: real;
  d1, d2, d3: -100.0..100.0 prec 15;
  set1, set2, set3: set of 0..100;
  b1, b2, b3: boolean;
begin
  if b1 then i := 1;
  if b1 or b2 then i := 2;
  if b1 orif b2 then i := 3;
  if i = j then i := 4;
  if i > j then i := 5;
  if i <= j then i := 6;
  if r1 < r2 then i := 7;
  if d1 = d2 then i := 8;
  if i = 7 then i := 9;
  if i < 0 then i := 10;
  b1 := i <> 0;
  b1 := i = 0;
  b1 := b2 andif (i = j+5);
  while i < k do i := i + 1;
end.
  