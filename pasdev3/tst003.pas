program tst003;
var
  i,j: integer;
begin
  for i := 1 to 100 do j := j + 1;
  for i := -10 to 0 do j := j + 1;
  for i := -10 to 1 do j := j + 1;
  for i := -10 to -1 do j := j + 1;
  for i := 1 to i do    j := j + 1;
  for i := i to 100 do j := j + 1;
  for i := i to j do j := j + 1;
end.
 