program agg_ass;

var r1 : record x : integer; case b : boolean of
	true : ( y : char ); false : ( z : string [2] ) end;
    a1 : array [1..3] of integer;
    a2 : ^ array [1..*] of integer;
    i : integer;

begin
  r1 := (i, true, r1.z[1]);
  r1 := (r1.x, false, 'B');
  a1 := (a1[2], i, 1);
  a2^ := (i, 1, a1[i]);
end.
   