program tst200;

var
    i, j, k: integer;
    a, b: array [1..20] of integer;
    p1, p2, p3: ^ integer;
    q1, q2, q3: ^ record
	f1, f2: integer;
	case tag: integer of
	    1: (v1: integer);
	    2: (v2: integer);
	    3: (v3: integer)
    end;

begin
  i := 3;
  j := k;
  k := i + j;
  a [j] := i;
  a [k] := 0;
  a [i] := p1^;
  a [2] := p2^;
  p1^ := j;
  k := p1^;
  p2^ := 10;
  k := p1^;
end.
