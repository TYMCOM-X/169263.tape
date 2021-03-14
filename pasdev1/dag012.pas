program dag012;

var i, j, k, l, m, n: integer;
    a, b: array [1..10] of integer;

begin
  case i of
    1:  begin
	  i := a [m];
	  a [m+1] := j;
	  k := i;
	end;

    2:  begin
	  i := a [m];
	  a [n] := j;
	  k := i;
	end;

    3:  begin
	  i := a [m];
	  n := 0;
	  k := i;
	end;

    4:  begin
	  i := a[m];
	  m := 0;
	  k := i;
	end;

    5:  begin
	  a [i] := a [j];
	  a [i+1] := 0;
	  k := a [i];
	end;

    6:  begin
	  a [i] := a [j];
	  a [k] := a [i];
	  k := a [i];
	end;

    7:  begin
	  i := a[b[j]];
	  a[b[j]+1] := k;
	  l := i;
	end;

    8:  begin
	  i := a[b[j]];
	  b := a;
	  k := i;
	end;

    9:  begin
	  i := a[b[j]];
	  a := b;
	  k := i;
	end;

    10: begin
	  i := a[b[j]];
	  j := 0;
	  k := i;
	end;

    11: begin
	  i := a[b[j]];
	  a[n] := 0;
	  k := i;
	end;

    12: begin
	  i := a[b[j]];
	  b[n] := 0;
	  k := i;
	end;

    13: begin
	  i := a[b[j]];
	  b[j+1] := 0;
	  k := i;
	end

  end;
end.
   