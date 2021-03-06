program quick_sort options notrace, nocheck;

const
    n = 2000;
    n1 = 2001;
    m = 9;

type
    data = 0..1 prec 16;

var
  l, r, i, j: integer;
  stack: array [1..20] of integer;
  stack_top: packed 0 .. 20;
  done: boolean;
  a: array [1..n1] of data;
  v, t: data;

begin
  stack_top := 0;
  l := 1;
  r := n;
  done := (n <= m);
  while not done do begin
    t := a [(l+r) div 2];
    a [(l+r) div 2] := a [l+1];
    a [l+1] := t;
    if a [l+1] > a [r] then begin
      t := a [l+1];
      a [l+1] := a [r];
      a [r] := t;
    end;
    if a [l] > a [r] then begin
      t := a [l];
      a [l] := a [r];
      a [r] := t;
    end;
    if a [l+1] > a [l] then begin
      t := a [l+1];
      a [l+1] := a [l];
      a [l] := t;
    end;
    i := l + 1;
    j := r;
    v := a [l];
    loop
      repeat
	i := i + 1
      until a [i] >= v;
      repeat
	j := j - 1
      until a [j] <= v;
    exit if j < i;
      t := a [i];
      a [i] := a [j];
      a [j] := t;
    end;
    t := a [l];
    a [l] := a [j];
    a [j] := t;
    if j - l >= r - i + 1 then begin
      if j - l <= m then
	if stack_top = 0 then
	  done := true
	else begin
	  l := stack [stack_top - 1];
	  r := stack [stack_top];
	  stack_top := stack_top - 2;
	end
      else if r - i + 1 <= m then
	r := j - 1
      else begin
	stack [stack_top + 1] := l;
	stack [stack_top + 2] := r;
	stack_top := stack_top + 2;
	l := i;
      end;
    end
    else (* j - l < r - i + 1 *) begin
      if r - i + 1 <= m then
	if stack_top = 0 then
	  done := true
	else begin
	  l := stack [stack_top - 1];
	  r := stack [stack_top];
	  stack_top := stack_top - 2;
	end
      else if j - l <= m then
	l := i
      else begin
	stack [stack_top + 1] := l;
	stack [stack_top + 2] := r;
	stack_top := stack_top + 2;
	r := j - 1;
      end;
    end;
  end;
  a [n+1] := maximum (data);
  for i := n - 1 downto 1 do
    if a [i] > a [i+1] then begin
      v := a [i];
      j := i + 1;
      repeat
	a [j-1] := a [j];
	j := j + 1;
      until a [j] >= v;
      a [j-1] := v;
    end;
end.
 