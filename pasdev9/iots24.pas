program iots24;

var
  f, g: text;
  r: -1.0e23 .. 1.0e23  prec 16;
  i: -10000 .. 10000;

procedure writeit (r: -1.0e23 .. 1.0e23 prec 16);
  var i: 1..20;

  begin
    for i := 1 to 20 do
      writeln (g, '[', r:24:i:e, '] [', r:22:i:f, '] [', r:22:i, ']');
    break (g);
  end;

begin
  rewrite (g, 'tty:');
  open (f, 'tty:');
  r := 2.0;
    loop
      write (g, 'Enter integer to be used:'); break (g); readln (f);
      read (f, i);
    exit if i = 0;
      r := 0.0;
      while i > 0 do begin
	i := i -1;
	r := r + 1.0
	end;
      writeit (sqrt (r) );
      writeit (exp (ln (r)));
      writeit (sinh (r) );
      writeit (1.0/r);
    end
end.
    