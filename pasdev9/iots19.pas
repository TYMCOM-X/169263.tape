program iots19;

var
  f: text;
  r: 1.0 .. 1.0e20  prec 10;

begin
  rewrite (f, 'TTY:');
  r := 2.0;
  repeat
    r := r * 2.0;
    write (f, r:20);
  until (r > 1.0) and (r < 2.3)
end.
   