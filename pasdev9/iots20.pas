program iots20;

var
  f: text;
  r: real;

begin
  rewrite (f, 'TTY:');
  r := 2.00001;
  repeat
    r := r * 2.0;
    writeln (f, '[', r:40:7:e, ']');
  until (r > 1.0) and (r < 2.3)
end.
 