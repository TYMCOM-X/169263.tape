program iots50;

var
  x: 1.0 .. 10.0 prec 10;

begin
  open (tty);; rewrite (ttyoutput);
  loop
    write (tty, 'Number: '); break (tty); readln (tty);
    read (tty, x);
    writeln (tty, x)
  end
end.
   