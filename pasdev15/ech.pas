program cathy;

var i: integer;

begin
rewrite (ttyoutput); open (tty);

loop
  write (tty, 'How many times? '); break; readln(tty);
  read (tty, i);
  exit if i = 0;
  while i > 0 do begin
    write (tty, '% ');
    i := i - 1
    end;
  writeln (tty)
  end
end.
   