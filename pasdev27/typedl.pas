program type_deals;
var m, n: integer;
    fname: file_name;
    line: string [60];
begin
  open (tty);
  rewrite (tty);
  write (tty, 'Deal listing file? ');
  break;
  readln (tty);
  read (tty, fname);
  reset (input, fname || '.LST');
  if iostatus <> io_ok then begin
    writeln (tty, 'Unable to open file');
    stop;
  end;
  writeln (tty, 'Adjust paper to top of page and type <CR>');
  break;
  readln (tty);
  n := 0;
  while not eof do begin
    n := n + 1;
    writeln (tty);
    for m := 1 to 20 do begin
      readln (line);
      write (tty, line);
      if m = 10 then
	write (tty, '': 55 - cursor (ttyoutput), 'DEAL ', n: 0);
      writeln (tty);
    end;
    writeln (tty);
  end;
  for m := 1 to 68 - 22 * (n mod 3) do
    writeln (tty);
end.
  