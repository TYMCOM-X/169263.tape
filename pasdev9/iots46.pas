program iots46;

var
  f: text;
  starfile: file of *;
  filename: string[50];
  width, i: 0..10000;

begin
  open (tty); rewrite (ttyoutput);
  write (tty, 'Enter file name to empty: '); break (tty); readln (tty);
  read (tty, filename);
  if filename = '' then stop;
  write (tty, 'Text file? '); break (tty); readln (tty);
  if eoln (tty) orif (uppercase (tty^) = 'Y') then begin
    rewrite (f, filename, [preserve]);
    empty (f)
    end
  else begin
    update (starfile, filename, [preserve]);
    empty (starfile)
    end
end.
 