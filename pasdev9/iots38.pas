program getblk;

type
  block = array [1..128] of integer;
  blockfile = file of block;

var
  f, g: text;
  a: blockfile;
  buf: block;
  i: integer;
  filename: string[50];

begin
  for i := 1 to 128 do buf[i] := 0;
  i := 0;
  open (f, 'tty:'); rewrite (g, 'tty:');
  write (g, 'Enter data file: '); break (g); readln (f);
  read (f, filename);
  update (a, filename, [randio,preserve]);
  loop
    writern (a, i, buf);
    readrn (a, i, buf)
  end
end.
   