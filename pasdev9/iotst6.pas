program iotst6;

var
  f,g: text;
  s: string[20];

begin
  open (f, 'FOO.INP');
  rewrite (g, 'FOO.OUT');
  s := 'Formatted write';
  writeln (g, '[', s, ']');
  get (f);
  read (f, substr (s, 2, 3):3 );
  writeln (g, 'First three chars [', s, ']' );
  read (f, s);
  writeln (g, 'Echo: [', s, ']');
  writeln (g, 'Your mom', substr (s, 3, 5):6:l, 'is cute.');
  writeln (g, '':10 );  (* just for yahs *)
  writeln (g, s:3, '[', s:30, '] [', '':2, ']');
  close (f);
  close (g)
end.
   