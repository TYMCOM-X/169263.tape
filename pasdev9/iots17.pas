program iots17;

var
  f, g: text;
  s1, s2: string[200];
  i, j: -1000 .. 10000;

begin
  open (f, 'tty:');
  rewrite (g, 'tty:');
    loop
      write (g, 'Enter string:');
      break (g); readln (f); read (f, s1);
      if s1 = '' then stop;
      getstring (s1, i:3); writeln (g, 'I= ', i);
      getstring (substr (s1, i), j:3); writeln (g, 'J= ', j);
      s2 := s1;
      getstring (s2, substr (s1, i, j) );
      writeln (g, '[', s1, ']')
    end
end.
   