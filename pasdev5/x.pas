program prog;
var s: string;
begin
  rewrite (tty); open (tty);
  loop
    write (tty,'File: '); break;
    readln (tty);
  exit if eoln (tty);
    read (tty,s);
    rewrite (output,s);
    if eof (output) then
      writeln (tty,filename(output))
    else writeln (tty,'Can''t open file ',s);
    close (output);
  end;
end.
   