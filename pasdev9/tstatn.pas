program prog;
var x, y, z: real;
const cvt = 180/arctan(0,-1);
begin
  rewrite (tty); open (tty);
  loop
    write (tty,'X, Y: '); break; readln (tty);
    read (tty,x,y);
    z := arctan (x,y);
    writeln (tty,'ARCTAN (',x,',',y,') = ',z*cvt);
  end;
end.
