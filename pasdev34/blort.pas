program blort;

var arg1 :minimum(real)..maximum(real) prec 16;
    arg2 :integer;

begin

   open(tty);
   rewrite(tty);
   writeln(tty,'Round Test');

   repeat

      write(tty,'round > ');
      break(tty);
      readln(tty);
      read(tty,arg1,arg2);
      writeln(tty,'before ',arg1);
      writeln(tty,'after  ',round(arg1,arg2));
      writeln(tty,' ');
   
   until false;
 end.
   