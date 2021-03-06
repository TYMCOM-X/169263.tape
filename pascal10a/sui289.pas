
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 289*)
(*TEST 6.9.2-2, CLASS=CONFORMANCE*)
(* This test checks that a read of a character variable is
  equivalent to correctly positioning the buffer variable.
  The compiler fails if the program does not compile or the program
  prints FAIL. *)
program t6p9p2d2;
var
   f:text;
   a,b,a1,b1:char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #289');
   rewrite(f);
   writeln(f,'ABC');
   reset(f);
   read(f,a);
   read(f,b);
   reset(f);
   a1:=f^; get(f);
   b1:=f^; get(f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if(a=a1) and (b=b1) then
      writeln(' PASS...6.9.2-2, READ')
   else
      writeln(' FAIL...6.9.2-2, READ');
end.
   