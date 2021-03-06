
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 293*)
(*TEST 6.9.3-1, CLASS=CONFORMANCE*)
(* This test checks that readln is correctly implemented.
  The compiler fails if the program does not compile or the
  program prints FAIL. *)
program t6p9p3d1;
var
   f:text;
   a,b,c:char;
   counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #293');
   counter:=0;
   rewrite(f);
   writeln(f,'ABC');
   writeln(f,'DE');
   reset(f);
   readln(f,a,b,c);
   read(f,a);
   if (a='D') then counter:=counter+1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   reset(f);
   read(f,a,b,c);
   readln(f);
   read(f,a);
   if(a='D') then counter:=counter+1;
   reset(f);
   read(f,a);
   while not eoln(f) do get(f);
   get(f);
   if (f^='D') then counter:=counter+1;
   if (counter=3) then
      writeln(' PASS...6.9.3-1, READLN')
   else
      writeln(' FAIL...6.9.3-1, READLN');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 