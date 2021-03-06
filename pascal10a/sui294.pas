
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 294*)
(*TEST 6.9.4-1, CLASS=CONFORMANCE*)
(* This test checks that a write procedure with many parameters
  is equivalent to many write procedures with one parameter each.
  The compiler fails if the program does not compile or the program
  prints FAIL. *)
program t6p9p4d1;
var
   f:text;
   a,b,c,d,e:char;
   a1,b1,c1,d1,e1:char;
   counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #294');
   counter:=0;
   rewrite(f);
   a:='A';
   b:='B';
   c:='C';
   d:='D';

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   e:='E';
   write(f,a,b,c,d,e);
   writeln(f);
   reset(f);
   read(f,a1,b1,c1,d1,e1);
   if (a=a1) and (b=b1) and (c=c1) and (d=d1) and (e=e1) then
      counter:=counter+1;
   rewrite(f);
   write(f,a);
   write(f,b);
   write(f,c);
   write(f,d);
   write(f,e);
   writeln(f);
   reset(f);
   read(f,a1,b1,c1,d1,e1);
   if(a1=a) and (b1=b) and (c1=c) and (d1=d) and (e1=e) then
      counter:=counter+1;
   if (counter=2) then
      writeln(' PASS...6.9.4-1, WRITE')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else
      writeln(' FAIL...6.9.4-1, WRITE');
end.
 