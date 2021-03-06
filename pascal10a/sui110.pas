
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 110*)
(*TEST 6.4.5-6, CLASS=CONFORMANCE*)
(* Two types are compatible if they are identical or if one is a
  subrange of the other, or if both are subranges of the same type.
  This program tests these points, but with only subranges of the
  same type having some overlap.
  If the message produced is incomplete, or the program does not
  compile, then the compiler fails.*)
program t6p4p5d6;
type
   colour   = (red,pink,orange,yellow,green,blue,brown);
   colourtoo= colour;
var
   col1  : colour;
   col2  : colourtoo;
   subcol1  : red..yellow;
   subcol2  : orange..blue;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #110');
   col1:=red;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   col2:=red;
   if col1 = col2 then write(' PA');
   subcol1:=red;
   if col1 = subcol1 then write('S');
   subcol1:=yellow;
   subcol2:=yellow;
   if subcol1 = subcol2 then writeln('S...6.4.5-6')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    