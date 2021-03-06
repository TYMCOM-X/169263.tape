
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 111*)
(*TEST 6.4.5-7, CLASS=CONFORMANCE*)
(* This program tests that two subranges of the same type with
  no overlap are considered as compatible by the compiler.
  The compiler fails if the program does not compile. *)
program t6p4p5d7;
type
   color = (red,pink,orange,yellow,green,blue,brown);
var
   col1 : red..yellow;
   col2 : green..brown;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #111');
   col1:=yellow;
   col2:=green;
   if col1 < col2 then writeln(' PASS...6.4.5-7')
                  else writeln(' FAIL...6.4.5-7')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    