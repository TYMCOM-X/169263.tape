
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 246*)
(*TEST 6.8.3.5-6, CLASS=ERRORHANDLING*)
(* This test is similar to the previous one - a case statement
  is given without a case-constant of the selected value. This
  time the value is a long way outside the case. An error should
  be produced at execution time. *)
program t6p8p3p5d6;
var
   i:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #246');
   i:=1000;
   case i of
   -3,3: writeln(' FAIL...6.8.3.5-6, CASE');
   end;
   writeln(' ERROR NOT DETECTED...6.8.3.5-6, CASE CONSTANT');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 