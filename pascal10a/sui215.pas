
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 215*)
(*TEST 6.7.2.2-3, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as the second operand
  of the DIV operator is 0.
  The error should be detected at run-time. *)
program t6p7p2p2d3;
var
   i, j, k : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #215');
   i:=6;
   j:=0;
   k:=i div j;       (* an error as j=0 *)
   writeln(' ERROR NOT DETECTED...6.7.2.2-3: ZERO DIVIDE (DIV)')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   