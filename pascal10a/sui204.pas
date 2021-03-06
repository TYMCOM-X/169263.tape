
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 204*)
(*TEST 6.6.6.4-4, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as the
  function succ is applied to the last value
  of an ordinal type.
  The error should be detected by the compiler
  or at run-time. *)
program t6p6p6p4d4;
type
   enumerated = (first,second,third,last);
var
   ordinal : enumerated;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #204');
   ordinal:=succ(last);
   writeln(' ERROR NOT DETECTED...6.6.6.4-4')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 