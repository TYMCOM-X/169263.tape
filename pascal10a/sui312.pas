
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 312*)
(*TEST 6.10-2, CLASS=IMPLEMENTATIONDEFINED*)
(* This program checks the effect of doing a rewrite on the
  standard file output. The effect is implementation dependent. *)
program t6p10d2;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #312');
   rewrite(output);       writeln('suite program #312');
   writeln(' IMPLEMENTATION DEPENDENT...6.10-2');
   writeln(' A REWRITE HAS BEEN PERFORMED ON FILE OUTPUT');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 