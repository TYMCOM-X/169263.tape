
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 129*)
(*TEST 6.4.6-12, CLASS=DEVIANCE*)
(* The standard specifies that a filetype T2 cannot be
  assignment-compatible with an identical type T1, nor can a
  structure containing such a filetype. This precludes any
  assignments involving files. The compiler deviates if the program
  compiles and prints DEVIATES. *)
program t6p4p6d12;
var
   f1,f2:text;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #129');
   rewrite(f1);
   writeln(f1,' DEVIATES');
   writeln(' DEVIATES...6.4.6-12, FILES');
   f2:=f1;
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   