
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  29*)
(*TEST 6.1.8-3, CLASS=CONFORMANCE*)
(* This program tests that if the compiler allows both
  forms of comments, must the delimiters be the same.
  If only one form of comment is permitted, the test is not relevant. *)
program t6p1p8d3;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #029');
   (* This is a standard comment *)
   (* This is an alternative form *)
   (* What will happen here? *).....*)
   (* Or here? *).....*)
   writeln(' PASS...6.1.8-3')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 