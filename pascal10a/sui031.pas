
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  31*)
(*TEST 6.1.8-5, CLASS=DEVIANCE*)
(* Nested comments are not permitted in Pascal and hence
  this program should not compile. The compiler deviates if the
  program compiles and prints DEVIATES. *)
program t6p1p8d5;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #031');
   (* writeln(' RAN')
   (* writeln(' RAN1') *)*)
   writeln(' RAN2') *)
   writeln(' DEVIATES...6.1.8-5, NESTED COMMENTS')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 