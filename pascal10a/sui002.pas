
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number   2*)
(*TEST 6.1.2-1, CLASS=DEVIANCE*)
(* This test checks that nil is implemented as a reserved
  word, as it should be. The compiler deviates if the program compiles
  and prints DEVIATES. *)
program t6p1p2d1;
var
   i:(tick,cross,nil);
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #002');
   i:=nil;
   writeln(' DEVIATES...6.1.2-1, NIL')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  