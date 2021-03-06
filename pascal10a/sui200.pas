
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 200*)
(*TEST 6.6.6.3-4, CLASS=DEVIANCE*)
(* This test checks that neither trunc nor round are permitted
  to have integer parameters. The Standard requires these to be
  real. The compiler deviates if the program compiles and prints
  DEVIATES. *)
program t6p6p6p3d4;
var
   i:integer;
   x:real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #200');
   i:=1979;
   x:=trunc(i)+round(i+1);
   writeln(' DEVIATES...6.6.6.3-4, TRUNC/ROUND')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   