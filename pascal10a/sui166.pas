
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 166*)
(*TEST 6.6.3.6-5, CLASS=DEVIANCE*)
(* This test checks that parameter list compatibility is correctly
  implemented. The compiler deviates if the program compiles
  and prints DEVIATES. *)
program t6p6p3p6d5;
type
   natural = 0..maximum(integer);
procedure actual(i:integer; j:integer; n:natural);
begin
   i:=n
end;
procedure p(procedure formal(a:integer;b:integer));
var
   k,l:integer;
begin
   k:=1; l:=2;
   formal(k,l)
end;
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #166');
   p(actual);
   writeln(' DEVIATES...6.6.3.6-5, NO OF PARS DIFFERENT')
end.
 