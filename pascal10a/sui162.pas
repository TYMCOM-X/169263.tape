
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 162*)
(*TEST 6.6.3.6-1, CLASS=DEVIANCE*)
(* This test checks that constants are not permitted
  as var parameters. The compiler deviates if the program
  compiles and prints DEVIATES. *)
program t6p6p3p6d1;
const
   x=1;
var
   y:integer;
procedure assign(var p:integer);
begin
   p:=100
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #162');
   assign(y);
   assign(x);   (*disallowed*)
   writeln(' DEVIATES...6.6.3.6-1, VAR PARAMS')
end.
    