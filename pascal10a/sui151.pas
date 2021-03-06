
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 151*)
(*TEST 6.6.3.1-3, CLASS=CONFORMANCE*)
(* This program tests that files may be passed to
  procedures as parameters, as a file is a type, and any
  type may be passed as a parameter.
  The compiler fails if the program does not compile. *)
program t6p6p3p1d3;
type
   fyle = text;
var
   elyf : fyle;
procedure test(var anyfile : fyle);
begin
   rewrite(anyfile);
   writeln(anyfile,'THIS FILE WAS A PARAMETER');
   writeln(' PASS...6.6.3.1-3')
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #151');
   test(elyf)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
    