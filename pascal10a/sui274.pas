
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 274*)
(*TEST 6.8.3.9-15, CLASS=CONFORMANCE*)
(* This program checks the order of evaluation of the limit expressions
  in a for statement.
  The compiler fails if the program prints FAIL. *)
program t6p8p3p9d15;
var
   i,j,k:integer;
function f(var k:integer) : integer;
begin
   k:=k+1;
   f:=k;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #274');
   k:=0;
   j:=0;
   for i:=f(k) to f(k)+10 do
   begin
      j:=j+1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      writeln(i);
   end;
   if (j=12) then
      writeln(' PASS...6.8.3.9-15, FOR')
   else
      writeln(' FAIL...6.8.3.9-15, FOR');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    