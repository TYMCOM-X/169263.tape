
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 260*)
(*TEST 6.8.3.9-1, CLASS=CONFORMANCE*)
(* This program checks that assignment follows the  evaluation of
  both expressions  in a for statement.
  The compiler fails if the program prints FAIL. *)
program t6p8p3p9d1;
var
   i,j:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #260');
   i:=1;
   j:=0;
   for i:= (i+1) to (i+10) do
   begin
      j:=j+1;
      writeln(i);
   end;
   if (j=10) then
      writeln(' PASS...6.8.3.9-1, FOR')
   else

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      writeln(' FAIL...6.8.3.9-1, FOR');
end.
 