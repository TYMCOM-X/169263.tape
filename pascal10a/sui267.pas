
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 267*)
(*TEST 6.8.3.9-8, CLASS=CONFORMANCE*)
(* This program checks that a control variable of a for statement
  is not undefined if the for statement is left via a goto
  statement. The compiler fails if the program does not compile
  or the program prints FAIL. *)
program t6p8p3p9d8;
label 100;
var
   i,j:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #267');
   j:=1;
   for i:=1 to 10 do
   begin
      if (j=5) then
         goto 100;
      j:=j+1;
   end;
100:

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if i=j then
      writeln(' PASS...6.8.3.9-8, FOR')
   else
      writeln(' FAIL...6.8.3.9-8, FOR');
end.
   