
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 258*)
(*TEST 6.8.3.8-1, CLASS=CONFORMANCE*)
(* This test checks that a while loop is not entered
  if the initial value of the boolean expression is false.
  The compiler fails if the program prints FAIL. *)
program t6p8p3p8d1;
var
   bool:boolean;
   counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #258');
   counter:=0;
   bool:=false;
   while bool do
   begin
      counter:=counter+1;
      bool:=false;
   end;
   if (counter=0) then
      writeln(' PASS...6.8.3.8-1, WHILE')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else
      writeln(' FAIL...6.8.3.8-1, WHILE');
end.
 