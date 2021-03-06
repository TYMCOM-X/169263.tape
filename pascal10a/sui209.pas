
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 209*)
(*TEST 6.6.6.5-2, CLASS=CONFORMANCE*)
(* This program tests the predicate odd.
  The compiler fails if the program does not compile or
  the program states that this is so. *)
program t6p6p6p5d2;
var
   i,counter : integer;
function myodd(i:integer):boolean;
   begin
      myodd := (abs(i mod 2) = 1);
   end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #209');
   counter:=0;
   for i:=-10 to 10 do
      if odd(i) then
      begin
         if myodd(i) then counter := counter+1
      end else begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

         if not myodd(i) then counter := counter+1
      end;
   if counter=21 then
      writeln(' PASS...6.6.6.5-2')
   else
      writeln(' FAIL...6.6.6.5-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   