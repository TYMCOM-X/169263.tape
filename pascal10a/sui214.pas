
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 214*)
(*TEST 6.7.2.2-2, CLASS=CONFORMANCE*)
(* This program checks that DIV and MOD are implemented by the
  rule specified by the Pascal Standard.
  The compiler fails if the program does not compile, or the
  program states that this is so. *)
program t6p7p2p2d2;
var
   i, j, counter : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #214');
   counter:=0;
   for i:=0 to 6 do
      for j:=1 to 4 do
         if ((i-j)<((i div j)*j)) and (((i div j)*j)<=i) then
            counter:=counter+1;
   for i:=0 to 6 do
      for j:=1 to 4 do
         if (i mod j)=(i-(i div j)*j) then
            counter:=counter+1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if counter=56 then
      writeln(' PASS...6.7.2.2-2')
   else
      writeln(' FAIL...6.7.2.2-2: DIV MOD')
end.
  