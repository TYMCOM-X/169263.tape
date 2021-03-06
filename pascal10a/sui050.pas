
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  50*)
(*TEST 6.2.2-10, CLASS=CONFORMANCE*)
(* This obscure program is nevertheless standard Pascal.
  An inner scope hides part of a type while leaving other parts
  accessible. The compiler fails if the program does not
  compile or the program prints FAIL. *)
program t6p2p2d10;
type
   colour=(red,amber,green);
var
   c:colour;
procedure nested;
type
   colour=(purple,red,blue);
var
   paint:colour;
begin
   c:=green;
   paint:=red;
   c:=pred(amber);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if (ord(c)<>0) or (ord(paint)<>1) then
      writeln(' FAIL...6.2.2-10, SCOPE');
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #050');
   nested;
   if (c<> red) then
      writeln(' FAIL...6.2.2-10, SCOPE')
   else
      writeln(' PASS...6.2.2-10, SCOPE')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   