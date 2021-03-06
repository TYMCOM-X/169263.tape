
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 296*)
(*TEST 6.9.4-3, CLASS=CONFORMANCE*)
(* This test checks the implementation of integer output.
  The compiler fails if the program does not compile or the program
  prints FAIL. *)
program t6p9p4d3;
var
   f:text;
   a:char;
   b:packed array [1..26] of char;
   i:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #296');
   rewrite(f);
   writeln(f,0:3,1:3,-1:3,10:3,99:3,100:3,-100:3,1111:3);
   reset(f);
   for i:=1 to 26 do
      read(f,b[i]);
   if (b='  0  1 -1 10 99100-1001111') then
      writeln(' PASS...6.9.4-3, WRITE INTEGERS')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else
      writeln(' FAIL...6.9.4-3, WRITE INTEGERS');
end.
    