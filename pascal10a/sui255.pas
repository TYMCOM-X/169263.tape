
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 255*)
(*TEST 6.8.3.7-1, CLASS=CONFORMANCE*)
(* This test checks that a repeat loop is executed at least once.
  The compiler fails if the program prints FAILS. *)
program t6p8p3p7d1;
var
   counter:integer;
   bool:boolean;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #255');
   bool:=true;
   counter:=0;
   repeat
      counter:=counter+1
   until bool;
   if(counter=1) then
      writeln(' PASS...6.8.3.7-1, REPEAT')
   else
      writeln(' FAIL...6.8.3.7-1, REPEAT');
end.
    