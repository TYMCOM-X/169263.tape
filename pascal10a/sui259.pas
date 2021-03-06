
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 259*)
(*TEST 6.8.3.8-2, CLASS=CONFORMANCE*)
(* This test checks that the compiler will accept a while
  loop containing no statements. The compiler fails if the
  program does not compile or the program prints FAIL. *)
program t6p8p3p8d2;
var
   a:integer;
function bool:boolean;
begin
   a:=a+1;
   bool:= (a>=5);
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #259');
   a:=0;
   while not bool do ;
   if (a=5) then
      writeln(' PASS...6.8.3.8-2, EMPTY WHILE')
   else

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      writeln(' FAIL...6.8.3.8-2, EMPTY WHILE');
end.
   