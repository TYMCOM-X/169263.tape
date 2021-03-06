
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 171*)
(*TEST 6.6.5.2-4, CLASS=CONFORMANCE*)
(* This program tests that the first element of a file f
  is assigned to the buffer variable f^ when the procedure
  reset is used with the file f. *)
program t6p6p5p2d4;
var
   fyle : text;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #171');
   rewrite(fyle);
   writeln(fyle,'ABC');
   writeln(fyle,'DEF');
   reset(fyle);
   if fyle^='A' then
      writeln(' PASS...6.6.5.2-4')
   else
      writeln(' FAIL...6.6.5.2-4')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   