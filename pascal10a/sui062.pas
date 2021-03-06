
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  62*)
(*TEST 6.4.2.2-3, CLASS=CONFORMANCE*)
(* The Pascal Standard states that type BOOLEAN has truth values
  denoted by the identifiers true and false, and that they are
  such that false is less than true.
  This program tests if the compiler allows this. *)
program t6p4p2p2d3;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #062');
   if false < true then
      writeln(' PASS...6.4.2.2-3')
   else
      writeln(' FAIL...6.4.2.2-3')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 