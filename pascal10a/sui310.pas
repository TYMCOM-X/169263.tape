
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 310*)
(*TEST 6.9.6-1, CLASS=CONFORMANCE*)
(* This program checks that the procedure page is implemented.
  This conformance test is unable to determine whether the compiler
  passes or fails - the user must check that a page has been
  generated. *)
program t6p9p6d1;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #310');
   writeln(' PAGE GENERATION TEST');
   page(output);
   writeln(' IF THIS LINE IS PRINTED ON THE TOP OF A NEW PAGE');
   writeln(' THEN PASS...6.9.6-1, PAGE');
   writeln(' ELSE FAIL...6.9.6-1, PAGE');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   