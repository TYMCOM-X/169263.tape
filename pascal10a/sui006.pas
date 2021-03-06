
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number   6*)
(*TEST 6.1.3-2, CLASS=CONFORMANCE*)
(* The Pascal Standard states that matching upper and lower
  case letters are equivalent in identifiers and word-symbols
  (i.e. reserved words) if they are permitted. If this is the
  case for this compiler, then the program shall print 'PASS'.
  The compiler fails  if the program cannot be compiled.
  This test is irrelevant for one case compilers. *)
program t6p1p3d2;
var
   conform  : integer;
Begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #006');
   BEGIN
      Conform:=1;
      CONFORM:=2;
      If conform = 2 then
         writeln(' PASS...6.1.3-2')
   enD
end.
  