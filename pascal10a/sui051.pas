
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  51*)
(*TEST 6.3-1, CLASS=CONFORMANCE*)
(* This program exhibits all legal productions for a constant
  in a const declaration. *)
program t6p3d1;
const
   ten = 10;
   minusten = -10;
   minustentoo = -ten;
   decade = ten;
   dot = '.';
   stars = '****';
   on_ = true;
   pi = 3.1415926;
   minuspi = - pi;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #051');
   writeln(' PASS...6.3-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 