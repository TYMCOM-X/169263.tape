
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  33*)
(*TEST 6.2.1-2, CLASS=CONFORMANCE*)
(* This program checks that multiple repetitions are possible
  in the declaration parts, and is provided as a check.  Practically
  all occurrences will re-appear elsewhere in the validation
  suite. *)
program t6p2p1d2;
label
   1,2,3;
const
   one=1;
   two=2;
   three=3;
type
   small = 1..3;
   larger = 1..10;
   biggest = 1..100;
var
   tiny : small;
   soso : larger;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   big : biggest;
procedure p(var x : small);
begin
   x:=1
end;
procedure q(var y : larger);
begin
   y:=2
end;
procedure r(var z : biggest);
begin
   z:=3
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #033');
   p(tiny); goto 2;
1: r(big); goto 3;
2: q(soso); goto 1;
3: if (tiny=one) and (soso=two) and (big=three) then
      writeln(' PASS...6.2.1-2')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
 