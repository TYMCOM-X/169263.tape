
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 236*)
(*TEST 6.8.2.4-1, CLASS=CONFORMANCE*)
(* This test checks that non-local goto statements are allowed *)
program t6p8p2p4d1;
label 1;
var
   b:boolean;
procedure q;
begin
   b:=true;
   goto 1;
end;   (*of q*)
begin
        rewrite(output,'suite.txt',[preserve]);   (*main*)       writeln('suite program #236');
   q;
   b:=false;
1: if b then
      writeln(' PASS...6.8.2.4-1 NON-LOCAL GOTO')
   else
      writeln(' FAIL...6.8.2.4-1 NON-LOCAL GOTO');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
 