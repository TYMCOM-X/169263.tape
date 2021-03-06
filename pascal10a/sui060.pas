
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  60*)
(*TEST 6.4.2.2-1, CLASS=CONFORMANCE*)
(* This program tests that the standard simple types have all
  been implemented. They are denoted by predefined type identifiers.
  The compiler fails if the program does not compile. *)
program t6p4p2p2d1;
var
   a : integer;
   b : real;
   c : boolean;
   d : char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #060');
   a:=6*2+3;
   b:=3.14159*2;
   c:=(a=15);
   d:='Z';
   writeln(' PASS...6.4.2.2-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    