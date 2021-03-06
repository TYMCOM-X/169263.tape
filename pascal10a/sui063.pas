
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  63*)
(*TEST 6.4.2.2-4, CLASS=CONFORMANCE*)
(* The Pascal Standard states that the character values representing
  the digits 0..9 are ordered and contiguous.
  The program tests these two criteria for these characters. *)
program t6p4p2p2d4;
var
   a,b : boolean;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #063');
   a:=(succ('0') = '1') and
      (succ('1') = '2') and
      (succ('2') = '3') and
      (succ('3') = '4') and
      (succ('4') = '5') and
      (succ('5') = '6') and
      (succ('6') = '7') and
      (succ('7') = '8') and
      (succ('8') = '9') ;
   b:=('0' < '1') and

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      ('1' < '2') and
      ('2' < '3') and
      ('3' < '4') and
      ('4' < '5') and
      ('5' < '6') and
      ('6' < '7') and
      ('7' < '8') and
      ('8' < '9') ;
   if a and b then
      writeln(' PASS...6.4.2.2-4')
   else
      writeln(' FAIL...6.4.2.2-4')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  