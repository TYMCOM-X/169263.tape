
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  77*)
(*TEST 6.4.3.2-3, CLASS=CONFORMANCE*)
(* An index type may be an ordinal type, This allows
  the use of BOOLEAN, INTEGER and some userdefined type
  names to be used as an index type.
  This program tests if the compiler will permit these
  except for INTEGER, which is included in a separate program. *)
program t6p4p3p2d3;
type
   digits   = '0'..'9';
   colour   = (red,pink,orange,yellow);
   intensity   = (bright,dull);
var
   alltoo   : array[boolean] of boolean;
   numeric  : array[digits] of integer;
   colours  : array[colour] of intensity;
   code     : array[char] of digits;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #077');
   numeric['0']:=0;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   colours[pink]:=bright;
   alltoo[true]:=false;
   code['A']:='0';
   writeln(' PASS...6.4.3.2-3')
end.
  