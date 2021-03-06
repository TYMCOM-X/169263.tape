
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  87*)
(*TEST 6.4.3.3-8, CLASS=ERRORHANDLING*)
(* Similar to 6.4.3.3-5, except that no tag-field is used.
  A change of variant occurs by reference to a field associated
  with a new variant. Again, these fields come into existance
  undefined.
  The compiler conforms if the program does not compile. *)
program t6p4p3p3d8;
type
   two = (a,b);
var
   variant : record
               case two of
                  a:(m:integer;
                     l:integer);
                  b:(n:integer;
                     o:integer)
             end;
   i : integer;
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #087');
   variant.n:=1;
   variant.o:=1;
   variant.m:=1;
   i:=variant.l;     (*illegal*)
   writeln(' ERROR NOT DETECTED...6.4.3.3-8')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  