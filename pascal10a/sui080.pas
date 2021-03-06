
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  80*)
(*TEST 6.4.3.3-1, CLASS=CONFORMANCE*)
(* This program simply tests that all valid productions from
  the syntax for record types (as specified by the Pascal Standard)
  are accepted by this compiler.
  The compiler fails if one or more cases are rejected. *)
program t6p4p3p3d1;
type
   string_   = packed array[1..25] of char;
   married  = (false,true);
   shape    = (triangle,rectangle,square,circle);
   angle    = 0..90;
   a        = record
               year : integer;
               month : 1..12;
               day : 1..31
              end;
   b        = record
               name,firstname : string_;
               age : 0..99;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

               case  married of
                  true: (spousename : string_);
                  false : ()
              end;
   c        = record
               case s : shape of
                  triangle : (side : real;
                              inclination,angle1,angle2 : angle);
                  square,rectangle : (side1,side2 : real;
                                      skew,angle3 : angle);
                  circle : (diameter : real)
              end;
   d        = record ; end;
   e        = record
                case married of
                  true : (spousename : string_);
                  false : ();
                end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #080');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   writeln(' PASS...6.4.3.3-1')
end.
    