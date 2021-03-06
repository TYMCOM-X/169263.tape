
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  73*)
(*TEST 6.4.3.1-2, CLASS=DEVIANCE*)
(* The Pascal Standard states that a structured type identifier
  may not be used in a PACKED type definition.
  The compiler passes if the program fails to compile. *)
program t6p4p3p1d2;
type
   complex = record
               realpart : real;
               imagpart : real;
             end;
   packcom = packed complex;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #073');
   writeln(' DEVIATES...6.4.3.1-2 : IMPROPER USE OF PACKED')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 