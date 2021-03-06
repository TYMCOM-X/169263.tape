
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 135*)
(*TEST 6.5.4-1, CLASS=ERRORHANDLING*)
(* The Pascal Standard states that an error occurs if a pointer
  variable has a value NIL at the time it is dereferenced.
  This program tests that the error is detected. The diagnostic
  should be checked for suitability. *)
program t6p5p4d1;
type
   rekord = record
               a : integer;
               b : boolean
            end;
var
   pointer : ^rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #135');
   pointer:=nil;
   pointer^.a:=1;
   pointer^.b:=true;
   writeln(' ERROR NOT DETECTED...6.5.4-1')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
  