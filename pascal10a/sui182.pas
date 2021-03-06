
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 182*)
(*TEST 6.6.5.3-8, CLASS=ERRORHANDLING*)
(* This test is similar to 6.6.5.3-7, except that the
  variable created is used as the variable in an assignment
  statement.
  The error should be detected by the compiler or at
  run-time. *)
program t6p6p5p3d8;
type
   two      = (a,b);
   rekord   = record
               case tagfield:two of
                  a : (m : boolean);
                  b : (n : char)
              end;
var
   ptr : ^rekord;
   r   : rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #182');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   new(ptr,b);
   r.tagfield:=b;
   r.n:='A';
   ptr^:=r;
   writeln(' ERROR NOT DETECTED...6.6.5.3-8')
end.
  