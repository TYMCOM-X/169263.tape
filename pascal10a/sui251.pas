
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 251*)
(*TEST 6.8.3.5-11, CLASS=DEVIANCE*)
(* This test checks that the compiler detects that a case index
  and the case constants are of different types.
  The compiler deviates if the program compiles and the program
  prints DEVIATES. *)
program t6p8p3p5d11;
var
   i,counter:integer;
   r:real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #251');
   counter:= 0;
   for i:= 1 to 4 do
   begin
      r:=i;
      case r of
      1: counter:=counter+1;
      2: counter:=counter+1;
      3: counter:=counter+1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      4: counter:=counter+1;
      end;
   end;
   if counter=4 then
      writeln(' DEVIATES...6.8.3.5-11, CASE CONSTANTS')
   else
      writeln(' PASS...6.8.3.5-11, CASE CONSTANTS');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    