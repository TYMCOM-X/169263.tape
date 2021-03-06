
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 269*)
(*TEST 6.8.3.9-10, CLASS=DEVIANCE*)
(* This program tests whether a real number can be assigned to
  a for statement control variable. The compiler deviates
  if the program compiles and prints DEVIATES. *)
program t6p8p3p9d10;
var
   i:integer;
   counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #269');
   counter:=0;
   for i:=0.0 to 3.5 do
      counter:=counter+1;
   if(counter=4) then
      writeln(' DEVIATES...6.8.3.9-10, FOR EXPRESSION ROUNDED')
   else
      writeln(' DEVIATES...6.8.3.9-10, FOR EXPRESSION TRUNCATED');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  