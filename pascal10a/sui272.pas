
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 272*)
(*TEST 6.8.3.9-13, CLASS=DEVIANCE*)
(* This program tests whether a formal parameter  can be used
  as a for statement control variable.
  The program deviates if the program compiles and prints
  DEVIATES. *)
program t6p8p3p9d13;
procedure p;
var
   i:integer;
   procedure loop_(var i:integer);
   var
      j:integer;
   begin
      j:=0;
      for i:=1 to 10 do
         j:=j+1;
   end;
begin
   i:=10;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   loop_(i)
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #272');
   p;
   writeln(' DEVIATES...6.8.3.9-13, FOR');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  