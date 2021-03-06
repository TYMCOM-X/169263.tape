
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  47*)
(*TEST 6.2.2-7, CLASS=DEVIANCE*)
(* It is possible to redefine a function name within the scope
  of a function name. This test checks that the inner function
  redefines f, whether an erroneous assignment to f is detected or
  whether the erroneous outer f, with no function assignment is
  allowed to execute. *)
program t6p2p2d7;
var
   bool:boolean;
   j:integer;
function f(i:integer) : integer;
   function f(i:integer) : integer;
   begin
      f:=i
   end;
begin
   if bool then
      writeln(' FAIL...6.2.2-7, PROCEDURE SCOPE')
      (* FAILs if the call is recursive *)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else begin
      bool:=true;
      f:=f(i);
   end
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #047');
   bool:=false;
   j:=f(1);
   if (j=1) then
      writeln(' DEVIATES...6.2.2-7, PROCEDURE SCOPE');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 