
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 140*)
(*TEST 6.6.1-5, CLASS=DEVIANCE*)
(* If the compiler permits the formal parameter list to be included
  in the subsequent procedure declaration of a forward procedure
  (6.6.1-4), does it check the parameter list is the same ?
  The compiler deviates if the program compiles, and only conforms
  if the second formal parameter list is flagged as an error. *)
program t6p6p1d5;
var
   c : integer;
procedure one(var a : integer);
   forward;
procedure two(var b : integer);
begin
   b:=b+1;
   one(b)
end;
procedure one(a : integer);
begin
   a:=a+1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if a = 1 then two(a)
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #140');
   c:=0;
   one(c);
   writeln(' DEVIATES...6.6.1-5')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  