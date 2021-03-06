
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  41*)
(*TEST 6.2.2-1, CLASS=CONFORMANCE*)
(* The Pascal Standard permits redefinition of a user name, by a
  further defining occurrence in a range (eg. procedure block)
  enclosed by the first defining occurence. This second range
  (and all ranges enclosed by it) are excluded from the scope of
  the defining occurence of the first range.
  This program tests the scope conformance of the compiler
  for user names. *)
program t6p2p2d1;
const
   range = 10;
var
   i : integer;
   pass : boolean;
procedure redefine;
const
   range = -10;
var
   i : integer;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

begin
   i:=range;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #041');
   i:=1;
   pass:=false;
   redefine;
   if range=-10 then
      writeln(' FAIL...6.2.2-1: SCOPE ERROR-RANGE')
   else
      pass:=true;
   if i=-10 then
      writeln(' FAIL...6.2.2-1: SCOPE ERROR-I')
   else
      if pass then
         writeln(' PASS...6.2.2-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    