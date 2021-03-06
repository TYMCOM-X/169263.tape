
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 230*)
(*TEST 6.7.2.5-2, CLASS=CONFORMANCE*)
(* This test checks the use of relational operators on sets.
  Thc compiler fails if the program does not compile, or the
  program states that this is so. *)
program t6p7p2p5d2;
var
   a,b:set of 0..10;
  c,counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #230');
   counter:=0;
   a:=[0,1,2,3,4,5];
   b:=[2,3,4];
   c:=3;
   if(a=[0,1,2,3,4,5]) then
     counter:=counter+1;
   if(a<>b) then
      counter:=counter+1;
   if(b<>[1,2,3,4,5]) then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      counter:=counter+1;
   if(b<=a) then
      counter:=counter+1;
   if(a>=b) then
      counter:=counter+1;
   if([0,1]<=a) then
      counter:=counter+1;
   if([1,2,3,4,5,6,10]>=b) then
      counter:=counter+1;
   if (1 in a) then
      counter:=counter+1;
   if(c in b) then
      counter:=counter+1;
   if(counter=9) then
      writeln(' PASS...6.7.2.5-2 SET RELATIONAL OPERATORS')
   else
      writeln(' FAIL...6.7.2.5-2 SET RELATIONAL OPERATORS');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    