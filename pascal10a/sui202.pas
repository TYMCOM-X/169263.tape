
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 202*)
(*TEST 6.6.6.4-2, CLASS=CONFORMANCE*)
(* This program checks the implementation of chr.
  The compiler fails if the program does not compile and run. *)
program t6p6p6p4d2;
var
   letter : char;
   counter : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #202');
   counter:=0;
   for letter:='0' to '9' do
      if chr(ord(letter))=letter then
         counter:=counter+1;
   if counter=10 then
      writeln(' PASS...6.6.6.4-2')
   else
      writeln(' FAIL...6.6.6.4-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 