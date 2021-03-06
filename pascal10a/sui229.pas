
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 229*)
(*TEST 6.7.2.5-1, CLASS=CONFORMANCE*)
(* This program tests the use of relational operators on strings.
  The operators denote lexicographic ordering according to the
  ordering of the character set.
  The compiler fails if the program does not compile, or the
  program states that this is so. *)
program t6p7p2p5d1;
type
   string_=packed array[1..7] of char;
var
   string1,
   string2 : string_;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #229');
   string1:='STRING1';
   string2:='STRING2';
   if (string1<>string2) and (string1<string2) then
   begin
      string1:='STRINGS';

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      string2:='STRINGZ';
      if (string1<>string2) and (string1<string2) then
         writeln(' PASS...6.7.2.5-1')
      else
         writeln(' FAIL...6.7.2.5-1')
   end
   else
      writeln(' FAIL...6.7.2.5-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  