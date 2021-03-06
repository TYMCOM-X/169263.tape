
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  65*)
(*TEST 6.4.2.2-6, CLASS=CONFORMANCE*)
(* The Pascal Standard states that the lower-case letters a-z are
  ordered, but not necessarily contiguous.
  This program determines if this is so, and prints
  a message as to whether the compiler passes or not .
   NOTE: this program uses lower-case char constants and may
         fail for this reason. The test is also irrelevant for
         one-case compilers. *)
program t6p4p2p2d6;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #065');
   if ('a' < 'b') and ('b' < 'c') and ('c' < 'd') and
      ('d' < 'e') and ('e' < 'f') and ('f' < 'g') and
      ('g' < 'h') and ('h' < 'i') and ('i' < 'j') and
      ('j' < 'k') and ('k' < 'l') and ('l' < 'm') and
      ('m' < 'n') and ('n' < 'o') and ('o' < 'p') and
      ('p' < 'q') and ('q' < 'r') and ('r' < 's') and
      ('s' < 't') and ('t' < 'u') and ('u' < 'v') and
      ('v' < 'w') and ('w' < 'x') and ('x' < 'y') and

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      ('y' < 'z') then
      writeln(' PASS...6.4.2.2-6')
   else
      writeln(' FAIL...6.4.2.2-6: NO ORDERING')
end.
  