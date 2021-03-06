
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  98*)
(*TEST 6.4.3.5-1, CLASS=CONFORMANCE*)
(* A file-type is a structured type consisting of a sequence of
  components which are all one type. All cases in this program
  should pass.
  The compiler fails if one or more cases are rejected. *)
program t6p4p3p5d1;
type
   i      = integer;
   ptrtoi = ^i;
var
   file1 : file of char;
   file2 : file of real;
   file3 : file of
            record
               a : integer;
               b : boolean
            end;
   file4 : file of set of (red,blue,green,purple);
   file5 : file of ptrtoi;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #098');
   writeln(' PASS...6.4.3.5-1')
end.
    