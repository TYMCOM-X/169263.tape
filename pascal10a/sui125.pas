
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 125*)
(*TEST 6.4.6-8, CLASS=ERRORHANDLING*)
(* This test is similar to 6.4.6-7, except that assignment
  compatibility for sets passed as parameters is tested.
  The program causes an error which should be detected. *)
program t6p4p6d8;
type
   colour   = (red,pink,orange,yellow,green,blue);
   subone   = red..green;
   settwo   = set of yellow..blue;
var
   setone : set of subone;
procedure test(a : settwo);
begin
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #125');
   setone:=[red,pink,orange];
   test(setone);
   writeln(' ERROR NOT DETECTED...6.4.6-8')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
   