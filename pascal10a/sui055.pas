
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  55*)
(*TEST 6.3-5, CLASS=DEVIANCE*)
(* This program tests that signed constants are not permitted
  in other contexts than const declarations. *)
program t6p3d5;
const
   dot = '.';
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #055');
   writeln(' DEVIATES', +dot, '..6.3-5')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    