
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 311*)
(*TEST 6.10-1, CLASS=DEVIANCE*)
(* This test checks the effect of using a default file not declared in
  the program heading. The compiler deviates if the program
  prints DEVIATES. *)
program t6p10d1;
begin   reset(input); (*MDSI nonstandard file init*)
   writeln(' DEVIATES...6.10-1, FILE DECLARATION');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   