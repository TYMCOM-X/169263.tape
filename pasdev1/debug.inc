(* DEBUG.INC - external declarations for Pascal Debugger
   module DEBUG$.  *)

external procedure a$$ert (bool_expr: boolean);
external function  query$ (packed array [1..*] of char): boolean;
 