(* DEBLEX.INC - external declarations for public routines of the Pascal
   Debugger module DEBLEX.  *)

external procedure lex$scan    (var lex_scan_rec_type;
                                var status_code);
external procedure lex$keyword (var lex_scan_rec_type;
                                var id_string);
 