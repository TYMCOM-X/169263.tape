(* DEBBOL.INC - external declarations for public routines of the Pascal
   Debugger module DEBBOL.  *)

external procedure eval$bool$expr (var lex_scan_rec_type;
                                       scope_type;
                                   var boolean;
                                   var status_code);
    