(* DEBPRT.INC - external declarations for public routines of Pascal
   Debugger module DEBPRT.  *)
  
external procedure print$ (descriptor; radix_type; var status_code );
external procedure a$$ign (var descriptor; var descriptor; var status_code);
 