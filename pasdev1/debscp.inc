 (* DEBSCP.INC - external declarations for public routines of Pascal
    Debugger module DEBSCP.PAS.  *)

external function  r50$asc (r50word): id_string;
external function  first$module: ^linkentry;
external function  next$module (^linkentry): ^linkentry;
external function  mod$lookup (id_string): ^linkentry;
external function  sym$lookup (id_string; var id_string): ^linkentry;
external function  over$laid: boolean;
external procedure open$deb$file (^prog_block; var status_code);
external function  main$frame (^stack_frame): boolean;
external function  proc$blk (^stack_frame): ^procedure_block;
external function  proc$name (^stack_frame): id_string;
external procedure open$frame (var stack_level; var scope_type; var status_code);
external procedure open$stack (^stack_frame; var scope_type; var status_code);
external procedure open$routine (var scope_id_record; var scope_type; var status_code);
external procedure load$overlay (^stack_frame);
external procedure ret$addr (^stack_frame; var unit_range; var ovl_mod_no);
external function  call$basis (^stack_frame): ^stack_frame;
    