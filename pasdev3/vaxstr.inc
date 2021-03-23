(* VAXSTR.INC - include file containing external declarations for
   the public routines of module VAXSTR.PAS.  *)

external function str_temp ( addr_desc; char_range ): str_desc;
external procedure move_string ( str_desc; str_desc; boolean;
  free_procedure; boolean; var addr_desc );
external function fetchstring ( expr; str_len_context ): str_desc;
external function copy_string ( str_desc ): str_desc;
external procedure str_assignment (tuple);
external procedure free_string ( str_desc );
external function skip_desc_word ( str_desc ): addr_desc;
external function skip_len_words ( str_desc ): addr_desc;
external function save_regs ( set_of_registers ): set_of_registers;
external procedure restore_regs ( set_of_registers );
external function fetchtranslated ( expr; str_len_context ): str_desc;
external function do_index_op ( expr ): addr_desc;
external function str_search_verify ( expr ): addr_desc;
external procedure str_compare ( expr );
external procedure str_parameter ( expr; sym_kind; typ; var ptr; var parm_index );
