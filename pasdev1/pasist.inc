$INCLUDE dtime.typ[31024,320156]
$INCLUDE passpf.typ
$INCLUDE pasist.typ

external var
  root_name: nam;                               (* base of unbalanced binary name tree *)
  env_name: nam;                                (* set in envmodule compilation *)
  env_dtime: dtime_int;                         (* date/time of envmodule compilation *)

  root_block: blk;                              (* imaginary outermost block containing standard names *)
  cur_block: blk;                               (* innermost block in scope *)
  ext_block: blk; (* block to represent all external routines *)
  lex_block: blk; (* start of the lex_thread chain, ordered by level *)
  blk_number: 0..2047;                  (* value given to cur_block^ *)
  max_level: level_index; (* highest block level in the program *)

  heap_chain: sym; (* chain of heap class var symbols *)
  file_chain: sym; (* chain of file class var symbols *)
  io_opsym: sym; (* input/output optimizer symbol *)

  cdatesym: sym; (* COMPDATE predefined symbol *)
  ctimesym: sym; (* COMPTIME predifined symbol *)

  sym_vl_number: id_range; (* numbering for var and label symbols *)
  vl_base: id_range; (* first free after symbol table initialization *)
  sym_nvl_number: id_range; (* numbering for other kinds of symbols *)
  nvl_base: id_range; (* first free after symbol table initialization *)

  vl_list, (* chain of var and label symbols *)
  vll_base: vl_link; (* chain after symbol table initialization *)

(*  Pointers to Standard Types  *)

external var
  type_int: typ; (* INTEGER *)
  type_fullword: typ; (* MACHINE_WORD *)
  type_non_neg: typ; (* non-negative integers *)
  type_bool: typ; (* BOOLEAN *)
  type_char: typ; (* CHAR *)
  type_real: typ; (* REAL *)
  type_ptr: typ; (* PTR *)
  type_text: typ; (* TEXT *)
  type_options: typ; (* IO_OPTIONS *)
  stat_io: typ; (* IO_STATUS *)
  stat_program: typ; (* PROGRAM_STATUS *)
  stat_math: typ; (* MATH_STATUS *)
  stat_special: typ; (* SPECIAL_STATUS *)

(*  Pointers to Standard File Symbols  *)

external var
  file_input: sym; (* INPUT *)
  file_output: sym; (* OUTPUT *)
  file_tty: sym; (* TTY *)
  filettyoutput: sym; (* TTYOUTPUT *)

(*  Address Information for Code Generation  *)

external var
  size_init: unit_range; (* initialized static variable area *)
  size_uninit: unit_range; (* uninitialized static variable area *)
  size_cond: unit_range; (* condition cell area *)
  