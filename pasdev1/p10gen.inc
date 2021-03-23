external var
  cur_bb: tuple;
  low_base: def;
  high_base: def;
external procedure emit_code ( var code_list; var unit_range; set_of_options );
external procedure gen_code ( var code_size, const_size, static_size: unit_range );
external procedure kill_temps;
external function cst_addr (mem:addr_desc): boolean;
external procedure do_blt (source, dest, units: expr; density: integer);
