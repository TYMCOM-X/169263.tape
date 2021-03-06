external var
  low_base: def;
  high_base: def;
  reset_needed: boolean;
external procedure gen_code ( var code_size, const_size, static_size: unit_range );
external procedure store ( registers; addr_desc; bit_range; data_alignment );
external function get_temp (unit_range): addr_desc;
external function cst_addr (mem:addr_desc): boolean;
external procedure do_blt (src_addr, dest_addr, width_addr: addr_desc);
external procedure bb_end;
external procedure do_check (expr);
    