external var
  code_area: code_list;			(* chain of code records for code area *)
  static_area: code_list;		(* chain for static area image *)
  cst_area: code_list;			(* chain for constant pool area *)
  fp_area: code_list;			(* chain of page and file blocks for debugger *)
  loc_code: code_address;			(* location counter for code area *)
  loc_static: code_address;		(* location counter for static area *)
  loc_cst: code_address;			(* location counter for constant area *)
  def_lists: array [def_types] of def;		(* start of chains of internal definitions *)

type rts_name_array = array[rt_first..rt_last_used] of string [15];
external var
  rts_name: rts_name_array;

external procedure gen_emit (var code_list; code);
external procedure gen_branch (opc_range; def);
external procedure gen_bb ( opc_range; addr_desc; addr_desc; def );
external procedure gen_byte (var code_list; byte);
external procedure gen_word (var code_list; word);
external procedure gen_longword (var code_list; longword);
external procedure gen_real ( var code_list; prec_type; real_type );
external procedure gen_opcode (opc_range);
external procedure gen1 (opc_range; addr_desc);
external procedure gen2 (opc_range; addr_desc; addr_desc);
external procedure gen3 (opc_range; addr_desc; addr_desc; addr_desc);
external procedure gen4 (opc_range; addr_desc; addr_desc; addr_desc;
                                    addr_desc);
external procedure gen5 (opc_range; addr_desc; addr_desc; addr_desc;
                                    addr_desc; addr_desc);
external procedure gen6 (opc_range; addr_desc; addr_desc; addr_desc;
                                    addr_desc; addr_desc; addr_desc);
external procedure move_immediate (int_type; addr_desc);
external procedure gen_string (var code_list; string);
external procedure gen_pstring (var code_list; string);
external procedure gen_rm (opc_range; registers; addr_desc);
external function make_rt_addr ( rt_symbol ): addr_desc;
external procedure gen_rt (parm_range; rt_symbol);
external procedure gen_mr (opc_range; addr_desc; registers);
external procedure gen_rr (opc_range; registers; registers);
external procedure gen_displacement (def; def; elem_sizes);
external function get_def ( def_types; id_range): def;
external function gen_cnode (val_ptr; typ): rel_syllable;
external procedure gen_init;
external procedure gen_term;
external function make_def (dtype: def_types): def;
external procedure def_value (d: def; value: code_address; relocatable: boolean);
external procedure del_def_list (dtype: def_types);
external function get_offset (intdef: def; off: int_type): def;
external function relrt (rtsym: rt_symbol): rel_syllable;
external function relsym (s: sym): rel_syllable;
external function reldef (intdef: def): rel_syllable;
external procedure gen_origin (var arealist: code_list; psectid: psect_type);
external procedure gen_mask (blk);
external procedure mark_def (var area_list: code_list; intdef: def);
external procedure gen_source (srcid: source_id);
external procedure gen_val (var area_list: code_list; value: val; vals_type: typ);
external procedure gen_vnode (var area_list: code_list; value: val_ptr; vals_type: typ);
external procedure skip_bytes (var code_list; var bit_range; bit_range);
external function gen_cval (value: val; vals_type: typ): rel_syllable;
external function gen_cint (int_cons: int_type): rel_syllable;
external procedure gen_cmt (var area_list: code_list; text: string);

type
    block_vector = array [index_range] of blk;

external var
    blk_list: ^ block_vector;
external function gen_cst ( int_type; vax_type ): addr_desc;
    