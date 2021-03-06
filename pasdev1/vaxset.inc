type set_desc = packed record
       nargs: 0..3;
       in_temp: boolean;
       lwb_exp,
       upb_exp: expr;
       arg: array [1..3] of addr_desc
     end;
  
external procedure genset_op_bounds (expr; var integer; var integer; var boolean);
external procedure shape_set (set_exp: expr; var shaped_lwb, shaped_len: set_range);
external procedure set_free (set_desc);
external function  setb_size (expr): set_range;
external function set_temporary (set_range; addr_desc): set_desc;
external procedure gen_c5 (opc_range; addr_desc; addr_desc; addr_desc; addr_desc);
external procedure set_move (set_desc; var set_desc);
external procedure prep_bitreference (addr_desc; var addr_desc; integer; integer; set_desc; var def);
external procedure emit_insv (integer; set_desc; set_desc);
external procedure gen_set (set_desc; var set_desc);
external function set_fetch (expr; set_range; set_range; addr_desc; boolean): set_desc;
external procedure set_assignment (tuple);
external function set_parameter (expr; sym_kind; set_range; set_range): addr_desc;
  