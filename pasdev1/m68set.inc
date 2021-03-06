external const
    no_preference: op_desc;
external function ops_equal (op1, op2: op_desc): boolean;
external function dupl_set (sdesc: set_desc): set_desc;
external procedure gset_op_bounds (    int_valued_exp: expr;
                                     var min_val, max_val: integer;
                                     var fixed: boolean);
external procedure shape_set (    exp: expr;
                              var shaped_lwb, shaped_len: set_range);
external procedure set_free (sdesc: set_desc);
external function set_temporary (ctxt_lwbbit: set_range; len_addr: op_desc): set_desc;
external procedure set_move (from_desc, to_desc: set_desc);
external procedure prep_bitreference (    element: op_desc; element_exp: expr;
                                          target_set: set_desc;
                                      var bit_pos, byte_loc: op_desc;
                                      var defn: def);
external procedure emit_bit_inst (bit_value: 0..1;
                                  source_set, target_set: set_desc);
external procedure genset (source_desc, target_desc: set_desc);
external function set_fetch (set_exp: expr;
                             ctxt_lwbbit, ctxt_lenbits: set_range;
                             desired_loc: op_desc; force_fixed: boolean): set_desc;
external procedure set_assignment (assign_tpl: tuple);
external procedure set_parameter (actual_expr: expr; parm_kind: sym_kind;
                                  parm_type: typ; p: procedure (op_desc; boolean));
   