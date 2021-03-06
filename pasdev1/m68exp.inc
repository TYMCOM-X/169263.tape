external function fetch ( e : expr;
                          allowed_modes : addr_mode_set;
                          preferred_mode : addr_mode;
                          allowed_sizes : op_size_set;
                          sign_required : boolean
                                                  ) : op_desc;
external function dfetch ( source : expr; dest : op_desc; expr_uses_ok : boolean ) : op_desc;
external function guard ( op : op_desc ) : guard_key;
external function unguard ( key : guard_key ) : op_desc;
external procedure grd_init;
external procedure grd_term;
external function loadr ( r : registers;
                          rval : reloc_value;
                          size : op_sizes;
                          sign_required : boolean
                                                  ) : op_desc;
external function loadi ( r : registers;
                          ival : integer;
                          size : op_sizes;
                          sign_required : boolean
                                                  ) : op_desc;
external function coerce ( desc : op_desc;
                           allowed_modes : addr_mode_set;
                           preferred_mode : addr_mode;
                           allowed_sizes : op_size_set;
                           sign_required : boolean
                                                   ) : op_desc;
external function make_postincr (desc: op_desc): op_desc;
external function pincr_free (op: op_desc): boolean;
external function ref_descriptor ( op : op_desc ) : op_desc;
external function deref_descriptor ( op : op_desc;
                                     size : op_sizes;
                                     signed : boolean
                                                      ) : op_desc;
external function increment_addr ( op : op_desc; delta : integer ) : op_desc;
external function offset_addr ( op : op_desc; delta : op_desc ) : op_desc;
external procedure pushi ( i : integer; size : op_sizes );
external procedure push ( op : op_desc; size : op_sizes );
external procedure pusha ( op : op_desc );
external procedure stk_bash;
external function copy_dreg ( source_desc : op_desc ) : op_desc;
external function mul_cst ( op : op_desc;
                            factor : integer;
                            op_size : op_sizes;
                            op_signed : boolean;
                            rslt_size : op_sizes
                                                 ) : op_desc;
external function load_frame_pointer ( access_block : blk ) : addr_regs;
external function upper_bound ( exp : expr; array_addr : op_desc ) : op_desc;
external function calc_width ( agg_type : typ; upb : op_desc ) : op_desc;
external function do_fetch ( e : expr;
                      allowed_modes : addr_mode_set;
                      preferred_mode : addr_mode;
                      allowed_sizes : op_size_set;
                      sign_required : boolean
                                              ) : op_desc;
external procedure dec_expr_usage (e: expr);
external procedure with_start ( with_rec : expr;
                                var num_withs_saved, max_withs_saved: unit_range;
                                savedwith_desc: op_desc );
external procedure with_end ( with_rec : expr;
                              var num_withs_saved: unit_range );
    