external var
  regdesc: reg_descriptor;
  with_base: registers;
  maxwith_regs: registers;
  savewithaddr: addr_desc;
external function load (exp: expr; size: bit_range): registers;
external function nond_load (exp: expr; size: bit_range): registers;
external function load_addr (addr: addr_desc; align: data_alignment; size: bit_range): registers;
external function fetch (exp: expr): addr_desc;
external function fetch_fullword (exp: expr): addr_desc;
external function argument (exp: expr): addr_desc;
external function is_register (maddr: addr_desc): boolean;
type
    op_attrs =
	(  commutative,		(* operands may be exchanged *)
	   mem_form,		(* opcM may be generated *)
	   double	);	(* must apply inst twice to process 2 word operands *)
    op_attr_set = set of op_attrs;
  
external function do_binary_op (opcode: opc_range;
	  op1, op2: expr;		(* left and right operands *)
	  inst_options: op_attr_set	(* properties of instruction to generate *)
			): registers;	(* register containing result returned *)
external procedure clr_rv;
external function precision (exp: expr): bit_range;
external function alignment (exp: expr): data_alignment;
external procedure reg_init;
external procedure decr_reg_usages (reg: registers);
external procedure free_and_disassociate (reg: registers);
external procedure incr_reg_usages (reg: registers; count: usage_range);
external procedure tag_reg (reg1, reg2: registers);
external function regs_used (maddr: addr_desc): registers;
external procedure free (maddr: addr_desc);
external procedure clear (reg: registers);
external function get_reg (p: bit_range): registers;
external function reg_addr ( reg: registers ): addr_desc;
external procedure with_start (withtpl: tuple);
external procedure with_end (withtpl: tuple);
external procedure with_restore;
external procedure left_shift (reg: registers; p: bit_range; n: integer);
external procedure right_shift (reg: registers; p: bit_range; n: integer);
external procedure do_move (reg: registers; mem: addr_desc; alignment: data_alignment; p: bit_range);
external function coerce_to_double ( addr: addr_desc; align: data_alignment ): registers;
external function iconstp (exp: expr; var val: integer): boolean;
external function aconstp (maddr: addr_desc; var val: integer): boolean;
external function duplicate_addr ( addr: addr_desc ): addr_desc;
external procedure get_bounds (exp: expr; var minval, maxval: integer);
external function int_value ( val: integer ): addr_desc;
external function expr_size ( expr_desc: expr_type_desc ): bit_range;
external procedure load_reg (reg: registers; exp: expr);
external function dynamic_flex ( array_expr: expr ): boolean;
external function lower_bound ( array_expr: expr; array_addr: addr_desc ): addr_desc;
external function upper_bound ( array_expr: expr; array_addr: addr_desc ): addr_desc;
external function increment_addr (addr_desc; unit_range; bit_offset; bit_range): addr_desc;
external function do_fetch (expr): addr_desc;
external function array_addr (expr): addr_desc;
external function copy_load (expr): registers;
  