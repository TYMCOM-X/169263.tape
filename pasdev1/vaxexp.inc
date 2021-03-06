external var
    regdesc: reg_descriptor;
    max_reg: registers;
    reg_use_by_block: ^reg_set_vector;
external procedure exp_init;
external function precision ( exp: expr ): bit_range;
external function expr_size ( expr ): bit_range;
external function load (exp: expr): registers;
external function load_addr (maddr: addr_desc; align: data_alignment ): registers;
external function copy_load ( expr ): registers;
external function move_address ( addr_desc ): registers;
external function array_addr ( expr ): addr_desc;
external function alignment (exp: expr): data_alignment;
external function do_fetch ( exp: expr; targ_addr: addr_desc ): addr_desc;
external function fetch (exp: expr; targ_addr: addr_desc ): addr_desc;
external function fetch_fullword (exp: expr): addr_desc;
external function argument ( arg: expr ): addr_desc;
external procedure reg_init;
external function get_reg (p: bit_range): registers;
external function get_vax_reg ( vax_type ): registers;
external procedure tag_reg (reg1, reg2: registers);
external function is_register (maddr: addr_desc): boolean;
external function is_immediate (addr_desc): boolean;
external function is_disp_mode (addr_desc): boolean;
external function is_symbol_addr (addr_desc): boolean;
external function indexable (addr_desc): boolean;
external procedure free (maddr: addr_desc);
external function iconstp (exp: expr; var val: integer): boolean;
external function aconstp (maddr: addr_desc; var val: integer): boolean;
type
  inst_opt_list =
      (  commutative,			(* operands may be exchanged *)
	 mem_form,			(* opcM may be generated *)
	 double	     );			(* must apply inst twice to process 2 word operands *)
  inst_opt_set = set of inst_opt_list;

external function do_binary_op
      (	opc_group: opc_range;			(* instruction to emit *)
	op1, op2: expr;			(* left and right operands *)
	inst_options: inst_opt_set;	(* properties of instruction to generate *)
	targ_addr : addr_desc		(* target address, if one prefered *)
		): addr_desc;		(* TARG_ADDR or register with result *)
external procedure with_start (tuple);
external function upper_bound (expr; addr_desc): addr_desc;
external function dynamic_flex (expr): boolean;
external function unpacked_vax_type ( typ ): vax_type;
external function expr_vax_type (expr): vax_type;
external function vax_type_size ( vax_type ): align_range;
external function duplicate_addr ( addr_desc ): addr_desc;
external function increment_addr ( addr_desc ; unit_range ): addr_desc;
external procedure regs_used (addr_desc; var registers; var registers);
external function offset_addr ( var addr_desc; addr_desc ): addr_desc;
external procedure mark_regs_used ( set_of_registers );
external procedure update_usages ( addr_desc; usage_range );
external function cvt_word ( addr_desc; data_alignment ): addr_desc;
external function cvt_long ( addr_desc; data_alignment ): addr_desc;
external procedure decr_reg_usages ( registers );
external function nonindexed_fetch ( expr ): addr_desc;
external function mem_fetch ( expr ): addr_desc;
   