external procedure procedure_call (node: tuple);
external function scl_function_call (node: expr; targ_addr:addr_desc ): addr_desc;
external procedure pas_call (node: expr; rv_addr: expr);

(* The following type declaration is only temporary. When the compiler
   compiles itself this record can be replaced by ^anything. But until then..*)
type param_ary = record i:integer end;
     param_ptr = ^param_ary;

external procedure add_to_param_ary(desc: addr_desc;
			align:data_alignment;
			val:boolean;
			var params : param_ptr;
			var param_ary_idx : parm_index );
   