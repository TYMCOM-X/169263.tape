external function subr_formal_type 
		    (  subr_sym: parse_node;
		       subr_parm_list: parse_node;
		       func_type: parse_node;
		       parms_required: boolean;
		       var params: sym_list  ): typ;

type tsm_flags = set of ( forward_ok, flexible_ok, flex_rec_ok );

external function type_semantics  ( typetree: parse_node; flags: tsm_flags ): typ;
