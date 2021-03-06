$INCLUDE pasemu.typ

external var with_list: wl_type; (* Initialized to nil in Body. *)
external procedure push_with (expr);
external procedure pop_with (var expr);
external function bad_expr: expr;
external function fbuffer (expr): expr;
external function new_ident (sym): expr;
external function op1 (tuple_opcodes; typ; expr): expr;
external function op2 (tuple_opcodes; typ; expr; expr): expr;
external function op3 (tuple_opcodes; typ; expr; expr; expr): expr;
external function arr_bound (tuple_opcodes; expr): expr;
external function new_substr (expr; expr; expr; boolean; parse_node): expr;
external function typename (typ): nam;
external function op_real (expr; prec_type): expr;
external function pcomponent (expr): boolean;
external procedure tag_scan (typ; parse_node; boolean; var typ; var tag_value_list; var expr);
external function rtsize (typ; expr; boolean): expr;
    