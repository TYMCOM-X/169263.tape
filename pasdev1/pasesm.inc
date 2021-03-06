external function  ck_arg_count (parse_node; parm_range; parm_range; parse_node): boolean;
external function  validate_call (expr; parse_node; parse_node): expr;
external function  val_semantics (parse_node): expr;
external function  base_semantics (parse_node; xrf_class): expr;
external function  trgt_semantics (parse_node): expr;
external function  subr_semantics (parse_node): expr;
external function  cond_semantics (parse_node; boolean; boolean): expr;
external procedure scl_limits (expr; var machine_word; var machine_word);
external procedure chk_scalar (typ; expr; parse_node);
external function cvt_semantics (expr; typ; parse_node; boolean; var boolean): expr;
external function  copy_semantics (parse_node; typ; boolean): expr;
external function  constant (parse_node; typ; var val; var typ): boolean;
  