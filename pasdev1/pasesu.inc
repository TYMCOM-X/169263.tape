external procedure ck_unknown_type (typ);
external procedure initexpr (expr; typ);
external procedure initstr (expr; boolean; char_range);
external function cst_expr (val; typ): expr;
external function constp (expr): boolean;
external function assignable (expr): boolean;
  