external procedure procedure_call (node: tuple);
external function scl_function_call (node: expr): addr_desc;
type
  return_value = (no_return_value, expr_return_value, addr_return_value);
external procedure pas_call (node: expr; ret_type: return_value; rv_expr: expr; rv_addr: addr_desc);
external procedure pf_access (sym_kind; expr; var registers; boolean);
   