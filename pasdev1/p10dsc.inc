external procedure str_assignment ( node: tuple );
external procedure str_index_op ( treg: reg_selector; node: tuple );
external procedure str_search_op ( treg: reg_selector; node: tuple );
external procedure str_verify_op ( treg: reg_selector; node: tuple );
external procedure str_compare_op ( treg: reg_selector; node: tuple );
external procedure ls_move (lhs, rhs: expr);
external procedure ls_operator (op: rt_symbol; arg1, arg2: expr; treg: registers);
external procedure ls_in_operator (op: rt_symbol; arg1, arg2: expr; treg: registers);
external function ls_setcvt (treg: reg_selector; tdesc: expr_type_desc; arg1: expr): registers;
external procedure rt_open_call ( treg: reg_selector; node: tuple );
external procedure read_write_call ( op: tuple );
    