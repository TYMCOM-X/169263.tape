external function fetchboolean ( test_expr : expr; negate, expr_uses_ok : boolean ): op_desc;
external procedure compare_integer_ops ( var op1, op2 : op_desc;
                                             rel : relations; action : action_proc );
external procedure test_and_jump ( test_expr : expr;
                                   true_loc, false_loc, next_loc : tuple );
 