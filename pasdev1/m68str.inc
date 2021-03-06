external function fetchstring (
                       exp: expr;
                       length_context: str_len_context;
                       temp_limit: char_range )
                            : string_descriptor;
external const
    no_limit: char_range;
external procedure free_string ( desc: string_descriptor );
external procedure str_desc_size ( var op: op_desc; size: op_sizes );
external function str_varying_dynamic_temp ( len: op_desc ): op_desc;
external procedure init_string_descriptor ( var desc: string_descriptor );
external function skip_desc_word ( var desc: string_descriptor ): op_desc;
external function skip_len_words ( var desc: string_descriptor ): op_desc;
external procedure pushstring ( var desc: string_descriptor; kind: string_kind );
external procedure pushlength ( desc: string_descriptor );
external procedure move_string (
          var lhs_desc: string_descriptor;
          var rhs_desc: string_descriptor;
          padded: boolean;
          free_proc: free_procedure;
          remainder: boolean;
          suppress_null_move: boolean);
external procedure str_case_ops ( exp: expr;  var base_expr: expr;
                                  var case_code: str_translation;
                                  use_exprs: boolean );
external function fetchtranslated ( exp: expr;  context: str_len_context ): string_descriptor;
external function str_index_op ( index_op_expr: expr ): op_desc;
external function str_search_verify ( exp: expr ): op_desc;
external procedure str_compare ( lhs_expr, rhs_expr: expr );
external procedure str_assignment ( node: tuple );
external procedure str_parameter (
                     actual_expr: expr;
                     parm_kind: sym_kind;
                     formal_type: typ;
                     add_parameter: procedure (op_desc; boolean) );
 