external const
    rts_name : array [rt_symbol] of string[8];
external procedure cvt_sreal ( x : real_type; var i : integer; prt_errors : boolean );
external procedure cvt_dreal ( x : real_type; var i1, i2 : integer );
external procedure get_code (input_code_ptr : code;
                             var var_array : array [1..5] of uns_word;
                             var var_print_len : 1..5;
                             var var_absolute_len : 1..5;
                             var var_op_1_flag : boolean;
                             var var_op_2_flag : boolean);
external function stc_offset ( s : sym ) : unit_range;
  