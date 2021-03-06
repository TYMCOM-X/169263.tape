

external var
  num_args: arg_index;

external procedure putinit;

external procedure new_arg;

external procedure arg_rescan;

external procedure new_call ( defn: definition );

external procedure exp_call;

external procedure end_call;

external procedure put_string ( s: string_parm );

external function  arg_length ( arg: arg_index ): string_index;

external function  arg_char ( arg: arg_index; i: string_index ): char;

external procedure arg_store ( arg: arg_index; i: string_index; c: char );

external procedure arg_copy ( arg: arg_index;
			      i, len: string_index;
			      dest: str_pointer;
			      d_i: string_index );

external procedure arg_back ( arg: arg_index; i, len: string_index );
   