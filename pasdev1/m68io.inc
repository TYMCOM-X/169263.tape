external procedure io_begins (input_node : tuple);
external procedure io_ends (input_node : tuple);
external procedure read_write_call (input_node : tuple);
external procedure rt_io_call (input_file : expr;
                               input_rts : rt_symbol);
external procedure rt_seek_call (input_file : expr;
                                 input_index : expr);
external function rt_open_call (input_file : expr) : op_desc;
  