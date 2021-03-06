external procedure init_output;
external procedure mac_init (input_pc : array [rel_sections] of code_address);
external procedure mac_list (input_list : code_list;
                             input_section : rel_sections);
external procedure mac_term (input_code_size : unit_range;
                             input_const_size : unit_range;
                             input_static_size : unit_range);
   