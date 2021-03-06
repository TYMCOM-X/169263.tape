external procedure emt_init;
external procedure remove_instruction (var cdl: code_list;
                                       var c: code);
external procedure emit_code (var list : code_list;
                              input_section : rel_sections;
                              input_options : set_of_options;
                              var code_size : unit_range;
                              instructions: boolean);
external procedure emt_fixup (input_pc : code_address;
                              input_long_flag : boolean;
                              input_reloc : reloc_value;
                              input_patch : integer);
external procedure emt_term (input_start_addr : def;
                             input_code_size : unit_range;
                             input_const_size : unit_range;
                             input_static_size : unit_range);
external procedure emt_fatal;
    