external procedure rel_patch (input_patch_pc : code_address;
                              input_curr_pc : code_address;
                              input_long_flag : boolean;
                              input_reloc : reloc_value;
                              input_patch : integer);
external procedure rel_init;
external procedure rel_emit (input_list : code_list;
                             input_section : rel_sections);
external procedure rel_fatal;
external procedure rel_term (input_start_addr : def;
                             input_size : array [rel_sections] of code_address);
    