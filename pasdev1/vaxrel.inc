external procedure rel_init;
external procedure rel_end (def; unit_range; unit_range);
external procedure fix_branches (code_list; unit_range);
external var fixing_branches: boolean;
external function op_length (code): unit_range;
external procedure wr_code ( var code_list; var unit_range; byte);
 