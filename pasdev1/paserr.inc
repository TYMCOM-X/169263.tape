$include paserr.typ

type
    user_range = 0..9999;
    level_range = 0..3;
    error_table= array[err_codes] of packed record
      user_number: user_range;
      level: level_range
    end;

external const
    err_table: error_table;

external procedure err_print (err_codes; source_id; string; line_index);
external procedure err_text (err_codes; string);
external procedure error (err_codes);
external procedure err_token (err_codes; token_type);
external procedure err_node (err_codes; parse_node);
external procedure err_op (err_codes; string; parse_node);
external procedure elf_open;
external procedure elf_close;
  