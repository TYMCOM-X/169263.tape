external procedure dmp_open (boolean);
external function cv_ptr (integer): packed array [1..6] of char;
external function sym_text (sym): line_string;
external function typ_text (typ): line_string;
external function block_id (blk): line_string;
external procedure prt_title (line_string);
external procedure dmp_close;
    