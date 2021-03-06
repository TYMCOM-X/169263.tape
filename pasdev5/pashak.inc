type
  intfile = file of integer;
  filblock= array[0..3] of integer;

external function fileb$ (var intfile): filblock;
external function fileb$text (var text): filblock;

external function left_shift (integer; integer): integer;

type
    proc_parm = procedure (ctp);

external procedure dump_tree (proc_parm; ctp; ctp; var boolean);

external function date_run: integer;
external function time_run: integer;
external function user_ppn: integer;

   