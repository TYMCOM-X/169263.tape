external function width (val: integer): integer;
external function cv_int (val: integer): parm_string;
external function cvf_int (val: integer; columns: line_index): parm_string;
external function cv_radix (val: integer; columns: line_index): parm_string;
external function cv_real (val: real_type): parm_string;
external function cv_fp (src: source_id): parm_string;
external function cv_source_id (src: source_id): parm_string;
   