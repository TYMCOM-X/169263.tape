external procedure initpatterns;
external procedure clearpatterns;
external procedure new_pattern ( reg_exp; str_list; str_list );
external procedure add_pattern ( reg_exp; str_list );
external procedure endpatterns;
external var       npatterns: integer;
external function  pat_re ( integer ): reg_exp;
external function  pat_action ( integer ): str_list;
external procedure pat_free ( str_list );
  