
(*********   S C N E R R  - -  SCANNR Error Handler   *********)

external procedure err_init;
external procedure err_loc ( number; line_index; parm_string );
external procedure err_token ( sym_value; parm_string );
external procedure err_print ( number );
external var       err_count: number;
 