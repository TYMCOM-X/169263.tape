$INCLUDE pascfm.typ

external var
    cmd_name_stack: cmd_stack;
    cmd_list: pending_command;
    last_cmd: pending_command;

external procedure rd_cmd_file ( var f: text );
external procedure rd_tmpcor ( name: parm_string; buf: tmp_buffer; len: integer );
external procedure cmd_save ( var f: file of * );
external procedure cmd_restore ( var f: file of * );
external procedure cmd_clear;
 