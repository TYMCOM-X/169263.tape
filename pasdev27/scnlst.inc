
(*********   S C N L S T  - - SCANNR Listing Package   *********)

external var       title: str_ptr;
external var       listfb: file_block;
external procedure list_input;
external procedure echo_line;
external procedure prt_message ( parm_string );
external procedure list_recognizer ( number; transition_matrix; re_vector; acc_vector );
external procedure list_scanner ( transition_matrix; st_vector; acc_pat_vector );
   