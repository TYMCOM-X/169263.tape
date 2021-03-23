external procedure termline (line: qstring;
	tab_print: boolean; var err: qerrcode);

(* outputs the line to the specified file, converting
   all control characters except &G through &M to &x form. *)

external procedure termlstlns (var buffer: qbuffer; low, high: qlineno;
	ctl_char, number, tab_print: boolean; var err: qerrcode);

(* termlstlns outputs the specified range of lines to
   the tty. uses termline. *)
