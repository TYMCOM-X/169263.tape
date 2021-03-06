external procedure prline (var outfile: text; line: qstring;
	tab_print: boolean; var err: qerrcode);

(* outputs the line to the specified file, converting
   all control characters except &G through &M to &x form. *)

external procedure qlistlines (var buffer: qbuffer; low, high: qlineno;
	var f: text; ctl_char, number, tab_print: boolean; var err: qerrcode);

(* qlistlines outputs the specified range of lines to
   the tty. uses prline. *)
  