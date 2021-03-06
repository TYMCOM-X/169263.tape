EXTERNAL PROCEDURE PRLINE (VAR OUTFILE: TEXT; LINE: QSTRING;
	TAB_PRINT: BOOLEAN; VAR ERR: QERRCODE);

(* outputs the line to the specified file, converting
   all control characters except &G through &M to &x form. *)

EXTERNAL PROCEDURE QLISTLINES (VAR BUFFER: QBUFFER; LOW, HIGH: QLINENO;
	VAR F: TEXT; CTL_CHAR, NUMBER, TAB_PRINT: BOOLEAN; VAR ERR: QERRCODE);

(* qlistlines outputs the specified range of lines to
   the tty. uses prline. *)
  