EXTERNAL PROCEDURE QJOINLINES			(* turn two (or more) lines into one *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST,
	LAST: QLINENO;				(* range of lines to join *)
	CONTMARK: QSTRING;			(* string to replace CRs with *)
	VAR ERR: QERRCODE);			(* error report *)
   