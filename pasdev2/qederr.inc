(**********  QEDERR.inc  last modified 3/6/81  **********)

TYPE QEDERRLEVEL = 1..10;

EXTERNAL PROCEDURE QEDERROR
    (	VAR F: TEXT;				(* file to write error message to *)
	CODE: QERRCODE;				(* error to report *)
	LEVEL: QEDERRLEVEL  );			(* level of message desired *)

  