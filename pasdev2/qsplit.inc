EXTERNAL FUNCTION QSPLITLINES			(* split selected lines into smaller ones *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	LINENO: QLINENO;			(* addr of line to split *)
	PAT: SPATTERN;				(* where to split each line *)
	OPTION_LIST: SPLIT_OPT_SET;		(* various action-controllers *)
	VAR CNT: QLINENO;			(* number of splits - new lines created *)
	VAR ERR: QERRCODE			(* error report *)
		): BOOLEAN;			(* true if a match for pat found *)
  