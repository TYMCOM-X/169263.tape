external procedure qjoinlines	(* turn two (or more) lines into one *)
(	var buffer: qbuffer;	(* working buffer *)
	first,
	last: qlineno;		(* range of lines to join *)
	contmark: qstring;	(* string to replace CRs with *)
	var err: qerrcode);	(* error report *)
   