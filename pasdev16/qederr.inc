(**********  QEDERR.inc  last modified 3/6/81  **********)

type qederrlevel = 1..10;

external procedure qederror
    (	var f: text;		(* file to write error message to *)
	code: qerrcode;		(* error to report *)
	level: qederrlevel  );	(* level of message desired *)

   