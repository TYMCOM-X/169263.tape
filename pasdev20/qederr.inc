(**********  QEDERR.inc  last modified 9/26/77  **********)

type qederrlevel = 1..10;

external procedure qederr
    (	var f: text;		(* file to write error message to *)
	code: qerrcode;		(* error to report *)
	level: qederrlevel  );	(* level of message desired *)

    