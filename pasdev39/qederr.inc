(**********  QEDERR.inc  last modified 3/6/81  **********)

(* Modified 7/28/82 by WNH to eliminate text file parameter *)

type qederrlevel = 1..10;

external procedure qederror (
	code: qerrcode;		(* error to report *)
	level: qederrlevel  );	(* level of message desired *)

   