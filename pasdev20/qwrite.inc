external procedure qfilewrite	(* write text to file *)
(	var buffer: qbuffer;	(* buffer to write from *)
	filename: file_id;	(* file to write to *)
	fno, lno: qlineno;	(* range to write *)
        confirm: boolean;	(* new/old file prompting desired? *)
	var err: qerrcode);	(* error report *)
 