external function qsplitlines	(* split selected lines into smaller ones *)
(	var buffer: qbuffer;	(* working buffer *)
	lineno: qlineno;	(* addr of line to split *)
	pat: spattern;		(* where to split each line *)
	option_list: split_opt_set;	(* various action-controllers *)
	var cnt: qlineno;	(* number of splits - new lines created *)
	var err: qerrcode	(* error report *)
		): boolean;	(* true if a match for pat found *)
  