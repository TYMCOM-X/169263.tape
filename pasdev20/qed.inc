external procedure qedcl		(* main command loop for QED *)
(	var buffer: qbuffer;		(* pre-setup buffer *)
	allowed_cmds: qed_cmd_set);	(* command set to work with *)

external procedure qinitexec            (* equiv to qinit and qinitbuf *)
(	var buffer: qbuffer);		(* buffer to be used *)

external procedure qinit		(* setup QED to run *)
(	var buffer: qbuffer);		(* buffer to initialize *)

external procedure qsettabdefault	(* SET TABS switch default set *)
(	default: boolean);		(* true for SET TABS ON default *)

external procedure qsetmarkdefault	(* mark string default setter *)
(	line: cmdline);			(* the default mark string *)

external procedure qexecute		(* parse and execute one command line *)
(	var buffer: qbuffer;		(* working buffer *)
	var line: cmdline;		(* command line to parse *)
	var lindex: cmdlineidx;		(* place marker *)
	var execrange: ldrange;		(* limits of execution *)
	var ble: qlineno;		(* bottommost line examined *)
	findflag: boolean;		(* running under find? *)
	allowed_cmds: qed_cmd_set;	(* commands permitted to be executed *)
	var err: qerrcode);		(* errors? *)
