(* QED.INC - modified 9/24/81 by djm to add const declarations of 
             tab, lf, cr, and esc *)

CONST   TAB = CHR (#O11);                       (* Tab             *)
        LF  = CHR (#O12);                       (* Line Feed       *)
        CR  = CHR (#O15);                       (* Carriage Return *)
        ESC = CHR (#O33);                       (* Escape          *)

EXTERNAL PROCEDURE QEDCL			(* main command loop for QED *)
(	VAR BUFFER: QBUFFER;			(* pre-setup buffer *)
	ALLOWED_CMDS: QED_CMD_SET);		(* command set to work with *)

EXTERNAL PROCEDURE QINITEXEC			(* equiv to qinit and qinitbuf *)
(	VAR BUFFER: QBUFFER);			(* buffer to be used *)

EXTERNAL PROCEDURE QINIT			(* setup QED to run *)
(	VAR BUFFER: QBUFFER);			(* buffer to initialize *)

EXTERNAL PROCEDURE QSETTABDEFAULT		(* SET TABS switch default set *)
(	DEFAULT: BOOLEAN);			(* true for SET TABS ON default *)

EXTERNAL PROCEDURE QSETMARKDEFAULT		(* mark string default setter *)
(	LINE: CMDLINE);				(* the default mark string *)

EXTERNAL PROCEDURE QEXECUTE			(* parse and execute one command line *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	VAR LINE: CMDLINE;			(* command line to parse *)
	VAR LINDEX: CMDLINEIDX;			(* place marker *)
	VAR EXECRANGE: LDRANGE;			(* limits of execution *)
	VAR BLE: QLINENO;			(* bottommost line examined *)
	FINDFLAG: BOOLEAN;			(* running under find? *)
	ALLOWED_CMDS: QED_CMD_SET;		(* commands permitted to be executed *)
	VAR ERR: QERRCODE);			(* errors? *)
 