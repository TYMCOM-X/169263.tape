EXTERNAL FUNCTION UUO_CALL
	  ( OPC: 1 .. #O100;		(* UUO opcode *)
	    AC: 0 .. #O17;		(* AC field of UUO *)
	    ACVAL: MACHINE_WORD;	(* value to go in AC *)
	    ADDR: PTR;			(* Addr field of UUO *)
	    VAR RETVAL: MACHINE_WORD	(* value returned in AC *)
	  ): BOOLEAN;			(* Success code *)
   