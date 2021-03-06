TYPE

(*
Legal object record sizes, number of items
*)
    OBJ_REC_SIZE = 0..512;
    OBJ_REC_ITEMS = 0..512;

(*
Kinds of object file records
*)
    OBJ_REC_ID = (MHD, GSD, TIR, FAL, EOM);
	(* Module HeaDer, Global Symbol Directory,
	   Text Information and Relocation,
	   Floating Address List, End Of Module *)

(*
Kinds of global symbol directory entries
*)
    GSD_ENT_ID = (PSECT_DEF, ENTRY_DEF, GLOBAL_DEF, GLOBAL_REF);

(*
Kinds of text information and relocation commands
*)
    TIR_CMD_ID = (STORE_IMM_CODE, STORE_IMM_DATA,
		  STACK_GLOBAL, STACK_LONGWORD, STACK_MODULE_PSECT_REL,
			STACK_IMAGE_PSECT_REL, STACK_ENTRY_FIELD,
		  STORE_LONGWORD, STORE_WORD, STORE_BYTE, STORE_REP_LONGWORD,
		  OPER_ADD, OPER_SUB, OPER_MUL, OPER_DIV, OPER_NEG,
		  CTL_SET_RELOC_BASE);

(*
Header for an object file record
*)
    OBJ_REC_HEADER = RECORD
	REC_ID: OBJ_REC_ID;		(* id of this record *)
	REC_LENGTHS: PACKED RECORD	(* object record length info *)
	    REC_SIZE: OBJ_REC_SIZE;	(* length of record in storage units,
					   excluding header *)
	    REC_ITEMS: OBJ_REC_ITEMS	(* number of items in the record *)
	    END
	END;

(*
Standard components of object file records:
*)
    OBJ_BYTE = 0..255;			(* a PVM byte *)
    OBJ_WORD = 0..65535;		(* a PVM word *)
    OBJ_LONGWORD = INTEGER;		(* a PVM longword *)
    OBJ_OFFSET = 0..MAXIMUM(INTEGER);	(* a legal offset value *)
    OBJ_SYMBOL = PACKED ARRAY [1..*] OF CHAR;	(* any identifier *)
    OBJ_PSECTNUM = OBJ_BYTE;		(* ordinal number of a psect *)
    OBJ_SYMBOLNUM = OBJ_WORD;		(* ordinal number of an
					   external symbol *)

(*
Module header record
consists of one of the following item
*)
    MHD_REC = RECORD
	STRUCTURE_LEVEL: INTEGER;	(* revision number of object language *)
	CREATION_TIME: PACKED ARRAY [1..18] OF CHAR;	(* dd-mmm-yy hh:mm:ss *)
	CREATOR: PACKED ARRAY [1..35] OF CHAR;	(* name of creating program *)
	MODULE_NAME: OBJ_SYMBOL		(* from program/module statement *)
	END;

(*
End of module record
consists of zero or one of the following item
*)
    EOM_REC = RECORD
	TRANSFER_PSECT: OBJ_PSECTNUM;	(* psect of transfer address *)
	TRANSFER_OFFSET: OBJ_OFFSET	(* transfer address offset in psect *)
	END;

(*
Global symbol directory record
consists of one or more global symbol directory entries
*)
    PSECT_ATTRIBUTES = (PSECT_EXECUTABLE, PSECT_READABLE, PSECT_WRITABLE,
	PSECT_DUMMY3, PSECT_DUMMY4, PSECT_DUMMY5, PSECT_DUMMY6,
	PSECT_DUMMY7, PSECT_DUMMY8, PSECT_DUMMY9, PSECT_DUMMY10,
	PSECT_DUMMY11, PSECT_DUMMY12, PSECT_DUMMY13, PSECT_DUMMY14,
	PSECT_DUMMY15);
    ENTRY_ATTRIBUTES = (ENTRY_RELATIVE, ENTRY_DUMMY1,
	ENTRY_DUMMY2, ENTRY_DUMMY3, ENTRY_DUMMY4, ENTRY_DUMMY5,
	ENTRY_DUMMY6, ENTRY_DUMMY7, ENTRY_DUMMY8, ENTRY_DUMMY9,
	ENTRY_DUMMY10, ENTRY_DUMMY11, ENTRY_DUMMY12, ENTRY_DUMMY13,
	ENTRY_DUMMY14, ENTRY_DUMMY15);
    GLOBAL_DEF_ATTRIBUTES = (DEF_RELATIVE, DEF_DUMMY1,
	DEF_DUMMY2, DEF_DUMMY3, DEF_DUMMY4, DEF_DUMMY5,
	DEF_DUMMY6, DEF_DUMMY7, DEF_DUMMY8, DEF_DUMMY9,
	DEF_DUMMY10, DEF_DUMMY11, DEF_DUMMY12, DEF_DUMMY13,
	DEF_DUMMY14, DEF_DUMMY15);
    GLOBAL_REF_ATTRIBUTES = (REF_DUMMY0, REF_DUMMY1, REF_DUMMY2, REF_DUMMY3,
	REF_DUMMY4, REF_DUMMY5, REF_DUMMY6, REF_DUMMY7,
	REF_DUMMY8, REF_DUMMY9, REF_DUMMY10, REF_DUMMY11,
	REF_DUMMY12, REF_DUMMY13, REF_DUMMY14, REF_DUMMY15);

    GSD_ENT = RECORD
	CASE ENT_ID: GSD_ENT_ID OF
	PSECT_DEF: (
		PSECT_ALIGNMENT: 0..9;	(* power of 2 for alignment boundary *)
		PSECT_FLAGS: SET OF PSECT_ATTRIBUTES;	(* flags for psect *)
		PSECT_ALLOCATION: OBJ_OFFSET;	(* number of bytes in this
						   module's contribution *)
		PSECT_NAME: OBJ_SYMBOL	(* name of psect *)
		);
	ENTRY_DEF: (
		ENTRY_PSECT: OBJ_PSECTNUM;   (* psect in which entry point is
					       defined *)
		ENTRY_FLAGS: SET OF ENTRY_ATTRIBUTES;
					(* flags for entry point definition *)
		ENTRY_FIELD: OBJ_LONGWORD;  (* value of entry field *)
		ENTRY_VALUE: OBJ_LONGWORD;  (* offset within psect *)
		ENTRY_NAME: OBJ_SYMBOL	(* name of entry point *)
		);
	GLOBAL_DEF: (
		GLOBAL_PSECT: OBJ_PSECTNUM;  (* psect in which symbol is
					       defined *)
		GLOBAL_DEF_FLAGS: SET OF GLOBAL_DEF_ATTRIBUTES;
					(* flags for global definition *)
		GLOBAL_VALUE: OBJ_LONGWORD; (* offset within psect *)
		GLOBAL_DEF_NAME: OBJ_SYMBOL (* name of global symbol *)
		);
	GLOBAL_REF: (
		GLOBAL_REF_FLAGS: SET OF GLOBAL_REF_ATTRIBUTES;
					(* flags for global reference *)
		GLOBAL_REF_NAME: OBJ_SYMBOL (* name of global symbol *)
		)
	END;

(*
Text information and relocation record
consists of one or more text information and relocation commands
*)
    TIR_CMD = RECORD
	CASE CMD_ID: TIR_CMD_ID OF
	STORE_IMM_CODE: (
		IMM_CODE: PACKED ARRAY [1..*] OF OBJ_WORD
					(* emitted code *)
		);
	STORE_IMM_DATA: (
		IMM_DATA: PACKED ARRAY [1..*] OF OBJ_BYTE
					(* emitted data *)
		);
	STACK_GLOBAL: (
		STACK_SYMBOL: OBJ_SYMBOLNUM
		(* ordinal number of symbol whose value is to be stacked *)
		);
	STACK_LONGWORD: (
		STACK_VALUE: OBJ_LONGWORD   (* value to be stacked *)
		);
	STACK_MODULE_PSECT_REL, STACK_IMAGE_PSECT_REL: (
		STACK_PSECT: OBJ_PSECTNUM;
			(* index of psect containing address to be stacked *)
		STACK_OFFSET: OBJ_OFFSET
			(* offset within psect of address to be stacked *)
		);
	STACK_ENTRY_FIELD: (
		STACK_ENTRY: OBJ_SYMBOL	(* name of entry point whose
					   entry field is to be stacked *)
		);
	STORE_LONGWORD, STORE_WORD, STORE_BYTE, STORE_REP_LONGWORD: (
		);
	OPER_ADD, OPER_SUB, OPER_MUL, OPER_DIV, OPER_NEG: (
		);
	CTL_SET_RELOC_BASE: (
		)
	END;

(*
Floating address list record
consists of one or more floating address list entries
*)
    FAL_LISTTYPE = (FAL_FLIST, FAL_DLIST, FAL_ILIST);

    FAL_ENT = RECORD
	FAL_LIST: FAL_LISTTYPE;
	FAL_PSECT: OBJ_PSECTNUM;
	FAL_OFFSETS: ARRAY [1..*] OF OBJ_OFFSET
	END;
  