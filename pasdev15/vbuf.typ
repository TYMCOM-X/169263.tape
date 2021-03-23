(*********************************************************************)
(*   INCLUDE file for VBUF primitive types *)
$options special

const
  WORDCOUNT := 256;		(* words per virtual block *)
  MAXINDEX := 255;		(* index into page = WORDCOUNT - 1 *)
  MAXVPAGE := 1777777777B;	(* 2**36 div WORDCOUNT *)
  WORDSPERDBLOCK := 128;	(* words per disk block *)
  NUM_HEAD_BLOCKS := 1;		(* # of header blocks for old paging file *)

type
  HALFWORD = 0..777777b;	(* just about everything is a halfword *)
  WORD = -1 .. 377777777777b;	(* and fundamental data type *)
  FILENAME = packed array
    [1..30] of char;		(* basic file identifier *)

  (* for readability, the following 'types' are defined *)

  VWORDINDEX = 0..MAXINDEX;	(* scalar index into page *)
  VPAGEID = 0..MAXVPAGE;	(* scalar page number *)

(****************)
  VIRTUALPOINTER = packed record
    VPAGE: VPAGEID;		(* virtual page number *)
    VINDEX: VWORDINDEX		(* and index within that page *)
  end;  (* VIRTUALPOINTER DEFINITION *)

const
  VNIL: VIRTUALPOINTER := (MAXVPAGE,MAXINDEX);

(***************)
type
  SPACEPTR = ^SPACE;

  SPACE = array[0..MAXINDEX] of WORD;

(***************)
type
  VPAGEPTR = ^VIRTUALPAGE;

  VIRTUALPAGE = packed record	(* core buffer plus control info *)
    VREFCT: HALFWORD;		(* number of real pointers to buffer *)
    VBLOCK: VPAGEID;		(* which block is here right now? *)
    VNEXT: VPAGEPTR;		(* points to next less previous *)

    VSPACE: SPACEPTR;		(* the data area pointer *)

    VDIRTY: boolean		(* am I dirty? *)
  end;	(* VIRTUALPAGE DEFINITION *)


(***************)
  VBUFCTL = ^VBUFREC;

  VBUFREC = record		(* control record for address space *)
    VBUFNUM,			(* number of in-core buffers available *)
    VBUFUSED: HALFWORD;		(* and number allocated from PASCAL *)

    VMRUHEAD: VPAGEPTR;		(* head of MRU chain *)

    VGARBBUF: SPACEPTR;		(* a temp for I/O operations *)

    VLOCKCT,			(* count of total number of locks *)
    VHPAGE: HALFWORD;		(* next usable blank page for heap *)
    VPAGEFILE: FILENAME;	(* paging file for this address space *)
    VFILEOPEN: boolean;		(* is file open and ready? *)
    VCHANNEL: WORD 		(* mach-dep't file identifier *)
  end;	(* VBUFREC DEFINITION *)


(***************)
  VERROR = (			(* error scalar type *)
    VOK,
    VNOBUF,			(* no unlocked buffers available *)
    VBADRP,			(* real pointer not within a core buffer *)
    VBADVP,			(* detectably invalid VIRTUALPOINTER *)
    VNOTINIT,			(* address space not initialized *)
    VTOOLONGNEW,		(* attempt to VNEW beyond page size *)
    VINTERR,			(* fatal internal error *)
    VLASTLOCKED,		(* LRU page locked (internal, fatal) *)
    VZERONEW,			(* attempt to VNEW a zero-length record *)
    VDISPTWICE,			(* attempt to VDISPOSE a disposed record *)
    VNEEDONE,			(* must have at least one buffer to init *)
    VIOERR,			(* I/O error on read or write *)
    VBADFILE );			(* virtual page file not openable *)

(*********************************************************************)
