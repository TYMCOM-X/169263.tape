(* INCLUDE file for TALK10 types and consts *)

type

  EIGHTBIT = 0..377B;

  TENFORMAT = packed record
    GARBAGE: 0..1777777777b;		(* fills out high-order 28 bits *)
    BYTE: EIGHTBIT			(* low-order 8 is all we use *)
  end (* TENFORMAT *);

  FORMATBLOCK =				(* for sending binary files *)
    packed array [1..512] of TENFORMAT;

  BINFILE = file of TENFORMAT;		(* file of DEC-10 words *)

  FNAME = string[30];			(* file descriptor for -10 and -11 *)
  FNPTR = ^FNREC;			(* list of file name records *)

  FNREC = record
    SRCFILE, DSTFILE: FNAME;		(* source and dest. file names *)
    BINARY: boolean;			(* true if source file is binary *)
    NEXT: FNPTR				(* next in command list *)
  end (* FNREC *);

  INTEGER = -1..1000000;		(* should be big enough *)

  LITLINE = packed array[1..30] of char;(* for getting literal lines *)

  TALKERR = (				(* scalar error type *)
    TOK,
    TNO10FILE,				(* SEND source file not found *)
    T11REFUSED				(* -11 sends NAK after directive *)
    );

const
  CTOKLEN := 10;			(* length of strings in command table *)

type
  COMTYPE = (				(* command scalar type *)
    ERRCOM,				(* bad command *)
    QUITCOM,				(* exit to Cyphernet *)
    EXITCOM,				(* synonym to QUIT *)
    HELPCOM,				(* prints help message *)
    GTCOMM,				(* list of transfers from 11 *)
    SENDCOM				(* list of transfers to 11 *)
    );
  COMLINE = string[255];		(* to contain line from user *)
  COMTOKEN = packed array[1..CTOKLEN]   (* table entry *)
    of char;
  COMTABLE = array[COMTYPE] of COMTOKEN;(* table of COMTOKEN strings *)
  COMSET = set of COMTYPE;		(* set of valid commands *)

const
  ALLCOMS: COMSET := [QUITCOM..SENDCOM];(* set of all real commands *)
  HIGHCMD: COMTYPE := SENDCOM;		(* highest valid command *)
  LOWCMD: COMTYPE := QUITCOM;		(* lowest valid command *)

  CTAB: COMTABLE := (			(* text of commands *)
    '',					(* to fill the table *)
    'QUIT',
    'EXIT',
    'HELP',
    'GET',
    'SEND'
    );


  CR: char := chr(15B);			(* carriage return *)
  LF: char := chr(12B);			(* line feed -- eoln char *)
  ALT: char := chr(33B);		(* escape *)
  ACK: char := chr(6);			(* ack is control-f *)
  NAK: char := chr(16B);		(* nak is control-n *)
  EOT: char := chr(61B);		(* end of transfer, '1' *)
  GETASC: char := chr(64B);		(* prepare to receive ASCII, '4' *)
  GETBIN: char := chr(75B);		(* prepare to receive binary, '=' *)
  SENDDIR: char := chr(67B);		(* prepare to send, '7' *)

  external procedure PASSCC;		(* to get control chars from GET *)
  external procedure FLTRCC;		(* to reset effect of PASSCC *)
  external procedure PASSCR;		(* to let cr's and lf's come thru *)
  external procedure FLTRCR;		(* to reset effect of PASSCR *)

  external procedure GETCH (var char);	(* get ch without dismissal of tty: *)

  external procedure BCSUM (var FORMATBLOCK; var EIGHTBIT; var EIGHTBIT);
  