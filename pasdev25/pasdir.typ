(* PASDIR.TYP Type definitions for PASCAL directory package *)
(*     for the M68000 VERSADos operating system             *)

type
  DIR_EXT_ID = string[80];		(* string describing directory *)
  DIR_INT_ID = 0..15;			(* internal identifier *)
  DIR_FNAME = string[40];		(* file name string *)
  DIR_FCHAR = packed array[1..9] of char; (* file name and extension *)
  DIR_M_STR = string[50];		(* pattern matching string *)

  DIR_ERRORS = (			(* error scalar type *)
    DIR_OK,
    DIR_NO_OPEN,			(* DIR_OPEN can't open directory file *)
    DIR_BAD_PPN,			(* invalid PPN string for DIR_OPEN *)
    DIR_BAD_INT_ID,			(* invalid internal ID to DIR_NEXT *)
    DIR_NOT_OPEN,			(* no directory open on internal ID *)
    DIR_EOF,				(* end of file reached on directory *)
    DIR_NO_FILE				(* DIR_ATTR can't find file *)
    );


  DIR_PROT = record
    RCODE : String[2];
    WCODE : String[2]
  End;


  M68_FILE_NAME = record
    CATALOG : packed array [1..8] of char;
    FNAME   : packed array [1..8] of char;
    EXT     : packed array [1..2] of char
  end;


  DIR_ATTRS = record
    NAME: M68_FILE_NAME;
    PROTECT: DIR_PROT; (* protection as digits *)
    SIZE: INTEGER;		(* size in words *)
    CREATION: DTIME_INT;	(* day/time of creation *)
    ACCESSED: DATE_INT		(* date of most recent access *)
    end;

  (* end of PASDIR.TYP *)
 