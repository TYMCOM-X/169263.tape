Type

DIR_INT_ID = ^Integer;		(* FAB pointer used by DIR_OPEN... *)

DIR_EXT_ID = File_name;		(* Directory string *)

DIR_FSIZE = Integer;		(* Size of file, in bytes *)

DIR_ACCESS = DTIME_INT;		(* REVISION day/time, access not avail. *)

DIR_FNAME = File_name;		(* filename for next's return *)

DIR_M_STR = String [ 50 ];	(* The match string for DIR_MATCH *)

DIR_ERRORS = ( DIR_OK,		(* Th normal successful return *)
	       DIR_NO_OPEN,	(* DIR_OPEN failure *)
	       DIR_BAD_INT_ID,	(* DIR_NEXT was pased bad INT_ID *)
	       DIR_EOF,		(* DIR_NEXT at end of directory *)
	       DIR_NO_FILE );	(* DIR_ATTR can't find file *)

DIR_PROT_SCLS = ( DIR_READ, DIR_WRITE, DIR_EXECUTE, DIR_DELETE );

DIR_ACCT_SCLS = ( DIR_SYSTEM, DIR_OWNER, DIR_GROUP, DIR_WORLD );

DIR_PROT_SET = Set of DIR_PROT_SCLS;

DIR_PROT = Packed Array [ DIR_ACCT_SCLS ] of DIR_PROT_SET;

DIR_ATTRS = Record
    NAME	: DIR_FNAME;	(* filename in string format *)
    PROTECT	: DIR_PROT;	(* Protection code as below  *)
    SIZE	: DIR_FSIZE;	(* File size in bytes        *)
    CREATION	: DTIME_INT;	(* DAY/TIME in DTIME format  *)
    ACCESSED	: DIR_ACCESS	(* REVISION day/time, access not avail. *)
  End;
