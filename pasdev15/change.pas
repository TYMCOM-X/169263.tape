$LENGTH 44
$OPTIONS SPECIAL
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        C H A N G E                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED:  9-Mar-78

     PURPOSE: To implement the 'CHANGE VERSION' command for ODMS.

     USAGE:
        procedure VERCHN (OLD, NEW,             (* version numbers *)
                          MODULE: INTEGER;      (* module index *)
                          FN: string[30];       (* filename *)
                          var AOK: boolean);    (* success *)

     INPUT: 

        OLD        Version to be changed.  Error if not found.

        NEW        New version number.  Error if already there.

        MODULE     Standard module index of module to be altered.

        FN         Standard DEC-10 file descriptor for database.

     OUTPUT:

        AOK        Returns true if change was  successful,  false  if
                   version  numbers will not work,  if database could
                   not be opened,  or an I/O error occured.

     REQUIREMENTS: This module requires GETDIRECT and SEEKMOD,  which
        are  contained  in  module DELMOD,  and the Pascal random I/O
        routines.

     EFFECTS: If the old version exists,  and  the  new  number  does
        not,  the  version's  number  is  changed.  The 'Last Update'
        timestamp is not changed.

     NOTES: If the database is not  write-access  to  the  user,  the
        update will not be performed,  and the package will return an
        'I/O error' message.  In ODMS,  the user may  use  the  PRINT
        VERSIONS command to see if the change was performed.

     RESPONSIBLE: Jerry Rosen.

     CHANGES: NONE.

     ---------------------------------------------------------------- *)

const
  SIZEBLOCK = 128;
  WORDSIZE = 36;
  NUMHEADWD = 3;
  BITSPERBLOCK = 4608;

type
  FILENAME = string[30];

$INCLUDE USETYP.INC[52250,247]
$INCLUDE MODTYP.INC[52250,247]
$INCLUDE PASIOR.INC[52250,247]
$PAGE
var
  BLOCK: IOBLOCK;
  DUM1, DUM2, VERPTR, TEMPTR: IOFILEPTR;
  BLOCKSIZE: IOBLOCKSIZE;
  ERRCO: IOERRCODE;
  DBID: IOFILEID;

external procedure GETDIRECT (var PTR,INDEX,MODNUM: IOFILEPTR; DBID: IOFILEID);

external function SEEKMOD(var VER,NEXT,PAGE,INDEX: IOFILEPTR; DBID:
							IOFILEID):boolean;
$page
public procedure VERCHN (
  OLDVER, NEWVER,		(* version numbers to be changed *)
  WHICHMODULE: INTEGER;  		(* which module *)
  FILENM: FILENAME;		(* name of database to be worked on *)
  var AOK: boolean);

  label 1,2,3,4,5,6,7;

  begin
    BLOCKSIZE := SIZEBLOCK;
    AOK := true;
    PASOPN (ERRCO, DBID, IOINOUT, IONODEL, FILENM);
    if ERRCO <> IONOERR then goto 1;

    GETDIRECT (VERPTR, TEMPTR, WHICHMODULE, DBID);
    if TEMPTR = 0 then goto 2;

    RDRAND (ERRCO, DBID, BLOCK, BLOCKSIZE, VERPTR);
    if ERRCO = IOERR then goto 3;

    VERPTR := BLOCK.WORDS[TEMPTR] * BLOCKSIZE;
    if VERPTR = 0 then goto 2;

    TEMPTR := VERPTR;
    if SEEKMOD (NEWVER, TEMPTR, DUM1, DUM2, DBID) then
	goto 4;  	(* new version already there *)
    if not SEEKMOD (OLDVER, VERPTR, DUM1, DUM2, DBID) then
	goto 5;		(* old version not there *)

    (* VERPTR now points to the first block of the version to be changed *)

    RDRAND (ERRCO, DBID, BLOCK, BLOCKSIZE, VERPTR);
    if ERRCO = IOERR then goto 3;

    BLOCK.FORMAT.VERNUM := NEWVER;
    VERPTR := VERPTR - BLOCKSIZE;
    WRRAND (ERRCO, DBID, BLOCK, BLOCKSIZE, VERPTR);
    if ERRCO = IOERR then goto 3 
    else goto 7;

	1:  writeln (tty, 'Can''t open file '||FILENM|| '.' );
	    goto 6;

	2:  writeln (tty, 'Can''t find module in database.');
	    goto 6;

	3:  writeln (tty, 'Strange I/O error in VERCHN.');
	    goto 6;

	4:  writeln (tty, 'Version', NEWVER, ' already exists.');
	    goto 6;

	5:  writeln (tty, 'Version', OLDVER, ' not found.');

	6:  AOK := false;

	7:  PASCLS (ERRCO, DBID, IONODEL)
end.
   