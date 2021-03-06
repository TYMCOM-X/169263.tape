$LENGTH 44
(*$M-,D-,&+*)
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        T R I C K Y                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 14-Jun-77

     PURPOSE: To  effect  a  deception  upon PASCAL and the PASIO I/O
        routines,  for the purpose of leaving two words open  at  the
        beginning of every module for database linking purposes.

     USAGE:
        READODD(<same calling sequence as RDRAND>)

     REQUIREMENTS: Watch  out  for  the declarations enclosed in ***.
        They must be changed for other machines.  Also,  insure  that
        BLOCKSIZE is correct in the caller.

     EFFECTS: Defines  type  IOBLOCK  differently (two words shorter)
        than  the  caller  (hopefully),  and  gives  to  RDRAND  this
        doctored  declaration.  It  reads  a  block two words shorter
        than usual,  into the last  words  of  the  block,  and  then
        zeroes the first two words.

     RESPONSIBLE: Jerry Rosen

     CHANGES: NONE.

     ---------------------------------------------------------------- *)
$INCLUDE USETYP.INC[52250,247]
 
(****************************************************************)
type	IOBLOCKSIZE = 1..128;
	IOBLOCK = packed record
		GOODSTUFF : packed array[1..125] of INTEGER
		end;
 
	DUMMY = packed record
		FAKE : packed array[1..3] of INTEGER;
		REALGOOD : IOBLOCK
		end;
(****************************************************************)
 
$INCLUDE PASIOR.INC[52250,247]
 
public procedure READODD(var ERRCODE : IOERRCODE;
		      FILEID : IOFILEID;
		  var BLOCK : DUMMY;
		      BLOCKSIZE : IOBLOCKSIZE;
		  var FILEPTR : IOFILEPTR);
begin
BLOCKSIZE := BLOCKSIZE - 3;
RDRAND(ERRCODE, FILEID, BLOCK.REALGOOD, BLOCKSIZE, FILEPTR);
BLOCK.FAKE[1] := 0;
BLOCK.FAKE[2] := 0;
BLOCK.FAKE[3] := 0
end.
    