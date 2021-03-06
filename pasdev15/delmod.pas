$LENGTH 44
$OPTIONS SPECIAL
const	SIZEBLOCK = 128;
	WORDSIZE = 36;
	BITSPERBLOCK = 4608;
	NUMHEADWD = 3;    (*DATABASE HEADER INFO WORDS*)

type	FILENAME = string[30];

 
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        D E L M O D                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED:  9-Mar-78

     PURPOSE: To implement the DELETE command for ODMS.

     USAGE:
        procedure DELMOD (MODNUM, VERNUM: INTEGER;
                          var SUCCESS: boolean;
                          FN: string[30]);

     INPUT: 

        MODNUM     The standard module index to be deleted.

        VERNUM     The specific version to be deleted;  (-1)  if  all
                   versions to be deleted.

        FN         The   standard   DEC-10  file  descriptor  of  the
                   database file.

     OUTPUT:

        SUCCESS    TRUE  indicates   the   deletion   was   performed
                   normally.   False if      some  error  occured--
                   DELMOD performs its own error diagnosis.

     REQUIREMENTS: This package uses the Pascal random I/O routines.

     EFFECTS: The program will walk the version chain of MODNUM,  and
        delete versions as appropriate.  The bitmap is altered as the
        final step,  and separately for each version  deleted.  Thus,
        integrity of the database is not affected by interrupting the
        processing,  but  you  may  lose  a  version  or   two,   and
        definitely some free space.

     RESPONSIBLE: Jerry Rosen

     CHANGES: NONE.

     ---------------------------------------------------------------- *)
 
$INCLUDE USETYP.INC[52250,247]
 
$INCLUDE MODTYP.INC[52250,247]
 
$INCLUDE PASIOR.INC
 
var	ERRCO : IOERRCODE;
	DBID : IOFILEID;
	BLOCK : IOBLOCK;
	BLOCKSIZE : IOBLOCKSIZE := SIZEBLOCK;
	PAGE, PAGEINDEX, NEXTPAGE, MODSIZE : IOFILEPTR;
 
 
external procedure FRESPACE(SIZE, ORIGIN : INTEGER;  BLOCKSIZE : IOBLOCKSIZE;
		   FID : IOFILEID);    
 
public procedure GETDIRECT(var PTR, INDEX, NUM : IOFILEPTR;  DBID : IOFILEID);
var	COUNT : INTEGER;
	PAGE : IOFILEPTR; 
begin
PAGE := (NUM div (BLOCKSIZE - 1)) + 1;
INDEX := (NUM mod (BLOCKSIZE - 1)) + 1;
COUNT := 1;
PTR := BLOCKSIZE;
RDRAND(ERRCO, DBID, BLOCK, BLOCKSIZE, PTR);
PTR := PTR - BLOCKSIZE;
if ERRCO = IOERR then
     INDEX := 0
else begin
     while PAGE <> COUNT do
	  begin
	  if BLOCK.FORMAT.POINTER <> 0 then
	       begin
	       PTR := BLOCK.FORMAT.POINTER * BLOCKSIZE;
	       RDRAND(ERRCO, DBID, BLOCK, BLOCKSIZE, PTR);
	       PTR := PTR - BLOCKSIZE;
	       if ERRCO = IOERR then BLOCK.FORMAT.POINTER := 0
	            else COUNT := COUNT + 1
	       end
	  else begin
	       COUNT := PAGE;
	       INDEX := 0
	       end
	  end
     end
end;
 
 
public function SEEKMOD(var VER, NEXT, PAGE, INDEX : IOFILEPTR;
					DBID: IOFILEID): boolean;
begin
BLOCK.FORMAT.VERNUM := VER + 1;
     loop
     if NEXT <> 0 then
	  begin
	  RDRAND(ERRCO, DBID, BLOCK, BLOCKSIZE, NEXT);
	  NEXT := NEXT - BLOCKSIZE
	  end;
     exit if (BLOCK.FORMAT.VERNUM = VER)  or
	     (VER = -1)  or (NEXT = 0)  or
	     (ERRCO = IOERR);
     PAGE := NEXT;
     NEXT := BLOCK.FORMAT.POINTER * SIZEBLOCK;
     INDEX := 1
     end;
SEEKMOD := (NEXT <> 0) and (ERRCO <> IOERR)
end;
 

public procedure DELMOD(MODNUM, VERNUM : INTEGER;  var SUCCESS : boolean;
	         FILNAM : FILENAME);
 
label	1, 2, 3;
 
begin
SUCCESS := true;  BLOCKSIZE := SIZEBLOCK;
PASOPN(ERRCO, DBID, IOINOUT, IONODEL, FILNAM);
if ERRCO = IOERR then goto 1;
GETDIRECT(PAGE, PAGEINDEX, MODNUM, DBID);
if PAGEINDEX = 0 then goto 2
else if BLOCK.WORDS[PAGEINDEX] = 0 then goto 2
     else begin
	  NEXTPAGE := BLOCK.WORDS[PAGEINDEX] * BLOCKSIZE;
	  if VERNUM = -1 then
	       begin
	       BLOCK.WORDS[PAGEINDEX] := 0;
	       WRRAND(ERRCO, DBID, BLOCK, BLOCKSIZE, PAGE);
	       if ERRCO = IOERR then goto 1
		    else PAGE := PAGE - BLOCKSIZE
	       end;
	  SUCCESS := false;
	  while SEEKMOD(VERNUM, NEXTPAGE, PAGE, PAGEINDEX,DBID) do
	       begin
	       SUCCESS := true;
	       writeln(tty,' VERSION ', BLOCK.FORMAT.VERNUM : 9, ' DELETED');
	       FRESPACE( (BLOCK.FORMAT.FILESIZE[1] + BLOCK.FORMAT.FILESIZE[2]),
			NEXTPAGE, BLOCKSIZE, DBID);
	       NEXTPAGE := BLOCK.FORMAT.POINTER;
	       RDRAND(ERRCO, DBID, BLOCK, BLOCKSIZE, PAGE);
	       if ERRCO = IOERR then goto 1;
	       BLOCK.WORDS[PAGEINDEX] := NEXTPAGE;
	       PAGE := PAGE - BLOCKSIZE;
	       WRRAND(ERRCO, DBID, BLOCK, BLOCKSIZE, PAGE);
	       PAGE := PAGE - BLOCKSIZE;
	       NEXTPAGE := NEXTPAGE * SIZEBLOCK
	       end
	  end;
if not SUCCESS then writeln(tty,' VERSION NOT FOUND');
goto 3;

2:   writeln(tty,'  MODULE NOT FOUND');
     SUCCESS := false;
     goto 3;
 
1:   writeln(tty,' I/O ERROR DURING DELETION');
     SUCCESS := false;
3:PASCLS(ERRCO, DBID, IONODEL)
end.
   