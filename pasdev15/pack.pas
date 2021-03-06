$LENGTH 44
(*$D-,&+,M-*)
 
(*   +--------------------------------------------------------------+
     I                                                              I
     I                          P A C K                             I
     I                          - - - -                             I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 14-Jul-77

     PURPOSE: To implement the PACK instruction for ODMS.

     USAGE:
        PACK(DBFILE : IOFILE;  LASTMOD : INTEGER);

     INPUT: 

        DBFILE     A 20-character string describing the database file
                   to be packed.

        FIRSTMOD   A pointer to the first module record in the symbol
                   table.

     OUTPUT:

        file GZRXT4.LFH   used as a temporary file;  the database  is
                   written to this file,  and then
                   renamed.

     REQUIREMENTS: If the file to be packed is not a database file of
        the USE construct (i.e.  with matching module  numbers),  any
        modules  with  index  higher  than the last module in the USE
        file will be lost.

     EFFECTS: PACK will remove any unused blocks in a database  file,
        placing  all  directory blocks at the start of the file.  The
        ordering of the version chain is preserved.

     RESPONSIBLE: Jerry Rosen

     CHANGES: NONE.

     ---------------------------------------------------------------- *)
const	SIZEBLOCK = 128;
	BITSPERWORD = 36;
	BITSPERBLOCK = 4608;
 
$INCLUDE USETYP.INC[52250,247]
 
$INCLUDE MODTYP.INC[52250,247]

$INCLUDE PASIOR.INC

type	FILENAME = string[30];

 
external procedure GETDIRECT(var PTR, INDEX, MODNUM : INTEGER;  ID : IOFILEID); 
 
external procedure RENAME(CHNNUM : IOFILEID;   var NEWNAME : FILENAME);  
 
var	DBERRCODE, TMPERRCODE : IOERRCODE;
	DBID, TMPID : IOFILEID;
	DBPTR, NEXTDB, TMPTR, TMPDIRPTR : IOFILEPTR;
	BLOCK : IOBLOCK;
	TMPFILE : IOFILE;
	BLOCKSIZE : IOBLOCKSIZE;
	PAGEINDEX, MODSIZE, MODINDEX, WRITTEN : INTEGER;
 
 
public procedure PACKDB(DBFILE : FILENAME;    LASTMOD : INTEGER);
label	1,2,3;
 
 
procedure FLIPBIT(SIZE, WHERE : INTEGER);
var 	BITMAP : IOBLOCK;  PTR : IOFILEPTR;
begin
PTR := 0;
RDRAND(TMPERRCODE, TMPID, BITMAP, BLOCKSIZE, PTR);
if TMPERRCODE = IOERR then goto 1;
SIZE := (SIZE + SIZEBLOCK - 1) div SIZEBLOCK;
WHERE := WHERE div SIZEBLOCK;
for PTR := 1 to SIZE do
     BITMAP.BITS[WHERE + PTR] := 1;
PTR := 0;
WRRAND(TMPERRCODE, TMPID, BITMAP, BLOCKSIZE, PTR);
if TMPERRCODE = IOERR then goto 1
end;
 
 
begin
BLOCKSIZE := SIZEBLOCK;
TMPFILE := 'GZRXT4.LFH          ';
PASOPN(DBERRCODE, DBID, IOINOUT, IONODEL, DBFILE);
if DBERRCODE = IOERR then goto 1;
PASOPN(TMPERRCODE, TMPID, IOINOUT, IONODEL, TMPFILE);
if DBERRCODE = IOERR then goto 1;
DBPTR := BLOCKSIZE;   TMPTR := BLOCKSIZE;
RDRAND(DBERRCODE, DBID, BLOCK, BLOCKSIZE, DBPTR);
if DBERRCODE = IOERR then goto 1;
while BLOCK.WORDS[1] <> 0 do
     begin
     DBPTR := BLOCK.WORDS[1] * BLOCKSIZE;
     RDRAND(TMPERRCODE, TMPID, BLOCK, BLOCKSIZE, TMPTR);
     if TMPERRCODE = IOERR then goto 1;
     BLOCK.WORDS[1] := TMPTR div SIZEBLOCK;
     TMPTR := TMPTR - BLOCKSIZE;
     WRRAND(TMPERRCODE, TMPID, BLOCK, BLOCKSIZE, TMPTR);
     if TMPERRCODE = IOERR then goto 1;
     RDRAND(DBERRCODE, DBID, BLOCK, BLOCKSIZE, DBPTR);
     if DBERRCODE = IOERR then goto 1;
     FLIPBIT(SIZEBLOCK, TMPTR)
     end;
MODINDEX := 1;  TMPTR := TMPTR + SIZEBLOCK;
while MODINDEX <= LASTMOD do
     begin
     GETDIRECT(DBPTR, PAGEINDEX, MODINDEX, DBID);
     TMPDIRPTR := ((MODINDEX div (BLOCKSIZE - 1)) + 1)  * BLOCKSIZE;
     RDRAND(DBERRCODE, DBID, BLOCK, BLOCKSIZE, DBPTR);
     if DBERRCODE = IOERR then goto 1;
     if PAGEINDEX = 0 then
	  DBPTR := 0
     else DBPTR := BLOCK.WORDS[PAGEINDEX] * SIZEBLOCK;
     while DBPTR <> 0 do
	  begin
	  RDRAND(DBERRCODE, DBID, BLOCK, BLOCKSIZE, DBPTR);
	  if DBERRCODE = IOERR then goto 1;
	  DBPTR := DBPTR - BLOCKSIZE;
	  NEXTDB := BLOCK.FORMAT.POINTER * SIZEBLOCK;
	  MODSIZE := BLOCK.FORMAT.FILESIZE[1] +
		     BLOCK.FORMAT.FILESIZE[2];
	  FLIPBIT(MODSIZE, TMPTR);
	  RDRAND(TMPERRCODE, TMPID, BLOCK, BLOCKSIZE, TMPDIRPTR);
	  if TMPERRCODE = IOERR then goto 1;
	  TMPDIRPTR := TMPDIRPTR - BLOCKSIZE;
	  BLOCK.WORDS[PAGEINDEX] := TMPTR div SIZEBLOCK;
	  WRRAND(TMPERRCODE, TMPID, BLOCK, BLOCKSIZE, TMPDIRPTR);
	  if TMPERRCODE = IOERR then goto 1;
	  TMPDIRPTR := TMPTR;
	  PAGEINDEX := 1;
	  RDRAND(DBERRCODE, DBID, BLOCK, BLOCKSIZE, DBPTR);
	  if DBERRCODE = IOERR then goto 1;
	  WRITTEN := 0;
	  while WRITTEN < MODSIZE do
	       begin
	       WRRAND(TMPERRCODE, TMPID, BLOCK, BLOCKSIZE, TMPTR);
	       if TMPERRCODE = IOERR then goto 1;
	       WRITTEN := WRITTEN + SIZEBLOCK;
	       RDRAND(DBERRCODE, DBID, BLOCK, BLOCKSIZE, DBPTR);
	       if DBERRCODE = IOERR then goto 1
	       end;
	  DBPTR := NEXTDB
	  end;     (*WHILE DBPTR*)
     MODINDEX := MODINDEX + 1
     end;
goto 2;
 
1:  writeln(tty,'ERROR IN PACK I/O');
PASCLS(DBERRCODE, TMPID, IODEL);
PASCLS(DBERRCODE, DBID, IONODEL);
goto 3;
 
2:  (*NORMAL EXIT*)
PASCLS(DBERRCODE, DBID, IODEL);
PASCLS(DBERRCODE, TMPID, IONODEL);   (*TO FLUSH BUFFERS*)
PASOPN(DBERRCODE, TMPID, IOINOUT, IONODEL, TMPFILE);
RENAME(TMPID, DBFILE);
PASCLS(DBERRCODE, TMPID, IONODEL);
 
3:end.
    