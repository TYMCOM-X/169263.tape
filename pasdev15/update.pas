$LENGTH 44
(*$D-,M-,&+*)
const	SIZEBLOCK = 128;		(*MACHINE-DEPENDENT PARAMETERS*)
        WORDSIZE = 36;			(*THIS SET IS FOR THE PDP-10*)
        BITSPERBLOCK = 4608;
	NUMHEADWD = 3;
 
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        U P D A T E                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 14-Jun-77

     PURPOSE: To perform the UPDATE command for ODMS.

     USAGE:
        UPDATE(MODNUM, VERSION : INTEGER;  var ERROR : boolean);

     INPUT: 

        MODNUM, VERSION   The module number and version number of the
                   module to be inserted into the database file.

        ERROR      Should be set to FALSE by the caller.

     OUTPUT:

        ERROR      Will  be  set  to TRUE by UPDATE if any I/O errors
                   have occured during UPDATE.

     REQUIREMENTS: Entry point UPINIT must be called prior to calling
        UPDATE.  Calling  sequence:  (FNAME,  DBNAME  :  IOFILE;  var
        ERROR : boolean);  Type IOFILE is a twenty-character filename
        for  the  module  file  and the database file,  respectively.
        ERROR will be true (initialize  FALSE)  if  the  module  file
        does not exist.  On a good return,  the module file will have
        been opened.

     EFFECTS: Will update the database by placing the new version  on
        the  end  of the module number's version chain,  deleting any
        old versions with the same version number.  There is no check
        for  overflowing  the database,  so it is a good idea to pack
        it regularly,  and prevent it from becoming larger than  4608
        blocks (on the PDP-10).  If the database file does not exist,
        it will be created.

     NOTES: Fatal I/O errors will leave the database in an  uncertain
        state.  However,  assuming the actual information on the disk
        is not  tromped  upon,  the  integrity  of  the  database  is
        preserved due to the ordering of operations.

     RESPONSIBLE: Jerry Rosen

     CHANGES: NONE.

     ---------------------------------------------------------------- *)
$INCLUDE USETYP.INC[52250,247]

$INCLUDE MODTYP.INC[52250,247]
$INCLUDE PASIOR.INC[52250,247]
 
 
type	FILENAME = string[30];

 
(*ASSUME THAT THERE IS ONLY ONE WRITER AT ONE TIME!!*)
 
var	BITMAP, BLOCK : IOBLOCK;
        MODERRCODE, DBERRCODE : IOERRCODE;
        MODFILEID, DBFILEID : IOFILEID;
        I, BLOCKSIZE, PAGEINDEX : IOBLOCKSIZE;
        MODSIZE, PAGECOUNT, PAGENUM : INTEGER;
        DIRPTR, TEMPTR, NEXTPTR, NEWPTR, DBFILEPTR, MODFILEPTR : IOFILEPTR;
        WHICH : IOFILEPTR;
        DBFILE : IOFILE;
        DELETE : boolean;

$PAGE
external procedure READODD(var ERRCODE : IOERRCODE;
                      FILEID : IOFILEID;
                  var BLOCK : IOBLOCK;
                      BLOCKSIZE : IOBLOCKSIZE;
                  var FILEPTR : IOFILEPTR);  
(*READODD IS AN EXTERNALLY COMPILED PROCEDURE WITH A DIFFERENT DEFINITION
  FOR TYPE <IOBLOCK>.  IT WILL READ IN <BLOCKSIZE - NUMHEADWD> WORDS INTO THE LAST
  <BLOCKSIZE - NUMHEADWD> WORDS OF A BLOCK, ZEROING THE FIRST WHATEVER.  THIS IS TO
  ALLOW ROOM FOR THE DATABASE POINTER AND VERSION NUMBER WITHOUT ANY
  CONSTRAINTS ON THE MODULE FILE (i.e. whether it was 'generated' for 
  insertion into a database or not).                      *)


external function UUO(OPCODE, ADDR : INTEGER;  var AC : INTEGER) : boolean;  


public procedure UPINIT(var FNAME, DBNAME : FILENAME;
                 var ERROR : boolean);
begin
PASOPN(MODERRCODE, MODFILEID, IOIN, IONODEL, FNAME);
if MODERRCODE = IOERR then ERROR := true;
DBFILE := DBNAME
END;
 
 
public procedure FRESPACE(SIZE, ORIGIN : INTEGER;
		   BLOCKSIZE : IOBLOCKSIZE;  FID : IOFILEID);
var	PTR : IOFILEPTR;
	ERRCO : IOERRCODE;
label	5;
begin
PTR := 0;
RDRAND(ERRCO, FID, BITMAP, BLOCKSIZE, PTR);
if ERRCO = IOERR then goto 5;
SIZE := (SIZE + (BLOCKSIZE - 1 - NUMHEADWD)) div BLOCKSIZE;
for PTR := 1 to SIZE do
     BITMAP.BITS[(ORIGIN div SIZEBLOCK) + PTR] := 0;
PTR := 0;
WRRAND(ERRCO, FID, BITMAP, BLOCKSIZE, PTR);
5:  end;
 
$page
 
public procedure UPDATE(MODNUM, VERSION, OVSIZE, STATSIZE : INTEGER;
                 var ERROR : boolean);
label	1,2,3,4;

        procedure MY_RDRAND(var ERRCODE : IOERRCODE;
                               FILEID : IOFILEID;
                           var BLOCK : IOBLOCK;
                               BLOCKSIZE : IOBLOCKSIZE;
                               FILEPTR : IOFILEPTR);
(*MY_RDRAND AND MY_WRRAND ARE CALLED FROM UPDATE INSTEAD OF THE PASIO
  PROCEDURES RDRAND AND WRRAND.  BEFORE THE READ/WRITE, THE CURRENT
  BLOCK NUMBER IS SAVED IN <WHICH>, TO FACILITATE ERROR DIAGNOSIS AND
  EXITING.  LABELS <1> AND <2> DEAL WITH ERRORS AT THE END OF UPDATE  *)
 
	var TPTR: IOFILEPTR;

 
        begin
        WHICH := FILEPTR div BLOCKSIZE;
	TPTR := FILEPTR;
        RDRAND(ERRCODE, FILEID, BLOCK, BLOCKSIZE, TPTR);
        if ERRCODE = IOERR then goto 2;
        end;
         
        procedure MY_WRRAND(var ERRCODE : IOERRCODE;
                               FILEID : IOFILEID;
                           var BLOCK : IOBLOCK;
                               BLOCKSIZE : IOBLOCKSIZE;
                               FILEPTR : IOFILEPTR);

	var TPTR: IOFILEPTR;

        begin
        WHICH := FILEPTR div BLOCKSIZE;
	TPTR := FILEPTR;
        WRRAND(ERRCODE, FILEID, BLOCK, BLOCKSIZE, TPTR);
        if ERRCODE = IOERR then goto 1
        end;
 procedure GETSPACE(    SIZE : INTEGER;
                   var PTR : IOFILEPTR);
var BITINDEX, ZEROCOUNT : INTEGER;
    TEMP : IOFILEPTR;
 
begin
TEMP := 0;
ZEROCOUNT := 0;
BITINDEX := 2; (*TO SKIP BITMAP AND FIRST DIR BLOCK*)
MY_RDRAND(DBERRCODE, DBFILEID, BITMAP, BLOCKSIZE, TEMP);
SIZE := (SIZE + SIZEBLOCK - 1) div SIZEBLOCK;
while (ZEROCOUNT < SIZE) and (BITINDEX <= BITSPERBLOCK) do
     begin
     BITINDEX := BITINDEX + 1;
     if BITMAP.BITS[BITINDEX] = 1
          then ZEROCOUNT := 0
          else ZEROCOUNT := ZEROCOUNT + 1
     end;
PTR := (BITINDEX - SIZE) * SIZEBLOCK;
for ZEROCOUNT := (BITINDEX - (SIZE - 1)) to BITINDEX do
        BITMAP.BITS[ZEROCOUNT] := 1;
MY_WRRAND(DBERRCODE, DBFILEID, BITMAP, BLOCKSIZE, TEMP)
end;
(*UPDATE'S HELPER TO SEEK OUT FREE SPACES ON THE DATABASE*)
 
 
 
 
	function DATIME : INTEGER;
	var DATE, TEMP, TIME : INTEGER;  RESULT : boolean;
	begin
	RESULT := UUO(47B, 14B, DATE);
	RESULT := UUO(47B, 23B, TIME);
	TEMP := ((DATE mod 31) +1) * 100B;
	DATE := DATE div 31;
	TEMP := TEMP + (((DATE mod 12) + 1) * 10000B);
	DATE := TEMP + (DATE div 12);
	TEMP := (TIME div 3600000) * 10000B;
	TIME := (TIME div 1000) mod 3600;
	TEMP := TEMP + ((TIME div 60) * 100B);
	TIME := TEMP + (TIME mod 60);
	DATIME := (DATE * 1000000B) + TIME 
	end; 



        procedure WRTMOD(HERE : IOFILEPTR);
        var MODPTR : IOFILEPTR;
            PAGES, NEWSIZE : INTEGER;
        begin
        MODPTR := 0;
        READODD(MODERRCODE, MODFILEID, BLOCK, BLOCKSIZE, MODPTR);
        if MODERRCODE = IOERR then goto 2;
        BLOCK.FORMAT.POINTER := 0;
        NEWSIZE := BLOCK.FORMAT.FILESIZE[1] + BLOCK.FORMAT.FILESIZE[2];
        BLOCK.FORMAT.VERNUM := VERSION;
	BLOCK.FORMAT.FILESIZE[2] := BLOCK.FORMAT.FILESIZE[2] + NUMHEADWD;
	(*TO INSURE THAT STATIC IS READ AT PROPER FILE LOCATION*)
	BLOCK.FORMAT.DATEWD.INTF := DATIME;
        PAGES := 1;
        MODPTR := SIZEBLOCK - NUMHEADWD;
        while PAGES <= (NEWSIZE + (BLOCKSIZE - 1)) div BLOCKSIZE do
             begin
             MY_WRRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, HERE);
             MY_RDRAND(MODERRCODE, MODFILEID, BLOCK, BLOCKSIZE, MODPTR);
             HERE := HERE + BLOCKSIZE;
             PAGES := PAGES + 1;
             MODPTR := MODPTR + BLOCKSIZE
             end
        end;(*ANOTHER HELPER TO ACCOMODATE READING ONLY (BLOCKSIZE-NUM)*)
            (*WORDS INTO THE BUFFER AT ADDRESS+NUM*)
 
 
$PAGE
begin  (*CODE FOR PROCEDURE UPDATE*)
BLOCKSIZE := SIZEBLOCK;
MODFILEPTR := 0;
WHICH := 0;
READODD(MODERRCODE, MODFILEID, BLOCK, BLOCKSIZE, MODFILEPTR);
if MODERRCODE = IOERR then goto 2;
MODSIZE := BLOCK.FORMAT.FILESIZE[1] + BLOCK.FORMAT.FILESIZE[2];
                (*GETS TOTAL MODFILE SIZE*)
if (BLOCK.FORMAT.CODE[1] > OVSIZE) or (BLOCK.FORMAT.STAT[1] > STATSIZE) then
	goto 4; (*KICK OUT IF MODULE WILL NOT FIT LIKE COMMAND SAID*)
PASOPN(DBERRCODE, DBFILEID, IOINOUT, IONODEL, DBFILE);
DBFILEPTR := 0;
GETSPACE(MODSIZE, NEWPTR);
PAGENUM := (MODNUM div (BLOCKSIZE - 1)) + 1;	(*PAGENUM AND INDEX OF *)
PAGEINDEX := (MODNUM mod (BLOCKSIZE - 1)) + 1;	(*PROPER DIRECTORY ENTRY*)
PAGECOUNT := 1;
DIRPTR := BLOCKSIZE;  (*TO GET SECOND BLOCK*)
MY_RDRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR);
while PAGECOUNT <> PAGENUM do
     begin
     if BLOCK.FORMAT.POINTER = 0 then
          begin
          GETSPACE(BLOCKSIZE, DBFILEPTR);
          for I := 1 to BLOCKSIZE do BLOCK.WORDS[I] := 0;
          MY_WRRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DBFILEPTR);
          (*WRITE OUT A NEW (BLANK) DIRECTORY PAGE*)
          MY_RDRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR);
          BLOCK.FORMAT.POINTER := DBFILEPTR div BLOCKSIZE;
          (*NOW FIX PREVIOUS PAGE TO POINT TO THIS ONE*)
          MY_WRRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR);
          DIRPTR := DBFILEPTR
          end
     else begin
          DIRPTR := (BLOCK.FORMAT.POINTER) * BLOCKSIZE;
          MY_RDRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR)
          end;
     PAGECOUNT := PAGECOUNT + 1
     end;
DBFILEPTR := BLOCK.WORDS[PAGEINDEX] * BLOCKSIZE;
 
(*FROM HERE ON DOWN, THE PAIR DIRPTR--PAGEINDEX WILL POINT TO THE POINTER
THAT HAS TO BE CHANGED IF AN IDENTICAL VERSION EXISTS i.e. DELETE SET TO
TRUE.  NEXTPTR WILL POINT TO THE MODULE THAT THE DELETED VERSION POINTED
TO, i.e. THE NEW VALUE FOR THE CHANGED POINTER*)
 
DELETE := false;
if DBFILEPTR = 0 then
     begin
     WRTMOD(NEWPTR);
     MY_RDRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR);
     (*GET DIR PAGE AND FIX UP IF NO MODULES ON CHAIN*)
     BLOCK.WORDS[PAGEINDEX] := NEWPTR DIV BLOCKSIZE;
     MY_WRRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR)
     end
else begin
          loop
          (*A WALK DOWN THE MODULE'S VERSION CHAIN TO FIND THE END, AND TO
            DETECT ANY PREVIOUSLY EXISTING VERSIONS*)
          (*TEMPTR GETS MODULE'S ADDRESS FOR BITMAP DEALLOCATION*)
          MY_RDRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DBFILEPTR);
          if not DELETE then 
               if BLOCK.FORMAT.POINTER <> 0 then
                         NEXTPTR := BLOCK.FORMAT.POINTER * BLOCKSIZE
                    else NEXTPTR := NEWPTR;
          if BLOCK.FORMAT.VERNUM = VERSION then
               begin
               DELETE := true;
               MODSIZE := BLOCK.FORMAT.FILESIZE[1] +
                          BLOCK.FORMAT.FILESIZE[2];
               TEMPTR := DBFILEPTR
               end;
          exit if BLOCK.FORMAT.POINTER = 0;
          if not DELETE then
               begin
               DIRPTR := DBFILEPTR;
               PAGEINDEX := 1
               end;
          DBFILEPTR := BLOCK.FORMAT.POINTER * BLOCKSIZE
          end;
     (*NOW WE CAN WRITE THE MODULE, READ IN THE LAST BLOCK, CHANGE ITS
       POINTER, AND WRITE IT BACK.*)
     WRTMOD(NEWPTR);
     MY_RDRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DBFILEPTR);
     BLOCK.FORMAT.POINTER := NEWPTR div BLOCKSIZE;
     MY_WRRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DBFILEPTR);
     if DELETE then
          begin
          (*TO DELETE A PREVIOUS VERSION, GET THE BLOCK POINTING TO IT
            (DIRPTR), TROMP ON ITS POINTER, CHANGING IT TO POINT TO
            THE DELETED'S SUCCESSOR, AND DEALLOCATE ITS SPACE  *)
          MY_RDRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR);
          BLOCK.WORDS[PAGEINDEX] := NEXTPTR div BLOCKSIZE;
          MY_WRRAND(DBERRCODE, DBFILEID, BLOCK, BLOCKSIZE, DIRPTR);
	  FRESPACE(MODSIZE, TEMPTR, BLOCKSIZE, DBFILEID)
          end
     end;
goto 3;
 
1:writeln(tty);
writeln(tty,' ERROR IN OUTPUT BLOCK  ',WHICH:7);
writeln(tty,' DATABASE INTEGRITY IN QUESTION');
ERROR := true;
goto 3;
 
4:writeln(tty);
writeln(tty,' MODULE TOO LARGE FOR ALLOCATED AREAS IN OVERLAY SCHEME');
writeln(tty,' UPDATE TO DATABASE NOT PERFORMED');
ERROR := true;
goto 3;
 
2:writeln(tty);
writeln(tty,' ERROR IN INPUT BLOCK   ',WHICH:7);
ERROR := true;
 
3:PASCLS(MODERRCODE, MODFILEID, IONODEL);
PASCLS(DBERRCODE, DBFILEID, IONODEL)
end.
    