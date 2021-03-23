(*------------------------------------------------------------------*)
(* INCLUDE FILE DEFINING TYPES AND ROUTINES FOR PASCAL-10
   RANDOM I/O PACKAGE *)
  (*DEFINITION OF TYPES IOBLOCK AND IOBLOCKSIZE REQUIRED BEFORE NOW*)
CONST IOFILEMAX = 34;  (*MAXIMUM FILE NAME BUFFER LENGTH*)
TYPE
  IOFILEID = 1..15;
  IOFILEPTR = 0..377777777777B;  
  IOERRCODE = (IONOERR,IOERR,IOEOF);
  IOMODE = (IOIN,IOOUT,IOINOUT);
  IOCONT = (IODEL,IONODEL);
  IOFILE = PACKED ARRAY[1..IOFILEMAX] OF CHAR;

(*PROCEDURES ... *)

EXTERNAL PROCEDURE PASOPN(VAR ERRCODE: IOERRCODE; VAR FILEID: IOFILEID;
  MODE: IOMODE; CONT: IOCONT; VAR FILENAME: IOFILE); 
EXTERNAL PROCEDURE PASCLS(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  CONT: IOCONT);
EXTERNAL PROCEDURE PASREN(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  VAR NEWNAME: IOFILE);
EXTERNAL PROCEDURE RDRAND(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  VAR BLOCK: IOBLOCK; BLOCKSIZE: IOBLOCKSIZE; VAR FILEPTR: IOFILEPTR);
EXTERNAL PROCEDURE WRRAND(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  VAR BLOCK: IOBLOCK; BLOCKSIZE: IOBLOCKSIZE; VAR FILEPTR: IOFILEPTR);
EXTERNAL PROCEDURE RDNEXT(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  VAR BLOCK: IOBLOCK; VAR BLOCKSIZE: IOBLOCKSIZE); 
EXTERNAL PROCEDURE WRNEXT(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  VAR BLOCK: IOBLOCK; BLOCKSIZE: IOBLOCKSIZE); 
EXTERNAL PROCEDURE GETSTA(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  VAR CURSOR, EOFPTR: IOFILEPTR; VAR MODE: IOMODE); 
EXTERNAL PROCEDURE SETSTA(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID;
  CURSOR: IOFILEPTR); 
EXTERNAL PROCEDURE  FLUSH1(VAR ERRCODE: IOERRCODE; FILEID: IOFILEID);
EXTERNAL PROCEDURE FLUSHA(VAR ERRCODE: IOERRCODE);
(*------------------------------------------------------------------*)
    