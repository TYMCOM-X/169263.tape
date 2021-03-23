(*$&+,T-,O-*)

CONST   NO = 0;                         (* RELOCATION CONSTANTS *)
        RIGHT = 1;
        LEFT = 2;
        BOTH = 3;
TYPE    RELBYTE = 0..3B;                (* RELOCATION FOR EACH WORD *)
        RELWORD = PACKED ARRAY [0..17] OF RELBYTE;
                                        (* FULL RELOCATION WORD *)

        ADDRRANGE = 0..777777B;          (* ADDRESS VALUE, HALF WORD *)

        L10ITEMHDR =
          PACKED RECORD                 (* BLOCK HEADER *)
            ITEM: ADDRRANGE;            (* TAG FIELD *)
            COUNT: ADDRRANGE            (* NUMBER OF DATA WORDS - REL WORD *)
          END;
	SYMTYPE  = PACKED RECORD CODE:0..17B;
			NAME:0..37777777777B  END;

        L10OVERLAY =
          RECORD                        (* CHEAP KLUDGE FOR READING *)
            CASE INTEGER OF
              0: ( DATA: INTEGER );
              1: ( ITEMHDR: L10ITEMHDR );
              2: ( RELOCATION: RELWORD );
		3:(HWORD:PACKED ARRAY[0..1] OF ADDRRANGE);
		4:(WORD: INTEGER);
		5:(SYMWORD: SYMTYPE);
		6:(SIXBIT: PACKED ARRAY[1..6] OF 0..77B)
          END;
	FILENAMETYPE =  PACKED ARRAY[1..9] OF CHAR;
	EXTTYPE = PACKED ARRAY [1..3] OF CHAR;


VAR     OVERLAY: L10OVERLAY;
	NAME: PACKED ARRAY [1..6] OF CHAR;
	FILENAME: FILENAMETYPE;
	OUTPUTNAME: FILENAMETYPE;
	CH:CHAR;
	PASSTWO : BOOLEAN;
	FIRSTENT: INTEGER;




TYPE RELFILETYPE = FILE OF INTEGER;
VAR RELFILE : RELFILETYPE; LIB: RELFILETYPE;
     RELOCATION: RELWORD;

FUNCTION  NXTWORD:INTEGER; BEGIN
	NXTWORD := RELFILE^;
	IF PASSTWO THEN BEGIN
	  LIB^ := RELFILE^;
	  PUT (LIB)
	END;
	GET (RELFILE)
       END;


PROCEDURE FNAME(VAR NAMBUF:FILENAMETYPE;EXT:EXTTYPE);
VAR I:1..10;CH:CHAR;
BEGIN  BREAK (*MAKE SURE WE ARE READY*);
	FOR I:=1 TO 6 DO BEGIN IF NOT EOLN(TTY) AND (TTY^<>'.') THEN
	   READ(TTY,CH) ELSE CH:=' '; NAMBUF[I]:=CH END;
	IF TTY^='.' THEN BEGIN GET(TTY);
	 FOR I:=7 TO 9 DO BEGIN READ(TTY,CH); NAMBUF[I]:=CH END END
		ELSE FOR I:=1 TO 3 DO NAMBUF[I+6]:=EXT[I];
	 END;

PROCEDURE GETRELOCATION;
 BEGIN OVERLAY.DATA := NXTWORD;
	RELOCATION:=OVERLAY.RELOCATION END;

PROCEDURE RAD50TOSYM(RAD50WORD:INTEGER);
VAR WORD,I: INTEGER;
CNV50: PACKED ARRAY [0..47B] OF CHAR;
BEGIN WORD:=RAD50WORD; I:=6;
      CNV50:=' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.$%';
  WHILE I> 0 DO BEGIN
    NAME[I]:=CNV50[WORD MOD 50B]; I:=I-1;WORD:=WORD DIV 50B END;
    WRITELN (TTY, NAME); BREAK
END;

FUNCTION PROCESS (VAR ITEMHDR: L10ITEMHDR): BOOLEAN;
 VAR READCNT: ADDRRANGE;
     WORD: INTEGER;
     I: ADDRRANGE;
     LIBWORD: L10OVERLAY;
 BEGIN
  PROCESS := TRUE;
  WITH ITEMHDR DO BEGIN
    CASE ITEM OF
      0,1,3,6..37B:
              BEGIN
		GETRELOCATION;
		WORD := NXTWORD;
                READCNT := 1;
                WHILE READCNT < COUNT DO BEGIN
                  IF (READCNT MOD 18) = 0 THEN GETRELOCATION;
                  I:=READCNT MOD 18; WORD := NXTWORD;
                  READCNT := READCNT + 1
                END;
              END;
      2:	BEGIN
		FOR I:=0 TO (COUNT DIV 2)-1 DO BEGIN
		  IF (I MOD 9 )=0 THEN GETRELOCATION;
		  OVERLAY.WORD:=NXTWORD;
		   CASE OVERLAY.SYMWORD.CODE * 4 OF
		     04B: BEGIN
			WORD := OVERLAY.SYMWORD.NAME;
			IF NOT PASSTWO THEN BEGIN
			  IF WORD <> FIRSTENT THEN BEGIN
			    RAD50TOSYM (WORD);
			    LIBWORD.ITEMHDR.ITEM := 4;
			    LIBWORD.ITEMHDR.COUNT := 1;
			    LIB^ := LIBWORD.DATA; PUT (LIB);
			    LIB^ := 0; PUT (LIB);
			    LIBWORD.SYMWORD.CODE := 0;
			    LIBWORD.SYMWORD.NAME := WORD;
			    LIB^ := LIBWORD.DATA; PUT (LIB)
			  END;
			END;
		      END
		   END;
		   WORD := NXTWORD;
		END
		END;
      4:      BEGIN
                GETRELOCATION;
                READCNT := 1;
                WHILE READCNT <= COUNT DO BEGIN
                  IF (READCNT MOD 18) = 0 THEN GETRELOCATION;
		  OVERLAY.DATA := NXTWORD;
		  FIRSTENT := OVERLAY.SYMWORD.NAME;
                  READCNT := READCNT + 1
                END;
              END;
	5:	BEGIN
		 GETRELOCATION; WORD := NXTWORD;
		  WORD := NXTWORD;
		 PROCESS := FALSE;
		 RETURN
		END
    END;
  END
 END;



VAR
  ITEMHDR:  L10ITEMHDR;
	I: INTEGER;

BEGIN
 REWRITE(TTYOUTPUT);WRITE (TTY,'RELFILE: '); BREAK; RESET(TTY);
	; FNAME(FILENAME,'REL'); RESET (RELFILE,FILENAME);

	WRITE(TTY,'OUTPUT: '); BREAK; READLN(TTY);
	 FNAME(OUTPUTNAME,'REL'); REWRITE(LIB,OUTPUTNAME);

  FIRSTENT := 0;
  PASSTWO := FALSE;
  REPEAT
    OVERLAY.DATA := NXTWORD; ITEMHDR := OVERLAY.ITEMHDR;
  UNTIL NOT PROCESS (ITEMHDR);

  PASSTWO := TRUE;
  RESET (RELFILE,FILENAME);
  REPEAT
    OVERLAY.DATA := NXTWORD; ITEMHDR:= OVERLAY.ITEMHDR
  UNTIL NOT PROCESS (ITEMHDR);

  WRITELN (TTY, 'FINISHED'); BREAK;
END.
   