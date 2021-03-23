	     (**********************************
	      *                                *
	      *       scribe-10 mainline       *
	      *                                *
	      *            contains            *
	      * pagination and command parsing *
	      *            routines            *
	      *                                *
	      **********************************)

CONST MAXTABS = 20;				(*maximum number of tab stops*)

EXTERNAL
$INCLUDE SCRVAR.INC
$INCLUDE stdtyp.inc
$include levels.typ
public   (* must immediately precede levels.var *)
$include levels.var
public var
  inf: array [1..8] of text;
  inf_depth: 1..8;             (* vars for include files *)
var entry_f: text := nilf;	(* table of contents file *)
  eof_inf: boolean := false;   (* true if include file just closed *)

$INCLUDE stdvar.pas

$INCLUDE cmdutl.typ[,320156]
$INCLUDE filutl.inc[,320156]
$INCLUDE jobnum.inc

		    (* general error routine *)

PUBLIC PROCEDURE ERROR(CODE:INTEGER);
VAR LNUM:LINENUM; LCNT:INTEGER;
BEGIN
  GETLNR(LNUM,LCNT);
  WRITE(TTY,'***LINE: ');
  IF LNUM='-----' THEN WRITELN(TTY,LCNT:5)
  ELSE WRITELN(TTY,LNUM);
  IF CODE<= 100 THEN (* NON-FATAL ERRORS *)
    CASE CODE OF
    1: WRITELN(TTY,'PAGE LENGTH TOO SHORT');
    2: WRITELN(TTY,'OUTPUT LINE TOO LONG');
    3: WRITELN(TTY,'NOT ENOUGH LINES AT TOP');
    4: WRITELN(TTY,'NOT ENOUGH LINES AT BOTTOM');
    5: WRITELN(TTY,'TITLE/FOOTNOTE TOO LONG');
    6: WRITELN(TTY,'INPUT LINE TOO LONG');
    7: WRITELN(TTY,'INPUT WORD TOO LONG')
    END
  ELSE BEGIN  (* FATAL ERRORS *)
    CASE CODE-100 OF
    1: WRITELN(TTY,'MAXIMUM LINE LENGTH EXCEEDED IN READER')
    END;
    WRITELN(TTY,'?  ***FATAL ERROR IN SCRIBE ***');
    STOP
  END;
  BREAK(TTY)
END;						(*error*)

   PROCEDURE INIT_TABLE;			(* initialize translation vector *)

     VAR
       CHRCTR: CHAR;


    BEGIN
      FOR CHRCTR := ' ' TO '~' DO
	MAP[CHRCTR] := CHRCTR;
      MAP['\'] := ' '
    END;
$PAGE utilities
(* predicate to determine if page should be printed *)

FUNCTION INRANGE: BOOLEAN;
 BEGIN
  INRANGE := (PAGERANGE <> NIL) ANDIF ((PAGERANGE^.FIRST <= PAGECNT) AND (PAGECNT <= PAGERANGE^.LAST))
 END;


(*utility to determine field size*)

FUNCTION FSIZE(V_VALUE:INTEGER):INTEGER;
VAR SIZE: INTEGER;
    VALUE: INTEGER;
BEGIN
  VALUE:= V_VALUE;
  SIZE:= 0;
  REPEAT
    SIZE:= SIZE+1;
    VALUE:= VALUE DIV 10
  UNTIL VALUE=0;
  FSIZE:= SIZE
END;						(*fsize*)

(* utility to free a header/trailer *)

PROCEDURE FREEHDRTRLR(VAR P:PSTRGDESC);
VAR PP: PSTRGDESC;

BEGIN
  WHILE P<>NIL DO BEGIN
    PP:= P; P:= P^.NEXTSTRG;
    DISPOSE(PP)
  END
END (*freehdrtrlr*);


PROCEDURE DISCARD_PAGERANGE;
 VAR PR: RANGE;
 BEGIN
  WHILE PAGERANGE <> NIL DO BEGIN		(* delete all remaining items *)
    PR := PAGERANGE;
    PAGERANGE := PAGERANGE^.NEXT;
    DISPOSE (PR)
  END
 END;
$PAGE basic output primatives: writesp, writech, writen, writel, writeol

PROCEDURE WRITESP(V_COUNT:INTEGER);
CONST SPACE: CHTYPE := (CHR(#O40), []); VAR I: INTEGER;
VAR COUNT: INTEGER;
BEGIN
  COUNT:= V_COUNT;
  IF (CHCNT+COUNT)>MAXLINELEN THEN COUNT:= MAXLINELEN-COUNT;
  FOR I:= 1 TO COUNT DO BEGIN CHCNT:= CHCNT+1; OUTBUF[CHCNT]:= SPACE END
END;						(*writesp*)


PROCEDURE WRITECH(V_CH: CHTYPE);
LABEL 1;
CONST TAB = CHR(#O11); 
VAR CH: CHTYPE;

   (*NOTE: Spaces resulting from an under/overlined tab are converted to
      backslash, which works fine, so long as backslashes
      are not converted to something besides space. Thus, this
      is a kludge which almost always has the desired result. *)

VAR TABCHCNT: LINEPTR; TABPOSN: 1..MAXTABS;

BEGIN						(*writech*)
  CH:= V_CH;
  IF CH.VALUE = TAB THEN
  BEGIN
    IF ([UNDERLINE,OVERLINE] * CH.ATTR) <> [] THEN CH.VALUE:= '\'
    ELSE CH.VALUE:= ' ';
    IF CHCNT<MAXLINELEN THEN
      BEGIN CHCNT:= CHCNT+1; OUTBUF[CHCNT]:= CH END;
    (*tabbing relative to full margin, i.e. discount all leading spaces*)
    TABCHCNT:= CHCNT - (CMARGIN+CLINDENT+PINDENT);
    FOR TABPOSN:= 1 TO CURTABCNT DO
      IF TABCHCNT<CURTABS[TABPOSN] THEN
      BEGIN
	WHILE (CURTABS[TABPOSN]-TABCHCNT)>1 DO
	BEGIN
	  IF CHCNT<MAXLINELEN THEN
	    BEGIN CHCNT:= CHCNT+1; OUTBUF[CHCNT]:= CH END;
	  TABCHCNT:= TABCHCNT+1
	END;
	GOTO 1
      END
  END
  ELSE
    BEGIN CHCNT:= CHCNT+1; OUTBUF[CHCNT]:= CH END;
1:
  END;						(*writech*)




PROCEDURE WRITEN(V_NUM:INTEGER; V_FSIZE:INTEGER);
CONST SPACE: CHTYPE := (CHR(#O40), []); VAR DIGIT,PSIZE: INTEGER;
VAR NUM, FSIZE: INTEGER;
BEGIN
  NUM:= V_NUM;
  FSIZE:= V_FSIZE;
  IF (CHCNT+FSIZE)>MAXLINELEN THEN FSIZE:= MAXLINELEN-CHCNT;
  PSIZE:= FSIZE;
  REPEAT					(*take advantage of call by value*)
    DIGIT:= NUM MOD 10;
    NUM:= NUM DIV 10;
    OUTBUF[CHCNT+FSIZE].VALUE := CHR(DIGIT + ORD('0'));
    OUTBUF[CHCNT+FSIZE].ATTR := [];
    FSIZE:= FSIZE-1
  UNTIL (NUM=0) OR (FSIZE<=0);
  WHILE FSIZE>0 DO
  BEGIN
    OUTBUF[CHCNT+FSIZE]:= SPACE; FSIZE:= FSIZE-1
  END;
  CHCNT:= CHCNT+PSIZE
END;						(*writen*)


PROCEDURE WRITEL(VAR OLINE:LINE; OLEN:LINEPTR);
CONST TAB = CHR(#O11);
VAR I: LINEPTR;
BEGIN
  FOR I:= 1 TO OLEN DO
    IF OLINE[I].VALUE <> TAB THEN BEGIN (* DONE IN-LINE FOR SPEED *)
      IF CHCNT<MAXLINELEN THEN
	BEGIN CHCNT:= CHCNT+1; OUTBUF[CHCNT]:= OLINE[I] END
    END
    ELSE WRITECH(OLINE[I])
END;						(*writel*)


PROCEDURE ADD_REVIS_BARS(VAR OUTBUF : LINE); (* ADD REVISION BARS IN MARGIN *)
VAR
    POS : INTEGER;

BEGIN
IF CMARGIN > 0 THEN BEGIN
  POS := MAX(1,CMARGIN DIV 2);
  OUTBUF[POS].VALUE := '|';
END;
END;
 
PROCEDURE WRITEOL;
  VAR
    I: LINEPTR;
    J: INTEGER;
BEGIN
  IF INRANGE THEN 
   BEGIN
     FOR J := 1 TO CHCNT DO 
       EXIT IF REVISION IN OUTBUF[J].ATTR DO
         ADD_REVIS_BARS(OUTBUF);
     WRTLINE(OUTPUT, OUTBUF, CHCNT, DO_UNDER, BACKSPACE, MAP, TERMINAL); WRITELN(OUTPUT);
     IF PAUSE THEN BREAK(OUTPUT)		(*to force page to be completely printed*)
   END;
  IF CHCNT>CWIDTH THEN ERROR(2);
  CHCNT:= 0
END;						(*writeol*)
$PAGE subordinate routines for pagination primatives 

PROCEDURE DONUM(NUM:INTEGER; JUST:JUSTYPE);	(*print page number line*)
VAR NSIZE: INTEGER;

BEGIN
  NSIZE:= FSIZE(NUM);
  CASE JUST OF
    JUSRIGHT: BEGIN
      IF (CWIDTH-CMARGIN-NSIZE)<0 THEN ERROR(5);
      WRITESP(CWIDTH-NSIZE);
      WRITEN(NUM,NSIZE)
    END;
    JUSLEFT: BEGIN
      IF (CWIDTH-CMARGIN-NSIZE)<0 THEN ERROR(5);
      WRITEN(NUM,NSIZE)
    END;
    JUSCENTER: BEGIN
      IF (CWIDTH-CMARGIN-NSIZE-4)<0 THEN ERROR(5);
      WRITESP(CMARGIN+(CWIDTH-CMARGIN-NSIZE-4) DIV 2);
      WRITECH(DASH); WRITESP(1);
      WRITEN(NUM,NSIZE);
      WRITESP(1); WRITECH(DASH)
    END
  END (*case*);
  WRITEOL
END (*donum*);


PROCEDURE NEW_JUST;				(* calculates new num just *)

  BEGIN
    WITH CNUMBER DO
    BEGIN
      IF ALTRNT THEN				(* numbers to appear on alternate sides *)
	CASE JUST OF

	  JUSRIGHT, JUSCENTER:
	  IF (PAGECNT MOD 2 ) = 0 THEN
	    NJUST := JUSLEFT
	  ELSE 
	    NJUST := JUSRIGHT;

	  JUSLEFT:
	  IF (PAGECNT MOD 2) = 0 THEN
	    NJUST := JUSRIGHT
	  ELSE
	    NJUST := JUSLEFT
	END					(* OF CASE *)
      ELSE
       NJUST := JUST
   END						(* OF WITH *)
   END;						(* OF NEW_JUST *)
 FUNCTION BACKSLASH(CH: CHTYPE): BOOLEAN;
   BEGIN
     BACKSLASH := CH.VALUE = '\'
   END;						(* backslash *)

PROCEDURE WRTHDRTXT(HDR:STRGDESC; NUM:INTEGER);	(*write title/foot line*)
VAR I:LINEPTR;

BEGIN
  WITH HDR DO
    FOR I:= 1 TO STRLEN DO
      IF BACKSLASH(STRVAL[I]) THEN WRITEN(NUM,FSIZE(NUM))
      ELSE WRITECH(STRVAL[I])
END (*wrthdrtxt*);

FUNCTION HDRLEN(HDR:STRGDESC; NUM:INTEGER): LINEPTR;	(*len(title/foot) line*)
VAR I,J: LINEPTR;

BEGIN
  WITH HDR DO BEGIN
    J:= STRLEN;
    FOR I:= 1 TO STRLEN DO
      IF BACKSLASH(STRVAL[I]) THEN J:= J+FSIZE(NUM)-1
  END;
  HDRLEN:= J
END (*hdrlen*);

PROCEDURE DOHDR(HDR:STRGDESC; NUM:INTEGER);	(*write just. tit/foot line*)
VAR I: LINEPTR;

BEGIN
  IF HDRLEN(HDR,NUM)>(CWIDTH-CMARGIN) THEN ERROR(5);
  CASE HDR.STRGJUST OF
    JUSRIGHT: WRITESP(CWIDTH-HDRLEN(HDR,NUM));
    JUSLEFT: WRITESP(CMARGIN);
    JUSCENTER: WRITESP(CMARGIN+(CWIDTH-CMARGIN-HDRLEN(HDR,NUM)) DIV 2)
  END (*case*);
  WRTHDRTXT(HDR,NUM);
  WRITEOL
END (*dohdr*);

PROCEDURE DOHDRNUM(HDR:STRGDESC; NUM:INTEGER; NUMJUST:JUSTYPE);	(*both*)
VAR NSIZE: INTEGER; HSIZE, FILLSIZE: LINEPTR;
TEMPJUST: JUSTYPE;

BEGIN						(*only called with numjust= right or left.  if hdr justfied center,
						  is justified on side opposite number*)
  NSIZE:= FSIZE(NUM); HSIZE:= HDRLEN(HDR,NUM);
  TEMPJUST := HDR.STRGJUST;

  (*blank fill count, position determined by justification*)
  FILLSIZE:= CWIDTH-CMARGIN-HSIZE-NSIZE;

  IF (HSIZE+NSIZE+1)>(CWIDTH-CMARGIN) THEN ERROR(5);
  WITH HDR DO BEGIN
    IF TEMPJUST=JUSCENTER THEN
      IF NUMJUST=JUSLEFT THEN TEMPJUST:= JUSRIGHT
      ELSE TEMPJUST:= JUSLEFT;
    WRITESP(CMARGIN);
    IF NUMJUST=JUSLEFT THEN BEGIN
      WRITEN(NUM,NSIZE);
      IF TEMPJUST=JUSLEFT THEN WRITESP(1)
      ELSE (*hdr right*) WRITESP(FILLSIZE);
      WRTHDRTXT(HDR,NUM)
    END
    ELSE BEGIN					(*number right*)
      IF TEMPJUST=JUSLEFT THEN BEGIN
	WRTHDRTXT(HDR,NUM);
	WRITESP(FILLSIZE)
      END
      ELSE BEGIN
	WRITESP(FILLSIZE-1);
	WRTHDRTXT(HDR,NUM);
	WRITESP(1)
      END;
      WRITEN(NUM,NSIZE)
    END;
    WRITEOL
  END						(*with*)
END (*dohdrnum*);

FUNCTION HDRCNT(P:PSTRGDESC): INTEGER;
BEGIN
  IF P=NIL THEN HDRCNT:= 0
  ELSE HDRCNT:= 1+HDRCNT(P^.NEXTSTRG)
END;


$PAGE start of run_scribe

PROCEDURE RUN_SCRIBE;

  LABEL 100;					(* abort on exhaustion of pagerange *)
  VAR 
    TEMPSPACES: INTEGER;
$PAGE dotop
PROCEDURE DOTOP;
VAR I,AFTER,MIDDLE: INTEGER; P: PSTRGDESC;
BEGIN

  (*update pagination parameters*)
  CTOP:= NTOP;
  CBOTTOM:= NBOTTOM; CLENGTH:= NLENGTH;
  IF NTITLE <> CTITLE THEN BEGIN
    FREEHDRTRLR(CTITLE); CTITLE:= NTITLE; 
  END;
  IF NFOOT <> CFOOT THEN BEGIN
    FREEHDRTRLR(CFOOT); CFOOT:= NFOOT; 
  END;
  CNUMBER:= NNUMBER; NNUMBER.NUM:= NNUMBER.NUM+1;

  PAGECNT := PAGECNT+1;				(* increment page counter *)
  IF PAGECNT > PAGERANGE^.LAST THEN BEGIN	(* exhausted range *)
    NPR := PAGERANGE;
    PAGERANGE := PAGERANGE^.NEXT;
    DISPOSE (NPR);
    IF PAGERANGE = NIL THEN BEGIN		(* abort early *)
      DONE := TRUE;
      ATTOP := FALSE;
      GOTO 100;					(* exit to end of run_scribe *)
    END
  END;

  IF (CLENGTH>0) AND (CLENGTH<CTOP+CBOTTOM+2) THEN
  BEGIN
    ERROR(1);
    CLENGTH:= CTOP+CBOTTOM+2
  END;

  IF INRANGE AND PAUSE
    THEN READLN (TTY);				(* do pause *)

  (*write line of dashes if specified*)
  IF DASHES THEN FOR I:= 1 TO CWIDTH DO WRITECH(DASH);
  WRITEOL;					(*in any case, blank line*)


  NEW_JUST;
  MIDDLE:= HDRCNT(CTITLE);
  IF CNUMBER.WHERE=NUMTOP THEN
    IF NJUST=JUSCENTER THEN MIDDLE:= MIDDLE+1;
  IF MIDDLE>0 THEN				(*check required room*)
    IF(MIDDLE+2)>CTOP THEN
      BEGIN ERROR(3); MIDDLE:= 0 END;

  AFTER:= (CTOP-MIDDLE) DIV 2;

  (*print blank lines before header ... if odd number
    of lines, extra line goes here*)
  FOR I:= 1 TO CTOP-AFTER-MIDDLE DO WRITEOL;

  (*print header*)
  IF MIDDLE>0 THEN
    (*first line involves number/title interaction*)
    IF HDRCNT(CTITLE)=0 THEN
      WITH CNUMBER DO DONUM(NUM,NJUST)
    ELSE BEGIN
      WITH CNUMBER DO IF WHERE=NUMTOP THEN
	IF NJUST=JUSCENTER THEN BEGIN
	  DONUM(NUM,JUSCENTER);
	  DOHDR(CTITLE^,NUM)
	END
	ELSE DOHDRNUM(CTITLE^,NUM,NJUST)
      ELSE DOHDR(CTITLE^,NUM);
      P:= CTITLE^.NEXTSTRG;
      WHILE P<>NIL DO BEGIN
	DOHDR(P^,CNUMBER.NUM); P:= P^.NEXTSTRG
      END
    END;					(*header case*)

  (*print blank lines after header*)
  FOR I:= 1 TO AFTER DO WRITEOL;
  LINECNT:= CTOP+1;
  ATTOP:= FALSE
END;						(*dotop*)
$PAGE dopage
PROCEDURE DOPAGE;
VAR I,BEFORE,MIDDLE,PAD: INTEGER; P: PSTRGDESC;
BEGIN
  IF NOT ATTOP THEN				(*do not page twice in a row*)
  BEGIN
    IF CLENGTH>0 THEN PAD:= CLENGTH-LINECNT-CBOTTOM (*to fill out page*)
    ELSE PAD:= 0;
    MIDDLE:= HDRCNT(CFOOT);
    IF CNUMBER.WHERE = NUMBOT THEN
      IF NJUST=JUSCENTER THEN MIDDLE:= MIDDLE+1;
    IF MIDDLE>0 THEN
      IF (MIDDLE+2)>CBOTTOM THEN
	BEGIN ERROR(4); MIDDLE:= 0 END;
    IF MIDDLE>0 THEN WITH CNUMBER DO
    BEGIN
      BEFORE:= (CBOTTOM-MIDDLE) DIV 2;		(*extra line after*)
      FOR I:= 1 TO BEFORE+PAD DO WRITEOL;
      (*put out trailer*)
      IF HDRCNT(CFOOT)=0 THEN DONUM(NUM,NJUST)
      ELSE BEGIN
	P:= CFOOT;
	WHILE P^.NEXTSTRG<>NIL DO BEGIN
	  DOHDR(P^,NUM); P:= P^.NEXTSTRG
	END;
	IF WHERE=NUMBOT THEN
	  IF NJUST=JUSCENTER THEN BEGIN
	    DOHDR(P^,NUM);
	    DONUM(NUM,JUSCENTER)
	  END
	  ELSE DOHDRNUM(P^,NUM,NJUST)
	ELSE DOHDR(P^,NUM)
      END;
      IF NOT CC THEN
	FOR I:= 1 TO CBOTTOM-MIDDLE-BEFORE DO WRITEOL
      ELSE IF INRANGE THEN WRITE(OUTPUT, CHR(#O14))
    END						(*middle>0*)
    ELSE IF NOT CC THEN FOR I:=1 TO PAD+CBOTTOM DO WRITEOL
    ELSE IF INRANGE THEN WRITE(OUTPUT,CHR(#O14));
    ATTOP:= TRUE
  END
END;						(*dopage*)

PROCEDURE WRITENL;
BEGIN
  IF NOT EOF_INF THEN
    BEGIN
      LINECNT:= LINECNT+1; WRITEOL;
      IF (LINECNT>=CLENGTH-CBOTTOM) AND (CLENGTH>0) THEN 
      BEGIN
	PAGE_TOP := FALSE;
	DOPAGE
      END
    END
  ELSE EOF_INF := FALSE

END  (* WRITENL *);
$PAGE getcmd
$INCLUDE getcmd.pas
$PAGE run_scribe mainline
 BEGIN;
    IF ADVANCE THEN WRITE(OUTPUT,CHR(#O14));
    WHILE NOT DONE DO
    BEGIN
      TEMPSPACES := CWIDTH - CMARGIN - CLINDENT - CRINDENT - PINDENT;
      IF TEMPSPACES < 0 THEN
	TEMPSPACES := 0;
      IF GETLINE(INF [INF_DEPTH], TEMPSPACES,
      CURSTATE,OUTLINE,OUTLEN,CMDLN,CMDLEN) THEN
      BEGIN					(*got an input line to write*)
	IF ATTOP THEN DOTOP;
	IF OUTLEN>0 THEN
	BEGIN
	  I:= CMARGIN+CLINDENT+PINDENT;
	  IF PRINTLNUM THEN
	  BEGIN GETLNR(LNUM,LCNT);
	    IF I>FSIZE(LCNT) THEN
	    BEGIN WRITEN(LCNT,FSIZE(LCNT));
	      I:= I-FSIZE(LCNT)
	    END
	  END;
	  WRITESP(I);
	  WRITEL(OUTLINE,OUTLEN)
	END;
	WRITENL;
	FOR I:= 1 TO CSPACING DO IF NOT ATTOP THEN WRITENL;
	PINDENT:= 0				(*cancel paragraph indentation*)
      END;
      IF CMDLEN>0 THEN GETCMD;
      IF ATTOP THEN				(*apply commands affecting justification now*)
	BEGIN CWIDTH:= NWIDTH; CMARGIN:= NMARGIN END
    END;					(*the loop*)
    DOPAGE;					(*finish the job*)
    IF DASHES THEN FOR I:= 1 TO CWIDTH DO WRITECH(DASH);
100 (* page range exhausted *) :
    WRITEOL;
 END;						(* run_scribe *)
$PAGE do_scribe - execute a command line
PUBLIC PROCEDURE DO_SCRIBE (LINE: CMDLINE; ECHO_CMD: BOOLEAN);
 VAR
   LINDEX: CMDLINEIDX;				(* parsing cursor *)
   INPFILE, OUTFILE: FILE_ID;			(* input and output file names *)
   OPT: CHAR;					(* option name *)
   ENTRY_NAME: FILE_NAME;			(* TOC FILE NAME *)


  (* syntactic error processor - prints message and aborts *)

  LABEL 100 (* Error abort *) ;			(* to epilogue of this procedure *)
  TYPE ERRMSG = STRING [24];

  PROCEDURE ABORT (MSG: ERRMSG);
   BEGIN
    IF ECHO_CMD THEN WRITELN (TTY, '[', LINE, ']');
    WRITELN (TTY, '? ', MSG); BREAK;			(* output message *)
    GOTO 100;					(* abort processing *)
   END;


  (* simple parsing utilities *)

  PROCEDURE SKIPBLANKS;
   BEGIN
    WHILE (LINDEX <= LENGTH (LINE)) ANDIF (ORD (LINE[LINDEX]) <= ORD (' '))
      DO LINDEX := LINDEX + 1
   END;

  FUNCTION CHECKEOL: BOOLEAN;
   BEGIN
    SKIPBLANKS;
    CHECKEOL := (LINDEX > LENGTH (LINE))
   END;

  FUNCTION CHECKPUNCT (CH: CHAR): BOOLEAN;
   BEGIN
    SKIPBLANKS;
    IF (LINDEX <= LENGTH (LINE)) ANDIF (CH = LINE[LINDEX])
      THEN BEGIN
	CHECKPUNCT := TRUE;
	LINDEX := LINDEX + 1
      END
    ELSE CHECKPUNCT := FALSE
   END;

  FUNCTION FILE_PARAMETER (VAR FID: FILE_ID): BOOLEAN;
   VAR L: CMDLINEIDX;
   BEGIN
    FID := '';
    FILE_PARAMETER := FALSE;
    SKIPBLANKS;
    IF LINDEX > LENGTH (LINE) THEN RETURN;	(* nothing on line - don't check for semicolon since
						   on some systems, semi is part of filename *)
    L := LINDEX;
    IF PR_FILE_ID (LINE, LINDEX, FID)		(* see if file here *)
      THEN FILE_PARAMETER := TRUE
      ELSE IF L = LINDEX			(* bad file, if cursor ... *)
	THEN FILE_PARAMETER := FALSE		(* ... didn't move, no filename there *)
	ELSE FILE_PARAMETER := FALSE		(* ... did move, error *)
   END;

  FUNCTION NUMBER (VAR NUM: INTEGER): BOOLEAN;
   VAR LNUM: INTEGER;
   BEGIN
    NUMBER := FALSE;				(* assume that no number will be found *)
    SKIPBLANKS;
    LNUM := 0;
    WHILE (LINDEX <= LENGTH (LINE)) ANDIF (LINE[LINDEX] IN ['0'..'9']) DO BEGIN
      LNUM := (LNUM * 10) + (ORD (LINE[LINDEX]) - ORD('0'));
      LINDEX := LINDEX + 1;
      NUMBER := TRUE
    END;
    IF LINDEX <= LENGTH (LINE)			(* must be followed by a non-letter *)
      THEN NUMBER := NUMBER OR (NOT (UPPERCASE (LINE[LINDEX]) IN ['A'..'Z']));
    IF NUMBER THEN NUM := LNUM			(* set only if number actually found *)
   END;

FUNCTION OPTION(VAR OPT : STRING[30]):BOOLEAN;
  VAR L : INTEGER;
  BEGIN
  OPTION := FALSE;
  IF CHECKEOL THEN RETURN;
  SKIPBLANKS;
  L := VERIFY(UPPERCASE(SUBSTR(LINE,LINDEX)),['A'..'Z'],LENGTH(LINE)-LINDEX+2);
  OPT := SUBSTR(LINE,LINDEX,L-1);
  LINDEX := LINDEX + L - 1;
  OPTION := TRUE;
  END;


  (* page range option processing *)

  PROCEDURE SET_PAGES (F, L: INTEGER);		(* creates a pagerange node *)
   VAR LPR,NPR: RANGE;
   BEGIN
    LPR := PAGERANGE;				(* find end of list *)
    IF LPR <> NIL THEN BEGIN
      WHILE LPR^.NEXT <> NIL DO LPR := LPR^.NEXT
    END;

    NEW (NPR);					(* create node *)
    WITH NPR^ DO BEGIN
      FIRST := F; LAST := L;                    (* copy range limits *);
      NEXT := NIL;
    END;
    IF LPR = NIL				(* thread onto list *)
      THEN PAGERANGE := NPR			(* first on list *)
      ELSE LPR^.NEXT := NPR;

    (* Now do semantic checks on the range. Note that this is done after
       threading the node on the list, so that the node is correctly 
       disposed in case of an error abort. *)

    IF (LPR <> NIL) ANDIF (F <= LPR^.FIRST)	(* ranges mustn ascending order *)
      THEN ABORT ('Page number out of order.');
    IF F > L THEN ABORT ('Page range out of order.');
   END;

  PROCEDURE T_OPTION;

    VAR
      TERM: STRING[255];

    BEGIN
      IF NOT CHECKPUNCT(':') THEN ABORT('":" expected.');
      SKIPBLANKS;
      TERM := '';
      WHILE (LINDEX <= LENGTH(LINE)) ANDIF (UPPERCASE(LINE[LINDEX]) IN ['A'..'Z']) DO
      BEGIN
	TERM := TERM || UPPERCASE(LINE[LINDEX]);
	LINDEX := LINDEX + 1
      END;
      IF TERM = 'DIABLO' THEN
	TERMINAL := DIABLO
      ELSE IF TERM = 'XEROX' THEN
	TERMINAL := XEROX
      ELSE
	ABORT('Illegal terminal type.')
  END;

PROCEDURE R_OPTION;
 VAR N: INTEGER;
 BEGIN
  IF NOT CHECKPUNCT (':') THEN ABORT ('":" expected.');
  IF NOT NUMBER (N) THEN ABORT ('Number expected.');

  IF N<1 THEN ABORT ('Repeat count too small.')
  ELSE IF N>8 THEN ABORT ('Repeat count too large.')
       ELSE REPEAT_CNT := N
 END;

PROCEDURE S_OPTION;
 VAR N: INTEGER;
 BEGIN
  IF NOT CHECKPUNCT (':') THEN ABORT ('":" expected.');
  IF NOT NUMBER (N) THEN ABORT ('Page number expected.');
  SET_PAGES (N, MAXIMUM (PAGECNT));
 END;

PROCEDURE O_OPTION;
 VAR N1, N2: INTEGER;
     L: CMDLINEIDX;
 BEGIN
  IF NOT CHECKPUNCT (':') THEN ABORT ('":" expected.');
  IF NOT NUMBER (N1) THEN ABORT ('Number expected.');
  REPEAT
    IF CHECKPUNCT ('-') THEN BEGIN		(* f-l format *)
      IF CHECKPUNCT ('*')			(* to end of file *)
	THEN N2 := MAXIMUM (PAGECNT)
      ELSE IF NOT NUMBER (N2)
	THEN ABORT ('Number or "*" expected.')
    END
    ELSE N2 := N1;				(* single page format *)
    SET_PAGES (N1, N2);				(* setup range *)
    L := LINDEX;				(* must look for ", number"; prepare to
						   backup if not found *)
  UNTIL NOT (CHECKPUNCT (',') ANDIF (NUMBER (N1)));
  LINDEX := L;					(* reset scan to the ',' *)
 END;

PROCEDURE RUNOFFSET_OPTION;
  VAR N : INTEGER;
  BEGIN
  IF NOT CHECKPUNCT(':') THEN
    RUNOFF_VAL := 1
  ELSE
    IF (NOT NUMBER(N)) THEN
      ABORT('Number expected')
    ELSE
      RUNOFF_VAL := N;
  END;


PROCEDURE RUN_OPTION;
  VAR 
    FBEGIN : INTEGER;
    IN_PPN : BOOLEAN;
  BEGIN
  IN_PPN := FALSE;
  IF (NOT CHECKPUNCT(':')) THEN ABORT('":" expected.');
  SKIPBLANKS;
  FBEGIN := LINDEX;
  WHILE (LINDEX <= LENGTH(LINE)) ANDIF
        (UPPERCASE(LINE[LINDEX]) IN ['A'..'Z','.','0'..'9','[',']',',']) DO BEGIN
    CASE LINE[LINDEX] OF
      '[': IN_PPN := TRUE;
      ']': IN_PPN := FALSE;
      OTHERS:  ;
    END (* CASE *);
    EXIT IF ((NOT IN_PPN) AND (LINE[LINDEX] = ','));
    LINDEX := LINDEX + 1;
  END  (* WHILE *);
  RUN_FLAG := TRUE;
  RUN_PGM_NAME := SUBSTR(LINE,FBEGIN,LINDEX-FBEGIN);
  END;


(************** do_scribe **************)

VAR  FFLAG : BOOLEAN;
BEGIN;
  INITSCRIBE; INITREADER; INITJUSTIFY;		(* init static storage *)
  FFLAG := FALSE;
  DODECAP := FALSE;				(* turn on default modes *)
  DOUNDER := TRUE;
  UNDER_STATE := [];
  INIT_TABLE;					(* initialize translate table *)

  LINDEX := 1;					(* parse from the beginning *)
  IF (LENGTH(LINE) > 0) ANDIF (LINE[LINDEX] <> '/') THEN BEGIN
    IF NOT FILE_PARAMETER (OUTFILE)
      THEN ABORT ('File expected.');
    IF CHECKPUNCT ('=') THEN BEGIN		(* file = file format *)
      IF NOT FILE_PARAMETER (INPFILE)
	THEN ABORT ('Input file expected.');
    END
    ELSE BEGIN					(* file format - tty = file implied *)
      INPFILE := OUTFILE;
      OUTFILE := '';   (* null outfile => output to tty *)
    END;
    FFLAG := TRUE;
  END;

  IF CHECKPUNCT ('/') THEN BEGIN		(* process option list *)
    LINDEX := LINDEX - 1;			(* process / as part of first option *)
    WHILE CHECKPUNCT ('/') ORIF CHECKPUNCT (',') DO BEGIN
      IF NOT OPTION (OPT_NAME)			(* no option name specified *)
	THEN ABORT ('Option name expected.');

      CASE UPPERCASE (OPT_NAME[1]) OF
	'S': S_OPTION;
	'O': O_OPTION;
	'D': DASHES := TRUE;
	'P': PAUSE := TRUE;
	'N': DO_UNDER := FALSE;
	'B': BACKSPACE := TRUE;
	'F': ADVANCE := TRUE;
	'C': CC := TRUE;
	'L': PRINTLNUM := TRUE;
	'T': T_OPTION;
        'E': EXIT_FLAG := TRUE;
        'R': IF UPPERCASE(OPT_NAME)='R' THEN R_OPTION ELSE
             IF UPPERCASE(OPT_NAME)='RUN' THEN RUN_OPTION ELSE
             IF UPPERCASE(OPT_NAME)='RUNOFFSET' THEN RUNOFFSET_OPTION ELSE
             ABORT('Invalid_option.');
	OTHERS:
	     ABORT ('Invalid option.')
      END
    END (* while option *) ;
  END;

  IF NOT CHECKEOL				(* should be end of command *)
    THEN ABORT ('Invalid option.');

  IF NOT FFLAG THEN GOTO 100;  (* only process switches *)
  RESET (INF[1], '.scr ' || INPFILE, [ASCII]);		(* open input file *)
  INF_DEPTH := 1;
  IF EOF (INF [INF_DEPTH]) THEN ABORT ('Bad input file.');

  IF LENGTH(OUTFILE) > 0 THEN BEGIN   (* output file specified *)
    REWRITE (OUTPUT, '.txt ' || OUTFILE);		(* open output file *)
    IF NOT EOF (OUTPUT) THEN ABORT ('Cannot open output file.');
  END
  ELSE OUTPUT:= TTYOUTPUT;   (* if no output file specified, use tty *)

  IF PAGERANGE = NIL				(* if no output pages specified, use whole file *)
    THEN SET_PAGES (1, MAXIMUM (PAGECNT));

  RUN_SCRIBE;
  BREAK;   (* force output to complete if to tty *)
  IF OUTPUT <> TTYOUTPUT THEN CLOSE(OUTPUT);   (* shut down output file *)
  CLOSE(INF [INF_DEPTH]);  (* and input file (will cause error if tty specified) *)

100 (* Error abort *) :

  IF ENTRY_F <> NILF THEN BEGIN		(* CLEAN UP UNUSED TABLE OF CONTENTS *)
    ENTRY_NAME := FILENAME ( ENTRY_F );
    CLOSE (ENTRY_F);
    REWRITE (ENTRY_F, ENTRY_NAME );
    SCRATCH (ENTRY_F);
    ENTRY_F := NILF
  END;
  DISCARD_PAGERANGE;				(* throw away heap storage *)
  IF CFOOT <> NFOOT THEN
  BEGIN
    FREEHDRTRLR(CFOOT);
    FREEHDRTRLR(NFOOT)
  END
  ELSE
    FREEHDRTRLR(CFOOT);

  IF CTITLE <> NTITLE THEN
  BEGIN
    FREEHDRTRLR(CTITLE);
    FREEHDRTRLR(NTITLE)
  END
  ELSE
    FREEHDRTRLR(CTITLE)
END.
   mF	�