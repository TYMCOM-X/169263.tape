PROGRAM RTOBJ;

CONST
   MAX_DATA = 40;
   MIN_INT = MINIMUM(INTEGER);

TYPE
   BYTE = 0..377B;
   WORD_TYPE = 0..177777B;
   RECD = RECORD
      MODIFIED: BOOLEAN;
      ONE: BYTE;
      ZERO1: BYTE;
      LEN_LOW: BYTE;
      LEN_HIGH: BYTE;
      TYP: BYTE;
      ZERO2: BYTE;
      DATA: ARRAY [1..MAX_DATA] OF BYTE;
      CKSUM: BYTE
   END;
   INT = MIN_INT..MAXIMUM(INTEGER);
   POS_INT = 1..MAXIMUM(INTEGER);

VAR
   FNAME: STRING[50];
   CH: CHAR;
   RSXOBJ, RTOBJ: FILE OF INT;
   REC: RECD;
   ERROR: BOOLEAN;
   ID: PACKED ARRAY [1..6] OF CHAR;

$PAGE

PROCEDURE ERROR_MSG(ERR_NUM: POS_INT; I_VAL: INT);

   BEGIN
      ERROR := TRUE;
      WRITELN(TTY, 'FATAL ERROR: ', ERR_NUM:3, I_VAL:13:O, '(8)' )
   END;

$PAGE

FUNCTION WORD(LOW_BYTE, HIGH_BYTE: BYTE): WORD_TYPE;

   BEGIN
      WORD := HIGH_BYTE * 256 + LOW_BYTE
   END;


$PAGE

FUNCTION RX50 (C: BYTE): CHAR;

BEGIN
  CASE C OF
    0:
      RX50 := ' ';
    1..26:
      RX50 := CHR (C+ORD('A')-1);
    27:
      RX50 := '$';
    28:
      RX50 := '.';
    29:
      RX50 := '_';
    30..39:
      RX50 := CHR (C-30+ORD('0'))
  END;
END;

PUBLIC PROCEDURE R50(BASE: POS_INT);

VAR
    I: INT;
    FWORD: WORD_TYPE;

BEGIN
  FWORD := WORD(REC.DATA[BASE],REC.DATA[BASE+1]);
  FOR I := 3 DOWNTO 1 DO BEGIN
    ID[I] := RX50(FWORD MOD 50B);
    FWORD := FWORD DIV 50B;
  END;
  FWORD := WORD(REC.DATA[BASE+2],REC.DATA[BASE+3]);
  FOR I := 6 DOWNTO 4 DO BEGIN
    ID[I] := RX50(FWORD MOD 50B);
    FWORD := FWORD DIV 50B;
  END;
END;

$PAGE
FUNCTION CALC_CKSUM(REC: RECD): BYTE;

   VAR
      SUM,I: INT;

   BEGIN
      WITH REC DO
      BEGIN
         SUM := ONE + ZERO1 + LEN_LOW + LEN_HIGH +
            TYP + ZERO2;
         FOR I := 1 TO LEN_LOW - 6 DO
            SUM := SUM + DATA[I];
         CALC_CKSUM := (400B - SUM MOD 400B) MOD 400B
      END
   END;

$PAGE

PROCEDURE RD_RECORD(VAR ERROR: BOOLEAN; VAR REC: RECD);

   VAR
      LENGTH,I: 0..MAXIMUM(INTEGER);

   BEGIN
      WITH REC DO
      BEGIN
         ONE := RSXOBJ^;   GET(RSXOBJ);
         ZERO1 := RSXOBJ^;   GET(RSXOBJ);
         LEN_LOW := RSXOBJ^;   GET(RSXOBJ);
         LEN_HIGH := RSXOBJ^;   GET(RSXOBJ);
         TYP := RSXOBJ^;   GET(RSXOBJ);
         ZERO2 := RSXOBJ^;   GET(RSXOBJ);

         LENGTH := WORD(LEN_LOW,LEN_HIGH);
         IF LENGTH > MAX_DATA + 6 THEN
            ERROR_MSG(2,LENGTH)
         ELSE
      
         BEGIN
            FOR I := 1 TO LENGTH - 6 DO
            BEGIN
               DATA[I] := RSXOBJ^;
               GET(RSXOBJ)
            END;
            (* FOLLOWING IS DUE TO FULL WORD CKSUMS PUT OUT BY PAS11 *)
            CKSUM := (256 - ABS(RSXOBJ^)) MOD 256;   GET(RSXOBJ);
            IF CKSUM <> CALC_CKSUM(REC) THEN
               ERROR_MSG(3,CKSUM);
            GET(RSXOBJ)    (* HIGH BYTE OF CHECKSUM *)
         END

      END (* WITH *)

   END;

$PAGE

PROCEDURE WR_RECORD(VAR REC: RECD);

   VAR
      I: 1..MAX_DATA;

   BEGIN
      WITH REC DO
      BEGIN
         RTOBJ^ := ONE;       PUT(RTOBJ);
         RTOBJ^ := ZERO1;     PUT(RTOBJ);
         RTOBJ^ := LEN_LOW;   PUT(RTOBJ);
         RTOBJ^ := LEN_HIGH;  PUT(RTOBJ);
         RTOBJ^ := TYP;      PUT(RTOBJ);
         RTOBJ^ := ZERO2;     PUT(RTOBJ);

         FOR I := 1 TO LEN_LOW - 6 DO
         BEGIN
            RTOBJ^ := DATA[I];
            PUT(RTOBJ)
         END;

         RTOBJ^ := CKSUM;   PUT(RTOBJ);
         RTOBJ^ := 0;       PUT(RTOBJ)
      END  (* WITH *)

   END;

$PAGE

PROCEDURE REMOVE(VAR REC: RECD; START, LENGTH: POS_INT);

   VAR
      NUM_TO_MOVE: 0..MAXIMUM(INTEGER);
      I: 0..MAX_DATA;

   BEGIN
      WITH REC DO
      BEGIN
         NUM_TO_MOVE := (LEN_LOW - 6) - (START + LENGTH - 1);
         FOR I := 1 TO NUM_TO_MOVE DO
            DATA[START-1+I] := DATA[START+LENGTH-1+I];
         LEN_LOW := LEN_LOW - LENGTH
      END

   END;

$PAGE

PROCEDURE GSD(VAR ERROR: BOOLEAN; VAR REC: RECD);

   CONST
      GSD_ITEM_LEN = 8;
   
   VAR
      BASE: INT;
      FLAGS: BYTE;

   BEGIN
      WITH REC DO
      BEGIN
         BASE := 1;

         REPEAT
            CASE DATA[BASE+5] OF

            0,1,2,3:    ;

            4:  (* GLOBAL DEFN OR REF *)
               BEGIN
                  FLAGS := DATA[BASE + 4];
                  IF FLAGS MOD 200B < 100B THEN
                     ERROR_MSG(4, FLAGS);   (* BIT 6 NOT SET *)
                  (* IF $$HEAP THEN CHANGE FROM DEF TO REF *)
                  R50(BASE);
                  IF ID = '$$HEAP' THEN
                  BEGIN
                     DATA[BASE+4] := DATA[BASE+4] - 8;
                     MODIFIED := TRUE
                  END
               END;

            5:        (* PSECT DEFN *)
               BEGIN
                  R50(BASE);
                  IF ID = '999999' THEN
                  BEGIN
                     REMOVE(REC,BASE,GSD_ITEM_LEN);
                     BASE := BASE - GSD_ITEM_LEN;
                     MODIFIED := TRUE
                  END
               END;

            6:      ;		(* PROGRAM VERSION IDENT *)

            7:		(* MAPPED ARRAY *)
               ERROR_MSG(5, 0);

            OTHERS:
               ERROR_MSG(6, DATA[BASE + 5])

            END;   (* CASE *)

            BASE := BASE + GSD_ITEM_LEN

         UNTIL ERROR OR (BASE = LEN_LOW - 6 + 1)

      END  (* WITH  *)

   END;

$PAGE

PROCEDURE RLD(VAR ERROR: BOOLEAN; VAR REC: RECD);

   TYPE
      RLD_ITEM_TAB = ARRAY [1..16] OF 1..8;

   CONST
      LC_MOD_CODE := 8;
      RLD_ITEM_LEN: RLD_ITEM_TAB = (4, 6, 4, 6, 8, 8, 8, 
         4, 2, 6, 2, 6, 8, 8, 8, 4 );

   VAR
      BASE: INT;
      IDX: BYTE;

   BEGIN
      WITH REC DO
      BEGIN
         BASE := 1;

         REPEAT

            IDX := DATA[BASE];
            IF IDX >= 200B THEN IDX := IDX - 200B;

            CASE IDX OF

            1,2,3,4,5,6:	;

            7:		;	(* LOCN COUNTER DEFN *)

            8:     ;			(* LOCN COUNTER MODIFICATION *)

            9:	;

            10,11:
               ERROR_MSG(7,IDX);

            12,13,14:	;	(* SECTION RELOCATIONS *)

            15,16:
               ERROR_MSG(7, IDX);

            OTHERS:
               ERROR_MSG(8, IDX)

            END  (* CASE *) ;

            BASE := BASE + RLD_ITEM_LEN[IDX]

         UNTIL (BASE = LEN_LOW - 6 + 1) OR ERROR;

      END  (* WITH *)

   END;

$PAGE

(*
 *  BEGIN MAIN ROUTINE.
 *)

BEGIN
   REWRITE(TTY);
   OPEN(TTY);

   LOOP     (* 1 FILE PROCESSED PER ITERATION *)

      WRITELN(TTY);
      WRITE(TTY, 'FILE: ');
      BREAK(TTY);
      READLN(TTY);
   EXIT IF EOLN(TTY);
      FNAME := '';
      WHILE NOT EOLN(TTY) DO
      BEGIN
         READ(TTY,CH);
         FNAME := FNAME || CH
      END;
      RESET(RSXOBJ, '.OBJ ' || FNAME);
      IF EOF(RSXOBJ) THEN
         WRITELN(TTY, '?CAN''T OPEN FILE ', FNAME)
      ELSE

      BEGIN
         REWRITE(RTOBJ, FNAME || '.RT1[,]');

         REPEAT   (* 1 OBJECT RECORD PROCESSED PER ITERATION *)
         
            RD_RECORD(ERROR,REC);
            IF NOT ERROR THEN
            BEGIN
               REC.MODIFIED := FALSE;

               CASE REC.TYP OF
     
                  1:	(* GSD *)
                     GSD(ERROR,REC);

                  2:	(* ENDGSD *) ;
                
                  3:	(* TXT *)  ;

                  4:	(* RLD *)
                     RLD(ERROR,REC);

                  5:	(* ISD *) ;

                  6:	(* ENDMOD *) ;

                  OTHERS:
                     ERROR_MSG(1, REC.TYP)

               END   (* CASE  *) ;

               IF REC.MODIFIED THEN REC.CKSUM := CALC_CKSUM(REC);
               IF (REC.LEN_LOW > 6) ORIF
                  (REC.TYP = 2) AND (REC.LEN_LOW = 6) ORIF
                  (REC.TYP = 6) AND (REC.LEN_LOW = 6) THEN
                  WR_RECORD(REC)


            END (* IF NOT ERROR *)

         UNTIL EOF(RSXOBJ) OR ERROR;

         CLOSE(RTOBJ)

      END  (* IF EOF  *) 

   END (* LOOP *)

END.
  