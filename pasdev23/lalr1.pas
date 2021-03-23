$TITLE LALR1: LALR(1) Parser Generator
  
  
(*      LALR1 - LALR(1) Parser Generator
  
        Ralph D. Jeffords, University of Mississippi
        Version 2 (distribution version March 1979)
  
        *NOTE - Prior knowledge about generation of LR parsers is assumed.  It is simply not practical to
                incorporate all of the necessary explanation in the comments.  A reader not already
                familiar with the subject should consult the excellent text:  Principles of Compiler
                Design, Aho & Ullman (1977)  (commonly refered to as the "dragon book" due to its cover
                illustrations).
  
  
        Modifications by Dave Wilson:
  
  
                                                                                                   *)
$PAGE Constant and Type Definitions
PROGRAM LALR1;
  
CONST
     MAXSYMLEN  =   30;   (* THE MAXIMUM LENGTH OF A SYMBOL *)
     STARTSYM   =    1;   (* CODE FOR TRUE START SYMBOL, HEREAFTER CALLED S *)
     SPRIME     =    0;   (* CODE FOR S', THE NEW START SYMBOL OF THE AUGMENTED GRAMMAR *)
     FIRST_ITEM =    1;   (* CODE FOR ITEM S' --> .S *)
     SECOND_ITEM=    2;   (* CODE FOR ITEM S' --> S. *)
     REGITEM    =    3;   (* CODE FIRST ITEM AFTER S' --> .S AND S' --> S. *)
     LAMBDA     =    0;   (* CODE FOR THE NULL STRING *)
     INITSTATE  =    1;   (* CODE FOR 1ST LR0 STATE *)
     ZEROPROD   =   -1;   (* CODE FOR AUGMENTING PRODUCTION S' --> S *)
     BUFFERLEN  =  135;   (* LENGTH OF INPUT BUFFER *)
     WORD_SIZE  =   36;   (* WORD SIZE IN BITS *)
  
     (* NO.'S ON RT. ARE REQUIRED FOR (SUBPASCAL,XPL) *)
  
     MAXSETS    =  500;   (* (127,185) MAX NUMBER OF LR0 SETS *)
     MAXNONTER  =   127;   (* (24,49) MAX NUMBER OF NONTERMINALS *)
     MIN_TERM   =  -71;   (* (33,42) MINIMUM CODE FOR TERMINALS *)
     MAXITEMS   =  720;   (* (196,338) MAX NUMBER OF LR(0) ITEMS SHOULD BE A MULTIPLE OF WORD_SIZE *)
     MIN_PROD   = -499;   (* (-54,-110) MIN CODE FOR PRODUCTIONS *)
     MAXCOLS    =   24;   (* MAX NUMBER OF COLUMNS OF PARSE/GOTO PRINTED/PAGE*)
     MAXBACK    =  100;   (* MAX NUMBER OF ENTRIES USED IN COMPUTING LALR1 LOOKAHEAD *)
     LR0REDUCE  = -500;   (* ADDEND INDICATING ELIMINATED LR0 REDUCE ROW *)
     MINCODE    = -999;   (* MUST BE LR0REDUCE + MIN_PRODS *)
  
  
TYPE
     TERRANGE   = MIN_TERM..LAMBDA;     (* TERMINAL CODE RANGE *)
     NONTERRANGE= SPRIME..MAXNONTER;    (* NONTERMINAL CODE RANGE *)
     ITEMRANGE  = FIRST_ITEM..MAXITEMS; (* LR(0) ITEM INDEX RANGE *)
     ALPHARANGE = MIN_TERM..MAXNONTER;  (* TERMINALS<0, LAMBDA(NULL) =0, 0<NONTERMINALS (S' NOT INCL.) *)
     PRODNORANGE= MIN_PROD..ZEROPROD;   (* PRODUCTION NUMBER CODE RANGE *)
  
     LR0SET     = PACKED ARRAY[ITEMRANGE] OF BOOLEAN;
     STATE      = 0..MAXSETS;
     SYMSET     = PACKED ARRAY[TERRANGE] OF BOOLEAN;
     RESTYPE    = (NORES, SELF, NOSELF);(* NO RESOLUTION OF CONFLICTS; SELF-PRECEDENCE; NO SELF-PREC. *)
  
     EXITEMRANGE= 0..MAXITEMS;          (* 0 UNION ITEMRANGE *)
$PAGE Variable Declarations
VAR
     LAST_ITEM: ITEMRANGE; (* TOTAL NUMBER OF LR(0) ITEMS *)
     PRODS: ARRAY[ITEMRANGE] OF
               RECORD                   (* IF ITEM IS [A --> ALPHA.X BETA] THEN... *)
                  LHS: NONTERRANGE;         (* SYMBOL FOR A *)
                  DOTSYMBOL: ALPHARANGE;    (* SYMBOL FOR X *)
                  RHSLENGTH: EXITEMRANGE;   (* LENGTH OF ALPHA X BETA *)
                  STARTRHS: ITEMRANGE;      (* INDEX OF [A --> .ALPHA X BETA] *)
                  PRODNO: PRODNORANGE;      (* PRODUCTION NUMBER OF A --> ALPHA X BETA *)
                  RESOLVE: RESTYPE          (* AUTOMATIC AMBIGUITY RESOLUTION *)
               END;
  
     LAST_TERM: TERRANGE; 
     TERM_TAB: ARRAY[TERRANGE] OF
                  RECORD
                     LENGTH : 0..MAXSYMLEN;                    (* LENGTH OF SYMBOL *)
                     SYM: PACKED ARRAY [1..MAXSYMLEN] OF CHAR  (* THE SYMBOL *)
                  END;
  
     LAST_NONTERM: 0..MAXNONTER; 
     NONTERM_TAB: ARRAY[0..MAXNONTER] OF
                     RECORD
                        LENGTH : 0..MAXSYMLEN;                    (* LENGTH OF SYMBOL *)
                        SYM: PACKED ARRAY [1..MAXSYMLEN] OF CHAR  (* THE SYMBOL *)
                     END;
  
     LCHAIN: ARRAY[NONTERRANGE] OF EXITEMRANGE;   (* LCHAIN[A] IS INDEX OF 1ST ITEM WITH LHS A *)
  
     EACHGOTO: ARRAY[ALPHARANGE] OF LR0SET;       (* EACHGOTO[X] IS SET OF ALL ITEMS WITH X IMMEDIATELY
                                                         BEFORE THE DOT *)
     CLOSURE: ARRAY[NONTERRANGE] OF LR0SET;       (* CLOSURE[B] = CLOSURE([A --> ALPHA . B BETA])
                                                         - [A --> ALPHA . B BETA] (DRAGON BOOK, PG 207) *)
     FIRST, FOLLOW: ARRAY[NONTERRANGE] OF SYMSET; (* THE STANDARD FIRST AND FOLLOW FUNCTIONS *)
  
     LAST_LR0_STATE: STATE;                       (* TOTAL NUMBER OF LR(0) STATES GENERATED *)
     LR0COLLECTION: ARRAY [STATE] OF LR0SET;      (* COLLECTION OF THOSE LR(0) STATES *)
   
     GOTOO: ARRAY[STATE,ALPHARANGE] OF
                              MIN_PROD..MAXSETS;  (* PARSING TABLE: COMBINED ACTION AND GOTO *)
     INPUT_ERRORS: BOOLEAN;
     INPUT_FILE,
     OUTPUT_FILE:  STRING[30];
$PAGE CREATE_PRODS declarations
PROCEDURE CREATE_PRODS;
  
   TYPE
      MSGRANGE = 1..5;
      MSGTYPE  = (DFA_ERROR, FATAL);
  
   VAR
      BUFFER_LENGTH,
      BUFFER_POS:     0..BUFFERLEN;
      BUFFER:         PACKED ARRAY[1..BUFFERLEN] OF CHAR;
 
      NEWSYM_LENGTH:  0..MAXSYMLEN;
      NEWSYM:         PACKED ARRAY [1..MAXSYMLEN] OF CHAR;
      NEWSYM_TYPE:    (TERMINAL, NONTERMINAL, META_DEFINE, META_OR,
                       META_END, EODATA,      META_SELF,   META_NOSELF);
 
      DFA_STATE:      (INIT, LEFT, BEGIN_RIGHT, RIGHT, FINAL);
      CURR_RHSLENGTH: EXITEMRANGE;
      OLD_LHSINDEX,
      LHSINDEX:       NONTERRANGE;
      SYMINDEX:       ALPHARANGE;
      CURR_START_ITEM,
      CURR_ITEM:      ITEMRANGE;
      CURRPRODNO:     PRODNORANGE;
      TEMP_RESOLVE:   RESTYPE;
$PAGE CREATE_PRODS local proc. GET_SYMBOL
   PROCEDURE GET_SYMBOL;  (* RETURNS SINGLE LEXEME IN NEWSYM *)
  
      TYPE
         MSGRANGE = 1..4;
  
      VAR
         TRUNCATED, LEXEME_FOUND: BOOLEAN;
         END_CHAR: CHAR;
         SCAN_STATE: (INIT, GOT_END_CHAR, END_OF_LINE, DONE);
  
      PROCEDURE SCANNER_ERROR(MSGNO: MSGRANGE);
  
         BEGIN
  
            INPUT_ERRORS := TRUE;
            WRITE('     *****     ');
            CASE MSGNO OF
               1: WRITELN('ZERO LENGTH NONTERMINAL');
               2: WRITELN('"::=" MISTYPED');
               3: WRITELN('ILLEGAL CHARACTER "',BUFFER[BUFFER_POS],'"');
               4: WRITELN('SYMBOL CONTINUES PAST END OF LINE')
            END
         END;
  
  
      PROCEDURE INSERTCH; (* INSERT CHAR INTO SYM *)
  
         BEGIN
            IF NEWSYM_LENGTH < MAXSYMLEN THEN
               BEGIN
                  NEWSYM_LENGTH := NEWSYM_LENGTH + 1;
                  NEWSYM[NEWSYM_LENGTH] := BUFFER[BUFFER_POS]
               END
            ELSE
               TRUNCATED := TRUE   (* RAN OUT OF ROOM *)
         END;
  
  
      BEGIN  (* GET_SYMBOL *)
  
         LEXEME_FOUND := FALSE;
         REPEAT
            IF BUFFER_POS >= BUFFER_LENGTH THEN   (* HAS BUFFER'S CONTENTS BEEN USED UP? *)
               REPEAT
                  BUFFER_LENGTH := 1;
                  IF NOT EOF(INPUT) THEN
                     WHILE NOT EOLN(INPUT) AND (BUFFER_LENGTH < BUFFERLEN) DO
                        BEGIN
                           BUFFER[BUFFER_LENGTH] := INPUT^;
                           GET(INPUT);
                           BUFFER_LENGTH := BUFFER_LENGTH + 1
                        END;
                  BUFFER[BUFFER_LENGTH] := CHR(127);   (* INTERNAL END OF BUFFER MARKER *)
                  BUFFER_POS := 0;
                  WRITELN('          ',BUFFER:(BUFFER_LENGTH - 1));
                  IF NOT EOF(INPUT) THEN
                     IF EOLN(INPUT) THEN
                        READLN
                     ELSE
                        WRITELN('     *****     WARNING: LINE IS SPLIT')
               UNTIL BUFFER[1] <> '*';   (* UNTIL NOT A COMMENT LINE *)
  
            REPEAT
               BUFFER_POS := BUFFER_POS + 1
            UNTIL BUFFER[BUFFER_POS] <> ' ';
  
            CASE BUFFER[BUFFER_POS] OF
               '''', '<':
                    BEGIN
                       IF BUFFER[BUFFER_POS] = '<' THEN
                          BEGIN
                             END_CHAR := '>';
                             NEWSYM_TYPE := NONTERMINAL
                          END
                       ELSE
                          BEGIN
                             END_CHAR := '''';
                             NEWSYM_TYPE := TERMINAL
                          END;
  
                       TRUNCATED := FALSE;
                       NEWSYM_LENGTH := 0;
                       NEWSYM := '********************';
                       SCAN_STATE := INIT;
                       REPEAT
                          BUFFER_POS := BUFFER_POS + 1;
                          CASE SCAN_STATE OF
                             INIT:
                                  IF BUFFER[BUFFER_POS] = END_CHAR THEN
                                     SCAN_STATE := GOT_END_CHAR
                                  ELSE IF BUFFER[BUFFER_POS] = CHR(127) THEN
                                     SCAN_STATE := END_OF_LINE
                                  ELSE
                                     INSERTCH;
                             GOT_END_CHAR:
                                  IF BUFFER[BUFFER_POS] = END_CHAR THEN
                                     BEGIN
                                        INSERTCH;
                                        SCAN_STATE := INIT
                                     END
                                  ELSE
                                     BEGIN
                                        SCAN_STATE := DONE;
                                        BUFFER_POS := BUFFER_POS - 1 (* BACKUP ONE CHAR *)
                                     END
                          END
                       UNTIL (SCAN_STATE=DONE) OR (SCAN_STATE = END_OF_LINE);
  
                       IF TRUNCATED THEN
                          WRITELN('     *****     WARNING: SYMBOL TRUNCATED');
                       IF SCAN_STATE = END_OF_LINE THEN
                          SCANNER_ERROR(4)
                       ELSE IF (NEWSYM_LENGTH = 0) AND (NEWSYM_TYPE = NONTERMINAL) THEN
                          SCANNER_ERROR(1)
                       ELSE
                          LEXEME_FOUND := TRUE
                    END;
               ':':
                    BEGIN
                       BUFFER_POS := BUFFER_POS + 1;
                       IF BUFFER[BUFFER_POS] <> ':' THEN
                          SCANNER_ERROR(2)
                       ELSE
                          BEGIN
                             BUFFER_POS := BUFFER_POS + 1;
                             IF BUFFER[BUFFER_POS] <> '=' THEN
                                SCANNER_ERROR(2)
                             ELSE
                                BEGIN
                                   NEWSYM_TYPE := META_DEFINE;  (* METASYMBOL "::=" *)
                                   LEXEME_FOUND := TRUE
                                END
                          END
                    END;
               '!':
                    BEGIN
                       NEWSYM_TYPE := META_OR;
                       LEXEME_FOUND := TRUE
                    END;
               ';':
                    BEGIN
                       NEWSYM_TYPE := META_END;
                       LEXEME_FOUND := TRUE
                    END;
               CHR(127):
                    IF EOF(INPUT) THEN
                       BEGIN
                          NEWSYM_TYPE := EODATA;
                          LEXEME_FOUND := TRUE
                       END;
               '+':
                    BEGIN
                       NEWSYM_TYPE := META_SELF;  (* SELF-PRECEDENCE INDICATOR *)
                       LEXEME_FOUND := TRUE
                    END;
               '-':
                    BEGIN
                       NEWSYM_TYPE := META_NOSELF;  (* NO SELF-PRECEDENCE *)
                       LEXEME_FOUND := TRUE
                    END;
               OTHERS:
                    SCANNER_ERROR(3)
            END
         UNTIL LEXEME_FOUND
  
      END;  (* GET_SYMBOL *)
$PAGE CREATE_PRODS local procs.
   PROCEDURE ERROUT(MSGNO: MSGRANGE; MTYPE: MSGTYPE);
  
      BEGIN
         CASE MTYPE OF
            DFA_ERROR:   (* STOP AFTER PROCESSING INPUT *)
               BEGIN
                  INPUT_ERRORS := TRUE; 
                  WRITE('     *****     ');
                  CASE MSGNO OF
                     1: WRITELN('INVALID LEFT HAND SIDE');
                     2: WRITELN('PRODUCTIONS WITH THIS LHS ALREADY SEEN');
                     3: WRITELN('"::=" NOT FOUND');
                     4: WRITELN('INVALID SYMBOL ON RIGHT HAND SIDE');
                     5: WRITELN('NO INPUT GRAMMAR')
                  END;
                  (* NOW FLUSH TO NEXT ; OR EOF *)
                  DFA_STATE := INIT;
                  IF NEWSYM_TYPE <> EODATA THEN
                     REPEAT
                        GET_SYMBOL
                     UNTIL NEWSYM_TYPE IN [EODATA,META_END]
               END;
            FATAL:
               BEGIN
                  CASE MSGNO OF
                     1: BEGIN
                           IF NEWSYM_TYPE = NONTERMINAL THEN
                              WRITELN(TTYOUTPUT,'?NONTERMINAL SYMBOL TABLE OVERFLOW')
                           ELSE
                              WRITELN(TTYOUTPUT,'?TERMINAL SYMBOL TABLE OVERFLOW')
                        END;
                     2: WRITELN(TTYOUTPUT,'?TOO MANY ITEMS')
                  END;
                  STOP
               END
         END
      END;  (* ERROUT *)
  
  
   PROCEDURE FIND(VAR X: ALPHARANGE);  (* FIND NEWSYM IN ITS APPROPRIATE SYMBOL TABLE *)
  
      BEGIN
         IF NEWSYM_TYPE = TERMINAL THEN
            BEGIN
               TERM_TAB[LAST_TERM-1].LENGTH := NEWSYM_LENGTH;
               TERM_TAB[LAST_TERM-1].SYM := NEWSYM;
               X := 0;
               WHILE (TERM_TAB[X].LENGTH <> NEWSYM_LENGTH) OR
                     (TERM_TAB[X].SYM <> NEWSYM) DO
                  X := X - 1;
               IF X < LAST_TERM THEN  (* NOT IN TABLE? *)
                  IF LAST_TERM > MIN_TERM+1 THEN
                     LAST_TERM := LAST_TERM - 1
                  ELSE
                     ERROUT(1,FATAL)
            END
         ELSE
            BEGIN
               NONTERM_TAB[LAST_NONTERM+1].LENGTH := NEWSYM_LENGTH;
               NONTERM_TAB[LAST_NONTERM+1].SYM := NEWSYM;
               X := 0;
               WHILE (NONTERM_TAB[X].LENGTH <> NEWSYM_LENGTH) OR
                     (NONTERM_TAB[X].SYM <> NEWSYM) DO
                  X := X + 1;
               IF X > LAST_NONTERM THEN  (* NOT IN TABLE *)
                  IF LAST_NONTERM < MAXNONTER-1 THEN
                     LAST_NONTERM := LAST_NONTERM + 1
                  ELSE
                     ERROUT(1,FATAL)
           END
      END;  (* FIND *)
  
  
   PROCEDURE FINISH_RHS;  (* COMPLETE INSERTION OF PRODUCTION INTO PRODS *)
  
      VAR
         I: ITEMRANGE;
  
      BEGIN
         PRODS[CURR_ITEM].LHS       := LHSINDEX;
         PRODS[CURR_ITEM].DOTSYMBOL := LAMBDA;            (* DOT IS AT FAR RIGHT *)
         PRODS[CURR_ITEM].STARTRHS  := CURR_START_ITEM;   (* CHAIN BACK TO 1ST ITEM FOR THIS PRODUCTION *)
         PRODS[CURR_ITEM].PRODNO    := CURRPRODNO;
         PRODS[CURR_ITEM].RESOLVE   := TEMP_RESOLVE;
         FOR I := CURR_START_ITEM TO CURR_ITEM DO         (* GO BACK AND FILL IN RHS LENGTH *)
            PRODS[I].RHSLENGTH := CURR_RHSLENGTH;
         IF CURR_ITEM = MAXITEMS THEN
            ERROUT(2,FATAL);
         CURR_ITEM := CURR_ITEM + 1;
         CURR_START_ITEM := CURR_ITEM                     (* WHERE NEXT PRODUCTION'S ITEMS WILL START *)
      END; 
  
  
   PROCEDURE CONTINUE_RHS;  (* INSERT NEXT SYMBOL OF RIGHT HAND SIDE OF A PRODUCTION INTO PRODS *)
  
      BEGIN
         PRODS[CURR_ITEM].LHS       := LHSINDEX;
         PRODS[CURR_ITEM].DOTSYMBOL := SYMINDEX;          (* DOT IN FRONT OF THIS SYMBOL *)
         PRODS[CURR_ITEM].STARTRHS  := CURR_START_ITEM;
         PRODS[CURR_ITEM].PRODNO    := CURRPRODNO;
         PRODS[CURR_ITEM].RESOLVE   := TEMP_RESOLVE;
         CURR_RHSLENGTH := CURR_RHSLENGTH + 1;
         IF CURR_ITEM = MAXITEMS THEN
            ERROUT(2,FATAL);
         CURR_ITEM := CURR_ITEM + 1
      END;
$PAGE CREATE_PRODS body
   BEGIN  (* CREATE_PRODS *)
  
      INPUT_ERRORS    := FALSE;
      BUFFER_LENGTH   := 0;
      BUFFER_POS      := 0;         (* FORCE INITIAL READ *)
      CURR_START_ITEM := REGITEM;   (* FIRST ITEM AFTER S' --> .S AND S' --> S.  *)
      CURR_ITEM       := REGITEM;   (*   "    "     "      "       "      "      *)
      OLD_LHSINDEX    := SPRIME;
      CURRPRODNO      := ZEROPROD;
      FOR LHSINDEX := STARTSYM TO MAXNONTER DO
         LCHAIN[LHSINDEX] := 0;     (* INITIALIZE LCHAIN TO BE EMPTY *)
  
      (* INSERT AUGMENTING PRODUCTION S' --> S INTO PRODS *)
  
      WITH PRODS[FIRST_ITEM] DO  (* S' --> .S *)
         BEGIN
            LHS       := SPRIME;
            DOTSYMBOL := STARTSYM;
            RHSLENGTH := 1;
            STARTRHS  := FIRST_ITEM;
            PRODNO    := CURRPRODNO;
            RESOLVE   := NORES
         END;
      WITH PRODS[SECOND_ITEM] DO  (* S' --> S. *)
         BEGIN
            LHS       := SPRIME;
            DOTSYMBOL := LAMBDA;
            RHSLENGTH := 1;
            STARTRHS  := FIRST_ITEM;
            PRODNO    := CURRPRODNO;
            RESOLVE   := NORES
         END;
      LCHAIN[SPRIME] := FIRST_ITEM;   (* FIRST ITEM HAVING S' FOR LHS *)
  
      LAST_NONTERM          := SPRIME;   (* INITIALIZE NONTERMINAL SYMBOL TABLE *)
      NONTERM_TAB[SPRIME].LENGTH := 2;
      NONTERM_TAB[SPRIME].SYM    := 'S''******************';
  
      LAST_TERM                  := LAMBDA;   (* INITIALIZE TERMINAL SYMBOL TABLE *)
      TERM_TAB[LAMBDA].LENGTH    := 0;
      TERM_TAB[LAMBDA].SYM       := '********************';
  
  
  
      (* FINITE STATE AUTOMATON WHICH ACCEPTS THE INPUT GRAMMAR AND PERFORMS DESIRED SEMANTICS TO
         CREATE PRODS DATA STRUCTURE, AND DO ERROR CHECKING.  THE SEMANTICS ARE ASSOCIATED WITH EACH
         TRANSITION OF THE DFA.
  
         REGULAR EXPRESSIONS FOR THE INPUT GRAMMAR BEING ACCEPTED:
  
            <RES>      ::= '' ! '+' ! '-';
            <RHS>      ::= <RES> (<TERMINAL> ! <NONTERMINAL>)* ;
            <PRODG>    ::= <NONTERMINAL> '::=' <RHS> ('!' <RHS>)* ;
            <PRODSET>  ::= <PRODG> (';' <PRODG>)*                                                  *)
$PAGE CREATE_PRODS body
      DFA_STATE := INIT;  (* STARTING STATE OF DFA *)
  
      REPEAT
         GET_SYMBOL;   (* OBTAIN NEXT TOKEN FROM INPUT (IN NEWSYM) *)
  
         CASE DFA_STATE OF
            INIT:
                 IF NEWSYM_TYPE = EODATA THEN
                    DFA_STATE := FINAL
                 ELSE IF NEWSYM_TYPE = NONTERMINAL THEN
                    BEGIN
                       FIND(LHSINDEX);   (* FIND IT IN THE NONTERMINAL SYMBOL TABLE (OR PUT IT IN) *)
                       DFA_STATE := LEFT;
                       IF OLD_LHSINDEX <> LHSINDEX THEN   (* A NEW LHS SYMBOL? *)
                          BEGIN
                             OLD_LHSINDEX := LHSINDEX;
                             IF LCHAIN[LHSINDEX] <> 0 THEN  (* ALREADY (GROUP OF) PRODS. FOR THIS LHS? *)
                                ERROUT(2,DFA_ERROR)
                             ELSE
                                LCHAIN[LHSINDEX] := CURR_ITEM (* REMEMBER FIRST ITEM WITH THIS LHS *)
                          END
                    END
                 ELSE
                    ERROUT(1,DFA_ERROR);
  
            LEFT:
                 IF NEWSYM_TYPE = META_DEFINE THEN
                    DFA_STATE := BEGIN_RIGHT
                 ELSE
                    ERROUT(3,DFA_ERROR);
  
            BEGIN_RIGHT:
                 BEGIN
                    TEMP_RESOLVE := NORES;  (* ASSUME NO RESOLUTION *)
                    CURR_RHSLENGTH := 0;
                    CURRPRODNO := CURRPRODNO - 1;  (* NEW PRODUCTION NUMBER *)
                    IF NEWSYM_TYPE IN [TERMINAL, NONTERMINAL, META_SELF, META_NOSELF] THEN
                       DFA_STATE := RIGHT;
                    CASE NEWSYM_TYPE OF
                       META_OR:
                          FINISH_RHS;
                       EODATA:
                          BEGIN
                             FINISH_RHS;
                             DFA_STATE := FINAL
                          END;
                       META_END:
                          BEGIN
                             FINISH_RHS;
                             DFA_STATE := INIT
                          END;
                       TERMINAL,
                       NONTERMINAL:
                          IF NEWSYM_LENGTH <> 0 THEN  (* NOT LAMBDA *)
                             BEGIN
                                FIND(SYMINDEX);
                                CONTINUE_RHS
                             END;
                       META_SELF:
                          TEMP_RESOLVE := SELF;  (* HAS SELF-PRECEDENCE *)
                       META_NOSELF:
                          TEMP_RESOLVE := NOSELF;  (* NO SELF-PRECEDENCE *)
                       OTHERS:
                          BEGIN
                             CURRPRODNO := CURRPRODNO + 1;  (* SET BACK TO OLD VALUE *)
                             ERROUT(4,DFA_ERROR)
                          END
                    END
                 END;
  
            RIGHT:
                 BEGIN
                    IF NEWSYM_TYPE IN [META_OR, META_END, EODATA] THEN  (* END OF RHS? *)
                       FINISH_RHS;
                    CASE NEWSYM_TYPE OF
                       TERMINAL,
                       NONTERMINAL:
                          IF NEWSYM_LENGTH > 0 THEN
                             BEGIN
                                FIND(SYMINDEX);
                                CONTINUE_RHS
                             END;
                       META_OR:
                          DFA_STATE := BEGIN_RIGHT;
                       META_END:
                          DFA_STATE := INIT;
                       EODATA:
                          DFA_STATE := FINAL;
                       OTHERS:
                          ERROUT(4,DFA_ERROR)
                    END
                 END
         END
      UNTIL DFA_STATE = FINAL;
  
  
      WRITELN;   (* SKIP SOME LINES AFTER ECHOING OF INPUT (AND ANY ERROR MESSAGES) *)
      WRITELN;
  
      IF CURR_START_ITEM = REGITEM THEN
         ERROUT(5,DFA_ERROR);
      LAST_ITEM := CURR_ITEM - 1;
      PRODS[CURR_ITEM].LHS := LAMBDA    (* PUT ENDMARK AFTER LAST ONE *)
  
   END;  (* CREATE_PRODS *)
$PAGE EACHGOTO_AND_CLOSURE
PROCEDURE EACHGOTO_AND_CLOSURE;
  
   VAR
      ITEM:   ITEMRANGE;
      SYMBOL: ALPHARANGE;
  
  
   PROCEDURE DOCLOSE(TLHS: NONTERRANGE);
  
      VAR
         TEMP_ITEM: ITEMRANGE;
  
      BEGIN
         TEMP_ITEM := LCHAIN[TLHS];
         REPEAT
            IF NOT CLOSURE[SYMBOL, TEMP_ITEM] THEN
               BEGIN
                  CLOSURE[SYMBOL, TEMP_ITEM] := TRUE;
                  IF PRODS[TEMP_ITEM].DOTSYMBOL >= STARTSYM THEN      (* NONTERMINAL AFTER DOT? *)
                     IF LCHAIN[PRODS[TEMP_ITEM].DOTSYMBOL] <> 0 THEN  (* PRODUCTION WITH IT ON LHS? *)
                        DOCLOSE(PRODS[TEMP_ITEM].DOTSYMBOL)
               END;
            TEMP_ITEM := TEMP_ITEM+PRODS[TEMP_ITEM].RHSLENGTH+1  (* ADVANCE TO NEXT PRODUCTION ...      *)
         UNTIL TLHS <> PRODS[TEMP_ITEM].LHS                   (* ... UNTIL NO MORE PRODUCTIONS FOR TLHS *)
      END;
  
  
   BEGIN  (* EACHGOTO_AND_CLOSURE *)
  
      (* FILL IN EACHGOTO - EACHGOTO[X] IS SET OF ALL ITEMS WITH THE DOT IMMEDIATELY AFTER X *)
  
      FOR SYMBOL := LAST_TERM TO LAST_NONTERM DO
         FOR ITEM := FIRST_ITEM TO LAST_ITEM DO
            EACHGOTO[SYMBOL, ITEM] := FALSE;   (* INITIALIZE SETS TO BE EMPTY *)
      FOR ITEM := FIRST_ITEM TO LAST_ITEM - 1 DO
         IF PRODS[ITEM].DOTSYMBOL <> LAMBDA THEN   (* SYMBOL TO RIGHT OF DOT? *)
            EACHGOTO[PRODS[ITEM].DOTSYMBOL, ITEM+1] := TRUE;   (* THEN ITS LEFT OF DOT IN NEXT ITEM *)
  
      (* FILL IN CLOSURE *)
  
      FOR SYMBOL := SPRIME TO LAST_NONTERM DO
         FOR ITEM := FIRST_ITEM TO LAST_ITEM DO
            CLOSURE[SYMBOL, ITEM] := FALSE;   (* INITIALIZE SETS TO BE EMPTY *)
      FOR SYMBOL := SPRIME TO LAST_NONTERM DO
         IF LCHAIN[SYMBOL] = 0 THEN   (* NO PRODUCTION WITH SYMBOL ON LHS? *)
            WITH NONTERM_TAB[SYMBOL] DO
               WRITELN('     *****          WARNING: USELESS NONTERMINAL: <',SYM:LENGTH,'>')
         ELSE
            DOCLOSE(SYMBOL)
  
   END;  (* EACHGOTO_AND_CLOSURE *)
$PAGE FIRST_AND_FOLLOW declarations
PROCEDURE FIRST_AND_FOLLOW;
  
   TYPE
      INCLUSION = RECORD
                     SUBFOL, FOL: NONTERRANGE;
                     NEXT: ^INCLUSION
                  END;
  
   VAR
      NOCHANGE,
      SETIT,
      SUFDERNULL,             (* SUFFIX DERIVES NULL *)
      PREDERNULL:  BOOLEAN;   (* PREFIX DERIVES NULL *)
      HEAD, POINT: ^INCLUSION;
      DER_LAMBDA:  PACKED ARRAY[NONTERRANGE] OF BOOLEAN;   (* DER_LAMBDA[A]=TRUE IFF A -->* LAMBDA *)
      ITEM, ITEM2: ITEMRANGE;
      NONT:        NONTERRANGE;
      TER:         TERRANGE;
  
  
   PROCEDURE UNION(VAR A, B: SYMSET);
  
      VAR
         TEMP_SET:   SYMSET;
         SYMBOL:     TERRANGE;
         EQUAL:      BOOLEAN;
  
      BEGIN
         EQUAL := TRUE;
         FOR SYMBOL := LAMBDA DOWNTO LAST_TERM DO
            BEGIN
               TEMP_SET[SYMBOL] := A[SYMBOL] OR B[SYMBOL];
               IF A[SYMBOL] <> TEMP_SET[SYMBOL] THEN
                  EQUAL := FALSE
            END;
         IF NOT EQUAL THEN
            BEGIN
               FOR SYMBOL := LAMBDA DOWNTO LAST_TERM DO
                  A[SYMBOL] := TEMP_SET[SYMBOL];
               NOCHANGE := FALSE
            END
      END;
$PAGE FIRST_AND_FOLLOW body
   BEGIN  (* FIRST_AND_FOLLOW *)
  
      (* COMPUTE DER_LAMBDA: ALL NONTERMINALS WHICH CAN DERIVE THE NULL STRING.  X DERIVES LAMBDA IF
         X --> LAMBDA,  OR IF X --> Y1 Y2 ... YN  WHERE EACH YI DERIVES LAMBDA.                       *)
  
      FOR NONT := SPRIME TO LAST_NONTERM DO
         DER_LAMBDA[NONT] := FALSE;   (* INITIALIZE *)
      REPEAT
         NOCHANGE := TRUE;
         ITEM := FIRST_ITEM;   (* START WITH FIRST ITEM OF FIRST PRODUCTION *)
         WHILE ITEM <= LAST_ITEM DO
            BEGIN
               SETIT := TRUE;
               WHILE SETIT AND (PRODS[ITEM].DOTSYMBOL <> LAMBDA) DO   (* I.E. NOT END OF RHS *)
                  BEGIN
                     IF PRODS[ITEM].DOTSYMBOL >= STARTSYM THEN   (* NONTERMINAL AFTER DOT? *)
                        BEGIN
                           IF NOT DER_LAMBDA[PRODS[ITEM].DOTSYMBOL] THEN
                              SETIT := FALSE  (* SYM AFTER DOT DOESN'T DERIVE NULL, OR WE DON'T KNOW IT *)
                        END
                     ELSE
                        SETIT := FALSE;   (* THERE'S A TERMINAL ON RHS, SO RHS CAN'T DERIVE NULL *)
                     ITEM := ITEM + 1     (* MOVE DOT RIGHT *)
                  END;
               IF SETIT AND NOT DER_LAMBDA[PRODS[ITEM].LHS] THEN
                  BEGIN
                     DER_LAMBDA[PRODS[ITEM].LHS] := TRUE;
                     NOCHANGE := FALSE
                  END;
               ITEM := PRODS[ITEM].STARTRHS + PRODS[ITEM].RHSLENGTH + 1  (* SKIP TO NEXT PRODUCTION *)
            END
      UNTIL NOCHANGE;
  
  
      (* INITIALIZE FIRST AND FOLLOW TO BE EMPTY *)
     
      FOR NONT := SPRIME TO LAST_NONTERM DO
         FOR TER := MIN_TERM TO LAMBDA DO
            BEGIN
               FIRST[NONT, TER] := FALSE;
               FOLLOW[NONT, TER] := FALSE
            END;
  
  
      (* COMPUTE FIRST (REFERENCE: P. 188 OF A & U).  LAMBDA WILL NOT BE PUT INTO THE SETS IT BELONGS IN
         AT THIS POINT.  MOST OF THE USAGES OF THE FIRST SETS IN THE COMPUTATION OF FIRST AND FOLLOW ARE
         OF THE VARIETY "ADD EVERY NON-LAMBDA SYMBOL IN FIRST(X) TO ... " SO IT IS CONVENIENT TO DELAY
         PUTTING IN THE LAMBDA'S.  DER_LAMBDA ALREADY HAS THE NECESSARY INFORMATION ANYWAY.            *)
  
      REPEAT
         NOCHANGE := TRUE;
         ITEM := FIRST_ITEM;   (* START WITH (FIRST ITEM FOR) FIRST PRODUCTION *)
         WHILE ITEM <= LAST_ITEM DO
            BEGIN
               PREDERNULL := TRUE;
               ITEM2 := ITEM;   (* START WITH FIRST ITEM FOR CURRENT PRODUCTION *)
               WHILE PREDERNULL DO
                  BEGIN
                     IF PRODS[ITEM2].DOTSYMBOL >= STARTSYM THEN
                        BEGIN
                           UNION(FIRST[PRODS[ITEM].LHS], FIRST[PRODS[ITEM2].DOTSYMBOL]); 
                           IF NOT DER_LAMBDA[PRODS[ITEM2].DOTSYMBOL] THEN
                              PREDERNULL := FALSE
                        END
                     ELSE
                        BEGIN  (* TERMINAL OR LAMBDA *)
                           PREDERNULL := FALSE;
                           IF PRODS[ITEM2].DOTSYMBOL < LAMBDA THEN   (* TERMINAL? *)
                              IF NOT FIRST[PRODS[ITEM].LHS, PRODS[ITEM2].DOTSYMBOL] THEN
                                 BEGIN
                                    FIRST[PRODS[ITEM].LHS, PRODS[ITEM2].DOTSYMBOL] := TRUE;
                                    NOCHANGE := FALSE
                                 END
                        END;
                     ITEM2 := ITEM2 + 1   (* MOVE DOT RIGHT WITHIN RHS *)
                  END;
               ITEM := PRODS[ITEM].STARTRHS + PRODS[ITEM].RHSLENGTH + 1   (* SKIP TO NEXT PRODUCTION *)
            END
      UNTIL NOCHANGE;
  
  
      (* COMPUTE CONTAINMENT OF THE FOLLOW SETS FOR USE IN STEP 3 BELOW.  IF A --> ALPHA B  (OR
         A --> ALPHA B BETA  WHERE BETA DERIVES NULL) THEN EVERYTHING IN FOLLOW(A) WILL BE IN FOLLOW(B).
         I.E. FOLLOW(A) IS A SUBSET OF FOLLOW(B).                                                       *)
  
      NEW(HEAD);  (* HEADER OF CONTAINMENT LIST *)
      HEAD^.NEXT := NIL;
      SUFDERNULL := TRUE;   (* ASSUME SUFFIX (BETA) DERIVES NULL, UNTIL WE SEE OTHERWISE *)
      FOR ITEM := LAST_ITEM DOWNTO FIRST_ITEM DO
         BEGIN
            IF PRODS[ITEM].DOTSYMBOL >= STARTSYM THEN   (* NONTERMINAL? *)
               BEGIN
                  IF SUFDERNULL THEN
                     BEGIN
                        IF PRODS[ITEM].LHS <> PRODS[ITEM].DOTSYMBOL THEN
                           BEGIN
                              NEW(POINT);
                              POINT^.SUBFOL := PRODS[ITEM].LHS;    (* FOLLOW(LHS) WILL BE A SUBSET ... *)
                              POINT^.FOL := PRODS[ITEM].DOTSYMBOL; (* ... OF FOLLOW(DOTSYMBOL)         *)
                              POINT^.NEXT := HEAD^.NEXT;
                              HEAD^.NEXT := POINT
                           END;
                        IF NOT DER_LAMBDA[PRODS[ITEM].DOTSYMBOL] THEN
                           SUFDERNULL := FALSE
                     END
               END
            ELSE IF PRODS[ITEM].DOTSYMBOL < LAMBDA THEN  (* TERMINAL? *)
               SUFDERNULL := FALSE
            ELSE  (* LAMBDA *)
               SUFDERNULL := TRUE   (* FORCE SWITCH BACK ON AFTER EACH PRODUCTION *)
         END;
  
  
      (* STEP 1: A & U(1977) P. 189 *)
  
      FOLLOW[SPRIME, LAMBDA] := TRUE;
  
      (* STEP 2 *)
  
      REPEAT
         NOCHANGE := TRUE;
         FOR ITEM := REGITEM TO LAST_ITEM-2 DO
            IF PRODS[ITEM].DOTSYMBOL >= STARTSYM THEN  (* ADD FIRST[BETA] TO FOLLOW[DOTSYMBOL] *)
               BEGIN 
                  PREDERNULL := TRUE;   (* PREDERNULL REFERS TO PREFIX OF BETA *)
                  ITEM2 := ITEM + 1;    (* MOVE DOT AFTER B (BEFORE BETA) *)
                  WHILE PREDERNULL DO
                     BEGIN
                        IF PRODS[ITEM2].DOTSYMBOL >= STARTSYM THEN
                           BEGIN
                              UNION(FOLLOW[PRODS[ITEM].DOTSYMBOL], FIRST[PRODS[ITEM2].DOTSYMBOL]); 
                              IF NOT DER_LAMBDA[PRODS[ITEM2].DOTSYMBOL] THEN
                                 PREDERNULL := FALSE
                           END
                        ELSE
                           BEGIN  (* TERMINAL OR LAMBDA *)
                              PREDERNULL := FALSE;
                              IF PRODS[ITEM2].DOTSYMBOL < LAMBDA THEN   (* TERMINAL? *)
                                 IF NOT FOLLOW[PRODS[ITEM].DOTSYMBOL, PRODS[ITEM2].DOTSYMBOL] THEN
                                    BEGIN
                                       FOLLOW[PRODS[ITEM].DOTSYMBOL, PRODS[ITEM2].DOTSYMBOL] := TRUE;
                                       NOCHANGE := FALSE
                                    END
                           END;
                        ITEM2 := ITEM2 + 1
                     END
               END;
  
         (* STEP 3:  COPY ALL CONTAINMENTS *)
  
         POINT := HEAD^.NEXT;
         WHILE POINT <> NIL  DO
            BEGIN
               UNION(FOLLOW[POINT^.FOL], FOLLOW[POINT^.SUBFOL]);
               POINT := POINT^.NEXT
            END
      UNTIL NOCHANGE;
  
  
      (* NOW COMPLETE FIRST BY ADDING LAMBDA TO FIRST(N) FOR EACH N SUCH THAT N -->* LAMBDA *)
  
      FOR NONT := STARTSYM TO LAST_NONTERM DO
         IF DER_LAMBDA[NONT] THEN
            FIRST[NONT, LAMBDA] := TRUE
;PAGE;
FOR NONT := STARTSYM TO LAST_NONTERM DO
   BEGIN
      WRITE(NONT:4,' ! ');
      FOR TER := LAMBDA DOWNTO MIN_TERM DO
         IF FIRST[NONT,TER] THEN
            WRITE(TER:3);
      WRITELN
   END
  
   END;  (* FIRST_AND_FOLLOW *)
$PAGE CREATE_LR0SETS
PROCEDURE CREATE_LR0SETS;
  
   (* CREATE THE LR(0) STATES, DETERMINE IF SLR(1), LALR(1), OR NOT (WITH OPTIONAL AUTOMATIC
      CONFLICT RESOLUTION), CREATE PARSING TABLE, AND PRINT TABLE, SYMBOLS, AND PRODUCTIONS.
      NOTE: LR1LA IS ACCESSED ONLY IF GRAMMAR IS NOT SLR(1)                                 *)
  
   VAR
      TEMP_ST,
      CURRSTATE,
      NEXTAVAIL:   STATE;
      ITEM,
      TEMP_ITEM,
      TEMP2_ITEM:  ITEMRANGE;
      GRAMMAR_SYM,
      LB, UB:      ALPHARANGE;
      TEMP_LR0SET: LR0SET;
  
      DUPLICATE,
      NOTSLR1,     TNOTSLR1,              (* "GLOBALLY NOT SLR1",   "SINGLE ROW IS NOT SLR1" *)
      NOTLALR1,    TNOTLALR1,             (* "GLOBALLY NOT LALR1",  "SINGLE ROW IS NOT LALR1" *)
      NOTALLRES,   TNOTALLRES:  BOOLEAN;  (* "GLOBALLY NOT ALL CONFLICTS RESOLVED",  
                                             "NOT ALL CONFLICTS RESOLVED IN SINGLE ROW" *)
      ACTUALLA:    SYMSET;
      TERSYMBOL:   TERRANGE;
$PAGE CREATE_LR0SETS local proc. LR1LA
   PROCEDURE LR1LA(GAMMA: STATE; ITEM: ITEMRANGE;  VAR ACTUALLA: SYMSET);
  
  
      (* THIS PROCEDURE USES THE FOLLOWING DEFINITION FOR COMPUTING THE ACTUAL
         LOOKAHEAD R(GAMMA,ITEM) FOR A GIVEN STATE GAMMA AND LOOKAHEAD R(GAMMA,ITEM) FOR A GIVEN ITEM:
     
         (1) R(INITIAL STATE, S' --> .S) = LAMBDA
     
         (2) R(GAMMA,  A --> . BETA) = UNION ( OVER ALL [B --> NU . A DELTA] IN V(GAMMA)
             SUCH THAT DELTA DERIVES LAMBDA )
             OF R(GAMMA, B --> NU . A DELTA) UNION  [ A IN T , A <> LAMBDA
             SUCH THAT A IN FIRST(DELTA)       AND
             [ B --> NU . A DELTA]   IN   V(GAMMA)  ]
     
         (3)IF ALPHA<>LAMBDA  R(GAMMA, A --> ALPHA . BETA) = UNION OVER
             ALL XI SUCH THAT GOTO(XI,ALPHA)=GAMMA AND A --> .ALPHA BETA
             IN V(XI) OF R( XI, A --> . ALPHA BETA)
     
         IN ACTUALITY ALL THAT IS DONE IS TO COPY ALL LOOKAHEAD DIRECTLY BACK TO ACTUALLA.
         REF: ANDERSON ET AL, P. 21                                                                *)
  
  
      CONST
         INITBACK = 1;  (* INDEX OF FIRST NODE CREATED *)
  
      TYPE
         BACKRANGE = INITBACK..MAXBACK;

         R = RECORD
                BACKSTATE: STATE;
                BACKITEM: ITEMRANGE
              (* R[BACKSTATE,BACKITEM] VALUE NOT NEEDED AS IT HAS ALREADY BEEN ADDED TO ACTUALLA *)
             END;
  
      VAR
         RLIST: ARRAY[BACKRANGE] OF R;
         CURRBACK, NEXTBACK: BACKRANGE;
         GAMMA_TEMP,
         XI:       STATE;
         ITEM_TEMP,
         TITEM,
         DELTA: ITEMRANGE;
         LAMBDAIN,           (* INDICATES IF LAMBDA BELONGS TO ACTUALLA *) 
         DELTANULL: BOOLEAN; (* INDICATES IF DELTA DERIVES NULL *) 
         TER: TERRANGE;
  
  
      FUNCTION GOTOOO(CSTATE: STATE; ITEM1,ITEM2: ITEMRANGE; GAMMA: STATE): BOOLEAN;
  
         (* CHECKS IF GOTO(CSTATE,ALPHA) = GAMMA WHERE
            ALPHA = PRODS[ITEM1].DOTSYMBOL...PRODS[ITEM2].DOTSYMBOL
            WILL ALWAYS RETURN FALSE IF GOTO(CSTATE,ALPHA) IS
            UNDEFINED ( LAMBDA ENCOUNTERED AS ENTRY IN
            GOTO WHILE COMPUTING) OR IF COLLISION RESOLUTION
            ENCOUNTERED IN GOTO WHILE COMPUTING ).                 *)
  
         VAR
            TITEM: ITEMRANGE;
            EQUAL: BOOLEAN;
            CSTATE_TEMP: STATE;
  
         BEGIN
            CSTATE_TEMP := CSTATE;
            TITEM := ITEM1;
            EQUAL := TRUE;
            WHILE EQUAL AND (TITEM <= ITEM2) DO
               BEGIN
                  IF GOTOO[CSTATE_TEMP, PRODS[TITEM].DOTSYMBOL] <= LAMBDA THEN
                    (* TRANSITION UNDER ALPHA NEVER EXISTED OR WAS DELETED DURING COLLISION RESOLUTION *)
                     EQUAL := FALSE
                  ELSE
                     CSTATE_TEMP := GOTOO[CSTATE_TEMP, PRODS[TITEM].DOTSYMBOL];
                  TITEM := TITEM + 1
               END;
            IF EQUAL THEN
               GOTOOO := (CSTATE_TEMP = GAMMA)
            ELSE
               GOTOOO := FALSE
         END;  (* GOTOOO *)
  
  
      PROCEDURE ADDR(CSTATE: STATE; ITEM: ITEMRANGE);
  
         VAR
            FOUND: BOOLEAN;
            TBACK: BACKRANGE;
  
         BEGIN
            TBACK := INITBACK;
            FOUND := FALSE;
            WHILE (NOT FOUND) AND (TBACK < NEXTBACK) DO
               BEGIN
                  IF ITEM = RLIST[TBACK].BACKITEM THEN
                     IF CSTATE = RLIST[TBACK].BACKSTATE THEN
                        FOUND := TRUE;
                  TBACK := TBACK + 1
               END;
            IF NOT FOUND THEN  (* INSERT RLIST[NEWBACK] AND PUT CURRBACK IN ARCS *)
               BEGIN
                  RLIST[NEXTBACK].BACKSTATE := CSTATE;
                  RLIST[NEXTBACK].BACKITEM := ITEM;
                  IF NEXTBACK < MAXBACK THEN
                     NEXTBACK := NEXTBACK + 1
                  ELSE
                     BEGIN
                        WRITELN(TTYOUTPUT,'?TOO MANY ELEMENTS');
                        STOP
                     END
               END
         END;  (* ADDR *)
$PAGE CREATE_LR0SETS local proc. LR1LA
      BEGIN  (* LR1LA *)
  
         LAMBDAIN := FALSE;  (* ASSUME NOT *)
         FOR TER := LAMBDA DOWNTO LAST_TERM DO    (* MAKE ACTUALLA EMPTY *)
            ACTUALLA[TER] := FALSE;
         CURRBACK := INITBACK;
         NEXTBACK := INITBACK + 1;
         RLIST[CURRBACK].BACKSTATE := GAMMA;
         RLIST[CURRBACK].BACKITEM := ITEM;

  
         REPEAT  (* CREATE GOTO GRAPH STRUCTURE *)
            GAMMA_TEMP := RLIST[CURRBACK].BACKSTATE;
            ITEM_TEMP := RLIST[CURRBACK].BACKITEM;
            IF ITEM_TEMP = PRODS[ITEM_TEMP].STARTRHS THEN (* DOT AT FAR LEFT? *)
               BEGIN  (* CASE 2 OF DEFINITION *)
                  (* SEARCH FOR ALL B --> NU . LHS DELTA WHERE
                     [ B --> NU . LHS DELTA ] IN V(GAMMA)
                     DELTA DERIVES LAMBDA AND ADD R(GAMMA, B --> NU . LHS DELTA)
                     IF NOT ALREADY THERE *)

                  FOR TITEM := FIRST_ITEM TO LAST_ITEM DO
                     IF LR0COLLECTION[GAMMA_TEMP, TITEM] THEN    (* IF TITEM IN V(GAMMA) *)
                        IF PRODS[TITEM].DOTSYMBOL = PRODS[ITEM_TEMP].LHS THEN   (* SYMBOL AFTER DOT IS LHS? *)
                           IF TITEM = FIRST_ITEM THEN
                              LAMBDAIN := TRUE  (* CASE 1 OF DEFN OF R *)
                           ELSE
                              BEGIN  (* TEST IF DELTA DERIVES LAMBDA *)
                                 DELTANULL := TRUE;
                                 DELTA := TITEM + 1;
                                 WHILE DELTANULL AND
                                           (DELTA <= PRODS[TITEM].STARTRHS + PRODS[TITEM].RHSLENGTH - 1) DO
                                    BEGIN
                                       IF PRODS[DELTA].DOTSYMBOL < LAMBDA THEN
                                          BEGIN
                                             DELTANULL := FALSE;
                                             ACTUALLA[PRODS[DELTA].DOTSYMBOL] := TRUE
                                          END
                                       ELSE
                                          BEGIN
                                             FOR TER := LAMBDA DOWNTO LAST_TERM DO
                                                ACTUALLA[TER] := ACTUALLA[TER]
                                                                  OR FIRST[PRODS[DELTA].DOTSYMBOL, TER];
                                             IF NOT FIRST[PRODS[DELTA].DOTSYMBOL, LAMBDA] THEN
                                                DELTANULL := FALSE
                                          END;
                                       DELTA := DELTA + 1
                                    END;

                                 IF DELTANULL THEN
                                    ADDR(GAMMA_TEMP, TITEM)
                              END
               END

            ELSE
               BEGIN  (* CASE 3 OF DEFINITION *)
                  (* GET TITEM A --> . ALPHA BETA CORRESPONDING TO CURRENT ITEM A --> ALPHA . BETA *)
                  TITEM := PRODS[ITEM_TEMP].STARTRHS;
                  (* NOW FIND ALL LR0 STATES XI SUCH THAT A --> . ALPHA BETA IN V(XI)
                     AND GOTO(XI,ALPHA)=GAMMA  AND ADD R(XI, A --> . ALPHA BETA)    *)

                  IF TITEM = FIRST_ITEM THEN
                     LAMBDAIN := TRUE  (* CASE 1 OF DEFN OF R*)
                  ELSE
                     FOR XI := INITSTATE TO LAST_LR0_STATE DO
                        IF GOTOO[XI, PRODS[TITEM].DOTSYMBOL] > LAMBDA THEN
                           (* FIRST RULE OUT IMPOSSIBILITIES IN GOTOO
                              AS PROBABLY FASTER THAN TESTING FOR ITEM *)
                           IF LR0COLLECTION[XI, TITEM] THEN
                              IF GOTOOO(XI, TITEM, ITEM_TEMP-1, (*=*)GAMMA_TEMP) THEN
                                 ADDR(XI, TITEM)
               END;
            CURRBACK := CURRBACK + 1
         UNTIL CURRBACK = NEXTBACK;
  
  
         ACTUALLA[LAMBDA] := LAMBDAIN  (* SET LAMBDA BIT RIGHT *)
  
      END;  (* LR1LA *)
$PAGE CREATE_LR0SETS
   PROCEDURE PRINTITEM(ITEM: ITEMRANGE);   (* PRINT AN ITEM OUT A LA A&U FORMAT *)
  
      BEGIN
         WITH NONTERM_TAB[PRODS[ITEM].LHS] DO
            WRITE('          <', SYM: LENGTH, '> ::= ');
         FOR TEMP_ITEM := PRODS[ITEM].STARTRHS TO PRODS[ITEM].STARTRHS + PRODS[ITEM].RHSLENGTH DO
            BEGIN
               IF TEMP_ITEM = ITEM THEN
                  WRITE('. ');
               IF PRODS[TEMP_ITEM].DOTSYMBOL >= STARTSYM THEN (* NONTERMINAL *)
                  WITH NONTERM_TAB[PRODS[TEMP_ITEM].DOTSYMBOL] DO
                     WRITE('<', SYM:LENGTH, '> ')
               ELSE IF PRODS[TEMP_ITEM].DOTSYMBOL <> LAMBDA THEN (* NOT NULL *)
                  WITH TERM_TAB[PRODS[TEMP_ITEM].DOTSYMBOL] DO
                     WRITE('''', SYM:LENGTH, ''' ')
            END;
         WRITELN
      END;  (* PRINTITEM *)
$PAGE CREATE_LR0SETS local proc. FILLINL
   PROCEDURE FILLINL(VAR TNOTLALR1, TNOTALLRES: BOOLEAN);
      (* FILL IN CURRSTATE'S LALR(1) ROW OF PARSING/ACTION*)
  
      VAR
         TGOTOO: ARRAY[TERRANGE] OF EXITEMRANGE;
         CONFLICTS: BOOLEAN;
  
  
      PROCEDURE FILLMORE(TERSYMBOL:TERRANGE);
         (* TEST FOR ANY CONFLICTS, WITH AUTOMATIC AMBIGUITY RESOLUTION IF REQUESTED *)
  
         BEGIN
            IF TGOTOO[TERSYMBOL] = 0 THEN
               BEGIN  (* NO CONFLICTS YET *)
                  TGOTOO[TERSYMBOL] := ITEM;
                  IF PRODS[ITEM].DOTSYMBOL = LAMBDA THEN
                     IF PRODS[ITEM].RESOLVE <> NORES THEN
                         (* FIX TO RESOLVE TO "REDUCE PRODS[ITEM].PRODNO" *)
                        GOTOO[CURRSTATE, TERSYMBOL] := LAMBDA
               END
            ELSE
               BEGIN  (* POSSIBLE CONFLICTS *)
                  CONFLICTS := FALSE;
                  IF PRODS[ITEM].DOTSYMBOL = LAMBDA THEN   (* CONFLICT (THIS REDUCE VS. SHIFT) OR
                                                  (THIS REDUCE VS. ANOTHER REDUCE): *)
                     CONFLICTS := TRUE
                  ELSE
                     IF PRODS[TGOTOO[TERSYMBOL]].DOTSYMBOL = LAMBDA THEN
                         (* CONFLICT (THIS SHIFT VS. REDUCE) *)
                        CONFLICTS := TRUE;

                  IF CONFLICTS THEN
                     BEGIN
                        TNOTLALR1 := TRUE;
                        IF PRODS[TGOTOO[TERSYMBOL]].RESOLVE = NORES THEN
                           TNOTALLRES := TRUE
                        ELSE  (* DO RESOLVE *)
                           IF PRODS[TGOTOO[TERSYMBOL]].PRODNO = PRODS[ITEM].PRODNO THEN (* SELF CONFLICT *)
                              IF PRODS[ITEM].RESOLVE = SELF THEN
                                 BEGIN
                                    TGOTOO[TERSYMBOL] := ITEM; (* RESOLVE TO REDUCE *)
                                    GOTOO[CURRSTATE, TERSYMBOL] := LAMBDA  (* FIX TO TRULY RESOLVE *)
                                 END
                              (* ELSE LEAVE AS IS: SHIFT *)
                     END
               END
         END;  (* FILLMORE *)
$PAGE CREATE_LR0SETS local proc. FILLINL
      BEGIN  (* FILLINL *)
  
         FOR TERSYMBOL := LAMBDA DOWNTO LAST_TERM DO
            TGOTOO[TERSYMBOL] := 0;

         FOR ITEM := FIRST_ITEM TO LAST_ITEM DO
            IF LR0COLLECTION[CURRSTATE, ITEM] THEN
               BEGIN
                  IF PRODS[ITEM].DOTSYMBOL = LAMBDA THEN
                     BEGIN  (* REDUCE *)
                        LR1LA(CURRSTATE, ITEM, ACTUALLA);
                        FOR TERSYMBOL := LAMBDA DOWNTO LAST_TERM DO
                           IF ACTUALLA[TERSYMBOL] THEN
                              FILLMORE(TERSYMBOL)
                     END
                  ELSE  (* SHIFT *)
                     IF PRODS[ITEM].DOTSYMBOL < LAMBDA THEN   (* IGNORE UNLESS TERMINAL AFTER DOT *)
                        FILLMORE(PRODS[ITEM].DOTSYMBOL)
               END;

         (* NOW TRANSFER THE INFO TO GOTOO  *)

         FOR TERSYMBOL := LAMBDA DOWNTO LAST_TERM DO
            IF GOTOO[CURRSTATE, TERSYMBOL] = LAMBDA THEN
               IF TGOTOO[TERSYMBOL] <> LAMBDA THEN
                  GOTOO[CURRSTATE, TERSYMBOL]  := PRODS[TGOTOO[TERSYMBOL]].PRODNO
  
      END;  (* FILLINL *)
$PAGE CREATELR0SETS body
   BEGIN  (* CREATE_LR0SETS *)
  
      (* INITIALIZE LR0COLLECTION TO EMPTY AND GOTOO TO LAMBDA *)
     
      FOR CURRSTATE := 0 TO MAXSETS DO
         BEGIN
            FOR ITEM := FIRST_ITEM TO MAXITEMS DO
               LR0COLLECTION[CURRSTATE, ITEM] := FALSE;
            FOR GRAMMAR_SYM := MIN_TERM TO MAXNONTER DO
               GOTOO[CURRSTATE, GRAMMAR_SYM] := LAMBDA
         END;
      PAGE;
  
  
      (* CREATE THE LR(0) STATES. REFERENCE:  A&U DRAGON BOOK, P. 208.  ACTUAL STEPS INDICATED HERE
         REFER TO EQUIVALENT ALGORITHM 5.8 OF A&U P,T,&C P. 386                                    *)
  
      (* BASIS: STEP 1 - COMPUTE INITIAL STATE = CLOSURE(S' --> .S) *)

      LR0COLLECTION[INITSTATE, FIRST_ITEM] := TRUE;   (* START WITH S' --> .S  ...   *)
      FOR TEMP_ITEM := FIRST_ITEM TO LAST_ITEM DO     (* ... AND COMPUTE THE CLOSURE *)
         LR0COLLECTION[INITSTATE, TEMP_ITEM] := LR0COLLECTION[INITSTATE, TEMP_ITEM]
                                                     OR CLOSURE[STARTSYM, TEMP_ITEM];
      NEXTAVAIL := INITSTATE + 1;
      CURRSTATE := INITSTATE;

      REPEAT
  
         (* STEP 2: A - MAKE COPY OF CURRENT STATE, WITH DOT SHIFTED RIGHT ONE SYMBOL IN EACH ITEM  *)
  
         FOR TEMP_ITEM := LAST_ITEM-1 DOWNTO FIRST_ITEM DO 
            TEMP_LR0SET[TEMP_ITEM + 1] := LR0COLLECTION[CURRSTATE, TEMP_ITEM];
         TEMP_LR0SET[FIRST_ITEM] := FALSE;
  
         (* FOR EACH SYMBOL X SUCH THAT GOTO(CURRSTATE, X) EXISTS AND HASN'T BEEN EXPLORED YET,
            BUILD THE NEW LR(0) STATE GOTO(CURRSTATE, X)  *)
  
         FOR ITEM := FIRST_ITEM TO LAST_ITEM DO
            IF LR0COLLECTION[CURRSTATE, ITEM] THEN     (* FOR EACH ITEM IN CURRSTATE  ...       *)
               IF (PRODS[ITEM].DOTSYMBOL <> LAMBDA)    (* ... WITH A NONNULL SYMBOL AFTER DOT:  *)
                    AND (GOTOO[CURRSTATE, PRODS[ITEM].DOTSYMBOL] = LAMBDA) THEN
                  BEGIN
                     GRAMMAR_SYM := PRODS[ITEM].DOTSYMBOL;
                     (* START NEW STATE WITH ITEMS A --> ALPHA GRAMMAR_SYM . BETA WHERE
                        A --> ALPHA .GRAMMAR_SYM BETA IS IN CURRENT STATE      *)
                     FOR TEMP_ITEM := FIRST_ITEM TO LAST_ITEM DO
                        LR0COLLECTION[NEXTAVAIL, TEMP_ITEM] :=
                             TEMP_LR0SET[TEMP_ITEM] AND EACHGOTO[GRAMMAR_SYM, TEMP_ITEM];

                     (* STEP 2: BC - TAKE CLOSURE OF THOSE ITEMS TO COMPLETE NEW STATE *)
                     FOR TEMP_ITEM := FIRST_ITEM TO LAST_ITEM DO
                        IF LR0COLLECTION[NEXTAVAIL, TEMP_ITEM]
                             AND (PRODS[TEMP_ITEM].DOTSYMBOL >= STARTSYM) THEN
                           FOR TEMP2_ITEM := FIRST_ITEM TO LAST_ITEM DO
                              LR0COLLECTION[NEXTAVAIL, TEMP2_ITEM] := LR0COLLECTION[NEXTAVAIL, TEMP2_ITEM]
                                                       OR CLOSURE[PRODS[TEMP_ITEM].DOTSYMBOL, TEMP2_ITEM];
  
                     (* CHECK FOR DUPLICATE LR(0) STATE.  NOTE: ONE NEED ONLY SEARCH THE COLUMN
                        GOTOO[--,GRAMMAR_SYM] FOR ANY POSSIBLE DUPLICATE DUE TO FACT THAT LR(0)
                        STATES GOTO(INITIAL,ALPHA), GOTO(INITIAL,BETA) CANNOT BE IDENTICAL UNLESS SOME
                        NONNULL SUFFIX OF BETA MATCHES A SUFFIX OF ALPHA.                            *)
              
                     DUPLICATE := FALSE;     (* ASSUME THE "NEW" STATE REALLY IS NEW *)
                     TEMP_ST := INITSTATE;   (* START SEARCH AT TOP OF COLUMN *)
                     WHILE TEMP_ST < NEXTAVAIL DO
                        BEGIN
                           IF GOTOO[TEMP_ST, GRAMMAR_SYM] > LAMBDA THEN   (* COMPARE THE TWO LR0SETS *)
                              BEGIN
                                 DUPLICATE := TRUE;
                                 FOR TEMP_ITEM := FIRST_ITEM TO LAST_ITEM DO
                                    IF LR0COLLECTION[NEXTAVAIL, TEMP_ITEM]
                                         <> LR0COLLECTION[GOTOO[TEMP_ST, GRAMMAR_SYM], TEMP_ITEM] THEN
                                       DUPLICATE := FALSE;
                                 IF DUPLICATE THEN   (* STATE ISN'T NEW AFTER ALL? *)
                                    BEGIN
                                       GOTOO[CURRSTATE, GRAMMAR_SYM] := GOTOO[TEMP_ST, GRAMMAR_SYM];
                                       TEMP_ST := NEXTAVAIL  (* FORCE EXIT FROM WHILE *)
                                    END
                              END;
                           TEMP_ST := TEMP_ST + 1   (* MOVE DOWN COLUMN *)
                        END;
                     IF NOT DUPLICATE THEN   (* NEW STATE REALLY IS NEW *)
                        BEGIN
                           GOTOO[CURRSTATE, GRAMMAR_SYM] := NEXTAVAIL;
                           IF NEXTAVAIL = MAXSETS THEN
                              BEGIN
                                 WRITELN(TTYOUTPUT,'?TOO MANY LR0 SETS - INCREASE MAXSETS IN LALR1');
                                 STOP
                              END;
                           NEXTAVAIL := NEXTAVAIL + 1
                        END
                  END;
         CURRSTATE := CURRSTATE + 1
      UNTIL CURRSTATE = NEXTAVAIL;
  
      LAST_LR0_STATE := CURRSTATE - 1;
$PAGE CREATELR0SETS body
      NOTSLR1   := FALSE;  (* INITIALLY ASSUME SLR1 *)
      NOTLALR1  := FALSE;  (*     "       "    LALR1 *)
      NOTALLRES := FALSE;  (*     "       "    RESOLVED *)

      (* PUT ALL REDUCE ACTIONS IN PARSING TABLE *)

      FOR CURRSTATE := INITSTATE TO LAST_LR0_STATE DO
         BEGIN
            TNOTSLR1   := FALSE;  (* ASSUME ROW FOR THIS STATE IS SLR1 *)
            TNOTLALR1  := FALSE;  (*   "     "   "   "     "   IS LALR1 *)
            TNOTALLRES := FALSE;  (*   "    CONFLICTS IN ROW FOR THIS STATE RESOLVED *)
  
            (* FILL IN CURRSTATE'S SLR(1) ROW OF PARSING/ACTION *)
  
            FOR ITEM := FIRST_ITEM TO LAST_ITEM DO
               IF LR0COLLECTION[CURRSTATE, ITEM] AND (PRODS[ITEM].DOTSYMBOL = LAMBDA) THEN
                  FOR TERSYMBOL := LAMBDA DOWNTO LAST_TERM DO
                     BEGIN
                        IF FOLLOW[PRODS[ITEM].LHS, TERSYMBOL] THEN
                           IF GOTOO[CURRSTATE, TERSYMBOL] <> 0 THEN
                              TNOTSLR1 := TRUE
                           ELSE
                              GOTOO[CURRSTATE, TERSYMBOL] := PRODS[ITEM].PRODNO
                     END;
            IF TNOTSLR1 THEN
               NOTSLR1 := TRUE;

            (* IF NOT SLR1 THEN SEE IF IT'S LALR1    *)

            IF TNOTSLR1 THEN   (* LALR(1)? *)
               BEGIN

                  (* ZERO OUT ALL PRODUCTION NUMBERS IN CURRENT ROW *)
                  FOR TERSYMBOL := LAMBDA DOWNTO LAST_TERM DO
                     IF GOTOO[CURRSTATE, TERSYMBOL] < 0 THEN
                        GOTOO[CURRSTATE, TERSYMBOL] := 0;

                  (* NOW CHECK TO SEE IF LALR1 *)
                  FILLINL(TNOTLALR1, TNOTALLRES);
                  IF TNOTLALR1 THEN
                     NOTLALR1 := TRUE;
                  IF TNOTALLRES THEN
                     NOTALLRES := TRUE;
               END;

            (* PRINT OUT INFORMATION ON THIS STATE *)

            WRITELN;
            WRITELN('     ',CURRSTATE:3,':**************');
            IF TNOTLALR1 THEN
               BEGIN
                  WRITELN('******** LALR1 CONFLICTS IN THIS STATE ********');
                  IF NOT TNOTALLRES THEN
                     WRITELN('******** CONFLICTS RESOLVED *********')
               END;
            FOR ITEM := FIRST_ITEM TO LAST_ITEM DO
               IF LR0COLLECTION[CURRSTATE, ITEM] THEN
                  BEGIN
                     PRINTITEM(ITEM);
                     IF PRODS[ITEM].DOTSYMBOL = LAMBDA THEN
                        IF TNOTSLR1 THEN   (* NOT SLR1? *)
                           BEGIN
                              LR1LA(CURRSTATE, ITEM, ACTUALLA);
                              WRITE('          ****LALR1LA: ');
                              FOR TERSYMBOL := LAMBDA DOWNTO LAST_TERM DO
                                 IF ACTUALLA[TERSYMBOL] THEN
                                    WITH TERM_TAB[TERSYMBOL] DO
                                       WRITE ('''',SYM:LENGTH,''' ');
                              WRITELN
                           END
                        ELSE   (* SLR1 *)
                           BEGIN
                              WRITE('          *****FOLLOW: ');
                              FOR TERSYMBOL := LAMBDA DOWNTO LAST_TERM DO
                                 IF FOLLOW[PRODS[ITEM].LHS, TERSYMBOL] THEN
                                    WITH TERM_TAB[TERSYMBOL] DO
                                       WRITE ('''',SYM:LENGTH,''' ');
                              WRITELN
                           END
                  END
         END;
  
      (* PRINT WHETHER SLR1, OR LALR1, OR NEITHER *)
  
      WRITELN;
      IF NOTSLR1 THEN
         IF NOTLALR1 THEN
            BEGIN
               WRITELN('***** GRAMMAR IS NOT LALR1 *****');
               IF NOT NOTALLRES THEN
                  WRITELN('***** ALL CONFLICTS RESOLVED *****')
            END
         ELSE
            WRITELN('***** GRAMMAR IS LALR1 BUT NOT SLR1 *****')
      ELSE
         WRITELN('***** GRAMMAR IS SLR1 *****');
  
      (* PRINT OUT PARSING ACTION/GOTO TABLE *)
  
      WRITELN(TTYOUTPUT);
      WRITE(TTYOUTPUT,'PRINT OUT TABLE? ');
      BREAK(TTYOUTPUT);
      READLN(TTY);
      IF (TTY^ = 'Y') OR (TTY^ = 'y') THEN
         BEGIN
            LB := LAST_TERM;
            REPEAT
               IF LB + MAXCOLS - 1 <= LAST_NONTERM THEN
                  UB := LB + MAXCOLS - 1
               ELSE
                  UB := LAST_NONTERM;
               PAGE;
               WRITELN('******* PARSING ACTION / GOTO TABLE *******');
               WRITELN;
               WRITE('      ');
               FOR GRAMMAR_SYM := LB TO UB DO
                  WRITE(GRAMMAR_SYM:4,' ');
               WRITELN;
               WRITE('    __');
               FOR GRAMMAR_SYM := LB TO UB DO
                  WRITE('_____');
               WRITELN;
               FOR CURRSTATE := INITSTATE TO LAST_LR0_STATE DO
                  BEGIN
                     WRITELN('    !');
                     WRITE (CURRSTATE:3, ' ! ');
                     FOR GRAMMAR_SYM := LB TO UB DO
                        WRITE(GOTOO[CURRSTATE,GRAMMAR_SYM]:4, ' ');
                     WRITELN
                  END;
               LB := UB + 1
            UNTIL LB > LAST_NONTERM;
            WRITELN;
            WRITELN
         END;
  
  
      (* PRINT CODES FOR ALL TERMINAL AND NONTERMINAL SYMBOLS *)
  
      PAGE;
      WRITELN('          ******** CODES FOR SYMBOLS ********');
      WRITELN('          CODE   SYMBOL');
      FOR GRAMMAR_SYM := LAST_TERM TO LAMBDA DO
         WITH TERM_TAB[GRAMMAR_SYM] DO
            WRITELN('          ',GRAMMAR_SYM:3,'   ''',SYM:LENGTH,'''');
      FOR GRAMMAR_SYM := STARTSYM TO LAST_NONTERM DO
         WITH NONTERM_TAB[GRAMMAR_SYM] DO
            WRITELN('          ',GRAMMAR_SYM:3,'   <',SYM:LENGTH,'>');
  
  
      (* PRINT ALL PRODUCTIONS AND THEIR PRODUCTION NUMBERS *)
  
      PAGE;
      WRITELN('          ********** CODES FOR THE PRODUCTIONS **********');
      WRITELN('          CODE          PRODUCTION');
      ITEM := FIRST_ITEM;
      WHILE ITEM <= LAST_ITEM DO
         BEGIN
            WRITE ('          ',PRODS[ITEM].PRODNO:4);
            PRINTITEM(ITEM);
            ITEM := ITEM + PRODS[ITEM].RHSLENGTH + 1
         END;
      WRITELN;
      WRITELN('          TOTAL NUMBER OF LR(0) ITEMS:',LAST_ITEM:4)
  
   END;  (* CREATE_LR0SETS *)
$PAGE DOMORE
PROCEDURE DOMORE;
  
   VAR
      REDUCE: BOOLEAN;
      NEWSTATE: ARRAY[STATE] OF MINCODE..MAXSETS;
      CURRPROD: MIN_PROD..0;
      CURRSTATE,
      TNEWSTATE: STATE;
      SYMBOL,
      LB, UB: ALPHARANGE;
      ITEM: ITEMRANGE;
  
  
   BEGIN

      (* DO LR0-REDUCE STATE ELIMINATION  (REFERENCE: ANDERSON ET AL)  *)

      TNEWSTATE := INITSTATE;
      NEWSTATE[LAMBDA] := 0;

      FOR CURRSTATE := INITSTATE TO LAST_LR0_STATE DO
         BEGIN
            SYMBOL := LAST_TERM;
            REDUCE := TRUE;
            CURRPROD := 0;
            WHILE SYMBOL <= LAST_NONTERM DO
               BEGIN
                  IF GOTOO[CURRSTATE, SYMBOL] > LAMBDA THEN
                     BEGIN
                        REDUCE := FALSE;
                        SYMBOL := LAST_NONTERM  (* FORCE EXIT *)
                     END
                  ELSE IF GOTOO[CURRSTATE, SYMBOL] <> LAMBDA THEN
		     IF GOTOO[CURRSTATE, SYMBOL] <> CURRPROD THEN
			IF CURRPROD = 0 THEN
			   CURRPROD := GOTOO[CURRSTATE, SYMBOL]
			ELSE
			   BEGIN
			      REDUCE := FALSE;
			      SYMBOL := LAST_NONTERM  (* FORCE EXIT *)
			   END;
		  SYMBOL := SYMBOL + 1
	       END;

	    IF REDUCE THEN
	       NEWSTATE[CURRSTATE] := LR0REDUCE + CURRPROD
	    ELSE
	       BEGIN
		  NEWSTATE[CURRSTATE] := TNEWSTATE;
		  TNEWSTATE := TNEWSTATE + 1
	       END
	 END;
$PAGE
      (* PRINT OUT REDUCE PARSING TABLE     *)

      WRITELN (TTYOUTPUT);
      WRITE(TTYOUTPUT,'PRINT REDUCE TABLE? ');
      BREAK(TTYOUTPUT);
      READLN(TTY);
      LB := LAST_TERM;
      IF (TTY^ = 'Y') OR (TTY^ = 'y') THEN
	 REPEAT
	    IF LB + MAXCOLS - 1 <= LAST_NONTERM THEN
	       UB := LB + MAXCOLS - 1
	    ELSE
	       UB := LAST_NONTERM;
	    PAGE;
	    WRITELN('*****LR0-REDUCED PARSING ACTION/GOTO TABLE*****');
	    WRITELN;
	    WRITE('      ');

	    FOR SYMBOL := LB TO UB DO
	       WRITE(SYMBOL:4,' ');
	    WRITELN;
	    WRITE('    --');

	    FOR SYMBOL := LB TO UB DO
	       WRITE('-----');
	    WRITELN;

	    FOR CURRSTATE := INITSTATE TO LAST_LR0_STATE DO
	       IF NEWSTATE[CURRSTATE] > LAMBDA THEN
		  BEGIN  (* NON-LR0 REDUCE ROW *)
		     WRITELN('    !');
		     WRITE(NEWSTATE[CURRSTATE]:3,' ! ');
		     FOR SYMBOL := LB TO UB DO
			IF GOTOO[CURRSTATE, SYMBOL] >= LAMBDA THEN
			   IF (SYMBOL > LAMBDA) AND
			      (NEWSTATE[GOTOO[CURRSTATE, SYMBOL]] < LR0REDUCE) THEN
			      WRITE((NEWSTATE[GOTOO[CURRSTATE, SYMBOL]]-LR0REDUCE):4, ' ')
			   ELSE
			      WRITE(NEWSTATE[GOTOO[CURRSTATE, SYMBOL]]:4, ' ')
			ELSE
			   WRITE(GOTOO[CURRSTATE, SYMBOL]:4, '');
		     WRITELN
		  END;
	    LB := UB + 1
	 UNTIL LB > LAST_NONTERM;
$PAGE
      (* PRINT REDUCED PARSING TABLE IN FORM OF PASCAL INITPROCEDURE   *)

      UB := 0;
      PAGE;
      WRITELN('INITPROCEDURE;');
      WRITELN('BEGIN  (* PARSING TABLE *)');
      FOR CURRSTATE := INITSTATE TO LAST_LR0_STATE DO
         IF NEWSTATE[CURRSTATE] > LAMBDA THEN
            FOR SYMBOL := LAST_TERM TO LAST_NONTERM DO
               IF GOTOO[CURRSTATE, SYMBOL] <> LAMBDA THEN
                  BEGIN
                     WRITE('P[',NEWSTATE[CURRSTATE]:3,',',SYMBOL:4,']:=');
                     IF GOTOO[CURRSTATE, SYMBOL] > LAMBDA THEN
                        IF (SYMBOL > LAMBDA) AND
                           (NEWSTATE[GOTOO[CURRSTATE, SYMBOL]] < LR0REDUCE) THEN
                           WRITE( (NEWSTATE[GOTOO[CURRSTATE, SYMBOL]] -
                                 LR0REDUCE): 4,';')
                        ELSE
                           WRITE( NEWSTATE[GOTOO[CURRSTATE, SYMBOL]]:4, ';')
                     ELSE
                        WRITE( GOTOO[CURRSTATE, SYMBOL]:4, ';');
                     UB := (UB + 1) MOD 3;
                     IF UB = 0 THEN
                        WRITELN
                  END;
      WRITELN;

      ITEM := REGITEM;
      CURRPROD := ZEROPROD - 1;

      WHILE ITEM <= LAST_ITEM DO
         BEGIN
            WITH PRODS[ITEM] DO
               BEGIN
                  WRITELN('LHS[',CURRPROD:4,']:=',LHS:3,
                          ';RHSLENGTH[',CURRPROD:4,']:=',RHSLENGTH:3,';');
                  ITEM := STARTRHS + RHSLENGTH + 1
               END;
            CURRPROD := CURRPROD - 1
         END;

      WRITELN('END;  (* PARSING TABLE *)')
   END;  (* DOMORE *)
$PAGE Mainline
BEGIN  (* LALR1 *)
  
   (* QUERY USER FOR FILE NAMES *)
  
   REWRITE(TTYOUTPUT,'TTY:');
   WRITELN(TTYOUTPUT);
   WRITE(TTYOUTPUT,'INPUT FILE-- ');
   BREAK(TTYOUTPUT);
   RESET(TTY,'TTY:');
   INPUT_FILE := '';
   WHILE NOT EOLN(TTY) DO
      BEGIN
         INPUT_FILE := INPUT_FILE || TTY^;
         GET(TTY)
      END;
   RESET(INPUT,INPUT_FILE);
   IF EOF(INPUT) THEN
      BEGIN
         WRITELN(TTYOUTPUT,'?INPUT FILE EMPTY OR NONEXISTENT');
         STOP
      END;
  
   WRITE(TTYOUTPUT,'OUTPUT FILE-- ');
   BREAK(TTYOUTPUT);
   READLN(TTY);
   OUTPUT_FILE := '';
   WHILE NOT EOLN(TTY) DO
      BEGIN
         OUTPUT_FILE := OUTPUT_FILE || TTY^;
         GET(TTY)
      END;
   REWRITE(OUTPUT,OUTPUT_FILE);
  
  
   (* GENERATE PARSER *)
  
   CREATE_PRODS;           (* PROCESS INPUT GRAMMAR AND PUT ITEMS INTO PROD DATA STRUCTURE *)
   IF INPUT_ERRORS THEN
      BEGIN
         WRITELN(TTYOUTPUT);
         WRITELN(TTYOUTPUT,'?ERRORS IN INPUT GRAMMAR');
         STOP
      END;
  
   EACHGOTO_AND_CLOSURE;   (* COMPUTE EACHGOTO AND CLOSURE *)
   FIRST_AND_FOLLOW;       (* COMPUTE FIRST AND FOLLOW FUNCTIONS *)
  
   CREATE_LR0SETS;
   DOMORE
  
END.  (* LALR1 *)
    Q `