$TITLE LEXGEN: Lexical Analyzer Generator
$OPTIONS STORAGE=8192
$WIDTH=106
  
  
(*      LEXGEN  -  Lexical Analyzer Generator
  
        Ralph D. Jeffords,  University of Mississippi,  April 1979
        Version 2  (First Distribution)
  
  
        Modifications by Dave Wilson, ADP Network Services, June 1979:
  
            - Totally reformatted and cleaned up for readability (according to my private prejudices).
   
            - Minimization always done, not optional, since minimization takes only a small fraction
               of the running time.  Unminimized DELTA is not printed at all.
    
            - Representation of DFA states changed from packed arrays of booleans to arrays of sets,
              since sets are operated on a word at a time, rather than bit by bit.  Speed improvement
              was a factor of 100 on a large example.
    
            - Intermediate-backup node numbering changed to not be wasteful, as recommended in the
              implementation notes.
    
            - DELTA rows allocated as needed, and packed.  The packing saves a great deal of core
              with no apparent loss of efficiency, while allocating rows means only large DFAs
              need use a lot of core for DELTA.
    
            - Added routine to print the scanner program from Appendix B of the LEXGEN User Manual,
              filling in all the constants and tables that LEXGEN has information about.  The
              additional code and tables required for the lookahead feature are included just
              if they are required.
    
            - The two options for printing NFA diagrams and graphs have been combined.
    
            - INTEGER types have been eliminated, and tag fields added to record variants that
              didn't have them, so that the "special" compiler option isn't required.
    
            - When handling semantics of productions that build terminal sets, the sets are
              kept on the stack, rather than carried "off the side", resulting in consistent
              handling of semantic information.
    
            - ascii "delete" character used as internal end-of-line mark rather than backslash to
              avoid any interference with use of the entire printable set of characters.
    
            - MINIMIZE_DFA changed to add DFA states to end of equivalence class member lists
              rather than at the beginning, making reindexing unnecessary.
  
            - Implemented transition table compaction technique described in Aho & Ullman.  The
              user is given the choice of having the compacted or uncompacted form of DELTA
              incorporated in the output scanner program.                                            *)
$PAGE Constant Definitions
PROGRAM LEXGEN;
  
CONST 
      MAXSETELEM  =  71;   (* COMPILER DEPENDENT LIMITATION ON SET TYPE *)
      SET_SIZE    =  72;   (*    "         "         "       "  "    "  *)
      MAXSETSUB   =   3;   (* UPPER SUBSCRIPT LIMIT FOR STATE "SETS" CONSTRUCTED AS ARRAYS OF SETS *)
      MAXSYMLEN   =  20;   (* THE MAXIMUM LENGTH OF A SYMBOL *)
      MAXCOLS     =  24;   (* MAX # OF COLUMNS ACROSS PAGE FOR DELTA *)
      LAMBDA      =   0;   (* CODE FOR NULL *)
      MAXNONTER   =  40;   (* MAX # OF NONTERMINALS FOR INPUT GRAMMAR *)
      MAXTERSETS  =  20;   (* MAX # OF SETS OF TERMINALS FOR INPUT GRAMMAR *)
      MIN_TERM    =-140;   (* MINIMUM CODE FOR TERMINALS FOR INPUT GRAMMAR *)
      MAXIFSTATES = 287;   (* MAX # OF IMPORTANT/BACKUP/FINAL STATES  (MAXSETSUB+1)*SET_SIZE - 1 *)
      DEAD        =   0;   (* DEAD STATE OF DFA *)
      MAXDFASTATES= 150;   (* MAX # OF DFA STATES *)
      BUFFERLEN   = 135;   (* LENGTH OF INPUT BUFFER *)
      FIRSTBLOCK  =   0;   (* FIRST EQUIVALENCE CLASS *)
      MAXBLOCKS   = MAXDFASTATES;   (* MAX POSSIBLE EQUIVALENCE CLASSES *)
      MAX_COM_TAB = 1500;   (* MAX SIZE OF COMPACTED TABLES *)
      WORD_SIZE   =  36;   (* BITS PER WORD *)

      (* SPECIAL CODES FOR MARKING NFA GRAPH: *)

      UNION       =  21;   (* CODE FOR UNION OF NFSA'S (MUST BE > MAXTERSETS) *)
      KLEENE_CLO  =  22;   (* CODE FOR KLEENE CLOSURE OF NFSA *)
      CONCAT      =  23;   (* CODE FOR CONCATENATION OF NFSA'S *)
      POSITIVE_CLO=  24;   (* CODE FOR POSITIVE CLOSURE OF NFSA *)

      (* CONSTANTS USED BY PARSER: *)

      MAXSTACK    =  50;   (* MAX DEPTH OF PARSER STACK *)
      MAXPARSEST  =  18;   (* LAST STATE OF PARSER *)
      MINPARCODE  =-521;   (* SMALLEST CODE FOR PARSING TABLE *)

      METALBRACK  = -13;   (*     ]     *)
      METARBRACK  = -12;   (*     [     *)
      METAOR      = -11;   (*     !     *)
      METARPAREN  = -10;   (*     )     *)
      METALPAREN  =  -9;   (*     (     *)
      TERMINAL    =  -8;
      METAPLUS    =  -7;   (*     +     *)
      METASTAR    =  -6;   (*     *     *)
      SETNAME     =  -5;
      METASLASH   =  -4;   (*     /     *)
      METADEFINE  =  -3;   (*    ::=    *)
      NONTERMINAL =  -2;
      METAEND     =  -1;   (*     ;     *)
      EODATA      =   0;   (*     $     *)

      TERMINALHEAD=   7;   (* LAST NONTERMINAL CODE FOR PARSER *)
$PAGE Type Definitions
TYPE 
     TERM_RANGE    = MIN_TERM..LAMBDA;               (* RANGE OF TERMINALS IN INPUT *)
     IFSTATESET    = ARRAY[0..MAXSETSUB] OF SET OF 0..MAXSETELEM;
     DFASTATERANGE = DEAD..MAXDFASTATES;             (* RANGE OF DFA STATES *)
     ERRSTATUS     = (OK, WARNING, RECOVER, FATAL);  (* INDICATORS OF ERROR STATUS FOR LEXGEN *)
  
     NODEFLAGTYPE = (PTR, IMPOR_FLAG, FINAL_FLAG, INTER_FLAG);
     NODEPTR = ^ NFA_NODE;
     NFA_NODE = RECORD
                   ARC:  MIN_TERM..POSITIVE_CLO;    (* ARC LABEL *)
                   PTR1: NODEPTR;                   (* MAJOR POINTER TO ANOTHER NODE *)
                   CASE NODEFLAG: NODEFLAGTYPE OF
                      PTR:          (* INITIALLY GIVEN TO ALL NODES *)
                           (PTR2: NODEPTR;          (* SECONDARY POINTER USED BY UNION AND CLOSURES *)
                            SCANNED: NODEPTR);      (* SPECIAL MARKER TO PREVENT INFINITE SEARCH IN NFA *)
                      IMPOR_FLAG,   (* IMPORTANT STATE *)
                      INTER_FLAG,   (* INTERMEDIATE-BACKUP STATE (LOOKAHEAD FEATURE USING "/") *)
                      FINAL_FLAG:   (* FINAL STATE *)
                           (IFCODE: 1..MAXIFSTATES) (* INDEX THIS IMPORTANT/BACKUP/FINAL ("IBF") STATE *)
                END;
  
     DELTA_ROW = PACKED ARRAY[TERM_RANGE] OF DFASTATERANGE;
     BLOCKRANGE = FIRSTBLOCK..MAXBLOCKS; (* RANGE OF BLOCKS *)
     MEMBER = RECORD
                 STATE: DFASTATERANGE;
                 NEXT_MEM: ^MEMBER
              END;
  
     (* REST OF TYPES REFER TO PARSER:  *)

     STACKRANGE = 1..MAXSTACK; (* RANGE OF PARSER STACK *)
     ELEMENTRANGE = MINPARCODE..MAXPARSEST;(* RANGE OF PARSE TABLE ENTRIES *)
  
     STACK_VARIANTS = (SYMBOL_VARIANT, NFA_VARIANT, SET_VARIANT);
     STACKTYPE = ARRAY[STACKRANGE] OF
                 RECORD
                    STACKSTATE: 1..MAXPARSEST;
                    CASE ST_VAR: STACK_VARIANTS OF
                       SYMBOL_VARIANT: (SYMBOL: MIN_TERM..MAXNONTER);  (* TERM.<0, LAMBDA=0, 0<NONTERM. *)
                       NFA_VARIANT:    (INITIAL,FINAL: NODEPTR); (* POINT TO INITIAL,FINAL NODES OF NFA *)
                       SET_VARIANT:    (SET_ELE: PACKED ARRAY[TERM_RANGE] OF BOOLEAN) (* TERMINAL SET *)
                 END;
$PAGE Variable Declarations
VAR 
     P: ARRAY[1..MAXPARSEST,METALBRACK..TERMINALHEAD] OF ELEMENTRANGE;  (* PARSING ACTION/GOTO TABLE *)
     LHS: ARRAY[-21..-2] OF 1..TERMINALHEAD;  (* LEFT HAND SIDES OF EACH PARSER PRODUCTION *)
     RHSLENGTH: ARRAY[-21..-2] OF 0..5;  (* LENGTH OF RIGHT HAND SIDE OF EACH PARSER PRODUCTION *)
     ST: 1..MAXPARSEST;
     LA: METALBRACK..TERMINALHEAD;
   
     T: RECORD   (* TERMINAL SYMBOL TABLE *)
           SYMMAX:   TERM_RANGE;
           SYMTABLE: ARRAY[TERM_RANGE] OF
                        RECORD
                           LENGTH:  0..MAXSYMLEN;
                           SYM:     PACKED ARRAY[1..MAXSYMLEN] OF CHAR
                        END
        END;
     
     S: RECORD   (* TERMINAL SET SYMBOL TABLE *)
           SYMMAX:   0..MAXTERSETS;
           SYMTABLE: ARRAY[1..MAXTERSETS] OF
                        RECORD
                           LENGTH:  0..MAXSYMLEN;
                           SYM:     PACKED ARRAY[1..MAXSYMLEN] OF CHAR;
                           DEFINED: BOOLEAN;
                           SET_ELE: PACKED ARRAY[TERM_RANGE] OF BOOLEAN
                        END
        END;
  
     N: RECORD   (* NONTERMINAL SYMBOL TABLE *)
           SYMMAX:   1..MAXNONTER;
           SYMTABLE: ARRAY[1..MAXNONTER] OF
                         RECORD
                           LENGTH:  0..MAXSYMLEN;
                           SYM:     PACKED ARRAY[1..MAXSYMLEN] OF CHAR;
                           INITIAL,           (* POINTER TO INITIAL NODE OF ASSOCIATED NFA *)
                           INTERMED,          (*    "    "  INTERMEDIATE-BACKUP NODE OF ASSOC. NFA *)
                           FINAL:   NODEPTR   (*    "    "  FINAL NODE OF ASSOCIATED NFA *)
                        END
        END;
  
     ERRORS:        ERRSTATUS;
     LAST_INT_BACK: 1..MAXIFSTATES;  (* ACTUAL INDEX OF LAST INTERMEDIATE-BACKUP STATE *)
     WHICH_NFA:     PACKED ARRAY[1..MAXIFSTATES] OF 1..MAXNONTER; (* CROSS REF. BACKUP STATES TO NFAS *)
     LAST_NFA:      1..MAXNONTER;    (* ACTUAL INDEX OF LAST FINAL STATE *)
     LAST_IBF:      1..MAXIFSTATES;  (* ACTUAL LAST IBF STATE *)
     LAST_SETSUB:   0..MAXSETSUB;    (* WILL BE LAST_IBF DIV SET_SIZE *)
     LAST_DFA:      DFASTATERANGE;   (* ACTUAL LAST DFA STATE *)
     DELTA:         ARRAY[DFASTATERANGE] OF ^DELTA_ROW;  (* TRANSITION TABLE FOR DFA *)
     DFASTATE:      ARRAY[DFASTATERANGE] OF IFSTATESET;  (* DFA STATES REPR. AS SETS OF IBF STATES *)
  
     WHICHBLOCK: ARRAY[DFASTATERANGE] OF BLOCKRANGE; (* CROSS REF. DFA STATES TO EQUIVALENCE CLASSES *)
     BLOCK: ARRAY[BLOCKRANGE] OF
               RECORD
                  ELEMENT_CNT: DFASTATERANGE; (* NO. OF MEMBERS IN BLOCK *)
                  FIRST_MEM,
                  LAST_MEM: ^MEMBER (* ^ CHAIN OF MEMBER DFA STATES IN THIS EQUIVALENCE CLASS *)
               END;
     LAST_BLOCK: BLOCKRANGE;  (* ACTUAL LAST EQUIVALENCE CLASS OF MINIMIZED DFA *)
     LA_FEATURE: BOOLEAN;     (* FLAG INDICATING USE OF THE LOOKAHEAD FEATURE *)
  
     BUFFER_LENGTH,
     BUFFER_POS:     0..BUFFERLEN;
     BUFFER:         PACKED ARRAY[1..BUFFERLEN] OF CHAR;
  
     NEWSYM_LENGTH:  0..MAXSYMLEN;
     NEWSYM:         PACKED ARRAY[1..MAXSYMLEN] OF CHAR;
     NEWSYM_TYPE:    METALBRACK..LAMBDA;
$PAGE SYMGET 
PROCEDURE SYMGET;   (* RETURN A SINGLE LEXEME FROM INPUT IN NEWSYM *)

   TYPE
      MSGRANGE = 1..4;

   VAR 
      TRUNCATED, LEXEME_FOUND: BOOLEAN;
      END_CHAR: CHAR;
      SCAN_STATE: (INIT, GOT_END_CHAR, END_OF_LINE, DONE);


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


   PROCEDURE ERROUT(MSGNO: MSGRANGE; MTYPE: ERRSTATUS);

      BEGIN
         WRITE('     *****      ');
         CASE MTYPE OF
            WARNING:
               BEGIN
                  IF ERRORS = OK THEN
                     ERRORS := WARNING;
                  WRITE('WARNING: ');
                  CASE MSGNO OF
                     1: WRITELN('SYMBOL TRUNCATED');
                     2: WRITELN('LINE IS SPLIT')
                  END
               END;
            RECOVER:
               BEGIN
                  IF ERRORS <> FATAL THEN
                     ERRORS := RECOVER;
                  CASE MSGNO OF
                     1: WRITELN('<> OR "" INVALID');
                     2: WRITELN('::= MISTYPED');
                     3: WRITELN('ILLEGAL CHARACTER ''',BUFFER[BUFFER_POS],'''');
                     4: WRITELN('SYMBOL CONTINUES PAST END OF LINE')
                  END
               END
         END
      END; (* ERROUT *)
$PAGE SYMGET 
   BEGIN (* SYMGET *)

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
               BUFFER[BUFFER_LENGTH] := CHR(127);   (* INTERNAL END OF LINE MARKER *)
               BUFFER_POS := 0;
               WRITELN('INPUT:   ',BUFFER:(BUFFER_LENGTH-1));
               IF NOT EOF(INPUT) THEN
                  IF EOLN(INPUT) THEN
                     READLN(INPUT)
                  ELSE
                     ERROUT(2,WARNING)
            UNTIL BUFFER[1] <> '*';   (* UNTIL NOT A COMMENT LINE *)

         NEWSYM := '********************';
         REPEAT 
            BUFFER_POS := BUFFER_POS + 1
         UNTIL BUFFER[BUFFER_POS] <> ' ';
  
         CASE BUFFER[BUFFER_POS] OF
            '"','''','<':
                 BEGIN (* LANGUAGE SYMBOLS *)
                    CASE BUFFER[BUFFER_POS] OF
                       '<':  BEGIN
                                END_CHAR := '>';
                                NEWSYM_TYPE := NONTERMINAL
                             END;
                       '"':  BEGIN
                                END_CHAR := '"';
                                NEWSYM_TYPE := SETNAME
                             END;

                       '''': BEGIN
                                END_CHAR := '''';
                                NEWSYM_TYPE := TERMINAL
                             END
                    END;
                    TRUNCATED := FALSE;
                    NEWSYM_LENGTH := 0;
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
                                     BUFFER_POS := BUFFER_POS - 1 (* BACK UP ONE CHAR *)
                                  END
                       END
                    UNTIL (SCAN_STATE = DONE) OR (SCAN_STATE = END_OF_LINE);

                    IF TRUNCATED THEN
                       ERROUT(1,WARNING);
                    IF SCAN_STATE = END_OF_LINE THEN
                       ERROUT(4,RECOVER)
                    ELSE IF (NEWSYM_TYPE <> TERMINAL) AND (NEWSYM_LENGTH = 0) THEN
                       ERROUT(1,RECOVER)
                    ELSE
                       LEXEME_FOUND := TRUE
                 END; (* LANGUAGE SYMBOLS *)
            ':':
                 BEGIN
                    BUFFER_POS := BUFFER_POS + 1;
                    IF BUFFER[BUFFER_POS] <> ':' THEN
                       ERROUT(2,RECOVER)
                    ELSE
                       BEGIN
                          BUFFER_POS := BUFFER_POS + 1;
                          IF BUFFER[BUFFER_POS] <> '=' THEN
                             ERROUT(2,RECOVER)
                          ELSE
                             BEGIN
                                NEWSYM_TYPE := METADEFINE;
                                LEXEME_FOUND := TRUE
                             END
                       END
                 END;
            '!':
                 BEGIN
                    NEWSYM_TYPE := METAOR;
                    LEXEME_FOUND := TRUE
                 END;
            ';':
                 BEGIN
                    NEWSYM_TYPE := METAEND;
                    LEXEME_FOUND := TRUE
                 END;
            CHR(127):
                 IF EOF(INPUT) THEN
                    BEGIN
                       NEWSYM_TYPE := EODATA;
                       LEXEME_FOUND := TRUE
                    END;
            ')':
                 BEGIN
                    NEWSYM_TYPE := METARPAREN;
                    LEXEME_FOUND := TRUE
                 END;
            '(':
                 BEGIN
                    NEWSYM_TYPE := METALPAREN;
                    LEXEME_FOUND := TRUE
                 END;
            '+':
                 BEGIN
                    NEWSYM_TYPE := METAPLUS;
                    LEXEME_FOUND := TRUE
                 END;
            '*':
                 BEGIN
                    NEWSYM_TYPE := METASTAR;
                    LEXEME_FOUND := TRUE
                 END;
            '[':
                 BEGIN
                    NEWSYM_TYPE := METALBRACK;
                    LEXEME_FOUND := TRUE
                 END;
            ']':
                 BEGIN
                    NEWSYM_TYPE := METARBRACK;
                    LEXEME_FOUND := TRUE
                 END;
            '/':
                 BEGIN
                    NEWSYM_TYPE := METASLASH;
                    LEXEME_FOUND := TRUE
                 END;
            OTHERS:
            ERROUT(3,RECOVER)
         END
      UNTIL LEXEME_FOUND

   END; (* SYMGET *)
$PAGE SEMANTICS declarations and local procedures
PROCEDURE SEMANTICS(VAR STACK: STACKTYPE;  ACTION: ELEMENTRANGE; 
                    TOS: STACKRANGE;  VAR PRINT_NFA: BOOLEAN); 

   TYPE 
      MSGRANGE = 1..3;
      TERM_OR_SET = MIN_TERM..MAXTERSETS;

   VAR 
      TEMP_INITIAL, TEMP_FINAL: NODEPTR;
      TSYM: TERM_RANGE;

   PROCEDURE NEWNODE(VAR N: NODEPTR);   

      BEGIN
         NEW(N,PTR);   (* ALLOCATE NODE WITH PTR VARIANT *)
         N^.ARC := LAMBDA;
         N^.PTR1 := NIL;
          N^.NODEFLAG := PTR;
         N^.SCANNED := NIL;
         N^.PTR2 := NIL
      END; (* NEWNODE *)


   PROCEDURE COPY(VAR I,F: NODEPTR; TI,TF: NODEPTR);    (* COPY THE SKELETAL NFA (TI,TF) TO (I,F)  *)

      BEGIN
         NEWNODE(I);
         I^.ARC := TI^.ARC;
         IF I^.ARC < UNION THEN   (* TERMINAL OR SET NFA *)
            BEGIN
               NEWNODE(F);
               I^.PTR1 := F;
               I^.PTR2 := NIL
            END
         ELSE IF I^.ARC = UNION THEN
            BEGIN
               NEWNODE(F);
               COPY(I^.PTR2, F^.PTR2, TI^.PTR2, TF^.PTR2);             (* DUPL. 2ND COMPONENT NFA *)
               COPY(I^.PTR1, F^.PTR2^.PTR1, TI^.PTR1, TF^.PTR2^.PTR1); (*  "    1ST     "      "  *)
               F^.PTR2^.PTR1^.PTR1 := F
            END
         ELSE IF I^.ARC = CONCAT THEN
            BEGIN
               COPY(I^.PTR1, I^.PTR2, TI^.PTR1, TI^.PTR2);   (* DUPL. 1ST COMPONENT NFA *)
               COPY(I^.PTR2^.PTR1, F, TI^.PTR2^.PTR1, TF)    (*  "    2ND     "      "  *)
            END
         ELSE
            BEGIN (* CLOSURE *)
               NEWNODE(F);
               COPY(I^.PTR1, F^.PTR2, TI^.PTR1, TF^.PTR2);   (* DUPL. COMPONENT NFA *)
               I^.PTR2 := F;
               F^.PTR2^.PTR1 := F
            END
      END; (* COPY *)
$PAGE SEMANTICS declarations and local procedures
   PROCEDURE ERROUT(MSGNO: MSGRANGE; MTYPE: ERRSTATUS);

      BEGIN
         WRITE('     *****      ');
         CASE MTYPE OF
            WARNING:
               BEGIN
                  IF ERRORS = OK THEN
                     ERRORS := WARNING;
                  WRITE('WARNING: ');
                  CASE MSGNO OF
                     1: WITH N.SYMTABLE[STACK[TOS].SYMBOL] DO
                           WRITELN('<',SYM:LENGTH,'> REASSIGNED');
                     2: WRITELN('DUPLICATE TOKEN');
                     3: WITH S.SYMTABLE[STACK[TOS].SYMBOL] DO
                           WRITELN('"',SYM:LENGTH,'" REASSIGNED')
                  END
               END;
            RECOVER:
               BEGIN
                  IF ERRORS <> FATAL THEN
                     ERRORS := RECOVER;
                  CASE MSGNO OF
                     1: WITH N.SYMTABLE[STACK[TOS].SYMBOL] DO
                           WRITE('<',SYM:LENGTH,'>');
                     2: WITH S.SYMTABLE[STACK[TOS].SYMBOL] DO
                           WRITE('"',SYM:LENGTH,'"')
                  END;
                  WRITELN(' IS UNDEFINED - WILL USE NULL STRING NFA AND CONTINUE')
               END
         END
      END; (* ERROUT *)


   PROCEDURE PRINTTERNFA(SYMBOL: TERM_OR_SET);    (* PRINT TERMINAL OR TERMINAL SET NFA *)

      BEGIN
         WRITELN;
         WRITELN;
         IF SYMBOL <= LAMBDA THEN    (* TERMINAL (OR LAMBDA) ? *)
            WITH T.SYMTABLE[SYMBOL] DO
               WRITELN(' ':21,'''',SYM:LENGTH,'''')
         ELSE    (* SET *)
            WITH S.SYMTABLE[SYMBOL] DO
               WRITELN(' ':21,'"',SYM:LENGTH,'"');
         WRITELN(TOS:3,':      O-------------------------------->(O)')
      END; (* PRINTTERNFA *)

   PROCEDURE PUNION; (* PRINT UNION NFA *)

      BEGIN
         WRITELN;
         WRITELN;
         WRITELN('                    ___________');
         WRITELN('                   /           \');
         WRITELN('                  /             \');
         WRITELN('               ->( 0    ',TOS:3,'    0 )____');
         WRITELN('              /   \             /     \');
         WRITELN('             /     \___________/       \');
         WRITELN('            /                           !');
         WRITELN('           /                            V');
         WRITELN(TOS:3,':      O                            (O)');
         WRITELN('           \                           /\');
         WRITELN('            \       ___________        /');
         WRITELN('             \     /           \      /');
         WRITELN('              \   /             \    /');
         WRITELN('               ->( 0    ',TOS+2:3,'    0 )__/');
         WRITELN('                  \             /');
         WRITELN('                   \___________/')
      END; (* PUNION *)

    PROCEDURE PCLOSURE; (* PRINT CLOSURE NFA *)

      BEGIN
         WRITELN;
         WRITELN;
         WRITELN('                    ____________');
         WRITELN('                   /            \');
         WRITELN('                  !              \');
         WRITELN('                  ! ___________   !');
         WRITELN('                  V/           \  !');
         WRITELN('                  /             \/');
         WRITELN('               ->( 0    ',TOS:3,'    0 )____');
         WRITELN('              /   \             /     \');
         WRITELN('             /     \___________/       \');
         WRITELN('            /                           !');
         WRITELN('           /                            V');
         WRITELN(TOS:3,':      O                            (O)');

         IF ACTION = -10  (* KLEENE CLOSURE *) THEN
            BEGIN
               WRITELN('           \                           /\');
               WRITELN('            \                          /');
               WRITELN('             \________________________/')
             END
      END; (* PCLOSURE *)

   PROCEDURE PCONCAT; (* PRINT CONCATENATION NFA *)

      BEGIN
         WRITELN;
         WRITELN;
         WRITELN('                  ___________            ___________');
         WRITELN('                 /           \          /           \');
         WRITELN('                /             \        /             \');
         WRITELN(TOS:3,':      O--->( 0    ',TOS:3,'    0 )----->( 0   ',TOS+1:3,'   (O) )');
         WRITELN('                \             /        \             /');
         WRITELN('                 \___________/          \___________/')
      END; (* PCONCAT *)
$PAGE SEMANTICS body
   BEGIN (* SEMANTICS *)

      CASE ACTION OF
  
         -3, (* <TOKENS> ::= 'NONTERMINAL' *)
         -4: (* <TOKENS> ::= <TOKENS> 'NONTERMINAL' *)
  
             BEGIN
                IF LAST_NFA+1 <> N.SYMMAX THEN  (* IF NONTERMINAL IS NEW, "FIND" JUST PUT IT IN TABLE *)
                   ERROUT(2,WARNING);
                LAST_NFA := N.SYMMAX
             END;
 
  
         -7: (* <PRODUCTION> ::= 'NONTERMINAL' '::=' <EXPRESSION> *)
  
             WITH N.SYMTABLE[STACK[TOS].SYMBOL] DO
                BEGIN
                   IF INITIAL <> NIL THEN   (* ALREADY NFA ASSOCIATED WITH NONTERMINAL? *)
                      ERROUT(1,WARNING);
                   INITIAL := STACK[TOS+2].INITIAL;   (* IDENTIFY NFA WITH NONTERMINAL *)
                   FINAL := STACK[TOS+2].FINAL;
                   INTERMED := NIL;
                   IF PRINT_NFA THEN
                      BEGIN
                         WRITELN;
                         WRITELN;
                         WRITELN('   <',SYM:LENGTH,'> ******* IS ASSIGNED THE NFA ****',TOS+2:3)
                      END
                END;

  
         -8: (* <PRODUCTION> ::= 'NONTERMINAL' '::=' <EXPRESSION> '/' <EXPRESSION> *)
  
             WITH N.SYMTABLE[STACK[TOS].SYMBOL] DO
                BEGIN
                   IF INITIAL <> NIL THEN   (* ALREADY NFA ASSOCIATED WITH NONTERMINAL? *)
                      ERROUT(1,WARNING);

                   (* CONCAT. THE TWO NFAS AND IDENTIFY THE RESULTING LOOKAHEAD NFA WITH NONTERMINAL *)
   
                   INTERMED := STACK[TOS+2].FINAL;  (* TOKEN ACTUALLY ENDS WITH END OF EXPR1 *)
                   NEWNODE(INITIAL);                (* INITIAL NODE OF CONCATENATION NFA *)
                   INITIAL^.ARC := CONCAT;
                   INITIAL^.PTR1 := STACK[TOS+2].INITIAL; (* ARC TO EXPR1 NFA'S INITIAL NODE *)
                   INITIAL^.PTR2 := INTERMED;       (* CONNECT NEW INITIAL TO EXPR1 FINAL (FOR COPYING) *)
                   STACK[TOS+2].FINAL^.PTR1 := STACK[TOS+4].INITIAL; (* FROM TOKEN NFA TO LOOKAHEAD NFA *)
                   FINAL := STACK[TOS+4].FINAL;
                   IF PRINT_NFA THEN
                      BEGIN
                         WRITELN;
                         WRITELN;
                         WRITELN('   <',SYM:LENGTH,'> **** IS ASSIGNED THE NFA ****',TOS+2:3,' //',TOS+4:3)
                      END
                END;

         -9: (* <PRODUCTION> ::= 'SETNAME' '::=' <TERMINALSET> *)
  
            WITH S.SYMTABLE[STACK[TOS].SYMBOL] DO
               BEGIN
                  IF DEFINED THEN     (* ALREADY SET ASSOCIATED WITH SETNAME? *)
                     ERROUT(3,WARNING);
                  FOR TSYM := MIN_TERM TO LAMBDA DO
                     SET_ELE[TSYM] := STACK[TOS+2].SET_ELE[TSYM];
                  DEFINED := TRUE
               END;

  
  
         -10, (* <EXPRESSION> ::= <EXPRESSION> '*' *)
         -11: (* <EXPRESSION> ::= <EXPRESSION> '+' *)
  
             BEGIN
                NEWNODE(TEMP_INITIAL);                       (* INITIAL NODE OF CLOSURE NFA *)
                NEWNODE(TEMP_FINAL);                         (* FINAL    "   "     "    "   *)
                WITH TEMP_INITIAL^ DO
                   BEGIN
                      IF ACTION = -10 THEN
                         ARC := KLEENE_CLO
                      ELSE
                         ARC := POSITIVE_CLO;
                      PTR1 := STACK[TOS].INITIAL;            (* ARC FROM NEW INITIAL TO OLD INIT. NODE *)
                      TEMP_FINAL^.PTR2 := STACK[TOS].FINAL;  (* CONNECT NEW FINAL TO OLD (FOR COPYING) *)
                      PTR2 := TEMP_FINAL;                    (* ARC FROM INITIAL TO FINAL NODE *)
                      STACK[TOS].FINAL^.PTR1 := TEMP_FINAL;  (* ARC FROM OLD FINAL TO NEW FINAL NODE *)
                      STACK[TOS].INITIAL := TEMP_INITIAL;    (* IDENTIFY NEW NFA WITH EXPRESSION *)
                      STACK[TOS].FINAL := TEMP_FINAL;        (*    "      "   "    "     "       *)
                      IF PRINT_NFA THEN
                         PCLOSURE
                   END
             END;

  
  
         -12: (* <EXPRESSION> ::= <EXPRESSION><EXPRESSION> *)
  
             BEGIN
                NEWNODE(TEMP_INITIAL);                          (* INITIAL NODE OF CONCATENATION NFA *)
                TEMP_INITIAL^.ARC := CONCAT;
                TEMP_INITIAL^.PTR1 := STACK[TOS].INITIAL;       (* ARC TO EXPR1 NFA'S INITIAL NODE *)
                TEMP_INITIAL^.PTR2 := STACK[TOS].FINAL; (* CONN. NEW INIT. TO EXPR1 FINAL (FOR COPYING)*)
                STACK[TOS].FINAL^.PTR1 := STACK[TOS+1].INITIAL; (* ARC FROM EXPR1 FINAL TO EXPR2 INIT. *)
                STACK[TOS].INITIAL := TEMP_INITIAL;             (* IDENTIFY NEW NFA WITH EXPRESSION *)
                STACK[TOS].FINAL := STACK[TOS+1].FINAL;         (*    "      "   "   "       "      *)
                IF PRINT_NFA THEN
                   PCONCAT
             END;
$PAGE SEMANTICS body
         -14: (* <EXPRESSION> ::= 'NONTERMINAL' *)
  
             WITH N.SYMTABLE[STACK[TOS].SYMBOL] DO
                BEGIN
                   STACK[TOS].ST_VAR := NFA_VARIANT;
                   IF INITIAL = NIL THEN   (* DEFINED? *)
                      BEGIN   (* NO - RECOVER BY ASSUMING NULL STRING NFA *)
                         ERROUT(1,RECOVER);
                         NEWNODE(STACK[TOS].INITIAL);
                         NEWNODE(STACK[TOS].FINAL);
                         STACK[TOS].INITIAL^.PTR1 := STACK[TOS].FINAL;
                         IF PRINT_NFA THEN
                            PRINTTERNFA(LAMBDA)
                      END
                   ELSE
                      BEGIN   (* YES - COPY NFA FROM NONTERMINAL *)
                         COPY(STACK[TOS].INITIAL,STACK[TOS].FINAL,INITIAL,FINAL);
                          IF PRINT_NFA THEN
                            BEGIN
                               WRITELN;
                               WRITELN;
                               WRITELN(TOS:3,': ****** IS COPIED FROM ***** <',SYM:LENGTH,'>')
                            END
                      END
                END;

  
         -15: (* <EXPRESSION> ::= 'SETNAME' *)
  
             BEGIN
                NEWNODE(TEMP_INITIAL);                (* INITIAL NODE OF NFA FOR SET *)
                NEWNODE(TEMP_FINAL);                  (* FINAL    "   "   "   "   "  *)
                TEMP_INITIAL^.PTR1 := TEMP_FINAL;     (* ARC FROM INITIAL TO FINAL NODE *)
                IF NOT S.SYMTABLE[STACK[TOS].SYMBOL].DEFINED THEN   (* SET PREVIOUSLY DEFINED? *)
                   BEGIN                              (* NO - RECOVER BY ASSUMING NULL STRING NFA *)
                      ERROUT(2,RECOVER);
                      IF PRINT_NFA THEN
                         PRINTTERNFA(LAMBDA)
                   END
                ELSE
                   BEGIN
                      TEMP_INITIAL^.ARC := STACK[TOS].SYMBOL; (* LABEL ARC WITH SETNAME (CODES>0) *)
                      IF PRINT_NFA THEN
                         PRINTTERNFA(STACK[TOS].SYMBOL)
                   END;
                STACK[TOS].ST_VAR := NFA_VARIANT;
                STACK[TOS].INITIAL := TEMP_INITIAL;   (* IDENTIFY NEW NFA WITH EXPRESSION *)
                STACK[TOS].FINAL := TEMP_FINAL        (*    "      "   "    "      "      *)
             END;
$PAGE SEMANTICS body
         -16: (* <EXPRESSION> ::= '(' <EXPRESSION> ')' *)
  
             BEGIN
                STACK[TOS].ST_VAR := NFA_VARIANT;
                STACK[TOS].INITIAL :=  STACK[TOS+1].INITIAL;
                STACK[TOS].FINAL := STACK[TOS+1].FINAL;
                IF PRINT_NFA THEN
                   BEGIN
                      WRITELN;
                      WRITELN;
                      WRITELN(TOS:3,': ******* IS THE NEW LABEL FOR ****',TOS+1:3)
                   END
             END;

  
  
         -17: (* <EXPRESSION> ::= <EXPRESSION> '!' <EXPRESSION> *)
   
             BEGIN
                NEWNODE(TEMP_INITIAL);                  (* INITIAL NODE OF UNION NFA *)
                TEMP_INITIAL^.ARC := UNION;
                TEMP_INITIAL^.PTR1 := STACK[TOS].INITIAL;   (* ARC TO EXPR1 NFA'S INITIAL NODE *)
                TEMP_INITIAL^.PTR2 := STACK[TOS+2].INITIAL; (*  "   " EXPR2   "      "     "   *)
                NEWNODE(TEMP_FINAL);                    (* FINAL NODE OF UNION NFA *)
                STACK[TOS].FINAL^.PTR1 := TEMP_FINAL;   (* ARC FROM EXPR1 FINAL TO NEW FINAL NODE *)
                TEMP_FINAL^.PTR2 := STACK[TOS+2].FINAL; (* CONNECT NEW FINAL TO EXPR2 FINAL(FOR COPYING)*)
                STACK[TOS+2].FINAL^.PTR1 := STACK[TOS].FINAL; (* CONN. EXPR2 FINAL TO EXPR1(FOR COPYING)*)
                STACK[TOS].INITIAL := TEMP_INITIAL;     (* IDENTIFY NEW NFA WITH EXPRESSION *)
                STACK[TOS].FINAL :=  TEMP_FINAL;        (*     "     "   "   "        "     *)
                IF PRINT_NFA THEN
                   PUNION
             END;

  
  
         -18, (* <EXPRESSION> ::= '' *)
         -13: (* <EXPRESSION> ::= 'TERMINAL' *)
  
             BEGIN
                TSYM := STACK[TOS].SYMBOL;              (* SAVE THE SYMBOL FROM RHS *)
                STACK[TOS].ST_VAR := NFA_VARIANT;
                NEWNODE(STACK[TOS].INITIAL);            (* INITIAL NODE OF NFA *)
                WITH STACK[TOS].INITIAL^ DO
                   BEGIN
                      ARC := TSYM;                      (* LABEL ARC WITH TERMINAL (CODES<=0) *)
                      NEWNODE(STACK[TOS].FINAL);        (* FINAL NODE OF NFA *)
                      PTR1 := STACK[TOS].FINAL;         (* ARC FROM INITIAL TO FINAL NODE *)
                      IF PRINT_NFA THEN
                         PRINTTERNFA(ARC)
                   END
             END;
$PAGE SEMANTICS body
         -20: (* <TERMINALHEAD> ::= '[' *)
  
             BEGIN
                STACK[TOS].ST_VAR := SET_VARIANT;
                FOR TSYM := MIN_TERM TO LAMBDA DO   (* INITIALIZE SET TO BE EMPTY *)
                   STACK[TOS].SET_ELE[TSYM] := FALSE
             END;

  
   
         -21: (* <TERMINALHEAD> ::= <TERMINALHEAD> 'TERMINAL' *)
  
             STACK[TOS].SET_ELE[STACK[TOS+1].SYMBOL] := TRUE;   (* ADD TERMINAL TO SET *)

  
  
         OTHERS: (* -2: <COMPLETE> ::= <TOKENS> ';' <PRODSET>
                    -5: <PRODSET> ::= <PRODUCTION>
                    -6: <PRODSET> ::= <PRODSET> ';' <PRODUCTION>
                   -19: <TERMINALSET> ::= <TERMINALHEAD> ']'     *)

                         (*  --- NO SEMANTICS ---  *)
  
      END (* CASE ACTION *)

   END; (* SEMANTICS *)
$PAGE INPUTG declarations and local procedures
PROCEDURE INPUTG;     (* PARSE INPUT AND CREATE NFA'S *)
  
   CONST 
      ACCEPT     =  -1;   (* CODE FOR ACCEPT ACTION *)
      ERRORSTATE =   0;   (* CODE FOR PARSING ERROR *)
      LR0REDUCE  =-500;   (* ADDEND TO INDICATE SPECIAL CODE FOR REPLACED LR0-REDUCE STATE *)
      SHIFT      =   0;   (* CODE FOR SHIFT *)
      EOD_TERM   =  -1;   (* CODE FOR END-OF-DATA TERMINAL *)

   VAR 
      STACK:        STACKTYPE;
      INPUT_FILE, OUTPUT_FILE: STRING[30];
      CURRENTSTATE: 1..MAXPARSEST;
      TOS:          STACKRANGE;
      ACTION, OLDACTION: ELEMENTRANGE;
      PRINT_NFA:    BOOLEAN;
      CURR_NFA:     1..MAXNONTER;
      TSYM:         TERM_RANGE;


   PROCEDURE NEWNODE(VAR N: NODEPTR);

      BEGIN
         NEW(N,PTR);   (* ALLOCATE NODE WITH PTR VARIANT *)
         N^.ARC := LAMBDA;
         N^.PTR1 := NIL;
         N^.NODEFLAG := PTR;
         N^.PTR2 := NIL;
         N^.SCANNED := NIL
      END; (* NEWNODE *)


   PROCEDURE FIND;   
 
      PROCEDURE TABLEFULL;

         BEGIN
            ERRORS := FATAL;
            CASE NEWSYM_TYPE OF
               TERMINAL:    WRITE(TTYOUTPUT,'TERMINAL');
               SETNAME:     WRITE(TTYOUTPUT,'SETNAME');
               NONTERMINAL: WRITE(TTYOUTPUT,'NONTERMINAL')
            END;
            WRITELN(TTYOUTPUT,' SYMBOL TABLE OVERFLOW');
            STOP
         END; (* TABLEFULL *)


      BEGIN (* FIND *)
  
         STACK[TOS].ST_VAR := SYMBOL_VARIANT;
         CASE NEWSYM_TYPE OF
            TERMINAL:
               WITH T DO
                  BEGIN
                     STACK[TOS].SYMBOL := 0;
                     SYMTABLE[SYMMAX-1].LENGTH := NEWSYM_LENGTH;   (* INSERT SYMBOL AT END OF TABLE *)
                     SYMTABLE[SYMMAX-1].SYM := NEWSYM;
                     WHILE (SYMTABLE[STACK[TOS].SYMBOL].LENGTH <> NEWSYM_LENGTH) OR   (* SEARCH TABLE *)
                           (SYMTABLE[STACK[TOS].SYMBOL].SYM <> NEWSYM) DO
                        STACK[TOS].SYMBOL := STACK[TOS].SYMBOL - 1;
                     IF STACK[TOS].SYMBOL < SYMMAX THEN   (* IT WASN'T IN TABLE? *)
                        IF SYMMAX > MIN_TERM THEN
                           SYMMAX := SYMMAX - 1
                        ELSE
                           TABLEFULL   (* NO MORE ROOM *)
                  END;

            SETNAME:
               WITH S DO
                  BEGIN
                     STACK[TOS].SYMBOL := 1;
                     SYMTABLE[SYMMAX+1].LENGTH := NEWSYM_LENGTH;   (* INSERT SYMBOL AT END OF TABLE *)
                     SYMTABLE[SYMMAX+1].SYM := NEWSYM;
                     WHILE (SYMTABLE[STACK[TOS].SYMBOL].LENGTH <> NEWSYM_LENGTH) OR   (* SEARCH TABLE *)
                           (SYMTABLE[STACK[TOS].SYMBOL].SYM <> NEWSYM) DO
                        STACK[TOS].SYMBOL := STACK[TOS].SYMBOL + 1;
                     IF STACK[TOS].SYMBOL > SYMMAX THEN   (* IT WASN'T IN TABLE? *)
                        IF SYMMAX < MAXTERSETS THEN
                           BEGIN
                              SYMMAX := SYMMAX + 1;
                              SYMTABLE[SYMMAX].DEFINED := FALSE
                           END
                        ELSE
                           TABLEFULL   (* NO MORE ROOM *)
                  END;

            NONTERMINAL:
               WITH N DO
                  BEGIN
                     STACK[TOS].SYMBOL := 1;
                     SYMTABLE[SYMMAX+1].LENGTH := NEWSYM_LENGTH;   (* INSERT SYMBOL AT END OF TABLE *)
                     SYMTABLE[SYMMAX+1].SYM := NEWSYM;
                     WHILE (SYMTABLE[STACK[TOS].SYMBOL].LENGTH <> NEWSYM_LENGTH) OR   (* SEARCH TABLE *)
                            (SYMTABLE[STACK[TOS].SYMBOL].SYM <> NEWSYM) DO
                        STACK[TOS].SYMBOL := STACK[TOS].SYMBOL + 1;
                     IF STACK[TOS].SYMBOL > SYMMAX THEN   (* IT WASN'T IN TABLE? *)
                        IF SYMMAX < MAXNONTER THEN
                           BEGIN
                              SYMMAX := SYMMAX + 1;
                              SYMTABLE[SYMMAX].INITIAL := NIL;
                              SYMTABLE[SYMMAX].INTERMED := NIL;
                              SYMTABLE[SYMMAX].FINAL := NIL
                           END
                        ELSE
                           TABLEFULL   (* NO MORE ROOM *)
                  END
         END
  
      END; (* FIND *)
$PAGE INPUTG declarations and local procedures
   PROCEDURE PUSHSYMBOL( ACTION: ELEMENTRANGE);    (* PUSH SYMBOL ONTO PARSER STACK *)

      BEGIN
         TOS := TOS + 1;
         IF TOS > MAXSTACK THEN
            BEGIN
               WRITELN(TTYOUTPUT,'PARSER STACK OVERFLOW');
               STOP
            END;
         IF ACTION = SHIFT THEN
            BEGIN
               IF (NEWSYM_TYPE = TERMINAL)
                 OR (NEWSYM_TYPE = NONTERMINAL)
                   OR (NEWSYM_TYPE = SETNAME) THEN
                  FIND   (* PUT SYMBOL TABLE INDEX OF NEW SYMBOL ONTO STACK *)
            END
         ELSE
            SEMANTICS(STACK,ACTION,TOS,PRINT_NFA)
      END; (* PUSHSYMBOL *)


   PROCEDURE PRINTNODE(I: NODEPTR);    (* PRINT A NODE OF NFA *)

      BEGIN
         WRITE(ORD(I): 10: O,':');
         IF I^.ARC > LAMBDA THEN
            WITH S.SYMTABLE[I^.ARC] DO
               WRITE('"',SYM:LENGTH,'"',' ':MAXSYMLEN-LENGTH+2)
         ELSE
            WITH T.SYMTABLE[I^.ARC] DO
               WRITE('''',SYM:LENGTH,'''',' ':MAXSYMLEN-LENGTH+2);
         IF I^.PTR1 = NIL THEN
            WRITE('  - NIL -   ')
         ELSE
            WRITE(ORD(I^.PTR1): 10: O,'  ');
         IF I^.PTR2 = NIL THEN
            WRITELN('  - NIL -  ')
         ELSE
            WRITELN(ORD(I^.PTR2): 10: O)
      END; (* PRINTNODE *)


   PROCEDURE FIX(VAR I,F: NODEPTR);    (* FIX UP NFA:  CONVERT NFA FROM SKELETAL FORM TO FINAL FORM *)

      BEGIN
         IF I^.ARC = UNION THEN
            BEGIN
               FIX(I^.PTR1,F^.PTR2^.PTR1);
               IF PRINT_NFA THEN
                  PRINTNODE(F^.PTR2^.PTR1);
               FIX(I^.PTR2,F^.PTR2);
               F^.PTR2^.PTR1 := F;
               IF PRINT_NFA THEN
                  PRINTNODE(F^.PTR2);
               F^.PTR2 := NIL;
               I^.ARC := LAMBDA   (* REPLACE "SPECIAL" LABEL WITH PROPER ONE *)
            END

         ELSE IF I^.ARC = CONCAT THEN
            BEGIN
               FIX(I^.PTR2^.PTR1,F);
               FIX(I^.PTR1,I^.PTR2);
               IF PRINT_NFA THEN
                  PRINTNODE(I^.PTR2);
               I^.ARC := LAMBDA;   (* REPLACE "SPECIAL" LABEL WITH PROPER ONE *)
               I^.PTR2 := NIL
            END

         ELSE IF (I^.ARC = KLEENE_CLO) OR (I^.ARC = POSITIVE_CLO) THEN
            BEGIN
               FIX(I^.PTR1,F^.PTR2);
               F^.PTR2^.PTR2 := I^.PTR1;
               IF PRINT_NFA THEN
                  PRINTNODE(F^.PTR2);
               F^.PTR2 := NIL;
               IF I^.ARC = POSITIVE_CLO THEN
                  I^.PTR2 := NIL;
               I^.ARC := LAMBDA   (* REPLACE "SPECIAL" LABEL WITH PROPER ONE *)
            END;
  
         IF PRINT_NFA THEN
            PRINTNODE(I)
      END; (* FIX *)
$PAGE INPUTG body
   BEGIN (* INPUTG *)

      (* QUERY USER *)

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
  
  
      WRITE(TTYOUTPUT,'PRINT NFA GRAPH(S)? ');
      BREAK(TTYOUTPUT);
      READLN(TTY);
      PRINT_NFA := (TTY^ = 'Y') OR (TTY^ = 'y');
  
  
      ERRORS := OK;
      BUFFER_LENGTH := 0;
       BUFFER_POS := 0; (* FORCE INITIAL READ *)
$PAGE INPUTG body
      (* INITIALIZE NONTERMINAL SYMBOL TABLE AND CREATE NFA FOR EOD *)

      N.SYMMAX := 1;                    (* TABLE STARTS WITH EOD *)
      WITH N.SYMTABLE[1] DO
         BEGIN
            LENGTH := MAXSYMLEN;
            SYM := '*****END OF DATA****';
            NEWNODE(INITIAL);           (* INITIAL NODE OF NFA FOR EOD *)
            NEWNODE(FINAL);             (* FINAL     "  "   "   "   "  *)
            INITIAL^.PTR1 := FINAL;     (* ARC FROM INITIAL TO FINAL NODE *)
            INITIAL^.ARC := EOD_TERM;   (* LABEL ARC *)
            INTERMED := NIL
         END;

  
      (* INITIALIZE TERMINAL SYMBOL TABLE *)

      T.SYMMAX := EOD_TERM;             (* TABLE STARTS WITH LAMBDA AND EOD *)
      T.SYMTABLE[LAMBDA].LENGTH := 0;
      T.SYMTABLE[LAMBDA].SYM := '********************';
  
      T.SYMTABLE[EOD_TERM].LENGTH := MAXSYMLEN;
      T.SYMTABLE[EOD_TERM].SYM := '*****END OF DATA****';

  
      (* INITIALIZE TERMINAL SETNAME SYMBOL TABLE *)

       S.SYMMAX := 0;                    (* TABLE IS EMPTY *)
$PAGE INPUTG body
      (* PARSE INPUT.  (see appendix B of LALR1 User Manual) *)

      TOS := 1;   (* INITIAL TOP OF STACK *)
      CURRENTSTATE := 1;
      LAST_NFA := 1; (* INITIALIZE *)
      SYMGET;

      REPEAT
         STACK[TOS].STACKSTATE := CURRENTSTATE;   (* PUSH CURRENT STATE ONTO PARSER STACK *)
         ACTION := P[CURRENTSTATE,NEWSYM_TYPE];

         (* SHIFT *)
  
         IF ACTION > ERRORSTATE THEN
            BEGIN
               PUSHSYMBOL(SHIFT);
               CURRENTSTATE := ACTION;
               SYMGET
            END

         (* REDUCE *)
  
         ELSE
            IF ACTION < ACCEPT THEN
               BEGIN
                  IF ACTION < LR0REDUCE THEN          (* ELIMINATED LR0 REDUCE? *)
                     BEGIN 
                        PUSHSYMBOL(SHIFT);
                        SYMGET;
                        ACTION := ACTION - LR0REDUCE  (* MODIFY TO PROD. # *)
                     END;
 
                  REPEAT
                     TOS := TOS - RHSLENGTH[ACTION];  (* POP PARSER STACK *)
                     OLDACTION := ACTION;
                     ACTION := P[STACK[TOS].STACKSTATE,LHS[ACTION]];
                     PUSHSYMBOL(OLDACTION)
                  UNTIL  ACTION >= ACCEPT;
                  IF ACTION >= 1 THEN
                     CURRENTSTATE := ACTION
               END;

         (* ERROR *)
  
         IF ACTION = ERRORSTATE THEN
            BEGIN
               WRITELN('*****SYNTAX ERROR:WILL FLUSH TO NEXT ";" OR EOF AND CONTINUE');
               IF ERRORS <> FATAL THEN
                  ERRORS := RECOVER;
               IF NEWSYM_TYPE <> EODATA THEN
                  BEGIN
                     WHILE (NEWSYM_TYPE <> EODATA) AND (NEWSYM_TYPE <> METAEND) DO
                        SYMGET;
                     IF NEWSYM_TYPE = METAEND THEN
                        BEGIN
                           SYMGET (* GET SYMBOL AFTER *);
                           ACTION := 1 (* DUMMY UP FOR CONTINUING *) 
                        END;

                     (* REINITIALIZE STACK AND CURRENTSTATE *)
                     STACK[2].STACKSTATE := 2;
                     TOS := 3;
                     CURRENTSTATE := 3
                  END
            END
      UNTIL (ACTION = ERRORSTATE) OR (ACTION = ACCEPT);
  
  

      (* CONVERT SKELETAL NFA'S TO FINAL FORM AND (OPTIONALLY) PRINT THEM *)

      FOR CURR_NFA := 1 TO LAST_NFA DO
         WITH N.SYMTABLE[ CURR_NFA ] DO
            IF INITIAL <> NIL THEN
                BEGIN
                  IF PRINT_NFA THEN
                     BEGIN
                        WRITELN;
                        WRITELN;
                        WRITELN('*** NFA <',SYM:LENGTH,'>:');
                        WRITELN
                     END;
                  FIX(INITIAL,FINAL);
                  IF PRINT_NFA THEN
                     PRINTNODE(FINAL)
               END
            ELSE
               WRITELN('     *****     WARNING: NO NFA <',SYM:LENGTH,'>');


      (*  PRINT CODES FOR TERMINALS,  AND FOR NONTERMINALS TO BE RECOGNIZED *)

      PAGE;
      WRITELN('****** CODES FOR TERMINAL SYMBOLS ******');
      WRITELN;
      WRITELN('          CODE   SYMBOL');
      FOR TSYM := T.SYMMAX TO -1 DO
         WITH T.SYMTABLE[TSYM] DO
            WRITELN('          ',TSYM:3,'   ''',SYM:LENGTH,'''');

      WRITELN;
      WRITELN;
      WRITELN('****** CODES FOR NONTERMINALS TO BE RECOGNIZED ******');
      WRITELN;
      WRITELN('          CODE   NONTERMINAL');
       FOR CURR_NFA := 1 TO LAST_NFA DO
         WITH N.SYMTABLE[CURR_NFA] DO
            WRITELN('          ',CURR_NFA:3,'   ''',SYM:LENGTH,'''')
   END; (* INPUTG *)
$PAGE CREATE_DFA declarations and local procedures
PROCEDURE CREATE_DFA;
  
   TYPE 
      QUEUENODE = RECORD   (* QUEUE TO SAVE IMPORTANT STATES FOR LATER PROCESSING TO CREATE NEXTIF *)
                     IMP:    NODEPTR;        (* ^ IMPORTANT STATE *)
                     IFCODE: 1..MAXIFSTATES; (* INDEX OF IMPORTANT STATE *)
                     NEXT:   ^QUEUENODE
                  END;

   VAR 
      TDFASTATE: ARRAY[TERM_RANGE] OF IFSTATESET;  (* TEMPORARY DFA STATES FOR EACH POSSIBLE TRANSITION *)
      NOTNULL:   ARRAY[TERM_RANGE] OF BOOLEAN;     (* INDICATES IF TRANSITION EXISTS *)
  
      (* NEXTIF[X] HAS NEXTSET = SET OF ALL IBF STATES REACHABLE FROM X UNDER A TRANSITION VIA SYMBOL.
                   (NOTE THAT EACH NFA NODE HAS AT MOST ONE NON-LAMBDA EXITING ARC DUE TO THE
                    CONSTRUCTION METHOD UTILIZING LAMBDA TRANSITIONS)                                *)
  
      NEXTIF: ARRAY[1..MAXIFSTATES] OF
                 RECORD
                    SYMBOL:  MIN_TERM..MAXTERSETS;
                    NEXTSET: IFSTATESET
                 END;

      TSYM:         MIN_TERM..-1;
      NSYM:         1..MAXNONTER;
      CURRDFA,
      NEXTDFA:      DFASTATERANGE;
      IFSTATE:      1..MAXIFSTATES;
      FINALFOUND:   BOOLEAN;
      HEADQUEUE,
      TAILQUEUE,
      QUEUE:        ^QUEUENODE;
      OLDI:         NODEPTR;
      SETSUB:       0..MAXSETSUB;
 


   PROCEDURE CREATENEXTIF(VAR I: NODEPTR; VAR IFF: IFSTATESET);
  
      (* FIND ALL IBF STATES REACHABLE UNDER LAMBDA TRANSITION FROM I AND ADD TO IFF *)

      BEGIN 
         IF I <> NIL THEN
            CASE I^.NODEFLAG OF
               PTR:
                    IF I^.ARC <> LAMBDA THEN
                       BEGIN (* IMPORTANT (I.E. HAS NON-LAMBDA EXITING ARC) NOT YET MARKED *)
                          I^.NODEFLAG := IMPOR_FLAG;
                          IF LAST_IBF = MAXIFSTATES THEN
                             BEGIN
                                WRITELN(TTY,'TOO MANY I-F STATES');
                                STOP
                             END;
                          LAST_IBF := LAST_IBF + 1;
                          I^.IFCODE := LAST_IBF;
                          SETSUB := I^.IFCODE DIV SET_SIZE;
                          IFF[SETSUB] := IFF[SETSUB] + [I^.IFCODE MOD SET_SIZE];   (* PUT INTO SET *)

                          WITH NEXTIF[I^.IFCODE] DO
                             BEGIN
                                SYMBOL := I^.ARC;
                                IF SYMBOL > LAMBDA THEN   (* TERMINAL SET? *)
                                   IF S.SYMTABLE[SYMBOL].SET_ELE[LAMBDA] THEN  (* LAMBDA IN SET? *)
                                      CREATENEXTIF(I^.PTR1,IFF)
                             END;
                          NEW(TAILQUEUE^.NEXT);
                          TAILQUEUE := TAILQUEUE^.NEXT;
                          TAILQUEUE^.IMP := I^.PTR1;   (* ADD I^.PTR1 TO QUEUE OF NODES WHOSE ARCS
                                                          HAVEN'T BEEN EXPLORED YET                *)
                          TAILQUEUE^.IFCODE := I^.IFCODE;
                          TAILQUEUE^.NEXT := NIL
                       END

                    ELSE (* NONFINAL WITH ARC = LAMBDA *)
                       IF I^.SCANNED <> OLDI THEN   (* NOT SCANNED YET? *)
                          BEGIN
                             I^.SCANNED := OLDI;    (* MARK TO AVOID INFINITE LOOPS *)
                             CREATENEXTIF(I^.PTR1,IFF);
                             CREATENEXTIF(I^.PTR2,IFF)
                          END;

               IMPOR_FLAG,  (* IMPORTANT ALREADY MARKED *)
               FINAL_FLAG:  (* FINAL STATE *)
                    BEGIN
                       SETSUB := I^.IFCODE DIV SET_SIZE;
                       IFF[SETSUB] := IFF[SETSUB] + [I^.IFCODE MOD SET_SIZE]   (* PUT INTO SET *)
                    END;

               INTER_FLAG:  (* INTERMEDIATE-BACKUP STATE *)
                    BEGIN
                       CREATENEXTIF(I^.PTR1,IFF);   (* WILL ALWAYS HAVE SINGLE LAMBDA TRANSITION *)
                       SETSUB := I^.IFCODE DIV SET_SIZE;
                       IFF[SETSUB] := IFF[SETSUB] + [I^.IFCODE MOD SET_SIZE]   (* PUT INTO SET *)
                    END
            END
      END; (* CREATENEXTIF *)



   FUNCTION DUPLICATE: BOOLEAN;
  
      (* COMPARE TDFASTATE[TSYM] WITH ALL DFA STATES ALREADY GENERATED AND ASSIGN
         DELTA[CURRDFA,TSYM] THE ONE WHICH MATCHES (OR NEXTDFA IF NONE MATCHES) *)

      VAR 
         ST:   DFASTATERANGE;
         FOUND_DUPL: BOOLEAN;

      BEGIN
         ST := 1;
         WHILE ST < NEXTDFA DO
            BEGIN
               FOUND_DUPL := TRUE;
               FOR SETSUB := 0 TO LAST_SETSUB DO
                  IF TDFASTATE[TSYM,SETSUB] <> DFASTATE[ST,SETSUB] THEN
                     FOUND_DUPL := FALSE;
               IF FOUND_DUPL THEN
                  BEGIN
                     DELTA[CURRDFA]^[TSYM] :=  ST;
                     ST := NEXTDFA (* FORCE EXIT *)
                  END;
               ST := ST + 1
            END;
 
         IF NOT FOUND_DUPL THEN
            DELTA[CURRDFA]^[TSYM] := NEXTDFA;
         DUPLICATE := FOUND_DUPL
      END; (* DUPLICATE *)
$PAGE CREATE_DFA body
   BEGIN (* CREATE_DFA *)
  
      (* FIRST, NUMBER THE FINAL AND INTERMEDIATE-BACKUP NODES IN THE NFAS *)
   
      LAST_INT_BACK := LAST_NFA;  (* NUMBERS FOR INTER.-BACKUP NODES WILL FOLLOW FINAL NODE NUMBERS *)
      FOR NSYM := 1 TO LAST_NFA DO
         WITH N.SYMTABLE[NSYM] DO
            IF FINAL <> NIL THEN
               BEGIN
                  FINAL^.NODEFLAG := FINAL_FLAG;
                  FINAL^.IFCODE := NSYM;                   (* NUMBER FOR FINAL NODE = NONTERMINAL CODE *)
                  IF INTERMED <> NIL THEN
                     BEGIN
                        INTERMED^.NODEFLAG := INTER_FLAG;
                        LAST_INT_BACK := LAST_INT_BACK+1;  (* ALLOCATE A NUMBER FOR BACKUP NODE *)
                        INTERMED^.IFCODE := LAST_INT_BACK;
                        WHICH_NFA[LAST_INT_BACK] := NSYM   (* CROSS REF. BACKUP NODE TO NFA *)
                     END
               END;
  
      NEW(DELTA[DEAD]);
      FOR TSYM := -1 DOWNTO T.SYMMAX DO
         DELTA[DEAD]^[TSYM] := DEAD;        (* INITIALIZE DEAD STATE TO POINT TO ITSELF *)
      FOR SETSUB := 0 TO MAXSETSUB DO
         DFASTATE[DEAD,SETSUB] := [];       (* NO NFA STATES IN DFA DEAD STATE *)
      LAST_IBF := LAST_INT_BACK;            (* NUMBERS FOR IMPORTANT NODES (I.E.NODES WITH NON-LAMBDA
                                               EXITING ARCS) WILL FOLLOW THE INTERMEDIATE-BACKUP NODES *)
      NEW(HEADQUEUE);                       (* INITIALIZE QUEUE WITH AN UNUSED ENTRY SO THAT CREATENEXTIF
                                               NEED NOT HANDLE SPECIAL CASE OF ADDING TO EMPTY QUEUE *)
      TAILQUEUE := HEADQUEUE;

      (* PUT ALL IBF STATES OF ALL NFA'S INITIALLY REACHABLE BY LAMBDA TRANSITIONS
         INTO THE INITIAL DFA STATE *)

      FOR SETSUB := 0 TO MAXSETSUB DO
         DFASTATE[1,SETSUB] := [];
      FOR NSYM := 1 TO LAST_NFA DO
         BEGIN
            OLDI := N.SYMTABLE[NSYM].INITIAL;
            CREATENEXTIF(OLDI, DFASTATE[1])
         END;
  
      (* BUILD NEXTIF - THE SET-VALUED TRANSITION FUNCTION OF THE NFAS *)

      QUEUE := HEADQUEUE^.NEXT;
      WHILE QUEUE <> NIL DO  (* WHILE THERE ARE NODES WHOSE EXITING ARCS HAVEN'T BEEN EXPLORED ... *)
         BEGIN
            FOR SETSUB := 0 TO MAXSETSUB DO
               NEXTIF[QUEUE^.IFCODE].NEXTSET[SETSUB] := [];
            OLDI := QUEUE^.IMP;
            CREATENEXTIF(OLDI, NEXTIF[QUEUE^.IFCODE].NEXTSET);
            QUEUE := QUEUE^.NEXT
         END;
      LAST_SETSUB := LAST_IBF DIV SET_SIZE;  (* WE CAN AVOID LOOPING TO MAXSETSUB UNLESS NECESSARY *)
$PAGE CREATE_DFA body
      (* CREATE DFA BY BREADTHFIRST SEARCH OF THE NFA'S TRANSITION FUNCTION NEXTIF *)
  
      CURRDFA := 1;
      NEXTDFA := 2;

      REPEAT
         NEW(DELTA[CURRDFA]);                         (* ALLOCATE A ROW FOR CURRDFA *)
         FOR TSYM := -1 DOWNTO T.SYMMAX DO
            BEGIN
               DELTA[CURRDFA]^[TSYM] := DEAD;         (* INITIALLY TRANSITIONS FROM CURRDFA ARE DEAD *)
               NOTNULL[TSYM] := FALSE;                (* ASSUME NO TRANSITIONS FROM CURRDFA *)
               FOR SETSUB := 0 TO LAST_SETSUB DO
                  TDFASTATE[TSYM,SETSUB] := []        (* ALL TEMPORARY DFA STATES SET TO NULL *)
            END;

         (* FIND ALL TRANSITIONS FROM CURRDFA *)
  
         FOR IFSTATE := 1 TO LAST_IBF DO              (* FOR EACH NFA STATE ...               *)
            IF (IFSTATE MOD SET_SIZE) IN
                 DFASTATE[CURRDFA,IFSTATE DIV SET_SIZE] THEN  (*  ... IN THE CURRENT DFA STATE, DO:  *)
               WITH NEXTIF[IFSTATE] DO
                  IF SYMBOL <= LAMBDA THEN            (* TERMINAL? *)
                     BEGIN
                        NOTNULL[SYMBOL] := TRUE;      (* TRANSITION FROM CURRDFA UNDER SYMBOL *)
                        FOR SETSUB := 0 TO LAST_SETSUB DO   (* ADD NFA STATES REACHABLE UNDER SYMBOL *)
                           TDFASTATE[SYMBOL,SETSUB] := TDFASTATE[SYMBOL,SETSUB] + NEXTSET[SETSUB]
                     END
                  ELSE     (* TERMINAL SET - PROCESS TRANSITIONS FOR EACH TERMINAL IN IT *)
                     FOR TSYM := -1 DOWNTO T.SYMMAX DO
                        IF S.SYMTABLE[SYMBOL].SET_ELE[TSYM] THEN
                           BEGIN
                              NOTNULL[TSYM] := TRUE;  (* TRANSITION UNDER TSYM *)
                              FOR SETSUB := 0 TO LAST_SETSUB DO (* ADD NFA STATES REACHABLE UNDER TSYM *)
                                 TDFASTATE[TSYM,SETSUB] := TDFASTATE[TSYM,SETSUB] + NEXTSET[SETSUB]
                           END;

         (* ADD ALL DFA STATE TRANSITIONS FROM CURRDFA TO DELTA *)

         FOR TSYM := -1 DOWNTO T.SYMMAX DO
            IF NOTNULL[TSYM] THEN
               IF NOT DUPLICATE THEN
                  BEGIN
                     FOR SETSUB := 0 TO LAST_SETSUB DO
                        DFASTATE[NEXTDFA,SETSUB] := TDFASTATE[TSYM,SETSUB];
                     IF NEXTDFA = MAXDFASTATES THEN
                        BEGIN
                           WRITELN(TTYOUTPUT,'TOO MANY DFA STATES');
                           STOP
                        END;
                     NEXTDFA := NEXTDFA + 1
                  END;

         CURRDFA := CURRDFA + 1
       UNTIL CURRDFA = NEXTDFA;   (* NO MORE NEW STATES *)
      LAST_DFA := CURRDFA - 1;
  
      (* CLEAN UP EACH DFA STATE IN PREPARATION FOR MINIMIZATION - ONLY THE HIGHEST PRECEDENCE
         FINAL NFA STATE AND THE BACKUP STATES ARE REQUIRED TO DETERMINE THE INITIAL PARTITION *)

      FOR CURRDFA := 1 TO LAST_DFA DO
         BEGIN
            FINALFOUND := FALSE;
            FOR IFSTATE := 1 TO LAST_NFA DO
               BEGIN
                  SETSUB := IFSTATE DIV SET_SIZE;
                  IF (IFSTATE MOD SET_SIZE) IN DFASTATE[CURRDFA,SETSUB] THEN
                     BEGIN
                        IF FINALFOUND THEN   
                           DFASTATE[CURRDFA,SETSUB] := DFASTATE[CURRDFA,SETSUB] - [IFSTATE MOD SET_SIZE];
                        FINALFOUND := TRUE   (* FOUND HIGHEST PREC. FINAL - KILL THE REST *)
                     END
               END;
            FOR IFSTATE := LAST_INT_BACK + 1 TO LAST_IBF DO
               BEGIN
                  SETSUB := IFSTATE DIV SET_SIZE;
                  DFASTATE[CURRDFA,SETSUB] := DFASTATE[CURRDFA,SETSUB] - [IFSTATE MOD SET_SIZE]
               END
         END
  
   END; (* CREATE_DFA *)
$PAGE MINIMIZE_DFA declarations
PROCEDURE MINIMIZE_DFA;

   VAR 
      FIRST_STATE,
      CURRDFA:      DFASTATERANGE;
      CURRBLOCK:    BLOCKRANGE;
      PREV_MEM,
      CURR_MEM:     ^MEMBER;
      MATCH,
      NEWBLOCK,
      NOCHANGE,
      NOMORE_DELETES,
      FINALFOUND:   BOOLEAN;
      TSYM:         TERM_RANGE;
      LB, UB:       MIN_TERM..0;
      SETSUB:       0..MAXSETSUB;
      IFSTATE:      1..MAXIFSTATES;


  
   PROCEDURE INSERT(NEWSTATE: DFASTATERANGE; X: BLOCKRANGE);  

      BEGIN
         WITH BLOCK[X] DO
            BEGIN
               IF FIRST_MEM = NIL THEN          (* EQUIVALENCE CLASS EMPTY? *)
                  BEGIN
                     NEW(FIRST_MEM);            (* NEWSTATE IS ONLY MEMBER *)
                     LAST_MEM := FIRST_MEM
                  END
               ELSE
                  BEGIN
                     NEW(LAST_MEM^.NEXT_MEM);   (* ADD NEWSTATE TO END OF LIST *)
                     LAST_MEM := LAST_MEM^.NEXT_MEM
                  END;
               LAST_MEM^.STATE := NEWSTATE;     (* FILL IN THE NES MEMBER *)
               LAST_MEM^.NEXT_MEM := NIL;       (* ITS THE LAST MEMBER *)
               ELEMENT_CNT := ELEMENT_CNT + 1
            END
      END; (* INSERT *)
$PAGE MINIMIZE_DFA body
   BEGIN (* MINIMIZE_DFA *)
  
  
      (* INITIALIZE *)
  
      FOR CURRBLOCK := FIRSTBLOCK TO MAXBLOCKS DO
         BEGIN
            BLOCK[CURRBLOCK].ELEMENT_CNT := 0;
            BLOCK[CURRBLOCK].FIRST_MEM := NIL
         END;
  

  
      (* FIND INITIAL PARTITION: EACH DISTINCT DFA STATE (AS DETERMINED BY INTERMEDIATE-BACKUP
         STATES AND HIGHEST PRECEDENCE FINAL STATE IN IT) IS ASSIGNED A DIFFERENT BLOCK      *)

  
      INSERT(DEAD,FIRSTBLOCK);                   (* DEAD BELONGS TO FIRSTBLOCK.  INITIALLY, ALL OTHER DFA
                                                    STATES NOT CONTAINING FINAL OR BACKUP STATES WILL BE
                                                    PUT THERE TOO. *)
      WHICHBLOCK[DEAD] := FIRSTBLOCK;            (* CROSS REFERENCE *)
      LAST_BLOCK := FIRSTBLOCK;                  (* SO FAR, ONLY HAVE THE ONE BLOCK *)
  
  
      FOR CURRDFA := 1 TO LAST_DFA DO
         BEGIN
            CURRBLOCK := FIRSTBLOCK;
  
            REPEAT
               MATCH := TRUE;
               FOR SETSUB := 0 TO LAST_SETSUB DO
                  IF DFASTATE[CURRDFA,SETSUB] <>
                        DFASTATE[BLOCK[CURRBLOCK].FIRST_MEM^.STATE,SETSUB] THEN
                     MATCH := FALSE;             (* DFA STATE IS DISTINCT FROM THOSE IN CURRENT BLOCK *)
                  IF NOT MATCH THEN
                     CURRBLOCK := CURRBLOCK + 1  (* KEEP LOOKING *)
            UNTIL MATCH OR (CURRBLOCK > LAST_BLOCK);
  
            WHICHBLOCK[CURRDFA] := CURRBLOCK;    (* INDICATE CURRDFA'S BLOCK *)
            IF CURRBLOCK = LAST_BLOCK+1 THEN     (* DISTINCT DFA STATE? *)
               BEGIN
                  LAST_BLOCK := LAST_BLOCK + 1;  (* ALLOCATE NEW BLOCK *)
                   INSERT(CURRDFA,LAST_BLOCK)     (* PUT STATE INTO NEW BLOCK *)
               END
            ELSE
               INSERT(CURRDFA,CURRBLOCK)         (* PUT STATE INTO BLOCK WITH OTHER SIMILAR STATES *)
         END;
$PAGE MINIMIZE_DFA body
      (* FIND EQUIVALENT STATES BY REFINING THE PARTITION UNTIL EACH BLOCK CONTAINS ONLY DFA STATES
         THAT ARE EQUIVALENT TO EACH OTHER.  AT WORST, EACH BLOCK WILL CONTAIN ONLY ONE DFA STATE.  *)

      REPEAT
         NOCHANGE := TRUE;
         CURRBLOCK := FIRSTBLOCK;
  
         WHILE CURRBLOCK <= LAST_BLOCK DO
            BEGIN
               TSYM := T.SYMMAX;
  
               WHILE (BLOCK[CURRBLOCK].ELEMENT_CNT > 1) AND (TSYM <= -1) DO
                  BEGIN
                     NEWBLOCK := FALSE;
                     FIRST_STATE := DELTA[ BLOCK[CURRBLOCK].FIRST_MEM^.STATE ]^[TSYM];
                     REPEAT   (* UNTIL NO MORE DFA STATES ARE REMOVED FROM CURRBLOCK *)
                        NOMORE_DELETES := TRUE;
                        PREV_MEM := BLOCK[CURRBLOCK].FIRST_MEM;   (* GET FIRST CONSECUTIVE PAIR *)
                        CURR_MEM := PREV_MEM^.NEXT_MEM;

                        WHILE CURR_MEM <> NIL DO   (* COMPARE MEMBERS OF THE CURRENT BLOCK *)
                           IF WHICHBLOCK[ DELTA[CURR_MEM^.STATE]^[TSYM] ] <> WHICHBLOCK[FIRST_STATE] THEN
                              BEGIN   (* IF DELTA(FIRST_MEM,TSYM) ISN'T IN SAME BLOCK AS
                                         DELTA(CURR_MEM,TSYM), CURR_MEM ISN'T EQUIVALENT TO FIRST_MEM *)
                                 WHICHBLOCK[CURR_MEM^.STATE] := LAST_BLOCK + 1;
                                 NOCHANGE := FALSE;
                                 NEWBLOCK := TRUE;
                                 INSERT(CURR_MEM^.STATE,LAST_BLOCK+1);
                                 PREV_MEM^.NEXT_MEM := CURR_MEM^.NEXT_MEM;   (* DELETE FROM CURRBLOCK *)
                                 NOMORE_DELETES := FALSE;
                                 BLOCK[CURRBLOCK].ELEMENT_CNT := BLOCK[CURRBLOCK].ELEMENT_CNT - 1;
                                 CURR_MEM := CURR_MEM^.NEXT_MEM
                              END
                           ELSE
                              BEGIN
                                 PREV_MEM := CURR_MEM;   (* SHIFT OVER TO NEXT PAIR IN CURRENT BLOCK *)
                                 CURR_MEM := CURR_MEM^.NEXT_MEM
                              END
                     UNTIL NOMORE_DELETES;
  
                     IF NEWBLOCK THEN
                        LAST_BLOCK := LAST_BLOCK + 1;
                     TSYM := TSYM + 1
                  END;
  
               CURRBLOCK := CURRBLOCK + 1
            END
      UNTIL NOCHANGE;
  
  
      (* PRINT MINIMIZED DELTA *)

      LB := T.SYMMAX;
      REPEAT
         IF (LB + MAXCOLS - 1) <= -1 THEN   (* NEED FULL WIDTH OF PAGE? *)
            UB := LB + MAXCOLS - 1
         ELSE
            UB := -1;   (* ONLY NEED PORTION OF WIDTH OF THIS PAGE *)
         PAGE;
         WRITELN('****** MINIMIZED DFA NEXT STATE TABLE ******');
         WRITELN;
         WRITE('      ');
         FOR TSYM := LB TO UB DO
            WRITE(TSYM:3,' ');
         WRITELN;
         WRITE('    __');
         FOR TSYM := LB TO UB DO
            WRITE('____');
         WRITELN;
         FOR CURRBLOCK := 1 TO LAST_BLOCK DO
            BEGIN
               WRITELN('    !');
               WRITE(CURRBLOCK:3,' ! ');
               FOR TSYM := LB TO UB DO
                  WRITE(WHICHBLOCK[DELTA[BLOCK[CURRBLOCK].FIRST_MEM^.STATE]^[TSYM]]:3,' ');
               WRITELN
            END;
         LB := UB + 1
      UNTIL LB > -1;
      WRITELN;
      WRITELN('****** START STATE: ',WHICHBLOCK[1]:4);
   

      (* PRINT INTERMEDIATE-BACKUP AND HIGH PRECEDENCE FINAL STATES IN EACH DFA STATE *)

      PAGE;
      WRITELN('****** FINAL/BACKUP STATES IN MINIMIZED DFA ******');
      WRITELN;
      WRITELN('          DFA STATE   FINAL/BACKUP STATE(S)');
      WRITELN('            0      *<**ERROR**>');
      LA_FEATURE := FALSE;                                  (* ASSUME LOOKAHEAD FEATURE ISN'T REQUIRED *)
      FOR CURRBLOCK := 1 TO LAST_BLOCK DO
         WITH BLOCK[CURRBLOCK] DO
            BEGIN 
               WRITE('          ',CURRBLOCK:3,'      ');
               FOR IFSTATE := 1 TO LAST_INT_BACK DO
                  IF (IFSTATE MOD SET_SIZE) IN DFASTATE[FIRST_MEM^.STATE,IFSTATE DIV SET_SIZE] THEN
                     IF IFSTATE <= LAST_NFA THEN
                        WITH N.SYMTABLE[IFSTATE] DO
                           WRITE('*<',SYM:LENGTH,'> ')      (* FINAL STATE *)
                     ELSE
                        BEGIN
                           WITH N.SYMTABLE[WHICH_NFA[IFSTATE]] DO
                              WRITE(' <',SYM:LENGTH,'> ');  (* INTERMEDIATE-BACKUP STATE *)
                           LA_FEATURE := TRUE
                        END;                                (* LOOKAHEAD FEATURE REALLY IS REQUIRED *)
               WRITELN
            END;
      PAGE
   END; (* MINIMIZE_DFA *)
$PAGE PRINT_PROGRAM declarations and local proc. COMPACT
PROCEDURE PRINT_PROGRAM;
  
   TYPE
      COM_TAB_RANGE = 0..MAX_COM_TAB;
      BIT_RANGE     = 0..10;
   
   VAR
      LINE_WIDTH:  0..80;
      WIDTH:       0..5;
      CURRBLOCK:   BLOCKRANGE;
      TSYM:        MIN_TERM..-1;
      USE_COMPACTED, FINALFOUND, NEEDED_LA: BOOLEAN;
      IFSTATE:     1..MAXIFSTATES;
      TOKEN_LA:    PACKED ARRAY[1..MAXNONTER] OF BOOLEAN;
      DEFAULT:     PACKED ARRAY[1..MAXBLOCKS] OF BLOCKRANGE;
      BASE:        ARRAY[1..MAXBLOCKS] OF COM_TAB_RANGE;
      NEXT,
      CHECK:       ARRAY[1..MAX_COM_TAB] OF BLOCKRANGE;
      LAST_NEXT_CHECK,
      TEMP_BASE:   COM_TAB_RANGE;
      PER_WORD, PER_WORD2: 1..WORD_SIZE;
  
  
   PROCEDURE COMPACT;
     
       VAR
         PAGENUM:   1..10;
         COL, COLS: 1..4;
         CHOSEN_BASE, NEXT_FREE, LINE, UB:  COM_TAB_RANGE;
         CURRBLOCK, BEST_STATE, TEMP_STATE:  BLOCKRANGE;
         CURR_ROW:  ARRAY[TERM_RANGE] OF BLOCKRANGE;
         NUM_SAME, BEST_SAME, FIRST_IN_TEMPLATE:  0..-MIN_TERM;
         CONFLICT, FOUND_FREE:  BOOLEAN;
         TEMPLATE:  ARRAY[TERM_RANGE] OF BOOLEAN;
     
     
      BEGIN (* COMPACT *)
  
         (* PUT ROW OF DELTA FOR FIRST STATE DIRECTLY INTO TABLE *)
  
         DEFAULT[1] := 0;   (* NO DEFAULT STATE REQUIRED - ALL ENTRIES WILL BE ENTERED EXPLICITLY *)
         BASE[1] := 0;      (* FIRST STATE WILL USE POSITIONS 1 TO -T.SYMMAX IN NEXT AND CHECK *)
         FOR TSYM := -1 DOWNTO T.SYMMAX DO   (* PUT ROW INTO NEXT AND CHECK *)
            BEGIN
               NEXT[-TSYM] := WHICHBLOCK[DELTA[BLOCK[1].FIRST_MEM^.STATE]^[TSYM]];
               CHECK[-TSYM] := 1
            END;
         LAST_NEXT_CHECK := -T.SYMMAX;
     
  
         (* FILL IN REMAINING STATES, TAKING ADVANTAGE OF SIMILAR STATES ALREADY IN THE TABLE
            WHENEVER POSSIBLE.  (SEE AHO & ULLMAN CH. 3 FOR EXPLANATION OF COMPACTION METHOD) *)
  
         FOR CURRBLOCK := 2 TO LAST_BLOCK DO
            BEGIN
  
               FOR TSYM := -1 DOWNTO T.SYMMAX DO       (* MAKE COPY OF CURRENT ROW (FOR EFFICIENCY) *)
                  CURR_ROW[TSYM] := WHICHBLOCK[DELTA[BLOCK[CURRBLOCK].FIRST_MEM^.STATE]^[TSYM]];
  
               BEST_SAME := 0;
               FOR TEMP_STATE := 1 TO CURRBLOCK-1 DO   (* SEARCH FOR SIMILAR STATE *)
                  BEGIN
                     NUM_SAME := 0;
                     FOR TSYM := -1 DOWNTO T.SYMMAX DO
                        IF WHICHBLOCK[DELTA[BLOCK[TEMP_STATE].FIRST_MEM^.STATE]^[TSYM]]
                              = CURR_ROW[TSYM] THEN
                           NUM_SAME := NUM_SAME + 1;
                     IF NUM_SAME > BEST_SAME THEN      (* A NEW "BEST"? *)
                        BEGIN
                           BEST_SAME := NUM_SAME;
                           BEST_STATE := TEMP_STATE    (* NEW CANDIDATE *)
                        END
                  END;
  
               IF BEST_SAME = 0 THEN   (* DON'T HAVE A "SIMILAR" STATE? *)
                  BEGIN   (* DO NOT HAVE A SIMILAR STATE - ENTER CURRENT STATE DIRECTLY INTO TABLE *)
                     DEFAULT[CURRBLOCK] := 0;
                     BASE[CURRBLOCK] := LAST_NEXT_CHECK;
                     IF LAST_NEXT_CHECK - T.SYMMAX <= MAX_COM_TAB THEN
                        LAST_NEXT_CHECK := LAST_NEXT_CHECK - T.SYMMAX
                     ELSE
                        BEGIN
                           WRITELN(TTYOUTPUT,'COMPACTED TABLE OVERFLOW');
                            STOP
                        END;
                     FOR TSYM := -1 DOWNTO T.SYMMAX DO
                        BEGIN
                           NEXT[BASE[CURRBLOCK]-TSYM] := CURR_ROW[TSYM];
                           CHECK[BASE[CURRBLOCK]-TSYM] := CURRBLOCK
                        END
                  END
  
               ELSE   (* I HAVE A SIMILAR STATE *)
                  BEGIN  
                     DEFAULT[CURRBLOCK] := BEST_STATE;   (* DEFAULT TO THE SIMILAR STATE *)
                     FIRST_IN_TEMPLATE := 0;
                     FOR TSYM := -1 DOWNTO T.SYMMAX DO   (* CONSTRUCT TEMPLATE OF POSITIONS WHERE CURRENT
                                                            STATE DOESN'T MATCH THE SIMILAR ONE *)
                        IF WHICHBLOCK[DELTA[BLOCK[BEST_STATE].FIRST_MEM^.STATE]^[TSYM]]
                               <> CURR_ROW[TSYM] THEN
                           BEGIN
                              TEMPLATE[TSYM] := TRUE;
                              IF FIRST_IN_TEMPLATE = 0 THEN
                                 FIRST_IN_TEMPLATE := -TSYM   (* REMEMBER POSITION OF FIRST NON-MATCH *)
                           END
                        ELSE
                           TEMPLATE[TSYM] := FALSE;
                     NEXT_FREE := 0;
                     IF FIRST_IN_TEMPLATE = 0 THEN   (* STATE EXACTLY MATCHES SIMILAR STATE? *)
                        CHOSEN_BASE := 0 (* ALIGN WITH 1ST STATE - DEFAULT (SIMILAR STATE) ALWAYS TAKEN *)
                     ELSE                (* HAVE TO FIND PLACE FOR NON-MATCHING ENTRIES *)
                        REPEAT           (* SLIDE TEMPLATE DOWN TABLE LOOKING FOR FIRST FIT *)
                           FOUND_FREE := FALSE;
                           REPEAT        (* FIND NEXT POSITION THAT MIGHT FIT *)
                              NEXT_FREE := NEXT_FREE + 1;
                              IF NEXT_FREE > LAST_NEXT_CHECK THEN
                                 FOUND_FREE := TRUE   (* ANYTHING PAST CURRENT END OF TABLE IS FREE *)
                              ELSE IF CHECK[NEXT_FREE] = 0 THEN
                                 FOUND_FREE := TRUE
                           UNTIL FOUND_FREE;
                           CHOSEN_BASE := NEXT_FREE - FIRST_IN_TEMPLATE;
                           CONFLICT := FALSE;
                           IF FIRST_IN_TEMPLATE < -T.SYMMAX THEN
                              FOR TSYM := -FIRST_IN_TEMPLATE-1 DOWNTO T.SYMMAX DO
                                 IF TEMPLATE[TSYM] THEN                (* IF I NEED THIS POSITION ...   *)
                                    IF CHOSEN_BASE - TSYM <= LAST_NEXT_CHECK THEN
                                       IF CHECK[CHOSEN_BASE-TSYM] > 0 THEN (* ... BUT ITS NOT FREE ...  *)
                                          CONFLICT := TRUE             (* ... THEN TEMPLATE DOESN'T FIT *)
                        UNTIL NOT CONFLICT;
                     BASE[CURRBLOCK] := CHOSEN_BASE;
                     IF CHOSEN_BASE - T.SYMMAX > LAST_NEXT_CHECK THEN  (* NEED TO EXPAND TABLE? *)
                        IF CHOSEN_BASE - T.SYMMAX <= MAX_COM_TAB THEN
                           BEGIN
                              FOR TEMP_BASE := LAST_NEXT_CHECK+1 TO CHOSEN_BASE-T.SYMMAX DO
                                 BEGIN
                                    NEXT[TEMP_BASE] := 0;
                                    CHECK[TEMP_BASE] := 0
                                 END;
                              LAST_NEXT_CHECK := CHOSEN_BASE - T.SYMMAX
                           END
                        ELSE
                           BEGIN
                              WRITELN(TTYOUTPUT,'COMPACTED TABLE OVERFLOW');
                              STOP
                           END;
                     FOR TSYM := -1 DOWNTO T.SYMMAX DO
                        IF TEMPLATE[TSYM] THEN
                           BEGIN
                              NEXT[CHOSEN_BASE-TSYM] := CURR_ROW[TSYM];
                              CHECK[CHOSEN_BASE-TSYM] := CURRBLOCK
                           END
                  END
            END; (* FOR CURRBLOCK *)
$PAGE PRINT_PROGRAM declarations and local proc. COMPACT
         (* PRINT THE COMPACTED TABLES *)
  
         WRITELN('****** COMPACTED VERSION OF MINIMIZED DFA NEXT STATE TABLE ******');
         WRITELN;
  
         FOR PAGENUM := 1 TO (LAST_BLOCK+199) DIV 200 DO   (* FIT 4 COLUMNS OF 50 TO A PAGE *)
            BEGIN
               IF 200 + (PAGENUM-1)*200 <= LAST_BLOCK THEN   (* NEED TO FILL THIS PAGE? *)
                  UB := 200 + (PAGENUM-1)*200   (* LAST ENTRY ON THIS PAGE *)
               ELSE
                  UB := LAST_BLOCK;   (* ONLY NEED PART OF THIS PAGE *)
               COLS := (UB - (PAGENUM-1)*200 + 49) DIV 50;
               FOR COL := 1 TO COLS DO
                  WRITE('             DEFAULT  BASE');
               WRITELN;
               WRITELN;
               IF UB > 50 + (PAGENUM-1)*200 THEN
                  UB := 50 + (PAGENUM-1)*200;   (* LAST ENTRY IN LEFT COLUMN ON THIS PAGE *)
               FOR LINE := 1 + (PAGENUM-1)*200 TO UB DO
                  BEGIN
                     FOR COL := 1 TO COLS DO
                        IF LINE+(COL-1)*50 <= LAST_BLOCK THEN
                           WRITE(LINE+(COL-1)*50:12, DEFAULT[LINE+(COL-1)*50]:7,BASE[LINE+(COL-1)*50]:7);
                        WRITELN
                  END;
               PAGE
            END;
  
         FOR PAGENUM := 1 TO (LAST_NEXT_CHECK+199) DIV 200 DO   (* FIT 4 COLUMNS OF 50 TO A PAGE *)
            BEGIN
               IF 200 + (PAGENUM-1)*200 <= LAST_NEXT_CHECK THEN   (* NEED TO FILL THIS PAGE? *)
                  UB := 200 + (PAGENUM-1)*200   (* LAST ENTRY ON THIS PAGE *)
               ELSE
                  UB := LAST_NEXT_CHECK;   (* ONLY NEED PART OF THIS PAGE *)
               COLS := (UB - (PAGENUM-1)*200 + 49) DIV 50;
               FOR COL := 1 TO COLS DO
                  WRITE('              NEXT  CHECK');
               WRITELN;
               WRITELN;
               IF UB > 50 + (PAGENUM-1)*200 THEN
                  UB := 50 + (PAGENUM-1)*200;   (* LAST ENTRY IN LEFT COLUMN ON THIS PAGE *)
               FOR LINE := 1 + (PAGENUM-1)*200 TO UB DO
                  BEGIN
                     FOR COL := 1 TO COLS DO
                        IF LINE + (COL-1)*50 <= LAST_NEXT_CHECK THEN
                           WRITE(LINE+(COL-1)*50:12,NEXT[LINE+(COL-1)*50]:6,CHECK[LINE+(COL-1)*50]:7);
                        WRITELN
                  END;
               PAGE
            END;
  
      END; (* COMPACT *)
$PAGE PRINT_PROGRAM local procedures
   FUNCTION BITS(MAXVAL: COM_TAB_RANGE): BIT_RANGE;
  
      VAR
         TEMP_VAL: COM_TAB_RANGE;
  
      BEGIN
         BITS := 0;
         TEMP_VAL := MAXVAL;
         REPEAT
            BITS := BITS + 1;
            TEMP_VAL := TEMP_VAL DIV 2
         UNTIL TEMP_VAL = 0
      END;
  
  
   PROCEDURE PRINT(VAL: COM_TAB_RANGE);
  
      VAR
         TEMP_VAL: COM_TAB_RANGE;
  
      BEGIN
         TEMP_VAL := VAL;
         WIDTH := 0;
         REPEAT                                  (* CALCULATE REQUIRED FIELD WIDTH *)
            TEMP_VAL := TEMP_VAL DIV 10;
            WIDTH := WIDTH + 1
         UNTIL TEMP_VAL = 0;
         IF (LINE_WIDTH + WIDTH + 1) > 75 THEN   (* NEED NEW LINE? *)
            BEGIN
               LINE_WIDTH := WIDTH + 7;
               WRITELN;
               WRITE('      ')
            END;
         LINE_WIDTH := LINE_WIDTH + WIDTH + 1;
         WRITE(VAL:WIDTH)
      END;
$PAGE PRINT_PROGRAM body
   BEGIN (* PRINT_PROGRAM *)
  
      (* COMPACT THE MINIMIZED VERSION OF DELTA *)
  
      COMPACT;
  
      (* LET USER CHOOSE BETWEEN COMPACTED AND UNCOMPACTED FROMS *)
  
      PER_WORD := WORD_SIZE DIV BITS(LAST_BLOCK);
      PER_WORD2 := WORD_SIZE DIV BITS(LAST_NEXT_CHECK);
      WRITELN(TTYOUTPUT);
      WRITELN(TTYOUTPUT,'THE UNCOMPACTED FORM OF DELTA REQUIRES',
               (-T.SYMMAX*LAST_BLOCK + PER_WORD - 1) DIV PER_WORD:6,' WORDS FOR STORAGE');
      WRITELN(TTYOUTPUT,'THE COMPACTED FORM REQUIRES',
                 (LAST_BLOCK + PER_WORD - 1) DIV PER_WORD                        (* FOR DEFAULT      *)
               + (LAST_BLOCK + PER_WORD2 - 1) DIV PER_WORD2                      (*  "  BASE         *)
               + 2*((LAST_NEXT_CHECK + PER_WORD - 1) DIV PER_WORD):17,' WORDS'); (*  "  NEXT & CHECK *)
      WRITELN(TTYOUTPUT);
      WRITE(TTYOUTPUT,'DO YOU WANT THE COMPACTED FORM? ');
      BREAK(TTYOUTPUT);
      READLN(TTY);
      USE_COMPACTED := (TTY^ = 'Y') OR (TTY^ = 'y');
  
  
      (* PRINT SCANNER PROGRAM WITH APPROPRIATE CONSTANTS, TABLES, AND CODE *)
  
      WRITELN('(********************************************)');
      WRITELN('(*   SCANNER ALGORITHM CREATED BY LEXGEN    *)');
      IF LA_FEATURE THEN
         WRITELN('(*    INCORPORATES THE LOOKAHEAD FEATURE    *)');
      WRITELN('(********************************************)');
      WRITELN;
      WRITELN('PROGRAM ??????;');
      WRITELN;
  
      WRITELN('CONST');
      IF LA_FEATURE THEN
         WRITELN('   MAXSTACK    =  ???;  (* MAX INDEX IN STACK FOR LOOKAHEAD *)');
      WRITELN('   MAXINDEX    =  ???;  (* MAX INDEX USED TO ACCESS BUFFER *)');
      WRITELN('   BUFFERSIZE  =  ???;  (* MAXINDEX + 1 *)');
      WRITELN('   MAXTOKEN    = ',LAST_NFA:4,';');
      WRITELN('   DFASTATE1   = ',WHICHBLOCK[1]:4,';  (* CODE FOR INITIAL STATE OF DFA *)');
      WRITELN('   MAXDFASTATE = ',LAST_BLOCK:4,';  (* CODE FOR MAX STATE OF DFA *)');
      WRITELN('   MINTERMINAL = ',T.SYMMAX:4,';  (* MIN TERMINAL CODE *)');
      WRITELN('   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)');
      WRITELN;
      WRITELN;
  
      WRITELN('TYPE');
      WRITELN('   STATERANGE  = 1..MAXDFASTATE;');
      WRITELN('   EXSTATERANGE= 0..MAXDFASTATE;');
      WRITELN('   INDEXRANGE  = 0..MAXINDEX;');
      WRITELN('   LEXTOKEN    = RECORD');
      WRITELN('                    TOKEN_TYPE: ???;');
      WRITELN('                    MORE: ???  (* POINTER TO SYMBOL TABLE, CODE');
      WRITELN('                                  TO DIFFERENTIATE DIFFERENT SYMBOLS');
      WRITELN('                                  SUCH AS RELATIONAL OPERATORS OF THE');
      WRITELN('                                  SAME TOKEN_TYPE, ETC.  *)');
      WRITELN('                 END;');
      WRITELN;
      WRITELN('VAR');
  
      IF USE_COMPACTED THEN
         BEGIN
            WRITE('   DEFAULT: PACKED ARRAY[STATERANGE] OF EXSTATERANGE := (');
            LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
            FOR CURRBLOCK := 1 TO LAST_BLOCK DO
               BEGIN
                  PRINT(DEFAULT[CURRBLOCK]);
                  IF CURRBLOCK < LAST_BLOCK THEN
                     WRITE(',')
               END;
            WRITELN(');');
            WRITELN;
            WRITE('   BASE: PACKED ARRAY[STATERANGE] OF 0..',LAST_NEXT_CHECK:4,' := (');
            LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
            FOR CURRBLOCK := 1 TO LAST_BLOCK DO
               BEGIN
                  PRINT(BASE[CURRBLOCK]);
                  IF CURRBLOCK < LAST_BLOCK THEN
                     WRITE(',')
               END;
            WRITELN(');');
            WRITELN;
            WRITELN('   NEXT: PACKED ARRAY[1..',LAST_NEXT_CHECK:4,'] OF EXSTATERANGE := (');
            LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
            FOR TEMP_BASE := 1 TO LAST_NEXT_CHECK DO
               BEGIN
                  PRINT(NEXT[TEMP_BASE]);
                  IF TEMP_BASE < LAST_NEXT_CHECK THEN
                     WRITE(',')
               END;
            WRITELN(');');
            WRITELN;
            WRITE('   CHECK: PACKED ARRAY[1..',LAST_NEXT_CHECK:4,'] OF EXSTATERANGE := (');
            LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
            FOR TEMP_BASE := 1 TO LAST_NEXT_CHECK DO
               BEGIN
                  PRINT(CHECK[TEMP_BASE]);
                  IF TEMP_BASE < LAST_NEXT_CHECK THEN
                     WRITE(',')
               END;
            WRITELN(');');
            WRITELN;
         END
      ELSE
         BEGIN
            WRITE('   DELTA: PACKED ARRAY[STATERANGE,MINTERMINAL..EODATA] OF EXSTATERANGE := (');
            LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
            FOR CURRBLOCK := 1 TO LAST_BLOCK DO
                FOR TSYM := T.SYMMAX TO -1 DO
                  BEGIN
                     PRINT(WHICHBLOCK[DELTA[BLOCK[CURRBLOCK].FIRST_MEM^.STATE]^[TSYM]]);
                     IF (CURRBLOCK < LAST_BLOCK) OR (TSYM < -1) THEN
                        WRITE(',')
                  END;
            WRITELN(');');
            WRITELN
         END;
  
      WRITELN('   (* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE');
      FOR IFSTATE := 1 TO LAST_NFA DO
         WITH N.SYMTABLE[IFSTATE] DO
            WRITELN('               ',IFSTATE:3,' IF STATE X RECOGNIZES <',SYM:LENGTH,'>');
      WRITELN(' ':65,'*)');
      WRITE('   FINAL: PACKED ARRAY[EXSTATERANGE] OF 0..MAXTOKEN := (');
      LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
      FOR CURRBLOCK := FIRSTBLOCK TO LAST_BLOCK DO
         BEGIN
            FINALFOUND := FALSE;
            FOR IFSTATE := 1 TO LAST_NFA DO
               IF (IFSTATE MOD SET_SIZE) IN
                     DFASTATE[BLOCK[CURRBLOCK].FIRST_MEM^.STATE,IFSTATE DIV SET_SIZE] THEN
                  BEGIN
                     PRINT(IFSTATE);
                     FINALFOUND := TRUE
                  END;
            IF NOT FINALFOUND THEN
               PRINT(0);
            IF CURRBLOCK < LAST_BLOCK THEN
               WRITE(',')
         END;
      WRITELN(');');
      WRITELN;
  
      IF LA_FEATURE THEN
         BEGIN
            WRITELN('   (* BACKUP[X] = SET OF ALL BACKUP TOKENS ASSOCIATED WITH STATE X.');
            WRITELN('                  SEE "FINAL" COMMENT FOR TOKEN CODES. *)');
            WRITELN;
             WRITE('   BACKUP: ARRAY[STATERANGE] OF SET OF 1..MAXTOKEN := (');
            LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
            FOR IFSTATE := 1 TO LAST_NFA DO
               TOKEN_LA[IFSTATE] := FALSE;   (* ASSUME NO TOKENS REQUIRE LOOKAHEAD TO BE RECOGNIZED *)
            FOR CURRBLOCK := 1 TO LAST_BLOCK DO
               BEGIN
                  IF LINE_WIDTH > 60 THEN
                     BEGIN
                        WRITELN;
                        WRITE('      ');
                        LINE_WIDTH := 6
                     END;
                  WRITE('[');
                  FINALFOUND := FALSE;
                  FOR IFSTATE := LAST_NFA+1 TO LAST_INT_BACK DO
                     IF (IFSTATE MOD SET_SIZE) IN 
                           DFASTATE[BLOCK[CURRBLOCK].FIRST_MEM^.STATE,IFSTATE DIV SET_SIZE] THEN
                        BEGIN
                           IF FINALFOUND THEN
                              WRITE(',');
                           WRITE(WHICH_NFA[IFSTATE]:3);
                           FINALFOUND := TRUE;
                           LINE_WIDTH := LINE_WIDTH + 4;
                           TOKEN_LA[WHICH_NFA[IFSTATE]] := TRUE  (* THIS TOKEN REQUIRES LOOKAHEAD *)
                        END;
                  WRITE(']');
                  IF CURRBLOCK < LAST_BLOCK THEN
                     WRITE(',');
                  LINE_WIDTH := LINE_WIDTH + 2
               END;
            WRITELN(');');
            WRITELN;
  
            WRITELN('   (* LOOKAHEADFINAL[X] = TRUE IFF LOOKAHEAD WAS REQUIRED TO RECOGNIZE');
            WRITELN('          TOKEN ASSOCIATED WITH STATE X.  "FINAL" INDICATES WHICH TOKEN');
            WRITELN('          THAT IS,  AND "BACKUP" IS USED TO FIGURE OUT WHERE TO BACKUP TO.  *)');
            WRITELN;
            WRITE('   LOOKAHEADFINAL: PACKED ARRAY[EXSTATERANGE] OF BOOLEAN := (');
            LINE_WIDTH := 75;   (* FORCE FIRST LINE *)
            FOR CURRBLOCK := FIRSTBLOCK TO LAST_BLOCK DO
               BEGIN
                  NEEDED_LA := FALSE;
                  FOR IFSTATE := 1 TO LAST_NFA DO
                     IF (IFSTATE MOD SET_SIZE) IN 
                           DFASTATE[BLOCK[CURRBLOCK].FIRST_MEM^.STATE,IFSTATE DIV SET_SIZE] THEN
                        IF TOKEN_LA[IFSTATE] THEN
                           NEEDED_LA := TRUE;
                  IF LINE_WIDTH + 6 > 75 THEN
                     BEGIN
                        WRITELN;
                        WRITE('      ');
                        LINE_WIDTH := 6
                     END;
                  LINE_WIDTH := LINE_WIDTH + 6;
                  WRITE(NEEDED_LA:5);
                  IF CURRBLOCK < LAST_BLOCK THEN
                     WRITE(',')
               END;
            WRITELN(');');
            WRITELN
         END;
  
      WRITELN('   BEGIN_INDEX, END_INDEX: INDEXRANGE;');
      WRITELN('   LEXEME: LEXTOKEN;');
      WRITELN('   BUFFER: ARRAY[INDEXRANGE] OF MINTERMINAL..EODATA;');
      WRITELN;
      WRITELN;
  
      WRITELN('PROCEDURE SCAN(VAR BEGIN_INDEX, END_INDEX: INDEXRANGE;');
      WRITELN('               VAR LEXEME: LEXTOKEN);');
      WRITELN;
      WRITELN('   VAR');
      WRITELN('      NEWTOKEN:  BOOLEAN;');
      WRITELN('      CURRSTATE, CURRFINAL: EXSTATERANGE;');
      WRITELN('      OLDINDEX:  INDEXRANGE;');
      IF LA_FEATURE THEN
         BEGIN
            WRITELN('      STACK: ARRAY[0..MAXSTACK] OF');
            WRITELN('                RECORD');
            WRITELN('                   INDEX: INDEXRANGE;');
            WRITELN('                   STATE: STATERANGE');
            WRITELN('                END;');
            WRITELN('      TOS: 0..MAXSTACK;  (* CURRENT TOP OF STACK INDEX *)');
            WRITELN;
            WRITELN;
            WRITELN('   PROCEDURE PUSH(ININDEX: INDEXRANGE; INSTATE: STATERANGE);');
            WRITELN('      BEGIN');
            WRITELN('         TOS := TOS + 1;');
            WRITELN('         STACK[TOS].INDEX := ININDEX;');
            WRITELN('         STACK[TOS].STATE := INSTATE');
            WRITELN('      END; (* PUSH *)')
         END;
      WRITELN;
      WRITELN;
  
      WRITELN('   PROCEDURE GETCHAR(NEWTOKEN: BOOLEAN);');
      WRITELN('      BEGIN');
      WRITELN('         <  THIS PROCEDURE OBTAINS THE NEXT INPUT CHARACTER (WHICH');
      WRITELN('            IS ASSUMED TO BE EODATA IF NO MORE INPUT) AND MODIFIES');
      WRITELN('            BEGIN_INDEX AND END_INDEX AS NECESSARY DEPENDING ON');
      WRITELN('            THE BUFFERING SCHEME SO THAT');
      WRITELN('             (1) IF NEWTOKEN, THEN BEGIN_INDEX POINTS TO THE INPUT');
      WRITELN('                 CHARACTER JUST OBTAINED, ELSE BEGIN_INDEX POINTS');
      WRITELN('                 TO THE SAME CHARACTER IT POINTED TO BEFORE.');
      WRITELN('             (2) END_INDEX IS THE INDEX OF THE NEW CHARACTER JUST');
      WRITELN('                 OBTAINED.');
      WRITELN('            SCAN ALLOWS FOR EITHER SEQUENTIAL OR CIRCULAR BUFFER  >');
      WRITELN('      END; (* GETCHAR *)');
      WRITELN;
      WRITELN;
  
      WRITELN('   BEGIN (* SCAN *)');
      WRITELN('      NEWTOKEN  := TRUE;');
      IF LA_FEATURE THEN
         WRITELN('      TOS := 0;');
      WRITELN('      CURRSTATE := DFASTATE1;  (* START IN INITIAL STATE *)');
      WRITELN('      CURRFINAL := 0;');
      WRITELN('      OLDINDEX  := 0;  (* WIDTH OF LEXEME AS OF LAST FINAL STATE *)');
      WRITELN;
      WRITELN('      WHILE CURRSTATE <> 0 DO');
      WRITELN('         BEGIN');
      IF LA_FEATURE THEN
         BEGIN
            WRITELN('            IF BACKUP[CURRSTATE] <> [] THEN');
            WRITELN('               PUSH((END_INDEX-BEGIN_INDEX) MOD BUFFERSIZE, CURRSTATE);')
         END;
      WRITELN('            IF FINAL[CURRSTATE] <> 0 THEN');
      WRITELN('               BEGIN');
      WRITELN('                  CURRFINAL := CURRSTATE;');
      WRITELN('                  OLDINDEX := (END_INDEX - BEGIN_INDEX) MOD BUFFERSIZE');
      WRITELN('               END;');
      WRITELN('            GETCHAR(NEWTOKEN);');
      WRITELN('            NEWTOKEN := FALSE;');
      IF USE_COMPACTED THEN
         BEGIN
            WRITELN('            WHILE CHECK[BASE[CURRSTATE]-BUFFER[END_INDEX]] <> CURRSTATE DO');
            WRITELN('               CURRSTATE := DEFAULT[CURRSTATE];');
            WRITELN('            CURRSTATE := NEXT[BASE[CURRSTATE]-BUFFER[END_INDEX]]')
         END
      ELSE
         WRITELN('            CURRSTATE := DELTA[CURRSTATE, BUFFER[END_INDEX]]');
      WRITELN('         END;');
      IF LA_FEATURE THEN
         BEGIN
            WRITELN('      IF LOOKAHEADFINAL[CURRFINAL] THEN');
            WRITELN('         BEGIN');
            WRITELN('            WHILE NOT (FINAL[CURRFINAL] IN BACKUP[STACK[TOS].STATE]) DO');
            WRITELN('               TOS := TOS - 1;');
            WRITELN('            END_INDEX := (STACK[TOS].INDEX + BEGIN_INDEX) MOD BUFFERSIZE');
             WRITELN('         END');
            WRITELN('      ELSE');
            WRITE('   ')
         END;
      WRITELN('      END_INDEX := (BEGIN_INDEX + OLDINDEX) MOD BUFFERSIZE;');
      WRITELN;
      WRITELN('       < COMPUTE LEXEME GIVEN FINAL[CURRFINAL], BEGIN_INDEX, END_INDEX,');
      WRITELN('         ETC.                                                          >');
      WRITELN;
      WRITELN('   END; (* SCAN *)');
      WRITELN;
      WRITELN;
  
      WRITELN('BEGIN (* MAINLINE *)');
       WRITELN('          .');
      WRITELN('          .');
      WRITELN('          .');
      WRITELN('   SCAN(BEGIN_INDEX, END_INDEX, LEXEME);');
      WRITELN('              (* AS NEEDED UNTIL END-OF-DATA LEXEME IS OBTAINED *)');
      WRITELN('          .');
      WRITELN('          .');
      WRITELN('          .');
      WRITELN('END. (* MAINLINE *)');
      WRITELN;
  
   END; (* PRINT_PROGRAM *)
$PAGE Mainline
BEGIN (* LEXGEN *)

   (* INITIALIZE PARSING TABLES *)

    FOR ST := 1 TO MAXPARSEST DO
      FOR LA := METALBRACK TO TERMINALHEAD DO
         P[ST,LA] := LAMBDA;
   P[ 1,  -2] := -503;  P[ 1,   1] :=  -1;   P[ 1,   2] :=   2;
   P[ 2,  -2] := -504;  P[ 2,  -1] :=   3;
   P[ 3,  -5] :=   6;   P[ 3,  -2] :=   5;   P[ 3,   3] :=   4;   P[ 3,   4] :=  -5;
   P[ 4,  -1] :=   7;   P[ 4,   0] :=  -2;
   P[ 5,  -3] :=   8;
   P[ 6,  -3] :=   9;
   P[ 7,  -5] :=   6;   P[ 7,  -2] :=   5;   P[ 7,   4] :=  -6;
   P[ 8, -11] := -18;   P[ 8,  -9] :=  11;   P[ 8,  -8] := -513;  P[ 8,  -7] := -18;
   P[ 8,  -6] := -18;   P[ 8,  -5] := -515;  P[ 8,  -4] := -18;   P[ 8,  -2] := -514;
   P[ 8,  -1] := -18;   P[ 8,   0] := -18;   P[ 8,   5] :=  10;
   P[ 9, -13] := -520;  P[ 9,   6] :=  -9;   P[ 9,   7] :=  12;
   P[10, -11] :=  15;   P[10,  -9] :=  11;   P[10,  -8] := -513;  P[10,  -7] := -511;
   P[10,  -6] := -510;  P[10,  -5] := -515;  P[10,  -4] :=  13;   P[10,  -2] := -514;
   P[10,  -1] :=  -7;   P[10,   0] :=  -7;   P[10,   5] :=  14;
   P[11, -11] := -18;   P[11, -10] := -18;   P[11,  -9] :=  11;   P[11,  -8] := -513;
   P[11,  -7] := -18;   P[11,  -6] := -18;   P[11,  -5] := -515;  P[11,  -2] := -514;
   P[11,   5] :=  16;
   P[12, -12] := -519;  P[12,  -8] := -521;
   P[13, -11] := -18;   P[13,  -9] :=  11;   P[13,  -8] := -513;  P[13,  -7] := -18;
   P[13,  -6] := -18;   P[13,  -5] := -515;  P[13,  -2] := -514;  P[13,  -1] := -18;
   P[13,   0] := -18;   P[13,   5] :=  17;
   P[14, -11] := -12;   P[14, -10] := -12;   P[14,  -9] := -12;   P[14,  -8] := -12;
   P[14,  -7] := -511;  P[14,  -6] := -510;  P[14,  -5] := -12;   P[14,  -4] := -12;
   P[14,  -2] := -12;   P[14,  -1] := -12;   P[14,   0] := -12;   P[14,   5] :=  14;
   P[15, -11] := -18;   P[15, -10] := -18;   P[15,  -9] :=  11;   P[15,  -8] := -513;
   P[15,  -7] := -18;   P[15,  -6] := -18;   P[15,  -5] := -515;  P[15,  -4] := -18;
   P[15,  -2] := -514;  P[15,  -1] := -18;   P[15,   0] := -18;   P[15,   5] :=  18;
   P[16, -11] :=  15;   P[16, -10] := -516;  P[16,  -9] :=  11;   P[16,  -8] := -513;
   P[16,  -7] := -511;  P[16,  -6] := -510;  P[16,  -5] := -515;  P[16,  -2] := -514;
   P[16,   5] :=  14;
   P[17, -11] :=  15;   P[17,  -9] :=  11;   P[17,  -8] := -513;  P[17,  -7] := -511;
   P[17,  -6] := -510;  P[17,  -5] := -515;  P[17,  -2] := -514;  P[17,  -1] :=  -8;
   P[17,   0] :=  -8;   P[17,   5] :=  14;
   P[18, -11] := -17;   P[18, -10] := -17;   P[18,  -9] :=  11;   P[18,  -8] := -513;
   P[18,  -7] := -511;  P[18,  -6] := -510;  P[18,  -5] := -515;  P[18,  -4] := -17;
   P[18,  -2] := -514;  P[18,  -1] := -17;   P[18,   0] := -17;   P[18,   5] :=  14;

   LHS[  -2] :=  1;   RHSLENGTH[  -2] :=  3;   (* <complete>   ::=  <tokens> ';' <prodset>              *)
   LHS[  -3] :=  2;   RHSLENGTH[  -3] :=  1;   (* <tokens>     ::=  'nonterminal'                       *)
   LHS[  -4] :=  2;   RHSLENGTH[  -4] :=  2;   (*                   <tokens> 'nonterminal'              *)
   LHS[  -5] :=  3;   RHSLENGTH[  -5] :=  1;   (* <prodset>    ::=  <production>                        *)
   LHS[  -6] :=  3;   RHSLENGTH[  -6] :=  3;   (*                   <prodset> ';' <production>          *)
   LHS[  -7] :=  4;   RHSLENGTH[  -7] :=  3;   (* <production> ::=  'nonterminal' '::=' <expression>    *)
   LHS[  -8] :=  4;   RHSLENGTH[  -8] :=  5;   (*                   'nonterminal' '::=' <expression> '/'
                                                                                          <expression>  *)
   LHS[  -9] :=  4;   RHSLENGTH[  -9] :=  3;   (*                   'setname' '::=' <terminalset>       *)
   LHS[ -10] :=  5;   RHSLENGTH[ -10] :=  2;   (* <expression> ::=  <expression> '*'                    *)
   LHS[ -11] :=  5;   RHSLENGTH[ -11] :=  2;   (*                   <expression> '+'                    *)
   LHS[ -12] :=  5;   RHSLENGTH[ -12] :=  2;   (*                   <expression> <expression>           *)
   LHS[ -13] :=  5;   RHSLENGTH[ -13] :=  1;   (*                   'terminal'                          *)
    LHS[ -14] :=  5;   RHSLENGTH[ -14] :=  1;   (*                   'nonterminal'                       *)
   LHS[ -15] :=  5;   RHSLENGTH[ -15] :=  1;   (*                   'setname'                           *)
   LHS[ -16] :=  5;   RHSLENGTH[ -16] :=  3;   (*                   '(' <expression> ')'                *)
   LHS[ -17] :=  5;   RHSLENGTH[ -17] :=  3;   (*                   <expression> '!' <expression>       *)
   LHS[ -18] :=  5;   RHSLENGTH[ -18] :=  0;   (*                   ''                                  *)
   LHS[ -19] :=  6;   RHSLENGTH[ -19] :=  2;   (* <terminalset> ::= <terminalhead> ']'                  *)
   LHS[ -20] :=  7;   RHSLENGTH[ -20] :=  1;   (* <terminalhead>::= '['                                 *)
   LHS[ -21] :=  7;   RHSLENGTH[ -21] :=  2;   (*                   <terminalhead> 'terminal'           *)

  
  
   INPUTG;   (* PROCESS INPUT AND CREATE NFA'S *)

   IF ERRORS <> FATAL THEN
      BEGIN
         CREATE_DFA;
         MINIMIZE_DFA;
         PRINT_PROGRAM
      END;

   WRITELN;
   WRITELN(TTYOUTPUT);
  
   CASE ERRORS OF
       OK:
         BEGIN
            WRITELN('NO ERRORS');
            WRITELN(TTYOUTPUT,'NO ERRORS')
         END;
      WARNING:
         BEGIN
            WRITELN('WARNING(S) GIVEN');
            WRITELN(TTYOUTPUT,'WARNING(S) GIVEN')
         END;
      RECOVER:
         BEGIN
            WRITELN('RECOVERY FROM MAJOR ERROR(S)');
            WRITELN(TTYOUTPUT,'RECOVERY FROM MAJOR ERROR(S)')
         END;
      FATAL:
         BEGIN
            WRITELN('FATAL ERROR(S)');
            WRITELN(TTYOUTPUT,'FATAL ERROR(S)')
         END
   END;

END. (* LEXGEN *)
  #x