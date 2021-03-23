(*$D+ *)
CONST
  (* VERY TEMPORARY ERROR MESSSAGES *)
  ERROR1 = ' ONE TO THREE DEPENDENT VARIABLES ONLY.';
  EXPRERROR = ' EXPRESSION ERROR.';
  EXPONERROR = ' FIRST THROUGH FOURTH ORDER ONLY.';
  SYNTAXERROR = ' SYNTAX ERROR.';
  NONLINEAR = ' NON-LINEAR SIMULTANEOUS EQUATIONS ARE NOT SUPPORTED.';
  NOVARIABLE = ' EQUATION MUST INVOLVE DEPENDENT VARIABLE(S).'; (*NECESSARY?*)
  PRINTERROR = ' NO EQUATIONS ENTERED.';
  COMMANDERROR = ' UNRECOGNIZABLE COMMAND.';
 
CONST
  OPENING = ' SYMBOL HERE.';
  STATHEADER = '          AQUIRED  REUSED';
  PREFIX = '> ';
  PROMPT = ':';
  BLANKS = '          ';
  NULL = ' ';
  COMMANDMAX = 6;
  MAXLINE = 69;
  NODESTAT = 1;
  SYMRECSTAT = 2;
  TOKENSTAT = 3;
  ROOTSTAT = 4;
  MAXSTAT = 4;
  NEWSTAT = 1;
  OLDSTAT = 2;
 
LABEL 1;
 
TYPE
  TAG = (CONSTANT,POINTER,VARIABLE,NONE);
  OPERTYPE = (ADD,SUB,MULT,DIVD,EXPON,FUNC,SPECIAL,CONS,VARBL);
  SYMBOLTYPE = (VARBLE,FUNCT);
  INT = -1000000..1000000;
  LINK = ^NODE;
  PTR = ^SYMRECORD;
  TOKEN = ^ITEM;
  ROOT = ^ROOTNODE;
  NODE = RECORD (*EXPRESSION TREE ENTRY*)
          OPER : OPERTYPE;
          LLINK, RLINK : LINK;
          NPTR : PTR; (*IF FUNC OR SPECIAL*)
          LTAG, RTAG : TAG; (*TERMINAL NODES ARE CONSTANTS AND VARIABLES*)
          LPTR,RPTR : PTR; (* INTO SYMBOLTABLE IF VARIABLES*)
          LVAL, RVAL : INT; (*CONSTANT VALUES*)
         END;
  ROOTNODE = RECORD (*HEAD OF EXPRESSION TREES*)
               RTTAG : TAG;
               RTVAL : INT;
               RTPTR : PTR;
               RTLINK : LINK;
               RTNEXT : ROOT (*LINKS FUNCTION DUMMY VARS/EXPRS*)
             END;
  SYMRECORD = RECORD
                NAME : ALFA;
                SLPTR, SRPTR : PTR;
                SYMTYPE : SYMBOLTYPE;
                DEPENDENT : BOOLEAN;
                SUBEXPR : ROOTNODE
              END;
  ITEM = RECORD (*IN CONVERSION TO POSTFIX*)
           XTAG = OPERTYPE;
           XVAL = INT;
           XPTR = PTR;
           XPREV = TOKEN
          END;
  STATISTIC : RECORD
                STATTYPE : ALFA;
                NEWSTAT,OLDSTAT : INT
              END;
 
VAR
  AVAILROOT : ROOT;         (*AVAILABLE ROOTS*)
  AVAILNODE : LINK;         (*AVAILABLE NODES*)
  AVAILSYMREC : PTR;        (*AVAILABLE SYMBOL TABLE NODES*)
  AVAILTOKEN : TOKEN;       (*AVAILABLE TOKEN NODES*)
  SYMPTR :                  (*USED IN SYMBOL TREE SEARCHES*)
  SYMBOLTABLE : PTR;        (*TO TOP ENTRY*)
  TOKENLIST : TOKEN;        (*LAST TOKEN BUILT*)
  WRT : ARRAY[1..3] OF PTR; (*DEPENDENT VARIABLES*)
  BASE : ARRAY[1..3,0..3] OF ROOTNODE; (*EQUATION MATRIX*)
  EQU : ARRAY[1..3,0..1] OF ROOTNODE; (*EQUATION TREES AS READ*)
  ALPHABET, DIGITS, PRIO1, PRIO2, OPERATORS : SET OF CHAR;
  LETTERSORDIGITS : SET OF CHAR;
  I, J, K, COUNT, LINE_COUNT, LASTCHAR, TOP, LINEPTR : INT;
  SEARCHING, FIRST, PARENFLAG, RUNNING, INITIALIZE, SCANNING : BOOLEAN;
  ADDSUB, MULDIV, FUNCSPEC : SET OF OPERTYPE;
  OPERCHAR : ARRAY[OPERTYPE] OF CHAR;
  STATS : ARRAY[1..MAXSTAT,1..2] OF STATISTIC; (* ON NODE MANAGEMENT*)
  FLAG : ARRAY[1..10] OF BOOLEAN;
         (* 0=> PRINT INFIX EXPRESSIONS
            1=> MARK ENTRY TO BUILD_TREE
            REST UNUSED *)
  P : LINK;                 (*MISC TEMPORARY*)
  CH, CH2 : CHAR;
  LPAREN, RPAREN : CHAR;
  PLUS, MINUS, ASTR, SLASH, EXP, SEPARATOR, LBRACK : CHAR;
  RBRACK, COMMA, PERIOD, COMMENT, CONTINUE : CHAR;
  EQU : ARRAY[1..3,0..1] OF ROOTNODE; (*EQUATION TREES AS READ*)
  ID, ID2 : ALFA;
  COMMAND : ARRAY[1..COMMANDMAX] OF ALFA;
 
INITPROCEDURE;
 
  BEGIN (*INITPROCEDURE*)
    ALPHABET := ['A'..'Z'];
    DIGITS := ['0'..'9'];
    LETTERSORDIGITS := ['A'..'Z','0'..'9'];
    OPERATORS := ['+','-','*','/'];
    PRIO1 := ['*','/'];
    PRIO2 := ['+','-'];
    ADDSUB := [ADD,SUB];
    MULDIV := [MULT,DIVD];
    FUNCSPEC := [FUNC,SPEC];
    OPERCHAR[ADD] := '+';
    OPERCHAR[SUB] := '-';
    OPERCHAR[MULT] := '*';
    OPERCHAR[DIVD] := '/';
    OPERCHAR[EXPON] := '^';
    LPAREN := '(';
    RPAREN := ')';
    PLUS := '+';
    MINUS := '-';
    ASTR := '*';
    SLASH := '/';
    LBRACK := '[';
    RBRACK := ']';
    COMMA := ',';
    PERIOD := '.';
    COMMENT := '$';
    CONTINUE := '#';
    COMMAND[1] := 'CLEAR     ';
    COMMAND[2] := 'ENTER     ';
    COMMAND[3] := 'FLAG      ';
    COMMAND[4] := 'PRINT     ';
    COMMAND[5] := 'QUIT      ';
    COMMAND[6] := 'SOLVE     ';
    RUNNING:=TRUE;
    INITIALIZE:=TRUE;
    AVAILNODE:=NIL;
    AVAILSYMREC:=NIL;
    AVAILTOKEN;=NIL;
    AVAILROOT:=NIL;
    FIRST:=TRUE
    (* ETC. *)
  END;  (*INITPROCEDURE*)
 
PROCEDURE FIND;
 
  BEGIN (*FIND*)
    CH:=NULL;
    IF EOLN(TTY) THEN SEARCHING:=FALSE
    ELSE WHILE (CH=NULL) AND SEARCHING DO BEGIN
      READ(TTY,CH)
      IF CH=COMMENT THEN SEARCHING:=FALSE
      ELSE IF CH=CONTINUE THEN BEGIN
        READLN(TTY);
        READ(TTY,CH)
      END;
      IF EOLN(TTY) THEN SEARCHING:=FALSE
    END
  END;  (*FIND*)
 
PROCEDURE SPAN;
 
  BEGIN (*SPAN*)
    WHILE (CH<>NULL) AND (NOT EOLN(TTY)) DO READ(TTY,CH)
  END;  (*SPAN*)
 
PROCEDURE READNEXTLINE;
 
  BEGIN (*READNEXTLINE*)
    BREAK(TTY); (*PATIENCE*)
    READLN(TTY)
  END;  (*READNEXTLINE*)
 
(*PROCEDURE ERROR (MESSAGE : ALFA);
 
  BEGIN (*ERROR*)   (*TO BE GROSSLY EXPANDED*)
    SCANNING:=FALSE;
    WRITELN(TTY,MESSAGE);
    INITIALIZE:=TRUE
  END;  (*ERROR*) *)
 
PROCEDURE RECLAIM_NODES;
VAR T : TOKEN;
  
  PROCEDURE UNBUILD_LINK(VAR L : LINK);
  
    BEGIN (*UNBUILD_LINK*)
      IF L<>NIL THEN WITH L^ DO BEGIN
        UNBUILD_LINK(LLINK); (*CLEAR OPERAND EXPRESSIONS AND*)
        UNBUILD_LINK(RLINK); (*RETURN TO AVAILABLE LIST*)
        RLINK:=AVAILNODE;
        AVAILNODE:=L
      END
    END;  (*UNBUILD_LINK*)
      
  PROCEDURE UNBUILD_SYM(VAR L : PTR);
 
    BEGIN (*UNBUILD_SYM*)
      IF L<>NIL THEN WITH L^ DO BEGIN
        UNBUILD_SYM(SLPTR);
        UNBUILD_SYM(SRPTR);
        IF SUBEXPR.RTTAG=POINTER THEN
          UNBUILD_LINK(SUBEXPR.RTLINK);
        SRPTR:=AVAILSYMREC;
        AVAILSYMREC:=L
      END
    END;  (*UNBUILD_SYM*)
 
  PROCEDURE UNBUILD_ROOT(VAR R : ROOT);
  VAR R1 : ROOT;
 
    BEGIN (*UNBUILD_ROOT*)
      IF R<>NIL THEN BEGIN
        R1:=R;
        WHILE R1^.RTNEXT<>NIL DO
          R1:=R1^.RTNEXT
        R1^.RTNEXT:=AVAILROOT;
        AVAILROOT:=R
      END
    END;  (*UNBUILD_ROOT*)
 
    BEGIN (*RECLAIM_NODES*)
      FOR I:=1 TO 3 DO BEGIN
        FOR J:=0 TO 3 DO BEGIN
          UNBUILD_LINK(BASE[I,J].RTLINK);
          IF J<2 THEN BEGIN
            UNBUILD_LINK(EQU[I,J].RTLINK);
            UNBUILD_ROOT(EQU[I,J].RTNEXT)
          END
        END;
        WRT[I]:=NIL
      END;
        UNBUILD_SYM(SYMBOLTABLE);
        IF TOKENLIST<>NIL THEN BEGIN
          T:=TOKENLIST;
          SEARCHING:=TRUE;
          WHILE SEARCHING DO BEGIN
            UNBUILD_SYM(T^.XPTR);
            IF T^.XPREV=NIL THEN SEARCHING:=FALSE
            ELSE T:=T^.XPREV
          END;
          T^.XPREV:=AVAILTOKEN;
          AVAILTOKEN:=TOKENLIST
        END
    END;  (*RECLAIM_NODES*)
 
PROCEDURE INIT_NODE(VAR L: LINK);
 
  BEGIN (*INIT_NODE*)
    IF AVAILNODE = NIL THEN BEGIN
      STATS[NODESTAT.NEWSTAT:=STATS[NODESTAT.NEWSTAT+1;
      NEW(L)
    END
    ELSE BEGIN
      STATS[NODESTAT.OLDSTAT:=STATS[NODESTAT.OLDSTAT+1;
      L:=AVAILNODE;
      AVAILNODE:=AVAILNODE^.RLINK
    END;
    WITH L^ DO BEGIN
      LLINK:=NIL;
      RLINK:=NIL;
      NPTR:=NIL;
      LPTR:=NIL;
      RPTR:=NIL;
      LTAG:=NONE;
      RTAG:=NONE
    END
  END;  (*INIT_NODE*)
 
PROCEDURE INIT_SYMREC(VAR P : PTR);
  
  BEGIN
    IF AVAILSYMREC=NIL THEN BEGIN
      NEW(P);
      STATS[SYMRECSTAT.NEWSTAT:=STATS[SYMRECSTAT.NEWSTAT+1
    END
    ELSE BEGIN
      STATS[SYMRECSTAT.OLDSTAT:=STATS[SYMRECSTAT.OLDSTAT+1;
      P:=AVAILSYMREC;
      AVAILSYMREC:=AVAILSYMREC^.SRPTR
    END;
    WITH P^ DO BEGIN
      SLPTR:=NIL;
      SRPTR:=NIL;
      DEPENDENT:=FALSE;
      SUBEXPR.TRNEXT:=NIL;
      SUBEXPR.RTTAG:=NONE
    END
  END;  (*INIT_SYMREC*)
 
PROCEDURE INIT_TOKEN(VAR T : TOKEN);
 
  BEGIN
    IF AVAILTOKEN=NIL THEN BEGIN
      NEW(T);
      STATS[TOKENSTAT.NEWSTAT:=STATS[TOKENSTAT.NEWSTAT+1
    END
    ELSE BEGIN
      STATS[TOKENSTAT.OLDSTAT:=STATS[TOKENSTAT.OLDSTAT+1;
      T:=AVAILTOKEN;
      AVAILTOKEN:=AVAILTOKEN^.XPREV
    END
  END;  (*INIT_TOKEN*)
 
PROCEDURE INIT_ROOT(VAR R : ROOT);
 
  BEGIN (*INIT_ROOT*)
    IF AVAILROOT=NIL THEN BEGIN
       NEW(R);
       STATS[ROOTSTAT.NEWSTAT:=STATS[ROOTSTAT.NEWSTAT+1;
    END
    ELSE BEGIN
      STATS[ROOTSTAT.OLDSTAT:=STATS[ROOTSTAT.OLDSTAT+1;
      R:=AVAILROOT;
      AVAILROOT:=AVAILROOT^.RTNEXT
    END;
    WITH R^ DO BEGIN
      RTPTR:=NIL;
      RTLINK:=NIL;
      RTNEXT:=NIL
    END
  END;  (*INIT_ROOT*)
 
PROCEDURE INSERTSYM(INAME : ALFA);
 
  BEGIN (*INSERTSYM*)
    IF SYMBOLTABLE=NIL THEN BEGIN
      INIT_SYMREC(SYMBOLTABLE);
      SYMPTR:=SYMBOLTABLE;
      SYMPTR^.NAME:=INAME
    END
    ELSE BEGIN
      SYMPTR:=SYMBOLTABLE;
      WITH SYMPTR^ DO BEGIN
        IF INAME<NAME THEN BEGIN
          IF SLPTR<>NIL THEN SYMPTR:=SLPTR
          ELSE BEGIN
            INIT_SYMREC(SLPTR);
            SYMPTR:=SLPTR;
            NAME:=INAME
          END
        ELSE BEGIN
          IF SRPTR<>NIL THEN SYMPTR:=SRPTR
          ELSE BEGIN
            INIT_SYMREC(SRPTR);
            SYMPTR:=SRPTR;
            NAME:=INAME
          END
        END
      END
    END
  END;  (*INSERTSYM*)
 
PROCEDURE NEWTOKEN;
VAR T : TOKEN;
 
  BEGIN (*NEWTOKEN*)
    INIT_TOKEN(T);
    T^.XPREV:=TOKENLIST;
    TOKENLIST:=T
  END;  (*NEWTOKEN*)
 
PROCEDURE PUTOPER (C : CHAR);
 
  BEGIN (*PUTOPER*)
    NEWTOKEN;
    WITH TOKENLIST^ DO
      CASE C OF
      '+': XTAG:=TADD;
   
      '-': XTAG:=TSUB;
   
      '*': XTAG:=TMULT;
   
      '/': XTAG:=TDIVD;
   
      '^': XTAG:=TEXPON;
 
      END
  END;  (*PUTOPER*)
 
PROCEDURE PUTCONST (VAL : INT);
 
  BEGIN (*PUTCONST*)
    NEWTOKEN;
    WITH TOKENLIST^ DO BEGIN
      XTAG:=TCONS;
      XVAL:=VAL
    END
  END;  (*PUTCONST*)
 
PROCEDURE PUTVAR (INAME : ALFA);
 
  BEGIN (*PUTVAR*)
    INSERTSYM(INAME);
    NEWTOKEN;
    WITH TOKENLIST^ DO BEGIN
      XPTR:=SYMPTR;
      CASE SYMPTR^.SYMTYPE OF
        VARBLE: XTAG:=TVAR;
        FUNCT : XTAG:=TFUNC
      END
    END
  END;  (*PUTVAR*)
 
PROCEDURE WRITECONST(V : INT);
VAR I, J, K : INT;
 
  BEGIN (*WRITECONST*)
    J:=V;
    K:=10;
    IF J<0 THEN BEGIN
      WRITECHAR(MINUS);
      J:=-J
    END;
    REPEAT
      ID2[K]:=CHR((J MOD 10) + ORD('0'));
      K:=K-1;
      J:=J DIV 10
    UNTIL K=1;
    SCANNING:=TRUE;
    FOR I:=1 TO 10 DO 
      IF (ID2[I]<>'0') OR NOT SCANNING OR (I=10)THEN BEGIN
        SCANNING:=FALSE;
        WRITE(TTY,ID[I]);
      END
    IF LASTCHAR>MAXLINE THEN BEGIN
      WRITELN(TTY);
      LASTCHAR:=0
    END
  END;  (*WRITECONST*)
 
PROCEDURE WRITECHAR(S : CHAR);
 
  BEGIN
    WRITE(TTY,S);
    LASTCHAR:=LASTCHAR+1;
    IF LASTCHAR>MAXLINE THEN BEGIN
      WRITELN(TTY);
      LASTCHAR=0
    END
  END;  (*WRITECHAR*)
 
PROCEDURE WRITESTRING(S : ALFA);
VAR I : INT;
 
  BEGIN (*WRITESTRING*)
    FOR I:=1 TO 10 DO
      IF S[I]<>NULL THEN BEGIN
        WRITE(TTY,S[I]);
        LASTCHAR:=LASTCHAR+1
      END;
    IF LASTCHAR>MAXLINE THEN BEGIN
      WRITELN(TTY);
      LASTCHAR:=0
    END
  END;  (*WRITESTRING*)
 
PROCEDURE WRITEEXPR(VAR PT: LINK);
 
VAR PARENFLAG : BOOLEAN;
 
  BEGIN (*WRITEEXPR*)
    PARENFLAG:=FALSE;
    CASE PT^.LTAG OF
      POINTER: BEGIN (*LEFT OPERAND AN EXPRESSION*)
                 IF (PT^.OPER IN MULDIV) AND (PT^.LLINK^.OPER IN ADDSUB) THEN BEGIN
                   PARENFLAG:=TRUE; (*E.G.:  (A+B)*C  *)
                   WRITECHAR(RPAREN)
                 END;
                 WRITEEXPR(PT^.LLINK);
                 IF PARENFLAG THEN WRITECHAR(RPAREN);
                 PARENFLAG:=FALSE
               END;
 
     VARIABLE: WRITESTRING(PT^.LPTR^.NAME);
 
     CONSTANT: WRITECONST(PT^.LVAL)
    END;
    IF NOT (PT^.OPER IN FUNCSPEC) 
      THEN WRITECHAR(OPERCHAR[PT^.OPER]) (* +,-,ETC. *)
    ELSE WRITESTRING(PT^.NPTR^.NAME); (* SIN,LOG,ETC. *)
    CASE PT^.RTAG OF
      POINTER:  BEGIN (*RIGHT OPERAND AN EXPRESSION*)
                 IF (PT^.OPER IN MULDIV) AND (PT^.RLINK^.OPER IN ADDSUB) THEN BEGIN
                   PERENFLAG:=TRUE;
                   WRITECHAR(LPAREN)
                 END;
                 WRITEEXPR(PT^.RLINK);
                 IF PARENFLAG THEN WRITECHAR(RPAREN);
                 PARENFLAG:=FALSE
               END;
 
     VARIABLE: WRITESTRING(PT^.RPTR^.NAME);
 
     CONSTANT: WRITECONST(PT^.RVAL)
    END
  END;  (*WRITEEXPR*)
 
PROCEDURE WRITEPOSTFIX;
VAR T : TOKEN; PREVTOKEN : BOOLEAN;
 
  BEGIN (*WRITEPOSTFIX*)
    LASTCHAR:=0;
    PREVTOKEN:=FALSE;
    T:=TOKENLIST;
    WHILE T<>NIL DO WITH T^ DO BEGIN
      CASE XTAG OF
       
       CONS: BEGIN
               IF PREVTOKEN THEN WRITECHAR(SEPARATOR);
               WRITECONST(XVAL)
             END;
 
       VARBL: BEGIN
                IF PREVTOKEN THEN WRITECHAR(SEPARATOR);
                WRITESTRING(XPTR^.NAME)
              END;
 
       FUNC,SPECIAL: (*LATER*)
 
       OTHERS : WRITECHAR(OPERCHAR[XTAG])
 
      END;
      PREVTOKEN:=(XTAG IN [CONS,VARBL,FUNC,SPECIAL]);
      T:=XPREV
    END;
  IF LASTCHAR>0 THEN WRITELN(TTY);
  BREAK(TTY)
  END;  (*WRITEPOSTFIX*)
 
PROCEDURE SCANIDENTIFIER;
 
  BEGIN (*SCANIDENTIFIER*)
    ID:=BLANKS;
    LENGTH:=0;
    REPEAT
      IF LENGTH<10 THEN BEGIN
        LENGTH=LENGTH+1;
        ID[LENGTH]:=CH
      END;
      READ(TTY,CH)
    UNTIL EOLN(TTY) OR NOT (CH IN LETTERSORDIGITS)
  END;  (*SCANIDENTIFIER*)
 
PROCEDURE POSTFIX;
 
  PROCEDURE EXPRESSION;
  VAR OP1 : CHAR;
 
    PROCEDURE TERM;
    VAR OP2 : CHAR;
   
      PROCEDURE FACTOR;
     
      BEGIN (*FACTOR*)
        IF CH='(' THEN BEGIN
          FIND;
          EXPRESSION
        END
        ELSE IF CH IN ALPHABET THEN BEGIN
          ID:=BLANKS;
          LENGTH:=0;
          REPEAT
            IF LENGTH<10 THEN BEGIN
              LENGTH:=LENGTH+1;
              ID[LENGTH]:=CH
            END;
            READ(TTY,CH)
          UNTIL EOLN(TTY) OR NOT(CH IN LETTERSORDIGITS);
          PUTVAR(ID)
        END
        ELSE IF CH IN DIGITS THEN BEGIN 
           VAL:=0;
           REPEAT
             VAL:=VAL*10+ORD(CH)-ORD('0');
             READ(TTY,CH)
           UNTIL EOLN(TTY) OR NOT(CH IN DIGITS);
           PUTCONST(VAL)
         END
        ELSE BEGIN
          SCANNING:=FALSE;
          WRITELN(TTY,SYNTAXERROR);
          INITIALIZE:=TRUE
        END;
        IF SCANNING THEN FIND
      END;  (* FACTOR *)
   
    BEGIN (*TERM*)
      FACTOR;
      IF SCANNING THEN WHILE CH IN PRIO1 DO BEGIN
        OP2:=CH;
        FIND;
        FACTOR;
        PUTCHAR(OP2)
      END
    END;  (*TERM*)
 
VAR SIGNED : BOOLEAN;
  BEGIN (*EXPRESSION*)
    SIGNED:=FALSE;
    IF CH='+' THEN FIND; (*SKIP UNARY + *)
    ELSE IF CH='-' THEN BEGIN
      SIGNED:=TRUE;
      PUTCONST(0)  (*IMPLIED TERM*)
    END;
    TERM;
    IF SCANNING THEN BEGIN
      IF SIGNED THEN PUTOPER(SUB); (*IMPLIED OPERATOR*)
      WHILE CH IN PRIO2 DO BEGIN
      OP1:=CH;
      FIND;
      TERM;
      PUTCHAR(OP1)
     END
    END
  END;  (*EXPRESSION*)
 
  BEGIN (*POSTFIX*)
    LASTCHAR:=0;
    FIND;
    REPEAT EXPRESSION
    UNTIL (CH IN ['=','.']) OR NOT SCANNING;
    TOP:=LASTCHAR;
    IF FLAG[1] THEN WRITEPOSTFIX
  END;  (*POSTFIX*)
 
PROCEDURE BUILD_TREE(VAR PTR: LINK);
 
  BEGIN (*BUILD_TREE*)
    IF TOKENLIST<>NIL THEN WITH TOKENLIST^ DO BEGIN
      CASE XTAG OF
    
       CONS: BEGIN
               PT^.RTAG:=CONSTANT;
               PT^.RVAL:=XVAL;
               TOKENLIST:=XPREV
             END;
 
       VARBL: BEGIN
                PT^.RTAG:=VARIABLE;
                PT^.RPTR:=XPTR;
                TOKENLIST:=XPREV
              END;
 
       OTHERS: BEGIN
                 PT^.OPERATOR:=XTAG;
                 PT^.RTAG:=POINTER;
                 INIT_NODE(PT^.RLINK);
                 TOKENLIST:=XPREV;
                 BUILD_TREE(PT^.RLINK)
               END
 
      END;
      CASE XTAG OF
 
       CONS: BEGIN
               PT^.LTAG:=CONSTANT;
               PT^.LVAL:=XVAL;
               TOKENLIST:=XPREV
             END;
 
       VARBL: BEGIN
                PT^.LTAG:=VARIABLE;
                PT^.LPTR:=XPTR;
                TOKENLIST:=XPREV
              END;
 
       OTHERS: BEGIN
                 PT^.OPERATOR:=XTAG;
                 PT^.LTAG:=POINTER;
                 INIT_NODE(PT^.LLINK);
                 TOKENLIST:=XPREV;
                 BUILD_TREE(PT^.LLINK)
               END
 
      END
    END
  END;  (*BUILD_TREE*)
 
PROCEDURE DUMPSTATS;
 
  BEGIN (*DUMPSTATS*)
    WRITELN(TTY,STATHEADER);
    FOR I:=1 TO MAXSTAT DO
      WITH STATS[I] DO
        WRITELN(TTY,STATTYPE:10,NEWSTAT:5,OLDSTAT:8);
    WRITELN(TTY)
  END;  (*DUMPSTATS*)
 
FUNCTION COMMANDSCAN : INT;
 
  BEGIN (*COMMANDSCAN*)
    SEARCHING:=TRUE;
    FIND;
    COMMANDSCAN:=0;
    IF SEARCHING THEN BEGIN
      IF NOT (CH IN ALPHABET THEN COMMANDSCAN:=COMMANDMAX+1
      ELSE BEGIN
        SCANIDENTIFIER;
        I:=1;
        WHILE SEARCHING AND (I<=COMMANDMAX) DO BEGIN
          IF ID>COMAND[I] THEN I:=I+1
          ELSE BEGIN
            SEARCHING:=FALSE;
            J:=1;
            WHILE (J<=10) AND (COMMAND[I,J]=ID[J]) DO
              J:=J+1;
            IF J<=10 THEN
              IF ID[J]=NULL THEN
                COMMANDSCAN:=I
              ELSE COMMANDSCAN:=COMMANDMAX+1
            ELSE COMMANDSCAN:=I
          END
        END
      END
    END
  END;  (*COMMANDSCAN*)
 
BEGIN (*SYMBOL*)
  FOR I:=1 TO 10 DO
    FLAG[I]:=FALSE;
  REWRITE(TTYOUTPUT);
  WRITELN(TTY);
  WRITELN(TTY,OPENING);
  OPEN(TTY);
  WHILE RUNNING DO BEGIN
    IF INITIALIZE THEN BEGIN
      INITIALIZE:=FALSE;
    NOT FIRST THEN RECLAIM_NODES;
      FIRST:=FALSE;
      FOR I:=1 TO 3 DO BEGIN
        FOR J:=0 TO 3 DO BEGIN 
          BASE[I,J]:=NIL;
          IF J<2 THEN EQU[I,J]:=NIL;
        END;
        WRT[I]:=' '
      END;
      FOR CH:='A' TO 'Z' DO
        DEPENDENT[CH]:=FALSE
      (*ETC*)
    END;
 1: WRITE(TTY,PROMPT);
    READNEXTLINE;
    IF EOLN(TTY) THEN GOTO 1;
    READ(TTY,CH);
    CASE CH OF
      'Q': RUNNING:=FALSE; (*QUIT*)
    
      'C': INITIALIZE:=TRUE; (*CLEAR*)
 
      'F': BEGIN (*FLAG*)
             SPAN;
             WHILE NOT EOLN(TTY) DO BEGIN
               READ(TTY,I);
               IF I IN [1..10] THEN BEGIN 
                 FLAG[I]:=TRUE;
                 WRITELN(TTY,' FLAG ',I:2,' = ON')
               END
               ELSE IF (I>=-10) AND (I<0) THEN BEGIN
                 I:=-I;
                 FLAG[I]:=FALSE;
                 WRITELN(TTY,' FLAG ',I:2,' = OFF');
               END;
               IF NOT EOLN(TTY) THEN READ(TTY,CH)
             END
           END;
 
      'P': BEGIN (*PRINT*) 
             SCANNING:=TRUE;
             FOR I:=1 TO 3 DO
               IF WRT[I] IN ALPHABET THEN SCANNING:=FALSE;
             IF SCANNING THEN BEGIN
               FOR I:=1 TO 3 DO BEGIN
                 IF EQU[I,0]<>NIL THEN BEGIN
                   LINEPTR:=0; SCANNING:=FALSE;
                   PRINTEXPR(EQU[I,0]);
                   PRINTCHAR('=');
                   PRINTEXPR(EQU[I,1]);
                   IF LINEPTR>0 THEN WRITELN(TTY)
                 END
               END;
               IF SCANNING THEN WRITELN(TTY,PRINTERROR)
             END
             ELSE BEGIN
               FOR I:=1 TO COUNT DO
                 FOR J:=1 TO 3 DO BEGIN
                   SCANNING:=TRUE; LINEPTR:=0;
                   IF BASE[I,J]<>NIL THEN BEGIN
                     IF NOT SCANNING THEN PRINTCHAR('+');
                     SCANNING:=FALSE;
                     PRINTEXPR(BASE[I,J])
                   END;
                   PRINTCHAR('=');
                   PRINTEXPR(BASE[I,0]);
                   IF LINEPTR>0 THEN WRITELN(TTY)
                 END
             END
           END;
    
      'S': BEGIN (*SOLVE*)
             WRITELN(TTY,' SOLUTION NOT YET IMPLEMENTED.')
           END;
 
      'E': BEGIN (*ENTER*)
             SPAN;
             COUNT:=0;
             FOR CH:='A' TO 'Z' DO DEPENDENT[CH]:=FALSE;
             WHILE NOT EOLN(TTY) DO BEGIN;
               READ(TTY,CH);
               IF (CH IN ALPHABET) THEN IF (DEPENDENT[CH]=FALSE) THEN BEGIN
                 COUNT:=COUNT+1;
                 DEPENDENT[CH]:=TRUE
               END 
             END;
             IF (COUNT<1) OR (COUNT>3) THEN WRITELN(TTY,ERROR1,' COUNT= ',COUNT)
             ELSE BEGIN
               FOR I:=1 TO 3 DO
                 FOR J:=0 TO 1 DO
                   EQU[I,J]:=NIL;
               LINE_COUNT:=1;
               SCANNING:=TRUE;
               WHILE SCANNING AND (LINE_COUNT<=COUNT) DO BEGIN
                 WRITE(TTY,LINE_COUNT:1,PREFIX);
                 READNEXTLINE;
                 POSTFIX;
                 IF SCANNING THEN BEGIN
                   INIT_NODE(EQU[LINE_COUNT,0]);
                   BUILD_TREE(EQU[LINE_COUNT,0]);
                   POSTFIX;
                   IF SCANNING THEN BEGIN
                     INIT_NODE(EQU[LINE_COUNT,1]);
                     BUILD_TREE(EQU[LINE_COUNT,1])
                   END
                 END;
               LINE_COUNT:=LINE_COUNT+1
               END
             END
           END;
      OTHERS : WRITELN(TTY,WHAT)
      END
    END
  END (*SYMBOL*).
 Zu=v