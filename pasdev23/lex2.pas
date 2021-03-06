0010? (********************************************)
 0011? (*   SCANNER ALGORITHM CREATED BY LEXGEN    *)
 0012? (*    INCORPORATES THE LOOKAHEAD FEATURE    *)
 0013? (********************************************)
 0014? 
  0015? PROGRAM TEST2;
   0016? 
  0017? CONST
  0018?    MAXSTACK    =   50;  (* MAX INDEX IN STACK FOR LOOKAHEAD *)
0019?    MAXINDEX    =  200;  (* MAX INDEX USED TO ACCESS BUFFER *)
 0020?    BUFFERSIZE  =  201;  (* MAXINDEX + 1 *)
0021?    MAXTOKEN    =    5;
0022?    DFASTATE1   =    9;  (* CODE FOR INITIAL STATE OF DFA *)
   0023?    MAXDFASTATE =   17;  (* CODE FOR MAX STATE OF DFA *)
  0024?    MINTERMINAL =  -40;  (* MIN TERMINAL CODE *)
0025?    EODATA      =   -1;  (* CODE FOR END-OF-DATA *)
  0026? 
  0027? 
  0028? TYPE
   0029?    STATERANGE  = 1..MAXDFASTATE;
0030?    EXSTATERANGE= 0..MAXDFASTATE;
0031?    INDEXRANGE  = 0..MAXINDEX;
   0032?    LEXTOKEN    = 0..MAXDFASTATE;
0033?    TERM_RANGE  = MINTERMINAL..EODATA;
0034? 
  0035? VAR
    0036?    DELTA: PACKED ARRAY[STATERANGE,MINTERMINAL..EODATA] OF EXSTATERANGE := (
  0037?       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0038?       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0039?       0,0,2,2,2,2,2,2,2,2,2,2,0,0,0,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
0040?       3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,3,3,3,3,3,3,3,3,3,3,
0041?       3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,11,11,11,11,11,11,11,11,11,11,0,0,
0042?       0,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,6,6,6,6,6,
0043?       6,6,6,6,6,0,0,0,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
0044?       3,3,6,6,6,6,6,6,6,6,6,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0045?       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,
0046?       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,8,8,8,8,8,8,8,8,8,0,0,0,0,3,3,3,
0047?       3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,17,3,3,12,3,3,3,2,2,2,2,2,2,2,2,2,
0048?       2,0,1,0,14,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
 0049?       10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,0,0,0,0,10,
 0050?       10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
0051?       10,10,10,11,11,11,11,11,11,11,11,11,11,0,0,0,0,3,3,3,3,3,3,3,3,3,
 0052?       3,3,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,7,0,13,
 0053?       13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
0054?       13,13,13,13,13,13,13,13,13,13,13,13,13,0,0,0,0,13,13,13,13,13,13,
 0055?       13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,8,8,8,
0056?       8,8,8,8,8,8,8,0,0,0,0,3,3,3,3,3,3,3,3,3,3,3,5,3,3,3,3,3,3,3,3,3,3,
0057?       3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,3,3,3,3,3,3,15,3,3,3,3,3,3,3,3,
    0058?       3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,3,3,3,3,3,3,3,3,
0059?       3,3,3,16,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0);
    0060? 
  0061?    (* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE
  0062?                  1 IF STATE X RECOGNIZES <*****END OF DATA****>
    0063?                  2 IF STATE X RECOGNIZES <DO>
  0064?                  3 IF STATE X RECOGNIZES <GOTO>
0065?                  4 IF STATE X RECOGNIZES <ID>
  0066?                  5 IF STATE X RECOGNIZES <NUMBER>
   0067?                                                                  *)
0068?    FINAL: PACKED ARRAY[EXSTATERANGE] OF 0..MAXTOKEN := (
 0069?       0,1,5,4,4,4,3,2,0,0,4,4,4,0,0,4,4,4);
    0070? 
  0071?    (* BACKUP[X] = SET OF ALL BACKUP TOKENS ASSOCIATED WITH STATE X.
0072?                   SEE "FINAL" COMMENT FOR TOKEN CODES. *)
0073? 
  0074?    BACKUP: ARRAY[STATERANGE] OF SET OF 1..MAXTOKEN := (
  0075?       [],[],[],[  2],[  3],[],[],[],[],[],[],[],[],[],[],[],[]);
   0076? 
  0077?    (* LOOKAHEADFINAL[X] = TRUE IFF LOOKAHEAD WAS REQUIRED TO RECOGNIZE
  0078?           TOKEN ASSOCIATED WITH STATE X.  "FINAL" INDICATES WHICH TOKEN
 0079?           THAT IS,  AND "BACKUP" IS USED TO FIGURE OUT WHERE TO BACKUP TO.  *)
    0080? 
  0081?    LOOKAHEADFINAL: PACKED ARRAY[EXSTATERANGE] OF BOOLEAN := (
 0082?       FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, TRUE, TRUE,FALSE,FALSE,FALSE,
0083?       FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
   0084? 
  0085?    BEGIN_INDEX, END_INDEX: INDEXRANGE;
    0086?    LEXEME: LEXTOKEN;
  0087?    BUFFER: ARRAY[INDEXRANGE] OF MINTERMINAL..EODATA;
0088?    BUFFPOS, TEMPPOS: INDEXRANGE;
0089?    FILENAME: STRING[30];
   0090? 
  0091? 
  0092? PROCEDURE SCAN(VAR BEGIN_INDEX, END_INDEX: INDEXRANGE;
   0093?                VAR LEXEME: LEXTOKEN);
0094? 
  0095?    VAR
 0096?       NEWTOKEN:  BOOLEAN;
  0097?       CURRSTATE, CURRFINAL: EXSTATERANGE;
 0098?       OLDINDEX:  INDEXRANGE;
    0099?       STACK: ARRAY[0..MAXSTACK] OF
   0100?                 RECORD
0101?                    INDEX: INDEXRANGE;
0102?                    STATE: STATERANGE
 0103?                 END;
  0104?       TOS: 0..MAXSTACK;  (* CURRENT TOP OF STACK INDEX *)
0105? 
  0106? 
  0107?    PROCEDURE PUSH(ININDEX: INDEXRANGE; INSTATE: STATERANGE);
  0108?       BEGIN
 0109?          TOS := TOS + 1;
   0110?          STACK[TOS].INDEX := ININDEX;
0111?          STACK[TOS].STATE := INSTATE
 0112?       END; (* PUSH *)
 0113? 
  0114? 
  0115?    PROCEDURE GETCHAR(NEWTOKEN: BOOLEAN);
  0116?       BEGIN
 0117?          IF NEWTOKEN THEN
  0118?             BEGIN
0119?                BEGIN_INDEX := END_INDEX + 1;
   0120?                END_INDEX := BEGIN_INDEX
   0121?             END
  0122?          ELSE
    0123?             END_INDEX := END_INDEX + 1;
   0124?          IF END_INDEX > BUFFPOS THEN
 0125?             BEGIN
0126?                BUFFPOS := 0;
    0127?                BEGIN_INDEX := 0;
0128?                END_INDEX := 0;
  0129?                WHILE NOT EOLN(INPUT) DO
   0130?                   BEGIN
    0131?                      IF (INPUT^ >= '0') AND (INPUT^ <= '9') THEN
   0132?                         BUFFER[BUFFPOS] := -ORD(INPUT^) + 45
  0133?                      ELSE IF (INPUT^ >= 'A') AND (INPUT^ <= 'Z') THEN
   0134?                         BUFFER[BUFFPOS] := -ORD(INPUT^) + 52
  0135?                      ELSE IF INPUT^ = '=' THEN
 0136?                         BUFFER[BUFFPOS] := -39
 0137?                      ELSE IF INPUT^ = ',' THEN
 0138?                         BUFFER[BUFFPOS] := -40;
0139?                      IF INPUT^ <> ' ' THEN
0140?                         BUFFPOS := BUFFPOS + 1;
0141?                      GET(INPUT)
 0142?                   END;
0143?                BUFFER[BUFFPOS] := -2;   (* SUBSTITUTE BLANK FOR EOL *)
  0144?                GET(INPUT);
 0145?                IF EOF(INPUT) THEN
    0146?                   BEGIN
    0147?                      BUFFPOS := BUFFPOS + 1;
   0148?                      BUFFER[BUFFPOS] := EODATA
 0149?                   END
 0150?             END
  0151?       END; (* GETCHAR *)
   0152? 
  0153? 
  0154?    BEGIN (* SCAN *)
   0155?       NEWTOKEN  := TRUE;
   0156?       TOS := 0;
  0157?       CURRSTATE := DFASTATE1;  (* START IN INITIAL STATE *)
   0158?       CURRFINAL := 0;
 0159?       OLDINDEX  := 0;  (* WIDTH OF LEXEME AS OF LAST FINAL STATE *)
0160? 
  0161?       WHILE CURRSTATE <> 0 DO
   0162?          BEGIN
   0163?             IF BACKUP[CURRSTATE] <> [] THEN
    0164?                PUSH((END_INDEX-BEGIN_INDEX) MOD BUFFERSIZE, CURRSTATE);
 0165?             IF FINAL[CURRSTATE] <> 0 THEN
 0166?                BEGIN
  0167?                   CURRFINAL := CURRSTATE;
 0168?                   OLDINDEX := (END_INDEX - BEGIN_INDEX) MOD BUFFERSIZE
  0169?                END;
   0170?             GETCHAR(NEWTOKEN);
  0171?             NEWTOKEN := FALSE;
  0172?             CURRSTATE := DELTA[CURRSTATE, BUFFER[END_INDEX]]
  0173?          END;
    0174?       IF LOOKAHEADFINAL[CURRFINAL] THEN
   0175?          BEGIN
   0176?             WHILE NOT (FINAL[CURRFINAL] IN BACKUP[STACK[TOS].STATE]) DO
 0177?                TOS := TOS - 1;
  0178?             END_INDEX := (STACK[TOS].INDEX + BEGIN_INDEX) MOD BUFFERSIZE
0179?          END
0180?       ELSE
  0181?          END_INDEX := (BEGIN_INDEX + OLDINDEX) MOD BUFFERSIZE;
0182? 
  0183?       LEXEME := FINAL[CURRFINAL]
0184? 
  0185?    END; (* SCAN *)
    0186?    FUNCTION CHARACTER(CH: TERM_RANGE): CHAR;
   0187?       BEGIN
 0188?          CASE CH OF
   0189?             -2:       CHARACTER := ' ';
   0190?             -12..-3:  CHARACTER := CHR(-CH+45);
0191?             -38..-13: CHARACTER := CHR(-CH+52);
0192?             -39:      CHARACTER := '=';
   0193?             -40:      CHARACTER := ','
    0194?          END
0195?       END;
  0196?   
0197?   
0198? 
  0199? 
  0200? BEGIN (* MAINLINE *)
  0201?   
0202?    REWRITE(TTYOUTPUT,'TTY:');
   0203?    WRITE(TTYOUTPUT,'INPUT FILE-- ');
 0204?    BREAK(TTYOUTPUT);
  0205?    RESET(TTY,'TTY:');
 0206?    FILENAME := '';
    0207?    WHILE NOT EOLN(TTY) DO
  0208?       BEGIN
 0209?          FILENAME := FILENAME !! TTY^;
    0210?          GET(TTY)
0211?       END;
  0212?    RESET(INPUT,FILENAME);
  0213?   
0214?    END_INDEX := 0;
    0215?    BUFFPOS := 0;
 0216?   
0217?    REPEAT
   0218?       SCAN(BEGIN_INDEX,END_INDEX,LEXEME); 
0219?       WRITE(TTYOUTPUT,'BACK FROM SCAN WITH LEXEME',LEXEME:4,' ');
  0220?       IF LEXEME <> 1 THEN
  0221?          FOR TEMPPOS := BEGIN_INDEX TO END_INDEX DO
 0222?             WRITE(TTYOUTPUT,'"',CHARACTER(BUFFER[TEMPPOS]),'"');
   0223?       WRITELN(TTYOUTPUT)
   0224?    UNTIL LEXEME=1;
    0225?   
0226?    WRITELN(TTYOUTPUT,'-- GOT END OF FILE --')
  0227?   
0228? END. (* MAINLINE *)
   