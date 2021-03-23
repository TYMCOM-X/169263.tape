INPUT:   <FILE SPEC>;
INPUT:   
INPUT:   *
INPUT:   * Define a dummy set of all the terminals just to control the order in which
INPUT:   * LEXGEN sees, and thus encodes, the terminals.
INPUT:   *
INPUT:   "dummy" ::= ['ALPHA' 'OCTAL_DIGIT' 'DECIMAL_DIGIT' '#' '?' '*' '[' ',' ']' '<' '>' '.' ':' '(' ')'];
INPUT:   "ALPHANUMS" ::= ['ALPHA' 'OCTAL_DIGIT' 'DECIMAL_DIGIT'];
INPUT:   "NAME CHARS" ::= ['ALPHA' 'OCTAL_DIGIT' 'DECIMAL_DIGIT' '#' '?' '*'];
INPUT:   
INPUT:   <DIRECTORY> ::= ('[' 'OCTAL_DIGIT'* ',' 'OCTAL_DIGIT'* ']') !
INPUT:           ('[' ']');
INPUT:   <PROT> ::= '<' 'OCTAL_DIGIT' 'OCTAL_DIGIT' 'OCTAL_DIGIT' '>';
INPUT:   <DIR OR PROT> ::= ( <DIRECTORY> ) ! ( <PROT> ) ! 
INPUT:           ( <DIRECTORY> <PROT> ) ! ( <PROT> <DIRECTORY> );
INPUT:   <EXTENSION> ::= '.' "NAME CHARS" *;
INPUT:   <NAME> ::= "NAME CHARS"+;
INPUT:   <DEVICE> ::= "ALPHANUMS"+ ':';
INPUT:   <DEC FILE> ::= ( <DEVICE> ! '' )  ( <NAME> )  ( <EXTENSION> ! '' )  (<DIR OR PROT> ! '' ) ;
INPUT:   
INPUT:   <ACNT NAME> ::= '(' "ALPHANUMS"+ ')';
INPUT:   <TYMSHARE FILE> ::= (<ACNT NAME> ! '') <NAME> (<EXTENSION> ! '');
INPUT:   
INPUT:   <FILE SPEC> ::= <DEC FILE> ! <TYMSHARE FILE>****** CODES FOR TERMINAL SYMBOLS ******

          CODE   SYMBOL
          -16   ')'
          -15   '('
          -14   ':'
          -13   '.'
          -12   '>'
          -11   '<'
          -10   ']'
           -9   ','
           -8   '['
           -7   '*'
           -6   '?'
           -5   '#'
           -4   'DECIMAL_DIGIT'
           -3   'OCTAL_DIGIT'
           -2   'ALPHA'
           -1   '*****END OF DATA*****'


****** CODES FOR NONTERMINALS TO BE RECOGNIZED ******

          CODE   NONTERMINAL
            1   '*****END OF DATA*****'
            2   'FILE SPEC'****** MINIMIZED DFA NEXT STATE TABLE ******

      -16 -15 -14 -13 -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2  -1 
    __________________________________________________________________
    !
  1 !   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
    !
  2 !   0   0  10  12   0  14   0   0   4   8   8   8   2   2   2   0 
    !
  3 !   0   0   0   0  19   0   0   0   0   0   0   0   0   0   0   0 
    !
  4 !   0   0   0   0   0   0  17   9   0   0   0   0   0   5   0   0 
    !
  5 !   0   0   0   0   0   0   0   9   0   0   0   0   0   5   0   0 
    !
  6 !   0  22   0   0   0   0   0   0   0   8   8   8   2   2   2   1 
    !
  7 !   0   0   0   0   0   0   0   0   0   0   0   0   0   3   0   0 
    !
  8 !   0   0   0  12   0  14   0   0   4   8   8   8   8   8   8   0 
    !
  9 !   0   0   0   0   0   0  17   0   0   0   0   0   0   9   0   0 
    !
 10 !   0   0   0   0   0   0   0   0   0  15  15  15  15  15  15   0 
    !
 11 !   0   0   0   0   0   0   0   0   0   0   0   0   0   7   0   0 
    !
 12 !   0   0   0   0   0  14   0   0   4  12  12  12  12  12  12   0 
    !
 13 !   0   0   0   0   0   0  20  25   0   0   0   0   0  29   0   0 
    !
 14 !   0   0   0   0   0   0   0   0   0   0   0   0   0  11   0   0 
    !
 15 !   0   0   0  12   0  14   0   0   4  15  15  15  15  15  15   0 
    !
 16 !   0   0   0   0   0   0   0   0   0  16  16  16  16  16  16   0 
    !
 17 !   0   0   0   0   0  28   0   0   0   0   0   0   0   0   0   0 
    !
 18 !   0   0   0  16   0   0   0   0   0  18  18  18  18  18  18   0 
    !
 19 !   0   0   0   0   0   0   0   0  13   0   0   0   0   0   0   0 
    !
 20 !   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
    !
 21 !  26   0   0   0   0   0   0   0   0   0   0   0  21  21  21   0 
    !
 22 !   0   0   0   0   0   0   0   0   0   0   0   0  21  21  21   0 
    !
 23 !   0   0   0   0  20   0   0   0   0   0   0   0   0   0   0   0 
    !
 24 !   0   0   0   0   0   0   0   0   0   0   0   0   0  23   0   0 
    !
 25 !   0   0   0   0   0   0  20   0   0   0   0   0   0  25   0   0 
    !
 26 !   0   0   0   0   0   0   0   0   0  18  18  18  18  18  18   0 
    !
 27 !   0   0   0   0   0   0   0   0   0   0   0   0   0  24   0   0 
    !
 28 !   0   0   0   0   0   0   0   0   0   0   0   0   0  27   0   0 
    !
 29 !   0   0   0   0   0   0   0  25   0   0   0   0   0  29   0   0 

****** START STATE:    6****** FINAL/BACKUP STATES IN MINIMIZED DFA ******

          DFA STATE   FINAL/BACKUP STATE (S)
            0      *<**ERROR**>
            1      *<*****END OF DATA*****> 
            2      *<FILE SPEC> 
            3      
            4      
            5      
            6      
            7      
            8      *<FILE SPEC> 
            9      
           10      
           11      
           12      *<FILE SPEC> 
           13      
           14      
           15      *<FILE SPEC> 
           16      *<FILE SPEC> 
           17      *<FILE SPEC> 
           18      *<FILE SPEC> 
           19      *<FILE SPEC> 
           20      *<FILE SPEC> 
           21      
           22      
           23      
           24      
           25      
           26      
           27      
           28      
           29      ****** COMPACTED VERSION OF MINIMIZED DFA NEXT STATE TABLE ******

             DEFAULT  BASE

           1      0      0
           2      1     15
           3      1     12
           4      1     22
           5      4     17
           6      2     29
           7      1     30
           8      2     32
           9      1     35
          10      1     45
          11      1     36
          12      8     51
          13      1     56
          14      1     38
          15     10     59
          16      1     71
          17      1     49
          18      1     77
          19      1     53
          20      1      0
          21      1     83
          22     21     46
          23      1     51
          24      1     65
          25      1     85
          26     18     56
          27      1     68
          28      1     86
          29     13     81              NEXT  CHECK              NEXT  CHECK              NEXT  CHECK

           1     0      1          51    15     10         101     0      0
           2     0      1          52    15     10         102     0      0
           3     0      1          53    12     12
           4     0      1          54    12     12
           5     0      1          55    12     12
           6     0      1          56    12     12
           7     0      1          57    12     12
           8     0      1          58    12     12
           9     0      1          59    29     13
          10     0      1          60    28     17
          11     0      1          61    13     19
          12     0      1          62     0     22
          13     0      1          63    20     23
          14     0      1          64     0     12
          15     0      1          65    25     13
          16     0      1          66    20     13
          17     2      2          67     4     15
          18     2      2          68    23     24
          19     2      2          69     0     26
          20     8      2          70    14     15
          21     8      2          71    24     27
          22     8      2          72    12     15
          23     4      2          73    16     16
          24    19      3          74    16     16
          25     5      4          75    16     16
          26    14      2          76    16     16
          27     0      5          77    16     16
          28    12      2          78    16     16
          29    10      2          79    18     18
          30     1      6          80    18     18
          31     9      4          81    18     18
          32    17      4          82    18     18
          33     3      7          83    18     18
          34     8      8          84    18     18
          35     8      8          85    21     21
          36     8      8          86    21     21
          37     0      6          87    21     21
          38     9      9          88    25     25
          39     7     11          89    27     28
          40     0      6          90    16     18
          41    11     14          91     0     29
          42     0      6          92     0      0
          43     0      6          93     0      0
          44    22      6          94     0      0
          45    17      9          95    20     25
          46     0      8          96     0      0
          47    15     10          97     0      0
          48    15     10          98     0      0
          49    15     10          99    26     21
          50    15     10         100     0      0(********************************************)
(*   SCANNER ALGORITHM CREATED BY LEXGEN    *)
(********************************************)

PROGRAM ??????;

CONST
   MAXINDEX    =  ???;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  ???;  (* MAXINDEX + 1 *)
   MAXTOKEN    =    2;
   DFASTATE1   =    6;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   29;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -16;  (* MIN TERMINAL CODE *)
   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)


TYPE
   STATERANGE  = 1..MAXDFASTATE;
   EXSTATERANGE= 0..MAXDFASTATE;
   INDEXRANGE  = 0..MAXINDEX;
   LEXTOKEN    = RECORD
                    TOKEN_TYPE: ???;
                    MORE: ???  (* POINTER TO SYMBOL TABLE, CODE
                                  TO DIFFERENTIATE DIFFERENT SYMBOLS
                                  SUCH AS RELATIONAL OPERATORS OF THE
                                  SAME TOKEN_TYPE, ETC.  *)
                 END;

VAR
   DELTA: PACKED ARRAY [STATERANGE, MINTERMINAL..EODATA] OF EXSTATERANGE := (
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,12,0,14,0,0,4,8,8,8,2,2,2,0,
      0,0,0,0,19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,9,0,0,0,0,0,5,0,0,
      0,0,0,0,0,0,0,9,0,0,0,0,0,5,0,0,0,22,0,0,0,0,0,0,0,8,8,8,2,2,2,1,0,
      0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,12,0,14,0,0,4,8,8,8,8,8,8,0,0,
      0,0,0,0,0,17,0,0,0,0,0,0,9,0,0,0,0,0,0,0,0,0,0,0,15,15,15,15,15,15,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,14,0,0,4,12,12,12,12,
      12,12,0,0,0,0,0,0,0,20,25,0,0,0,0,0,29,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,11,0,0,0,0,0,12,0,14,0,0,4,15,15,15,15,15,15,0,0,0,0,0,0,0,0,0,
      0,16,16,16,16,16,16,0,0,0,0,0,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,
      0,0,0,0,18,18,18,18,18,18,0,0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,26,0,0,0,0,0,0,0,0,0,0,0,21,21,21,0,0,0,
      0,0,0,0,0,0,0,0,0,0,21,21,21,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,23,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,25,0,0,0,
      0,0,0,0,0,0,0,0,18,18,18,18,18,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,27,0,0,0,0,0,0,0,0,0,25,0,0,0,0,0,29,0,
      0);

   (* FINAL [X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA*****>
                 2 IF STATE X RECOGNIZES <FILE SPEC>
                                                                 *)
   FINAL: PACKED ARRAY [EXSTATERANGE] OF 0..MAXTOKEN := (
      0,1,2,0,0,0,0,0,2,0,0,0,2,0,0,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0);

   BEGIN_INDEX, END_INDEX: INDEXRANGE;
   LEXEME: LEXTOKEN;
   BUFFER: ARRAY [INDEXRANGE] OF MINTERMINAL..EODATA;


PROCEDURE SCAN (VAR BEGIN_INDEX, END_INDEX: INDEXRANGE;
               VAR LEXEME: LEXTOKEN);

   VAR
      NEWTOKEN:  BOOLEAN;
      CURRSTATE, CURRFINAL: EXSTATERANGE;
      OLDINDEX:  INDEXRANGE;


   PROCEDURE GETCHAR (NEWTOKEN: BOOLEAN);
      BEGIN
         <  THIS PROCEDURE OBTAINS THE NEXT INPUT CHARACTER (WHICH
            IS ASSUMED TO BE EODATA IF NO MORE INPUT) AND MODIFIES
            BEGIN_INDEX AND END_INDEX AS NECESSARY DEPENDING ON
            THE BUFFERING SCHEME SO THAT
             (1) IF NEWTOKEN, THEN BEGIN_INDEX POINTS TO THE INPUT
                 CHARACTER JUST OBTAINED, ELSE BEGIN_INDEX POINTS
                 TO THE SAME CHARACTER IT POINTED TO BEFORE.
             (2) END_INDEX IS THE INDEX OF THE NEW CHARACTER JUST
                 OBTAINED.
            SCAN ALLOWS FOR EITHER SEQUENTIAL OR CIRCULAR BUFFER  >
      END (* GETCHAR *);


   BEGIN (* SCAN *)
      NEWTOKEN  := TRUE;
      CURRSTATE := DFASTATE1;  (* START IN INITIAL STATE *)
      CURRFINAL := 0;
      OLDINDEX  := 0;  (* WIDTH OF LEXEME AS OF LAST FINAL STATE *)

      WHILE CURRSTATE <> 0 DO
         BEGIN
            IF FINAL [CURRSTATE] <> 0 THEN
               BEGIN
                  CURRFINAL := CURRSTATE;
                  OLDINDEX := (END_INDEX - BEGIN_INDEX) MOD BUFFERSIZE
               END;
            GETCHAR (NEWTOKEN);
            NEWTOKEN := FALSE;
            CURRSTATE := DELTA [CURRSTATE, BUFFER [END_INDEX]]
         END;
      END_INDEX := (BEGIN_INDEX + OLDINDEX) MOD BUFFERSIZE;

       < COMPUTE LEXEME GIVEN FINAL [CURRFINAL], BEGIN_INDEX, END_INDEX, 
         ETC.                                                          >

   END (* SCAN *);


BEGIN (* MAINLINE *)
          .
          .
          .
   SCAN (BEGIN_INDEX, END_INDEX, LEXEME);
              (* AS NEEDED UNTIL END-OF-DATA LEXEME IS OBTAINED *)
          .
          .
          .
END. (* MAINLINE *)


NO ERRORS
   