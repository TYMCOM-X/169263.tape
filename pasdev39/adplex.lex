INPUT:   <FILE SPEC>;
INPUT:   
INPUT:   *
INPUT:   * Define a dummy set of all the terminals just to control the order in which
INPUT:   * LEXGEN sees, and thus encodes, the terminals. 
INPUT:   *
INPUT:   "dummy" ::= ['ALPHA' 'OCTAL_DIGIT' 'DECIMAL_DIGIT' '#' '?' '*' '[' ',' ']' '<' '>' '.' ':'];
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
INPUT:   <FILE SPEC> ::= <DEC FILE>****** CODES FOR TERMINAL SYMBOLS ******

          CODE   SYMBOL
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

      -14 -13 -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2  -1 
    __________________________________________________________
    !
  1 !   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
    !
  2 !  10  12   0  14   0   0   4   8   8   8   2   2   2   0 
    !
  3 !   0   0  15   0   0   0   0   0   0   0   0   0   0   0 
    !
  4 !   0   0   0   0  16   9   0   0   0   0   0   5   0   0 
    !
  5 !   0   0   0   0   0   9   0   0   0   0   0   5   0   0 
    !
  6 !   0   0   0   0   0   0   0   8   8   8   2   2   2   1 
    !
  7 !   0   0   0   0   0   0   0   0   0   0   0   3   0   0 
    !
  8 !   0  12   0  14   0   0   4   8   8   8   8   8   8   0 
    !
  9 !   0   0   0   0  16   0   0   0   0   0   0   9   0   0 
    !
 10 !   0   0   0   0   0   0   0   8   8   8   8   8   8   0 
    !
 11 !   0   0   0   0   0   0   0   0   0   0   0   7   0   0 
    !
 12 !   0   0   0  14   0   0   4  12  12  12  12  12  12   0 
    !
 13 !   0   0   0   0  17  20   0   0   0   0   0  23   0   0 
    !
 14 !   0   0   0   0   0   0   0   0   0   0   0  11   0   0 
    !
 15 !   0   0   0   0   0   0  13   0   0   0   0   0   0   0 
    !
 16 !   0   0   0  22   0   0   0   0   0   0   0   0   0   0 
    !
 17 !   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
    !
 18 !   0   0  17   0   0   0   0   0   0   0   0   0   0   0 
    !
 19 !   0   0   0   0   0   0   0   0   0   0   0  18   0   0 
    !
 20 !   0   0   0   0  17   0   0   0   0   0   0  20   0   0 
    !
 21 !   0   0   0   0   0   0   0   0   0   0   0  19   0   0 
    !
 22 !   0   0   0   0   0   0   0   0   0   0   0  21   0   0 
    !
 23 !   0   0   0   0   0  20   0   0   0   0   0  23   0   0 

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
           18      
           19      
           20      
           21      
           22      
           23      ****** COMPACTED VERSION OF MINIMIZED DFA NEXT STATE TABLE ******

             DEFAULT  BASE

           1      0      0
           2      1     13
           3      1     10
           4      1     20
           5      4     15
           6      2     27
           7      1     28
           8      2     30
           9      1     33
          10      8     34
          11      1     34
          12      8     46
          13      1     51
          14      1     36
          15      1     38
          16      1     44
          17      1      0
          18      1     44
          19      1     54
          20      1     55
          21      1     59
          22      1     60
          23     13     54              NEXT  CHECK              NEXT  CHECK

           1     0      1          51    12     12
           2     0      1          52    12     12
           3     0      1          53    12     12
           4     0      1          54    23     13
           5     0      1          55    22     16
           6     0      1          56    17     18
           7     0      1          57    18     19
           8     0      1          58    20     20
           9     0      1          59     0     12
          10     0      1          60    20     13
          11     0      1          61    17     13
          12     0      1          62    19     21
          13     0      1          63    21     22
          14     0      1          64     0     23
          15     2      2          65    17     20
          16     2      2          66     0      0
          17     2      2          67     0      0
          18     8      2          68     0      0
          19     8      2          69     0      0
          20     8      2          70     0      0
          21     4      2          71     0      0
          22    15      3          72     0      0
          23     5      4          73     0      0
          24    14      2          74     0      0
          25     0      5
          26    12      2
          27    10      2
          28     1      6
          29     9      4
          30    16      4
          31     3      7
          32     8      8
          33     8      8
          34     8      8
          35     0      6
          36     9      9
          37     7     11
          38     0      6
          39    11     14
          40     0      6
          41     0      6
          42     0     10
          43    16      9
          44     0      8
          45     0     10
          46    13     15
          47     0     10
          48    12     12
          49    12     12
          50    12     12(********************************************)
(*   SCANNER ALGORITHM CREATED BY LEXGEN    *)
(********************************************)

PROGRAM ??????;

CONST
   MAXINDEX    =  ???;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  ???;  (* MAXINDEX + 1 *)
   MAXTOKEN    =    2;
   DFASTATE1   =    6;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   23;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -14;  (* MIN TERMINAL CODE *)
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
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,12,0,14,0,0,4,8,8,8,2,2,2,0,0,0,15,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,9,0,0,0,0,0,5,0,0,0,0,0,0,0,9,0,0,
      0,0,0,5,0,0,0,0,0,0,0,0,0,8,8,8,2,2,2,1,0,0,0,0,0,0,0,0,0,0,0,3,0,
      0,0,12,0,14,0,0,4,8,8,8,8,8,8,0,0,0,0,0,16,0,0,0,0,0,0,9,0,0,0,0,0,
      0,0,0,0,8,8,8,8,8,8,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,14,0,0,4,
      12,12,12,12,12,12,0,0,0,0,0,17,20,0,0,0,0,0,23,0,0,0,0,0,0,0,0,0,
      0,0,0,0,11,0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,22,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,18,0,0,0,0,0,0,17,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,
      0,0,0,0,0,19,0,0,0,0,0,0,0,0,0,0,0,0,0,21,0,0,0,0,0,0,0,20,0,0,0,0,
      0,23,0,0);

   (* FINAL [X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA*****>
                 2 IF STATE X RECOGNIZES <FILE SPEC>
                                                                 *)
   FINAL: PACKED ARRAY [EXSTATERANGE] OF 0..MAXTOKEN := (
      0,1,2,0,0,0,0,0,2,0,0,0,2,0,0,2,2,2,0,0,0,0,0,0);

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
    