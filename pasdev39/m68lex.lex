INPUT:   <file spec> ;
INPUT:   
INPUT:   *
INPUT:   * Define a dummy set of all of the terminals just to control the order in which
INPUT:   * LEXGEN sees, and thus encodes, them.
INPUT:   *
INPUT:   "dummy" ::= ['a_thru_p' 'q_thru_z' 'digit' '(' ')' '.' '#' ':' '-' '=' ',' ';'];
INPUT:   "alphas" ::= [ 'a_thru_p' 'q_thru_z' ] ;
INPUT:   "alphanums" ::= [ 'a_thru_p' 'q_thru_z' 'digit' ] ;
INPUT:   
INPUT:   <identifier> ::= "alphanums"+ ;
INPUT:   <digit sequence> ::= 'digit'+ ;
INPUT:   
INPUT:   <key letter> ::= 'a_thru_p' ;
INPUT:   <key pair> ::= <key letter> <key letter> ;
INPUT:   <opt key> ::= '(' <key pair> <key pair> ')'  !
INPUT:                 '(' <key pair> ')'  !
INPUT:                 '' ;
INPUT:   
INPUT:   <extension> ::= <identifier>  !  '' ;
INPUT:   <file name> ::= <identifier>  ;
INPUT:   <catalog> ::= <identifier>  !  '' ;
INPUT:   <user> ::= <digit sequence>  !  '' ;
INPUT:   <volume> ::= <identifier>  !  '' ;
INPUT:   <user file>::= <user> '.' <catalog> '.' <file name> '.' <extension> <opt key> ;
INPUT:   <opt user file> ::= <user file>  !  '' ;
INPUT:   
INPUT:   <device name> ::= '#' ( ''  !  <identifier> ) ;
INPUT:   
INPUT:   <file descriptor> ::= <device name> !
INPUT:                         <volume> ':' <opt user file>  !
INPUT:                         <user file>  !
INPUT:                         <catalog> '.' <file name> '.' <extension> <opt key>  !
INPUT:                         <file name> '.' <extension> <opt key>  !
INPUT:                         <file name>  ;
INPUT:   
INPUT:   <rw access> ::= "alphas"  !  '-' "alphas" ;
INPUT:   <access code> ::= <rw access>  !
INPUT:                     <rw access> <rw access> ;
INPUT:   <option> ::= "alphas"  !
INPUT:                "alphas" '=' <digit sequence>  !
INPUT:                <access code> ;
INPUT:   <another option> ::= ',' <option> ;
INPUT:   <option list>::= ';' <option> <another option>* ;
INPUT:   
INPUT:   <resource name string> ::= <file descriptor> <option list>  !
INPUT:                              <file descriptor>  !
INPUT:                              <option list> ;
INPUT:   
INPUT:   <file spec> ::= <resource name string> ****** CODES FOR TERMINAL SYMBOLS ******

          CODE   SYMBOL
          -13   ';'
          -12   ','
          -11   '='
          -10   '-'
           -9   ':'
           -8   '#'
           -7   '.'
           -6   ')'
           -5   '('
           -4   'digit'
           -3   'q_thru_z'
           -2   'a_thru_p'
           -1   '*****END OF DATA*****'


****** CODES FOR NONTERMINALS TO BE RECOGNIZED ******

          CODE   NONTERMINAL
            1   '*****END OF DATA*****'
            2   'file spec'****** MINIMIZED DFA NEXT STATE TABLE ******

      -13 -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2  -1 
    ______________________________________________________
    !
  1 !   0   0   0   0   0   0   0   0   0   0   0   0   0 
    !
  2 !   7   0   0   0  18   0  17   0   0   2   2   2   0 
    !
  3 !   7   0   0   0  18  10   6   0   0  28   2   2   1 
    !
  4 !   0   0   0   0   0   0  17   0   0   4   4   4   0 
    !
  5 !   0   0   0   0   0   0   0  31   0   0   0  14   0 
    !
  6 !   0   0   0   0   0   0  12   0   0   4   4   4   0 
    !
  7 !   0   0   0  13   0   0   0   0   0   0   9   9   0 
    !
  8 !   0   0   0   0   0   0   0   0   0   0   0   5   0 
    !
  9 !   0  20  19  30   0   0   0   0   0   0  15  15   0 
    !
 10 !   7   0   0   0   0   0   0   0   0  10  10  10   0 
    !
 11 !   0   0   0   0   0   0   0  31   0   0   0   0   0 
    !
 12 !   0   0   0   0   0   0   0   0   0  29  29  29   0 
    !
 13 !   0   0   0   0   0   0   0   0   0   0  22  22   0 
    !
 14 !   0   0   0   0   0   0   0   0   0   0   0  11   0 
    !
 15 !   0  20   0   0   0   0   0   0   0   0   0   0   0 
    !
 16 !   7   0   0   0   0   0  12   0  21  26  26  26   0 
    !
 17 !   7   0   0   0   0   0   0   0  21  24  24  24   0 
    !
 18 !   7   0   0   0   0   0  27   0   0  32   0   0   0 
    !
 19 !   0   0   0   0   0   0   0   0   0  23   0   0   0 
    !
 20 !   0   0   0  13   0   0   0   0   0   0   9   9   0 
    !
 21 !   0   0   0   0   0   0   0   0   0   0   0   8   0 
    !
 22 !   0  20   0  30   0   0   0   0   0   0  15  15   0 
    !
 23 !   0  20   0   0   0   0   0   0   0  23   0   0   0 
    !
 24 !   7   0   0   0   0   0  25   0  21  24  24  24   0 
    !
 25 !   7   0   0   0   0   0   0   0  21  25  25  25   0 
    !
 26 !   7   0   0   0   0   0  17   0  21  26  26  26   0 
    !
 27 !   0   0   0   0   0   0  12   0   0  27  27  27   0 
    !
 28 !   7   0   0   0  18   0  16   0   0  28   2   2   0 
    !
 29 !   0   0   0   0   0   0  25   0   0  29  29  29   0 
    !
 30 !   0   0   0   0   0   0   0   0   0   0  15  15   0 
    !
 31 !   7   0   0   0   0   0   0   0   0   0   0   0   0 
    !
 32 !   0   0   0   0   0   0  27   0   0  32   0   0   0 

****** START STATE:    3****** FINAL/BACKUP STATES IN MINIMIZED DFA ******

          DFA STATE   FINAL/BACKUP STATE (S)
            0      *<**ERROR**>
            1      *<*****END OF DATA*****> 
            2      *<file spec> 
            3      
            4      
            5      
            6      
            7      
            8      
            9      *<file spec> 
           10      *<file spec> 
           11      
           12      
           13      
           14      
           15      *<file spec> 
           16      *<file spec> 
           17      *<file spec> 
           18      *<file spec> 
           19      
           20      
           21      
           22      *<file spec> 
           23      *<file spec> 
           24      *<file spec> 
           25      *<file spec> 
           26      *<file spec> 
           27      
           28      *<file spec> 
           29      
           30      
           31      *<file spec> 
           32      ****** COMPACTED VERSION OF MINIMIZED DFA NEXT STATE TABLE ******

             DEFAULT  BASE

           1      0      0
           2      1     12
           3      2     16
           4      1     24
           5      1     16
           6      4     22
           7      1     30
           8      1     28
           9      1     32
          10      1     34
          11      1     33
          12      1     46
          13      1     43
          14      1     39
          15      1     39
          16      6     50
          17     10     54
          18      1     57
          19      1     56
          20      7      0
          21      1     60
          22      9     54
          23     15     62
          24     17     60
          25     17     69
          26     16     61
          27      6     72
          28      2     73
          29     12     62
          30      1     76
          31      1     68
          32     18     69              NEXT  CHECK              NEXT  CHECK

           1     0      1          51    20     15
           2     0      1          52    26     16
           3     0      1          53    26     16
           4     0      1          54    26     16
           5     0      1          55    21     16
           6     0      1          56    24     17
           7     0      1          57    24     17
           8     0      1          58    24     17
           9     0      1          59    21     17
          10     0      1          60    23     19
          11     0      1          61    32     18
          12     0      1          62     8     21
          13     0      1          63     7     16
          14     2      2          64    27     18
          15     2      2          65     0     22
          16     2      2          66    23     23
          17     1      3          67    25     24
          18    14      5          68    17     26
          19    17      2          69    25     29
          20    28      3          70     7     18
          21    18      2          71    25     25
          22    31      5          72    25     25
          23     6      3          73    25     25
          24    10      3          74    27     27
          25     7      2          75    27     27
          26     4      4          76    27     27
          27     4      4          77    28     28
          28     4      4          78    15     30
          29    12      6          79    15     30
          30     5      8          80    16     28
          31    17      4          81     7     31
          32     9      7          82     0     32
          33     9      7          83     0      0
          34    15      9          84     0      0
          35    15      9          85     0      0
          36    10     10          86     0      0
          37    10     10          87     0      0
          38    10     10          88     0      0
          39    31     11          89     0      0
          40    13      7
          41    11     14
          42    30      9
          43    19      9
          44    20      9
          45    22     13
          46    22     13
          47     7     10
          48    29     12
          49    29     12
          50    29     12(********************************************)
(*   SCANNER ALGORITHM CREATED BY LEXGEN    *)
(********************************************)

PROGRAM ??????;

CONST
   MAXINDEX    =  ???;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  ???;  (* MAXINDEX + 1 *)
   MAXTOKEN    =    2;
   DFASTATE1   =    3;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   32;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -13;  (* MIN TERMINAL CODE *)
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
      0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,18,0,17,0,0,2,2,2,0,7,0,0,0,18,
      10,6,0,0,28,2,2,1,0,0,0,0,0,0,17,0,0,4,4,4,0,0,0,0,0,0,0,0,31,0,0,
      0,14,0,0,0,0,0,0,0,12,0,0,4,4,4,0,0,0,0,13,0,0,0,0,0,0,9,9,0,0,0,0,
      0,0,0,0,0,0,0,0,5,0,0,20,19,30,0,0,0,0,0,0,15,15,0,7,0,0,0,0,0,0,0,
      0,10,10,10,0,0,0,0,0,0,0,0,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,29,29,
      0,0,0,0,0,0,0,0,0,0,0,22,22,0,0,0,0,0,0,0,0,0,0,0,0,11,0,0,20,0,0,
      0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,12,0,21,26,26,26,0,7,0,0,0,0,0,0,0,
      21,24,24,24,0,7,0,0,0,0,0,27,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,23,0,
      0,0,0,0,0,13,0,0,0,0,0,0,9,9,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,20,0,30,
      0,0,0,0,0,0,15,15,0,0,20,0,0,0,0,0,0,0,23,0,0,0,7,0,0,0,0,0,25,0,
      21,24,24,24,0,7,0,0,0,0,0,0,0,21,25,25,25,0,7,0,0,0,0,0,17,0,21,
      26,26,26,0,0,0,0,0,0,0,12,0,0,27,27,27,0,7,0,0,0,18,0,16,0,0,28,2,
      2,0,0,0,0,0,0,0,25,0,0,29,29,29,0,0,0,0,0,0,0,0,0,0,0,15,15,0,7,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,27,0,0,32,0,0,0);

   (* FINAL [X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA*****>
                 2 IF STATE X RECOGNIZES <file spec>
                                                                 *)
   FINAL: PACKED ARRAY [EXSTATERANGE] OF 0..MAXTOKEN := (
      0,1,2,0,0,0,0,0,0,2,2,0,0,0,0,2,2,2,2,0,0,0,2,2,2,2,2,0,2,0,0,2,0);

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
 