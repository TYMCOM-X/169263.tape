INPUT:   <file spec>  <bad file spec> ;
INPUT:   
INPUT:   *
INPUT:   * Define a dummy set of all the terminals just to control the order in which
INPUT:   * LEXGEN sees, and thus encodes, them.
INPUT:   *
INPUT:   "dummy" ::= ['alpha' 'digit' '$' '_' '.' ',' '[' ']' '<' '>' ':' ';' '-' '"'];
INPUT:   "alphanums" ::= [ 'alpha' 'digit' ];
INPUT:   "logicals"  ::= [ 'alpha' 'digit' '$' '_' ];
INPUT:   "not_quote" ::= [ 'alpha' 'digit' '$' '_' '.' ',' '[' ']' '<' '>' ':' ';' '-' ];
INPUT:   
INPUT:   <log name> ::= "logicals"+ ;
INPUT:   <node> ::= <log name> ( ('"' "not_quote" * '"') ! '') ':' ':' ;
INPUT:   <opt node> ::= <node> ! '';
INPUT:   <bad node> ::= <log name> '"' ;
INPUT:   
INPUT:   <device> ::= <log name> ':' ;
INPUT:   <opt device> ::= <device> ! '';
INPUT:   
INPUT:   <dir spec> ::= ( 'digit'+ ',' 'digit'+ )
INPUT:           ! ( "alphanums"+ ! '-'* ) ( '.' "alphanums"+ )* ;
INPUT:   <opt dir spec> ::= <dir spec> ! '';
INPUT:   <dir> ::= ( '['  <opt dir spec> ']' ) !
INPUT:             ( '<'  <opt dir spec> '>' ) ;
INPUT:   <bad dir> ::= ( '[' ! '<' ) <opt dir spec> ;
INPUT:   <opt dir> ::= <dir> ! '' ;
INPUT:   
INPUT:   <fname> ::= "logicals"+ ;
INPUT:   <opt fname> ::= <fname> ! '' ;
INPUT:   
INPUT:   <file_type> ::= '.' "alphanums" * ;
INPUT:   <opt ftype> ::= <file_type> ! '' ;
INPUT:   
INPUT:   <version> ::= ('.' ! ';') 'digit'+ ;
INPUT:   <bad ver> ::= ';' ;
INPUT:   <opt version> ::= <version> ! '' ;
INPUT:   
INPUT:   <file spec> ::= <opt node>
INPUT:                   <opt device>
INPUT:                   <opt dir>
INPUT:                   <fname>
INPUT:                   <opt ftype>
INPUT:                   <opt version> ;
INPUT:   
INPUT:   <bad file spec> ::= ( <bad node> )
INPUT:                     ! ( <opt node> <opt device> <bad dir> )
INPUT:                     ! (   <opt node>
INPUT:                           <opt device>
INPUT:                           <opt fname>
INPUT:                           <opt ftype>
INPUT:                           <bad ver> ) ****** CODES FOR TERMINAL SYMBOLS ******

          CODE   SYMBOL
          -15   '"'
          -14   '-'
          -13   ';'
          -12   ':'
          -11   '>'
          -10   '<'
           -9   ']'
           -8   '['
           -7   ','
           -6   '.'
           -5   '_'
           -4   '$'
           -3   'digit'
           -2   'alpha'
           -1   '*****END OF DATA*****'


****** CODES FOR NONTERMINALS TO BE RECOGNIZED ******

          CODE   NONTERMINAL
            1   '*****END OF DATA*****'
            2   'file spec'
            3   'bad file spec'****** MINIMIZED DFA NEXT STATE TABLE ******

      -15 -14 -13 -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2  -1 
    ______________________________________________________________
    !
  1 !   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
    !
  2 !   9   0  24  13   0   0   0   0   0   8   2   2   2   2   0 
    !
  3 !   0  12   0   0   0   0   6   0   0   7   0   0  31  23   0 
    !
  4 !   0   0  10   0   0  11   0   3   0  14   2   2   2   2   1 
    !
  5 !   0   0   0  15   0   0   0   0   0   0   0   0   0   0   0 
    !
  6 !   0   0   0   0   0   0   0   0   0   0  28  28  28  28   0 
    !
  7 !   0   0   0   0   0   0   0   0   0   0   0   0  23  23   0 
    !
  8 !   0   0  24   0   0   0   0   0   0  17   0   0   8   8   0 
    !
  9 !  16  34  34  34  34  34  34  34  34  34  34  34  34  34   0 
    !
 10 !   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
    !
 11 !   0  25   0   0   6   0   0   0   0  35   0   0  29  22   0 
    !
 12 !   0  12   0   0   0   0   6   0   0   7   0   0   0   0   0 
    !
 13 !   0   0  10  15   0  11   0   3   0  14  21  21  21  21   0 
    !
 14 !   0   0  10   0   0   0   0   0   0   0   0   0  14  14   0 
    !
 15 !   0   0  10   0   0  11   0   3   0  14  20  20  20  20   0 
    !
 16 !   0   0   0   5   0   0   0   0   0   0   0   0   0   0   0 
    !
 17 !   0   0   0   0   0   0   0   0   0   0   0   0  19   0   0 
    !
 18 !   0   0   0   0   0   0   0   0   0   0   0   0  32   0   0 
    !
 19 !   0   0   0   0   0   0   0   0   0   0   0   0  19   0   0 
    !
 20 !   0   0  24  26   0   0   0   0   0   8  20  20  20  20   0 
    !
 21 !   0   0  24   0   0   0   0   0   0   8  21  21  21  21   0 
    !
 22 !   0   0   0   0   6   0   0   0   0  35   0   0  22  22   0 
    !
 23 !   0   0   0   0   0   0   6   0   0   7   0   0  23  23   0 
    !
 24 !   0   0   0   0   0   0   0   0   0   0   0   0  19   0   0 
    !
 25 !   0  25   0   0   6   0   0   0   0  35   0   0   0   0   0 
    !
 26 !   0   0  10   0   0  11   0   3   0  14  21  21  21  21   0 
    !
 27 !   0   0   0   0   0   0   0   0   0   0   0   0  30   0   0 
    !
 28 !   0   0  17   0   0   0   0   0   0  33  28  28  28  28   0 
    !
 29 !   0   0   0   0   6   0   0   0  27  35   0   0  29  22   0 
    !
 30 !   0   0   0   0   6   0   0   0   0   0   0   0  30   0   0 
    !
 31 !   0   0   0   0   0   0   6   0  18   7   0   0  31  23   0 
    !
 32 !   0   0   0   0   0   0   6   0   0   0   0   0  32   0   0 
    !
 33 !   0   0  17   0   0   0   0   0   0  17   0   0  33  33   0 
    !
 34 !  16  34  34  34  34  34  34  34  34  34  34  34  34  34   0 
    !
 35 !   0   0   0   0   0   0   0   0   0   0   0   0  22  22   0 

****** START STATE:    4****** FINAL/BACKUP STATES IN MINIMIZED DFA ******

          DFA STATE   FINAL/BACKUP STATE (S)
            0      *<**ERROR**>
            1      *<*****END OF DATA*****> 
            2      *<file spec> 
            3      *<bad file spec> 
            4      
            5      
            6      
            7      
            8      *<file spec> 
            9      *<bad file spec> 
           10      *<bad file spec> 
           11      *<bad file spec> 
           12      *<bad file spec> 
           13      
           14      
           15      
           16      
           17      
           18      
           19      *<file spec> 
           20      *<file spec> 
           21      *<file spec> 
           22      *<bad file spec> 
           23      *<bad file spec> 
           24      *<bad file spec> 
           25      *<bad file spec> 
           26      
           27      
           28      *<file spec> 
           29      *<bad file spec> 
           30      *<bad file spec> 
           31      *<bad file spec> 
           32      *<bad file spec> 
           33      *<file spec> 
           34      
           35      ****** COMPACTED VERSION OF MINIMIZED DFA NEXT STATE TABLE ******

             DEFAULT  BASE

           1      0      0
           2      1     14
           3      1     19
           4      2     29
           5      1     11
           6      1     43
           7      1     29
           8      1     47
           9      1     59
          10      1      0
          11      1     73
          12      3     49
          13      4     87
          14      1     80
          15      4     53
          16      1     12
          17      1     31
          18      1     33
          19     17      0
          20     15     88
          21      8    100
          22     11     83
          23      3    104
          24     17      0
          25     11     75
          26     13     26
          27      1     37
          28      6    102
          29     11    102
          30     27     32
          31      3    103
          32     18     50
          33      8    109
          34      9      0
          35      1     78              NEXT  CHECK              NEXT  CHECK              NEXT  CHECK

           1     0      1          51     0     12         101    24     20
           2     0      1          52     0     12         102    21     21
           3     0      1          53    17      8         103    21     21
           4     0      1          54     0     15         104    21     21
           5     0      1          55    20     15         105    21     21
           6     0      1          56    20     15         106     8     21
           7     0      1          57    20     15         107    23     23
           8     0      1          58    20     15         108    33     28
           9     0      1          59     6     32         109    27     29
          10     0      1          60    24      8         110    18     31
          11     0      1          61    34      9         111    33     33
          12     0      1          62    34      9         112    33     33
          13     0      1          63    34      9         113     0      0
          14     0      1          64    34      9         114     0      0
          15     0      1          65    34      9         115    17     28
          16     2      2          66    34      9         116     0     29
          17     2      2          67    34      9         117     0     31
          18     2      2          68    34      9         118     0     23
          19     2      2          69    34      9         119     0      0
          20     8      2          70    34      9         120     0      0
          21    23      3          71    34      9         121     0      0
          22    31      3          72    34      9         122    17     33
          23    15      5          73    34      9         123     0      0
          24     5     16          74    16      9         124     0      0
          25     7      3          75    22     11
          26    13      2          76    29     11
          27    24      2          77     0     25
          28     6      3          78     0     25
          29     9      2          79    35     11
          30     1      4          80    22     35
          31    23      7          81    22     35
          32    23      7          82    14     14
          33    12      3          83    14     14
          34    19     17          84     6     11
          35    14      4          85     0      0
          36    32     18          86    22     22
          37     3      4          87    25     11
          38     0     26          88     0     13
          39    11      4          89    21     13
          40    30     27          90    21     13
          41     0      4          91    21     13
          42    10      4          92    21     13
          43     6     30          93    10     14
          44     0      4          94     8     20
          45    28      6          95     0      0
          46    28      6          96     0     20
          47    28      6          97     0     22
          48    28      6          98     0     20
          49     8      8          99    15     13
          50     8      8         100    26     20(********************************************)
(*   SCANNER ALGORITHM CREATED BY LEXGEN    *)
(********************************************)

PROGRAM ??????;

CONST
   MAXINDEX    =  ???;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  ???;  (* MAXINDEX + 1 *)
   MAXTOKEN    =    3;
   DFASTATE1   =    4;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   35;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -15;  (* MIN TERMINAL CODE *)
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
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,0,24,13,0,0,0,0,0,8,2,2,2,2,0,0,12,
      0,0,0,0,6,0,0,7,0,0,31,23,0,0,0,10,0,0,11,0,3,0,14,2,2,2,2,1,0,0,0,
      15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,28,28,28,28,0,0,0,0,
      0,0,0,0,0,0,0,0,0,23,23,0,0,0,24,0,0,0,0,0,0,17,0,0,8,8,0,16,34,34,
      34,34,34,34,34,34,34,34,34,34,34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,25,0,0,6,0,0,0,0,35,0,0,29,22,0,0,12,0,0,0,0,6,0,0,7,0,0,0,0,0,0,
      0,10,15,0,11,0,3,0,14,21,21,21,21,0,0,0,10,0,0,0,0,0,0,0,0,0,14,14,
      0,0,0,10,0,0,11,0,3,0,14,20,20,20,20,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,19,0,0,0,0,24,26,0,0,0,0,0,8,20,20,20,20,0,0,
      0,24,0,0,0,0,0,0,8,21,21,21,21,0,0,0,0,0,6,0,0,0,0,35,0,0,22,22,0,
      0,0,0,0,0,0,6,0,0,7,0,0,23,23,0,0,0,0,0,0,0,0,0,0,0,0,0,19,0,0,0,
      25,0,0,6,0,0,0,0,35,0,0,0,0,0,0,0,10,0,0,11,0,3,0,14,21,21,21,21,
      0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,17,0,0,0,0,0,0,33,28,28,28,28,
      0,0,0,0,0,6,0,0,0,27,35,0,0,29,22,0,0,0,0,0,6,0,0,0,0,0,0,0,30,0,0,
      0,0,0,0,0,0,6,0,18,7,0,0,31,23,0,0,0,0,0,0,0,6,0,0,0,0,0,32,0,0,0,
      0,17,0,0,0,0,0,0,17,0,0,33,33,0,16,34,34,34,34,34,34,34,34,34,34,
      34,34,34,0,0,0,0,0,0,0,0,0,0,0,0,0,22,22,0);

   (* FINAL [X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA*****>
                 2 IF STATE X RECOGNIZES <file spec>
                 3 IF STATE X RECOGNIZES <bad file spec>
                                                                 *)
   FINAL: PACKED ARRAY [EXSTATERANGE] OF 0..MAXTOKEN := (
      0,1,2,3,0,0,0,0,2,3,3,3,3,0,0,0,0,0,0,2,2,2,3,3,3,3,0,0,2,3,3,3,3,
      2,0,0);

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