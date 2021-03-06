$PAGE
(* VAX include file for CMD_FILE_NAME routine *)
CONST
   MAXTOKEN    =    4;
   DFASTATE1   =    1;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   38;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -15;  (* MIN TERMINAL CODE *)
   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)


TYPE
   STATERANGE  = 1..MAXDFASTATE;
   EXSTATERANGE= 0..MAXDFASTATE;
   lextoken = ( error_token , default_token , file_token );

const
   DELTA: PACKED ARRAY[STATERANGE,MINTERMINAL..EODATA] OF EXSTATERANGE := (
      0,0,13,0,0,14,0,4,0,9,3,3,3,3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,0,
      25,8,0,0,0,0,0,11,3,3,3,3,0,0,15,0,0,0,0,7,0,0,17,0,0,32,24,0,0,0,
      0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,18,0,0,0,0,6,0,
      0,0,0,0,0,19,29,29,29,29,0,0,0,13,10,0,14,0,4,0,9,22,22,22,22,0,0,
      0,13,0,0,0,0,0,0,6,0,0,9,9,0,0,0,13,0,0,14,0,4,0,9,21,21,21,21,0,0,
      0,25,0,0,0,0,0,0,34,0,0,11,11,0,16,37,37,37,37,37,37,37,37,37,37,
      37,37,37,0,0,0,0,0,0,0,0,0,0,0,0,0,18,0,0,0,26,0,0,7,0,0,0,0,27,0,
      0,30,23,0,0,15,0,0,0,0,7,0,0,17,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,24,0,0,0,0,0,0,0,0,0,0,0,0,0,18,0,
      0,0,0,6,0,0,0,0,0,0,6,0,0,19,19,0,0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,
      0,25,38,0,0,0,0,0,11,21,21,21,21,0,0,0,25,0,0,0,0,0,0,11,22,22,22,
      22,0,0,0,0,0,7,0,0,0,0,27,0,0,23,23,0,0,0,0,0,0,0,7,0,0,17,0,0,24,
      24,0,0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,26,0,0,7,0,0,0,0,27,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,23,23,0,0,0,0,0,0,0,0,0,0,0,0,0,33,0,0,0,
      0,34,0,0,0,0,0,0,35,29,29,29,29,0,0,0,0,0,7,0,0,0,36,27,0,0,30,23,
      0,0,0,0,0,7,0,0,0,0,0,0,0,31,0,0,0,0,0,0,0,0,7,0,28,17,0,0,32,24,0,
      0,0,0,0,0,0,7,0,0,0,0,0,33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,
      34,0,0,0,0,0,0,34,0,0,35,35,0,0,0,0,0,0,0,0,0,0,0,0,0,31,0,0,16,
      37,37,37,37,37,37,37,37,37,37,37,37,37,0,0,0,13,0,0,14,0,4,0,9,22,
      22,22,22,0);

$PAGE
   (* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA****>
                 2 IF STATE X RECOGNIZES <file spec>
                 3 IF STATE X RECOGNIZES <bad file spec>
                 4 IF STATE X RECOGNIZES <default file>
                                                                 *)
   FINAL: PACKED ARRAY[EXSTATERANGE] OF 0..MAXTOKEN := (
      0,4,1,2,3,0,0,4,4,4,4,2,3,3,3,3,0,0,4,4,2,2,2,3,3,3,3,0,0,2,3,3,3,
      3,0,2,0,0,4);


   (* The array CHAR_SET_CODES is set up to represent the following
      code assignments:
 
	  'A'..'Z':	character code = -2;
	  '0'..'9':	character code = -3;
	  '$':		character code = -4;
	  '_':		character code = -5;
	  '.':		character code = -6;
	  ',':		character code = -7;
	  '[':		character code = -8;
	  ']':		character code = -9;
	  '<':		character code = -10;
	  '>':		character code = -11;
	  ':':		character code = -12;
	  ';':		character code = -13;
	  '-':		character code = -14;
	  '"':		character code = -15;
	  others:	character code = -1	( end of data )   *)

   CHAR_SET_CODES: PACKED ARRAY[chr(0)..chr(127)] OF MINTERMINAL..EODATA = (
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1,-15, -1, -4, -1, -1, -1, -1, -1, -1, -1, -7,-14, -6, -1,
      -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,-12,-13,-10, -1,-11, -1,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -8, -1, -9, -1, -5,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1) ;


   token_kind : packed array [ 0..maxtoken] of lextoken = (
     error_token, default_token, file_token, error_token, default_token );
    