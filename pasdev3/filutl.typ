$PAGE
(* VAX .TYP file for the FILUTL RDLIB routines *)

CONST
   MAXINDEX    =  cmdlinelen;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  maximum(cmdlineidx);  (* MAXINDEX + 1 *)
   MAXTOKEN    =    3;
   DFASTATE1   =    4;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   35;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -15;  (* MIN TERMINAL CODE *)
   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)


TYPE
   STATERANGE  = 1..MAXDFASTATE;
   EXSTATERANGE= 0..MAXDFASTATE;
   INDEXRANGE  = 0..MAXINDEX;
   lextoken = ( file_token , error_token );
   terminal_range = minterminal..eodata;

const
   delta: packed array [ staterange , terminal_range ] of exstaterange := (
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
$PAGE
   (* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA****>
                 2 IF STATE X RECOGNIZES <file spec>
                 3 IF STATE X RECOGNIZES <bad file spec>
                                                                 *)
   final : PACKED ARRAY[EXSTATERANGE] OF 0..MAXTOKEN := (
      0,1,2,3,0,0,0,0,2,3,3,3,3,0,0,0,0,0,0,2,2,2,3,3,3,3,0,0,2,3,3,3,3,
      2,0,0);


  (* The array CHAR_SET_CODES is set up to represent the following
     code assignments:

	    'A'..'Z':	character code = -2;
	    '0'..'9':	character code = -3;
	    '$':	character code = -4;
	    '_':	character code = -5;
	    '.':	character code = -6;
	    ',':	character code = -7;
	    '[':	character code = -8;
	    ']':	character code = -9;
	    '<':	character code = -10;
	    '>':	character code = -11;
	    ':':	character code = -12;
	    ';':	character code = -13;
	    '-':	character code = -14;
	    '"':	character code = -15;
	    others:	character code = -1     ( end of data )   *)

   CHAR_SET_CODES: PACKED ARRAY[chr(0)..chr(127)] OF MINTERMINAL..EODATA = (
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1,-15, -1, -4, -1, -1, -1, -1, -1, -1, -1, -7,-14, -6, -1,
      -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,-12,-13,-10, -1,-11, -1,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -8, -1, -9, -1, -5,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1) ;


   token_kind: packed array [ 0..maxtoken ] of lextoken := (
	error_token , error_token , file_token , error_token );
 