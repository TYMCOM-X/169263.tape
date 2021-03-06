$PAGE
(* M68 .TYP file for the FILUTL RDLIB routines *)

CONST
   MAXINDEX    =  cmdlinelen;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  maximum(cmdlineidx);  (* MAXINDEX + 1 *)
   MAXTOKEN    =    2;
   DFASTATE1   =    3;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   32;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -13;  (* MIN TERMINAL CODE *)
   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)


TYPE
   STATERANGE  = 1..MAXDFASTATE;
   EXSTATERANGE= 0..MAXDFASTATE;
   INDEXRANGE  = 0..MAXINDEX;
   lextoken = ( file_token , error_token );
   terminal_range = minterminal..eodata;

const
   delta: packed array [ staterange , terminal_range ] of exstaterange := (
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
$PAGE
   (* FINAL [X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA*****>
                 2 IF STATE X RECOGNIZES <file spec>
                                                                 *)
   FINAL: PACKED ARRAY [EXSTATERANGE] OF 0..MAXTOKEN := (
      0,1,2,0,0,0,0,0,0,2,2,0,0,0,0,2,2,2,2,0,0,0,2,2,2,2,2,0,2,0,0,2,0);


  (* The array CHAR_SET_CODES is set up to represent the following
     code assignments:

	    'A'..'P':	character code = -2;
	    'Q'..'Z':	character code = -3;
	    '0'..'9':	character code = -4;
	    '(':	character code = -5;
	    ')':	character code = -6;
	    '.':	character code = -7;
	    '#':	character code = -8;
	    ':':	character code = -9;
	    '-':	character code = -10;
	    '=':	character code = -11;
	    ',':	character code = -12;
	    ';':	character code = -13;
	    others:	character code = -1     ( end of data )   *)

   CHAR_SET_CODES: PACKED ARRAY[chr(0)..chr(127)] OF MINTERMINAL..EODATA = (
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -8, -1, -1, -1, -1, -5, -6, -1, -1,-12,-10, -7, -1,
      -4, -4, -4, -4, -4, -4, -4, -4, -4, -4, -9,-13, -1,-11, -1, -1,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -1, -1, -1, -1, -1,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -1, -1, -1, -1, -1) ;


   token_kind: packed array [ 0..maxtoken ] of lextoken := (
	error_token , error_token , file_token );
 