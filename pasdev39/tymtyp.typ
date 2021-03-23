$PAGE
(* Tymshare .TYP file for the FILUTL RDLIB routines *)
 
CONST
   MAXINDEX    =  cmdlinelen;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  maximum ( cmdlineidx );  (* MAXINDEX + 1 *)
   MAXTOKEN    =    2;
   DFASTATE1   =    6;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   29;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -16;  (* MIN TERMINAL CODE *)
   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)


TYPE
   STATERANGE  = 1..MAXDFASTATE;
   EXSTATERANGE= 0..MAXDFASTATE;
   INDEXRANGE  = 0..MAXINDEX;
   LEXTOKEN    = ( file_token, error_token );
   terminal_range = minterminal..eodata;

const
   DELTA: packed array [ staterange, terminal_range ] of exstaterange := (
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
$PAGE
   (* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA****>
                 2 IF STATE X RECOGNIZES <FILE SPEC>
                                                                 *)
   final: packed array [ exstaterange ] of 0..maxtoken := (
      0,1,2,0,0,0,0,0,2,0,0,0,2,0,0,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0);


  (* The array CHAR_CODE_SET is set up to represent the following
     code assignments:

	  'A'..'Z':	character code = -2;
	  '0'..'7':	character code = -3;
	  '8','9':	character code = -4;
	  '#':		character code = -5;
	  '?':		character code = -6;
	  '*':		character code = -7;
	  '[':		character code = -8;
	  ',':		character code = -9;
	  ']':		character code = -10;
	  '<':		character code = -11;
	  '>':		character code = -12;
	  '.':		character code = -13;
	  ':':		character code = -14;
	  '(':		character code = -15;
	  ')':		character code = -16;
	  others:	character code = -1	( end of data )   *)

   CHAR_SET_CODES: PACKED ARRAY[chr(0)..chr(127)] OF MINTERMINAL..EODATA = (
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -5, -1, -1, -1, -1, -15,-16, -7, -1, -9, -1,-13, -1,
      -3, -3, -3, -3, -3, -3, -3, -3, -4, -4, -14, -1,-11, -1,-12, -6,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -8, -1,-10, -1, -1,
      -1, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,
      -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1) ;


   token_kind: packed array [ 0..maxtoken ] of lextoken := (
                 error_token, error_token, file_token );
