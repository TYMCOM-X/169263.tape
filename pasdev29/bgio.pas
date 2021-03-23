$Title Backgammon I/O Support package
Module BGIO Options Special ( WORD , PTR , COERCIONS );

$System BG.INC


External Var GAME : GAME_REC;
External Var FACTOR : FACTOR_REC;

External Const CMD_CONST : CMD_ARY;

$if adp
$System (pasdev2)DTIME.TYP
$System (pasdev2)DTIME.INC
Var CUR_TIME : DTIME_INT;
    THE_DAY  : WEEK_DAY;
    THE_TIME : TIMEREC;
$endif
$PAGE CONSTANTS - System dependant
(* The following conditionally compiled statements, define the system
   dependant filenames used by BG *)

$ifany (vax, adp) static var bg_help_file : file_name := 'BG.HLP';
$ifnone (vax, adp) const bg_help_file : file_name := '(pasdev26)bg.hlp';

$ifany (vax, adp) static var bg_stats_file : file_name := 'BGSTAT.HLP';
$ifnone (vax, adp) const bg_stats_file : file_name := '(pasdev26)bgstat.hlp';

static var user_stats_file : file_name := 'BGUSER.ST';

$ifany (vax, adp) static var bg_factors_file : file_name := 'BGFACT.HLP';
$ifnone (vax, adp) const bg_factors_file : file_name := '(pasdev26)bgfact.hlp';

(* File variable definitions *)

Var STATS_FILE : File of WIN_REC;
    FACTORS_FILE : File of FACTOR_REC;

Var F : Text;
$PAGE SCREEN Display constants
Const
  NULL = Char ( 0b );
  SOH= Char ( 1b );
  ACK= Char ( 6b );
  BS = Char ( 10b );
  HT = Char ( 11b );
  LF = Char ( 12b );
  VT = Char ( 13b );
  FF = Char ( 14b );
  CR = Char ( 15b );
  NAK= Char ( 25b );
  SUB= Char ( 32b );
  ESC= Char ( 33b );
  FS = Char ( 34b );
  RS = Char ( 36b );


Type TERMINAL_CODES = Array [ TERMINAL_TYPE ] Of BG_STRING;
     X_COORD = 0..79;		(* Chars per line, o is at LEFT margin *)
     Y_COORD = 0..23;		(* Lines per screen, 0 is at TOP *)
     SCREEN_TYPE = Packed Array [ X_COORD, Y_COORD ] of Char;
     COORDS = Record
	X : X_COORD;
	Y : Y_COORD
       End;

Const CLEAR_SCREEN : TERMINAL_CODES := ( '' , SUB , ESC||FF||NULL||NULL||NULL||NULL , FF );

Const LOAD_CURSOR : TERMINAL_CODES := ( '', ESC || '=' , FS , ESC || 'Y' );

Const LEFT : TERMINAL_CODES := ( '' , BS , BS , BS );

$if vax Const RIGHT : TERMINAL_CODES := ( '' , FF , '' , ACK );
$ifnot vax Const RIGHT : TERMINAL_CODES := ( '' , FF , HT , ACK );

$if vax Const UP : TERMINAL_CODES := ( '' , '' , '' , SUB );
$ifnot vax Const UP : TERMINAL_CODES := ( '' , VT , '' , SUB );

Const DOWN : TERMINAL_CODES := ( '' , LF , LF , LF );
$PAGE Define the SCREEN static and some BG_specific board stuff
Static Var SCREEN : SCREEN_TYPE;

Static Var BOARD_ON_SCREEN : Array [ 0..25 ] of SMALL_INT;

Static Var CURSOR_POSITION : Record
			DEFINED : Boolean;
			X : X_COORD;
			Y : Y_COORD
		    End := ( False , 0 , 0 );

Type PT_COORD_ARRAY = Array [ 0..25 ] Of COORDS;

Const BG_POINT_SCREEN_ADDRESSES : PT_COORD_ARRAY := (
	( 67 , 12 ) ,		(* Point 0, Player BAR *)
	( 59 , 12 ) ,		(* Point 1 *)
	( 54 , 12 ) ,
	( 49 , 12 ) ,
	( 44 , 12 ) ,
	( 39 , 12 ) ,
	( 34 , 12 ) ,
	( 26 , 12 ) ,
	( 21 , 12 ) ,
	( 16 , 12 ) ,
	( 11 , 12 ) ,
	(  6 , 12 ) ,
	(  1 , 12 ) ,		(* Point 13 *)
	(  1 , 7  ) ,		(* Point 12 *)
	(  6 , 7  ) ,
	( 11 , 7  ) ,
	( 16 , 7  ) ,
	( 21 , 7  ) ,
	( 26 , 7  ) ,
	( 34 , 7  ) ,
	( 39 , 7  ) ,
	( 44 , 7  ) ,
	( 49 , 7  ) ,
	( 54 , 7  ) ,
	( 59 , 7  ) ,
	( 67 , 7  )   );
$PAGE Define constants for the BG board, message lines, etc.
(* Define the BOARD, border anyway, for the VISUAL BG game *)

Const TOP_OF_BOARD_LINE_POSN = 0;
      BOTTOM_OF_BOARD_LINE_POSN = 14;
      DICE_LINE_POSN = 16;
      MSG_LINE_POSN = 17;
      PROMPT_LINE_POSN = 18;
      ERROR_CURSOR_LINE_POSN = 19;
      TEXT_LINE_POSN = 19;
      ERROR_MSG_LINE_POSN = 20;

Const TOP_OF_BOARD : BG_STRING :=
'  13   14   15   16   17   18      19   20   21   22   23   24     BAR';

Const BOTTOM_OF_BOARD : BG_STRING :=
'  12   11   10   9    8    7       6    5    4    3    2    1      BAR';

Const PLAYERS_MEN = '*';
      COMPUTERS_MEN = 'O';

Public Procedure PR_BOARD ( BOARD  : BOARD_REC ); Forward;
$PAGE Constants and Variables for setting line characteristics
$ifnone (vax,adp)
$Include UUOCAL[31024,320156]

Type CHARACTERISTICS = ( LC_DM1, LC_DM2, LC_ECS, LC_PSS, LC_OBS, LC_HDX,
			 LC_ESC, LC_CRD, LC_DFR, LC_NOP, LC_NFC, LC_BRK,
			 LC_AXC, LC_NCM, LC_HHT, LC_LCP, LC_PTM, LC_HFF );

     LINE_CHAR_RECORD = Packed Record
	Case Boolean of
	  FALSE: ( BITS: Set of CHARACTERISTICS;
		   PORT: 0..#o777777 );
	  TRUE : ( VALUE: MACHINE_WORD );
     End;

Static Var SAVED_CHARS : LINE_CHAR_RECORD;

Var X : MACHINE_WORD;
    CHARS : LINE_CHAR_RECORD;
$endif
$if vax
$Include TERCHR.INC[31024,320156]
$Include IMGNAM.INC[31024,320156]

Static Var OLD_TER_CHAR : TC_SET;

Var TER : TC_SET;		(* Set of terminal characteristics *)
    TERR: TCERR;		(* Terminal char errors *)
    IMGERR : IMAGE_ERROR;
    IMAGE_NAME : File_name;
$endif

$if adp
$Include INFPAC.INC[31024,320156]

Var JOBSTUFF : JOBREC;
$endif
$PAGE SET_LINE_CHARACTERISTICS
Public Procedure SET_LINE_CHARACTERISTICS;

Begin

$if adp
  JOBINFO ( JOBSTUFF );

  If JOBSTUFF.PROGDIR <> '[52250,230]'
    Then Begin
      Writeln ( tty , 'Its not nice to steal programs!' );
      Stop
    End;

  BG_STATS_FILE := BG_STATS_FILE || JOBSTUFF.PROGDIR;
  USER_STATS_FILE := USER_STATS_FILE || JOBSTUFF.LSEGPPN;
  BG_HELP_FILE := BG_HELP_FILE || JOBSTUFF.PROGDIR;
  BG_FACTORS_FILE := BG_FACTORS_FILE || JOBSTUFF.PROGDIR;

  CUR_TIME := DAYTIME;		(* RDLIB Routine, returns time of day *)
  THE_DAY := DAY_OF_WEEK ( EXTR_DATE ( CUR_TIME ) );
  THE_TIME := DC_TIME ( EXTR_TIME ( CUR_TIME ) );

  If ( THE_DAY in [MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRIDAY] )
  And ( THE_TIME.HOURS in [ 8..11 , 13..16 ] )
  And ( JOBSTUFF.PROJECTID <> '' )
    Then Begin
      Writeln ( tty , 'Sorry, Backgammon is not available from 8-12AM, or 1-5PM Monday thru Friday.');
      Writeln ( tty , 'Please try again later.' );
      Stop
    End
  Else Writeln ( tty , '$$ff on,0,0;htab on,1,0;len 0;vtab off,0,1;crt on' );
$endif

$ifnone (vax, adp)
  CHARS.VALUE := -1;		(* Clear the value *)

  (* Now read the current characteristics *)

  If UUO_CALL ( #o51, 6, 0, Address ( CHARS ), X )
    Then ;

  (* Save  the characteristics, and reset them by anding in the ones we
     want added *)

  SAVED_CHARS := CHARS;
  CHARS.BITS := CHARS.BITS + [ LC_NFC , LC_HHT , LC_HFF ];

  (* Reset the characteristics the way we want them *)

  If UUO_CALL ( #o51, 7, 0, Address ( CHARS ), X )
    Then ;
$endif


$if vax
  GET_TERMINAL_CHARACTERISTICS ( TER , TERR );
  Assert ( TERR = TC_OK );

  OLD_TER_CHAR := TER;		(* Save them for the restore *)

  TER := TER - [ TC_WRAP ] + [ TC_SCOPE , TC_LOWER , TC_ESCAPE , TC_MECHFORM ];

  SET_TERMINAL_CHARACTERISTICS ( TER , TERR );
  Assert ( TERR = TC_OK );

  IMAGE_FILE_NAME ( IMAGE_NAME , IMGERR );
  IMAGE_NAME := Substr ( IMAGE_NAME , 1 , Search(IMAGE_NAME, [ ']' ] ) );
  BG_STATS_FILE := IMAGE_NAME || BG_STATS_FILE;
  BG_HELP_FILE := IMAGE_NAME || BG_HELP_FILE;
  BG_FACTORS_FILE := IMAGE_NAME || BG_FACTORS_FILE;
$endif

End;		(* SET_LINE_CHARACTERISTICS *)
$PAGE RESTORE_LINE_CHARACTERISTICS
Public Procedure RESTORE_LINE_CHARACTERISTICS;

Begin

$ifnone (adp, vax)
  If UUO_CALL ( #o51, 7, 0, Address ( SAVED_CHARS ), X )
     Then ;
$endif

$if adp Writeln(tty); Writeln ( tty , '$$htab off,8;vtab off,0,4' );

$if vax
  SET_TERMINAL_CHARACTERISTICS ( OLD_TER_CHAR , TERR );
  Assert ( TERR = TC_OK );
$endif

End;		(* RESTORE_LINE_CHARACTERISTICS *)
$PAGE ISSUE_CARRIAGE_RETURN
$if vax
Procedure ISSUE_CARRIAGE_RETURN;

Var I : Integer;

Begin

  If CURSOR_POSITION.DEFINED
  And ( CURSOR_POSITION.Y < Maximum ( Y_COORD ) )
    Then Begin
      Writeln ( tty );
      I := CURSOR_POSITION.Y + 1;
      CURSOR_POSITION := ( True , Minimum ( X_COORD ) , I );
    End

End;		(* ISSUE_CARRIAGE_RETURN *)
$endif
$PAGE POSITION_CURSOR
Procedure POSITION_CURSOR ( CURSOR : COORDS );

Begin

$if vax
  If CURSOR.X in [ 0 , 1 , 2 ]
    Then ISSUE_CARRIAGE_RETURN;
$endif

  If Not CURSOR_POSITION.DEFINED
  Or ( CURSOR_POSITION.X <> CURSOR.X )
  Or ( CURSOR_POSITION.Y <> CURSOR.Y )
    Then Begin
      If CURSOR_POSITION.DEFINED
      And ( CURSOR_POSITION.X = CURSOR.X )
      And ( CURSOR_POSITION.Y = CURSOR.Y - 1 )
      And ( DOWN [ GAME.DISPLAY_TYPE ] <> '' )
	Then Write ( tty , DOWN [ GAME.DISPLAY_TYPE ] )
      Else If CURSOR_POSITION.DEFINED
      And ( CURSOR_POSITION.X = CURSOR.X )
      And ( CURSOR_POSITION.Y = CURSOR.Y + 1 )
      And ( UP [ GAME.DISPLAY_TYPE ] <> '' )
	Then Write ( tty , UP [ GAME.DISPLAY_TYPE ] )
      Else If CURSOR_POSITION.DEFINED
      And ( CURSOR_POSITION.Y = CURSOR.Y )
      And ( CURSOR_POSITION.X = CURSOR.X - 1 )
      And ( RIGHT [ GAME.DISPLAY_TYPE ] <> '' )
	Then Write ( tty , RIGHT [ GAME.DISPLAY_TYPE ] )
      Else If CURSOR_POSITION.DEFINED
      And ( CURSOR_POSITION.Y = CURSOR.Y )
      And ( CURSOR_POSITION.X = CURSOR.X + 1 )
      And ( LEFT [ GAME.DISPLAY_TYPE ] <> '' )
	Then Write ( tty , LEFT [ GAME.DISPLAY_TYPE ] )
      Else If GAME.DISPLAY_TYPE = ADM_3A
	Then Begin
	  Write ( tty , LOAD_CURSOR [ ADM_3A ] ,
			Char ( Ord ('7') - ( 23 - CURSOR.Y ) ),
			Char ( Ord (' ') + CURSOR.X ) );
	End
      Else If GAME.DISPLAY_TYPE = TEK_4023
	Then Begin
	  Write ( tty , LOAD_CURSOR [ TEK_4023 ] ,
			Char ( CURSOR.X + 32 ) ,
			Char ( CURSOR.Y + 32 ) );
	End
      Else If GAME.DISPLAY_TYPE = ADDS
	Then Begin
	  Write ( tty , LOAD_CURSOR [ ADDS ] ,
			Char ( CURSOR.Y + 32 ) ,
			Char ( CURSOR.X + 32 ) ) ;
	End
    End;

  CURSOR_POSITION := ( True , CURSOR.X , CURSOR.Y );

End;		(* POSITION_CURSOR *)
$PAGE UPDATE_SCREEN
Procedure UPDATE_SCREEN ( COORD : COORDS;
			  CH    : Char );

Begin

  If SCREEN [ COORD.X , COORD.Y ] <> CH
    Then Begin
      POSITION_CURSOR ( COORD );

      Write ( tty , CH );

      SCREEN [ COORD.X , COORD.Y ] := CH;

      With CURSOR_POSITION Do
	Begin
	  X := Min ( COORD.X + 1 , Maximum ( X_COORD ) );
	  Y := COORD.Y;
	  DEFINED := True
	End
    End
$if vax
  Else If ( COORD.X = 0 ) And ( COORD.Y < Maximum ( Y_COORD ) )
    Then ISSUE_CARRIAGE_RETURN;
$endif

End;		(* UPDATE_SCREEN *)
$PAGE EMPTY_LINE
Procedure EMPTY_LINE ( CUR : COORDS );

Var I : X_COORD;

Begin

  For I := CUR.X to Maximum ( X_COORD ) Do
    UPDATE_SCREEN ( (I , CUR.Y ) , ' ' );

  CURSOR_POSITION.DEFINED := False

End;		(* EMPTY_LINE *)
$PAGE EMPTY_SCREEN
Procedure EMPTY_SCREEN ( FROM_Y : Y_COORD;
			 TO_Y   : Y_COORD );

Var I : Y_COORD;
    J : X_COORD;

Begin

  If GAME.DISPLAY_TYPE <> REGULAR
    Then begin
      For I := FROM_Y to TO_Y Do
	EMPTY_LINE ( ( 0, I ) );

      CURSOR_POSITION.DEFINED := False;
    End

End;		(* EMPTY_SCREEN *)
$PAGE DISPLAY_LINE
Procedure DISPLAY_LINE ( LINE : Y_COORD;
			 STR  : BG_STRING );

Var I : Integer;		(* Loop index *)

Begin

  If GAME.DISPLAY_TYPE <> REGULAR		(* Some kind of CRT *)
    Then Begin

      EMPTY_LINE ( (Length ( STR) , LINE ) );

      For I := 0 to Length ( STR ) - 1 Do
	UPDATE_SCREEN ( (I, LINE), STR [ I + 1 ] );

    End
  Else Writeln ( tty , STR );

End;		(* DISPLAY_LINE *)
$PAGE DISPLAY_POINT
Procedure DISPLAY_POINT ( WHICH_POINT : SMALL_INT;
			  MEN : SMALL_INT );

Type OFFSET_ARY = Array [ 0..15 ] Of SMALL_INT;
Const X_OFFSET : OFFSET_ARY := (0,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4 );

Var I : SMALL_INT;
    X_POS : X_COORD;
    Y_POS : Y_COORD;

Begin

  If BOARD_ON_SCREEN [ WHICH_POINT ] <> MEN
    Then Begin

      (* Update the board on the screen *)

      BOARD_ON_SCREEN [ WHICH_POINT ] := MEN;

      For I := 0 to 15 Do		(* Max number of men on a given point *)
	Begin
	  (* First address the screen position for the given character *)
	  X_POS := BG_POINT_SCREEN_ADDRESSES [ WHICH_POINT ].X;
	  If I in [ 0..5 ]
	    Then X_POS := X_POS + 1
	  Else If I in [ 6..10 ]
	    Then X_POS := X_POS + 2;
	  Y_POS := BG_POINT_SCREEN_ADDRESSES [ WHICH_POINT ].Y;
	  If WHICH_POINT >= 13
	    Then Y_POS := Y_POS - 5 + X_OFFSET [ I ] 
	  Else Y_POS := Y_POS - X_OFFSET [ I ];

	  (* Now X_POS and Y_POS contain the address of the cell on the screen *)

	  If ( Abs ( MEN ) < I ) Or ( MEN = 0 )
	    Then UPDATE_SCREEN ( (X_POS, Y_POS) , ' ' )
	  Else If MEN > 0
	    Then UPDATE_SCREEN ( (X_POS , Y_POS) , PLAYERS_MEN )
	  Else UPDATE_SCREEN ( (X_POS , Y_POS ) , COMPUTERS_MEN );
	End
    End
End;		(* DISPLAY_POINT *)
$PAGE PR_MESSAGE
Public Procedure PR_MESSAGE ( MSG : BG_STRING );

Begin

  EMPTY_SCREEN ( MSG_LINE_POSN , Maximum ( Y_COORD ) );

  DISPLAY_LINE ( MSG_LINE_POSN , MSG )

End;		(* PR_MESSAGE *)
$PAGE PR_ERROR
Public Procedure PR_ERROR ( IDX : Integer;
			MSG : BG_STRING );

Var STR : BG_STRING;

Begin

  EMPTY_SCREEN ( ERROR_MSG_LINE_POSN+1 , Maximum ( Y_COORD ) );

  If IDX <> 0
    Then Begin
      Putstring ( STR , ' ':IDX+3 , '^' );
      DISPLAY_LINE ( ERROR_CURSOR_LINE_POSN , STR )
    End;

  DISPLAY_LINE ( ERROR_MSG_LINE_POSN ,  MSG )

End;		(* PR_ERROR *)
$PAGE PROMPT_THE_USER
Procedure PROMPT_THE_USER ( PROMPT : BG_STRING );

Begin

  If GAME.DISPLAY_TYPE = REGULAR
    Then Write ( tty , PROMPT )
  Else Begin
$if vax ISSUE_CARRIAGE_RETURN;
    DISPLAY_LINE ( PROMPT_LINE_POSN , PROMPT );

    POSITION_CURSOR ( ( Length ( PROMPT ) , PROMPT_LINE_POSN ) )
  End;

  Break ( tty )

End;		(* PROMPT_THE_USER *)
$PAGE READ_A_LINE
Public Function READ_A_LINE ( PROMPT : BG_STRING ) : BG_STRING;

Var I : Integer;

Begin

  PROMPT_THE_USER ( PROMPT );

  Readln ( tty );
  Read ( tty , READ_A_LINE );
  READ_A_LINE := Uppercase ( READ_A_LINE );

  If CURSOR_POSITION.DEFINED
    Then Begin
      For I := 1 to Length ( READ_A_LINE ) Do
	If ( CURSOR_POSITION.X + I - 1 ) <= Maximum ( X_COORD )
	  Then SCREEN [ I - 1 + CURSOR_POSITION.X , CURSOR_POSITION.Y ] := READ_A_LINE [ I ];
      CURSOR_POSITION.DEFINED := False;
    End;

  EMPTY_SCREEN ( PROMPT_LINE_POSN+1 , Maximum ( Y_COORD ) );

End;		(* READ_A_LINE *)
$PAGE MY_QUERY
Public Function MY_QUERY ( PROMPT : BG_STRING ) : Boolean;

Var STR : BG_STRING;

Begin

  Repeat
    STR := READ_A_LINE ( PROMPT || '? ' );

    If ( STR <> 'NO' ) And ( STR <> 'N' )
    And ( STR <> 'YES' ) And ( STR <> 'YE' ) And ( STR <> 'Y' ) And ( STR <> '' )
      Then PR_ERROR ( 0 , 'YES/NO Only' )

  Until ( STR = 'N' ) Or ( STR = 'NO' )
  Or ( STR = '' ) or ( STR = 'Y' ) Or ( STR = 'YE' ) Or ( STR = 'YES' );

  MY_QUERY := Not ( ( STR = 'N' ) Or ( STR = 'NO' ) );

End;		(* MY_QUERY *)
$PAGE PR_DICE
Public Procedure PR_DICE ( DICE : DICE_REC );

Var STR : BG_STRING;

Begin

  If Not ( DICE.DIE1 in [ 1..6 ] )
  Or Not ( DICE.DIE2 in [ 1..6 ] )
    Then PR_ERROR ( 0 , 'The DICE have not been rolled yet.' )
  Else Begin
    Putstring ( STR , 'Dice: ' , DICE.DIE1:0 , '  ' , DICE.DIE2:0 );
    DISPLAY_LINE ( DICE_LINE_POSN , STR )
  End

End;		(* PR_DICE *)
$PAGE PRINT_TEXT
Procedure PRINT_TEXT ( Var LINE_NUM : Y_COORD;
			   STR : BG_STRING );

Begin

  DISPLAY_LINE ( LINE_NUM , STR );

  If LINE_NUM = Maximum ( Y_COORD )
    Then LINE_NUM := TEXT_LINE_POSN
  Else LINE_NUM := LINE_NUM + 1;

End;		(* PRINT_TEXT *)
$PAGE PR_ROLL_STATS
Public Procedure PR_ROLL_STATS ( GAME : GAME_REC );

Var STR : BG_STRING;
    CUR_LINE : Y_COORD;

Begin

  CUR_LINE := TEXT_LINE_POSN;

  Putstring ( STR , 'You rolled the dice ' , GAME.PLAYER_DICE_ROLLS:0 ,
      ' times, rolling ', GAME.PLAYER_DOUBLES:0 , ' doubles,' );
  PRINT_TEXT ( CUR_LINE , STR );
  Putstring ( STR , '  losing ' ,
      GAME.PLAYER_DIE_COUNT - GAME.PLAYER_DIE_USED:0 , ' pip, and using ' ,
      GAME.PLAYER_DIE_USED:0 , ' pip, out of a possible ' ,
      GAME.PLAYER_DIE_COUNT:0 , '.' );
  PRINT_TEXT ( CUR_LINE , STR );
  Putstring ( STR , '  You were hit ', GAME.P_MEN_HIT:0, ' times, losing ',
	GAME.P_POSN_LOST:0 , ' positions.' );
  PRINT_TEXT ( CUR_LINE , STR );

  Putstring ( STR , 'I rolled the dice ' , GAME.COMPUTER_DICE_ROLLS:0 ,
      ' times, rolling ' , GAME.COMPUTER_DOUBLES:0 , ' doubles,' );
  PRINT_TEXT ( CUR_LINE , STR );
  Putstring ( STR , '  losing ' ,
      GAME.COMPUTER_DIE_COUNT-GAME.COMPUTER_DIE_USED:0 , ' pip, and using ' ,
      GAME.COMPUTER_DIE_USED:0 , ' pip, out of a possible ' ,
      GAME.COMPUTER_DIE_COUNT:0 , '.' );
  PRINT_TEXT ( CUR_LINE , STR );
  Putstring ( STR , '  I was hit ', GAME.C_MEN_HIT:0 , ' times, losing ',
	GAME.C_POSN_LOST:0 , ' positions.' );
  PRINT_TEXT ( CUR_LINE , STR );

End;		(* PR_ROLL_STATS *)
$PAGE CLEAR_THE_SCREEN
Procedure CLEAR_THE_SCREEN;

Var X : X_COORD;
    Y : Y_COORD;
    I : SMALL_INT;

Begin

  Write ( tty , CLEAR_SCREEN [ GAME.DISPLAY_TYPE ] );

  (* Clear the internal representation of the screen *)

  For X := Minimum ( X_COORD ) to Maximum ( X_COORD ) Do
    For Y := Minimum ( Y_COORD ) to Maximum ( Y_COORD ) Do
      SCREEN [ X , Y ] := ' ';

  (* Clear the boards on the screen,, internally *)

  For I := 0 to 25 Do
    BOARD_ON_SCREEN [ I ] := 0;

  CURSOR_POSITION := ( True , 0 , 0 );

End;		(* CLEAR_THE_SCREEN *)
$PAGE PR_GAME_STATE
Public Procedure PR_GAME_STATE ( GAME : GAME_REC );
Var STR : BG_STRING;
    CUR_LINE : Y_COORD;
Begin
  CUR_LINE := TEXT_LINE_POSN;
  If GAME.GAME_TYP = NORMAL
    Then PRINT_TEXT ( CUR_LINE , 'Playing against me' )
  Else PRINT_TEXT ( CUR_LINE , 'Evaluating single board positions' );
  If GAME.PLAY_YOURSELF
    Then PRINT_TEXT ( CUR_LINE , 'I will play against myself.')
  Else PRINT_TEXT ( CUR_LINE , 'I will play you.' );
  If GAME.AUTO_ROLL
    Then PRINT_TEXT ( CUR_LINE , 'I am rolling the dice' )
  Else PRINT_TEXT ( CUR_LINE , 'You must roll the dice' );
  If GAME.SHOW_EVAL
    Then PRINT_TEXT ( CUR_LINE , 'Displaying my evaluations' )
  Else PRINT_TEXT ( CUR_LINE , 'Not displaying my moves' );
  If GAME.PLAYERS_MOVES
    Then PRINT_TEXT ( CUR_LINE , 'Display your possible moves' )
  Else PRINT_TEXT ( CUR_LINE , 'Not displaying your possible moves' );
  If GAME.DOUBLING
    Then PRINT_TEXT ( CUR_LINE , 'DOUBLING is permitted' )
  Else PRINT_TEXT ( CUR_LINE , 'DOUBLING is NOT permitted'  Case GAME.TERSE_SETTING Of
    VERBOSE : PRINT_TEXT ( CUR_LINE , 'Verbose mode.' );
    TERSE : PRINT_TEXT ( CUR_LINE , 'Terse mode.' );
    VERY_TERSE : PRINT_TEXT ( CUR_LINE , 'Very terse mode.' )
  End;		(* Of Case *)
  Putstring ( STR , 'The doubling cube has the value: ', GAME.DOUBLE_VALUE:0,
	' and is currently ' );
  Case GAME.DOUBLING_CUBE Of
    CUBE_FREE : PRINT_TEXT ( CUR_LINE , STR || 'Free.' );
    PLAYER_CONTROLS: PRINT_TEXT ( CUR_LINE , STR || 'yours.' );
    COMPUTER_CONTROLS: PRINT_TEXT ( CUR_LINE , STR || 'mine.' )
  End;
  Case GAME.DISPLAY_TYPE Of
    REGULAR : PRINT_TEXT ( CUR_LINE , 'Displaying on a hardcopy device.' );
    ADDS : PRINT_TEXT ( CUR_LINE , 'Displaying on an ADDS terminal.' );
    ADM_3A : PRINT_TEXT ( CUR_LINE , 'Displaying on an ADM-3A.' );
    TEK_4023 : PRINT_TEXT ( CUR_LINE , 'Displaying on a Tektronix 4023.' )
  End;		(* Of Case *)
End;		(* PR_GAME_STATE *)
$PAGE PR_INSTRUCTIONS
Public Procedure PR_INSTRUCTIONS;

Var CUR_LINE : Y_COORD;

Begin

  CUR_LINE := TEXT_LINE_POSN;

  PRINT_TEXT ( CUR_LINE , 'You are going to challenge the computer to an ancient');
  PRINT_TEXT ( CUR_LINE , 'game of luck and skill. Your home board is points 1..6.');
  PRINT_TEXT ( CUR_LINE , 'The computer''s home board is numbered from 19 to 24.');
  PRINT_TEXT ( CUR_LINE , 'The positive numbers are your men and the negative ones');
  PRINT_TEXT ( CUR_LINE , 'are the computer''s. To enter a move, enter the position');
  PRINT_TEXT ( CUR_LINE , 'you wish to move the man from, a comma or space, and');
  PRINT_TEXT ( CUR_LINE , 'the destination. To specify a man from the bar, use');
  PRINT_TEXT ( CUR_LINE , 'BAR or 25. To bearoff a man, specify: 0 or BEAroff.');
  PRINT_TEXT ( CUR_LINE , 'The program will play the game on hard copy, ADM-3A, ADDS, or');
  PRINT_TEXT ( CUR_LINE , 'Tektronix 4023 terminals. Normally, the game runs on a');
  PRINT_TEXT ( CUR_LINE , 'hard copy terminal. To specify ADM-3A, enter ADM at the');
  PRINT_TEXT ( CUR_LINE , 'BG prompt, TEK gets you software for the Tektronix.');
  PRINT_TEXT ( CUR_LINE , 'ADDS gets you software for the ADDS terminal.' );
  PRINT_TEXT ( CUR_LINE , 'START will begin a game. For more information you can');
  PRINT_TEXT ( CUR_LINE , 'use the HELP command. HELP at the BG prompt gets you');
  PRINT_TEXT ( CUR_LINE , 'a list of the available commands. HELP <command> tells');
  PRINT_TEXT ( CUR_LINE , 'you more about that command.');
  PRINT_TEXT ( CUR_LINE , 'The computer plays a betting game, i.e. doubling is');
  PRINT_TEXT ( CUR_LINE , 'allowed. To disable doubling enter DOUBLING. If you');
  PRINT_TEXT ( CUR_LINE , 'want to roll the dice, enter ROLL.');
  PRINT_TEXT ( CUR_LINE , 'GOOD LUCK!' );

End;		(* PR_INSTRUCTIONS *)
$PAGE UPDATE_BOARD
Public Procedure UPDATE_BOARD ( BOARD : BOARD_REC;
				SCL : UPDATE_SCL );

Var I : SMALL_INT;

Begin

  If GAME.DISPLAY_TYPE = REGULAR
    Then Begin
      If SCL = ANYWAY
	Then PR_BOARD ( BOARD )
    End
  Else Begin
    DISPLAY_LINE ( TOP_OF_BOARD_LINE_POSN , TOP_OF_BOARD );
    DISPLAY_LINE ( BOTTOM_OF_BOARD_LINE_POSN , BOTTOM_OF_BOARD );

    DISPLAY_POINT ( 0 , - BOARD.C_BAR );
    DISPLAY_POINT ( 25, BOARD.P_BAR );

    For I := 24 Downto 1 Do
      DISPLAY_POINT ( I , BOARD.BOARD [ I ] )
  End

End;		(* UPDATE_BOARD *)
$PAGE PR_BOARD
Public Procedure PR_BOARD ( BOARD : BOARD_REC );

Var I : SMALL_INT;

Begin

  If GAME.DISPLAY_TYPE = REGULAR
    Then Begin
      Writeln ( tty );
      Writeln ( tty , '  13  14  15  16  17  18     19  20  21  22  23  24    BAR');
      For I := 13 to 18 Do
	Write ( tty , ' ' , BOARD.BOARD [ I ]:3 );
      Write ( tty , '   ' );
      For I := 19 to 24 Do
	Write ( tty , ' ' , BOARD.BOARD [ I ]:3 );
      Writeln ( tty , '    ' , BOARD.P_BAR );
      Writeln ( tty );
      For I := 12 Downto 7 Do
	Write ( tty , ' ' , BOARD.BOARD [ I ]:3 );
      Write ( tty , '   ' );
      For I := 6 Downto 1 Do
	Write ( tty , ' ' , BOARD.BOARD [ I ]:3 );
      Writeln ( tty , '    ' , - BOARD.C_BAR );
      Writeln ( tty , '  12  11  10   9   8   7      6   5   4   3   2   1    BAR');
      Writeln ( tty )
    End
  Else Begin		(* Some type of screen terminal *)
    CLEAR_THE_SCREEN;

    UPDATE_BOARD ( BOARD , ONLY_IF_CRT );
  End

End;		(* PR_BOARD *)
$PAGE CONVERT_MOVE_TO_STRING
Function CONVERT_MOVE_TO_STRING ( MOVES : ^MOVE_LIST ) : BG_STRING;

Var I : SMALL_INT;
    STR : BG_STRING;

Begin

  STR := '';
  CONVERT_MOVE_TO_STRING := '';

  For I := 1 to Upperbound ( MOVES^ ) Do
    Begin
      If MOVES^[ I ].FROM = -1
	Then Putstring ( STR , 'From BAR' )
      Else Putstring ( STR , MOVES^[ I ].FROM:0 );
      CONVERT_MOVE_TO_STRING := CONVERT_MOVE_TO_STRING || STR;

      If ( MOVES^[ I ].TARG <= 0 )
      Or ( MOVES^[ I ].TARG >= 25 )
	Then Putstring ( STR , ' Bearing off, ' )
      Else Putstring ( STR , ' to ' , MOVES^[ I ].TARG:0 , ', ' );
      CONVERT_MOVE_TO_STRING := CONVERT_MOVE_TO_STRING || STR;
    End;

End;		(* CONVERT_MOVE_TO_STRING *)
$PAGE PR_MOVE
Public Procedure PR_MOVE ( MOVES : ^MOVE_LIST;
			   MSG   : BG_STRING );

Begin

  DISPLAY_LINE ( MSG_LINE_POSN , MSG || CONVERT_MOVE_TO_STRING ( MOVES ) )

End;		(* PR_MOVE *)
$PAGE PR_LIST_OF_MOVES
Public Procedure PR_LIST_OF_MOVES ( MOVES : ^MOVE_VALUE;
			    EVAL  : Boolean );

Var WALK : ^MOVE_VALUE;
    STR : BG_STRING;
    CUR_LINE : Y_COORD;

Begin

  CUR_LINE := TEXT_LINE_POSN;

  WALK := MOVES;

  While WALK <> Nil Do
    Begin

      If EVAL
	Then Putstring ( STR , ' value: ' , WALK^.VALUE )
      Else STR := '';

      STR := CONVERT_MOVE_TO_STRING ( WALK^.MOVES ) || STR;

      PRINT_TEXT ( CUR_LINE , STR );

      WALK := WALK^.NEXT
    End;

End;		(* PR_LIST_OF_MOVES *)
$PAGE RD_FACTORS
Public Procedure RD_FACTORS;

Begin

  Reset ( FACTORS_FILE , BG_FACTORS_FILE , [ retry ] );

  If Iostatus ( FACTORS_FILE ) <> Io_ok
    Then Begin
      (* The file doesn't exist, so create it *)

      Rewrite ( FACTORS_FILE , BG_FACTORS_FILE , [ retry ] );
      If Iostatus ( FACTORS_FILE ) <> Io_ok
	Then PR_ERROR ( 0 , '??? Unable to write factors file ???' )
      Else Begin
	Write ( FACTORS_FILE , FACTOR );
	If Iostatus ( FACTORS_FILE ) <> Io_ok
	  Then PR_ERROR ( 0 , '??? Unable to write factors to factor file ???' );
	Close ( FACTORS_FILE );

	If Iostatus ( FACTORS_FILE ) <> Io_ok
	  Then PR_ERROR ( 0 , '??? Unable to close factors file ???' )
      End
    End
  Else Begin	(* Read the file *)
    Read ( FACTORS_FILE , FACTOR );

    If Iostatus ( FACTORS_FILE ) <> Io_ok
      Then PR_ERROR ( 0 , '??? Unable to read from factors file ???' );

    Close ( FACTORS_FILE )
  End

End;		(* RD_FACTORS *)
$PAGE WR_FACTORS
Public Procedure WR_FACTORS;

Begin

  Update ( FACTORS_FILE , BG_FACTORS_FILE , [ retry ] );

  If Iostatus ( FACTORS_FILE ) <> Io_ok
    Then PR_ERROR ( 0 , '??? Unable to open factors file ???' )
  Else Begin
    Write ( FACTORS_FILE , FACTOR );

    If Iostatus ( FACTORS_FILE ) <> Io_ok
      Then PR_ERROR ( 0 , '??? Unable to write to factors file ???' );

    Close ( FACTORS_FILE );

    If Iostatus ( FACTORS_FILE ) <> Io_ok
      Then PR_ERROR ( 0 , '??? Unable to close factors file ???' )

  End

End;		(* WR_FACTOR *)
$PAGE PR_STATS
Public Procedure PR_STATS ( WINS : WIN_REC;
			MSG  : BG_STRING );

Var STR : BG_STRING;
    CUR_LINE : Y_COORD;

Begin

  CUR_LINE := TEXT_LINE_POSN;

  If WINS.GAMES = 0
    Then PRINT_TEXT ( CUR_LINE , MSG || ' No games played.' )
  Else Begin
    PRINT_TEXT ( CUR_LINE , MSG );
    PRINT_TEXT ( CUR_LINE , '          WINS      GAMMONS       BACKGAMMONS        Score' );
    PRINT_TEXT ( CUR_LINE , '' );
    Putstring ( STR , 'Player    ' , WINS.P_WINS:4 , ' ':8 ,
	      WINS.P_GAMMONS:3 , ' ':13 , WINS.P_BGAMMONS:2 ,
	      ' ':14 , WINS.P_SCORE:4 );
    PRINT_TEXT ( CUR_LINE , STR );
    Putstring ( STR , 'Computer  ' , WINS.C_WINS:4 , ' ':8 ,
	      WINS.C_GAMMONS:3 , ' ':13 , WINS.C_BGAMMONS:2 ,
	      ' ':14 , WINS.C_SCORE:4 );
    PRINT_TEXT ( CUR_LINE , STR )
  End

End;		(* PR_STATS *)
$PAGE ADD_WIN_RECS
Function ADD_WIN_RECS ( WIN1 : WIN_REC;
			WIN2 : WIN_REC ) : WIN_REC;

Begin
  ADD_WIN_RECS.GAMES := WIN1.GAMES + WIN2.GAMES;
  ADD_WIN_RECS.C_SCORE := WIN1.C_SCORE + WIN2.C_SCORE;
  ADD_WIN_RECS.P_SCORE := WIN1.P_SCORE + WIN2.P_SCORE;
  ADD_WIN_RECS.C_WINS := WIN1.C_WINS + WIN2.C_WINS;
  ADD_WIN_RECS.P_WINS := WIN1.P_WINS + WIN2.P_WINS;
  ADD_WIN_RECS.C_GAMMONS := WIN1.C_GAMMONS + WIN2.C_GAMMONS;
  ADD_WIN_RECS.C_BGAMMONS := WIN1.C_BGAMMONS + WIN2.C_BGAMMONS;
  ADD_WIN_RECS.P_GAMMONS := WIN1.P_GAMMONS + WIN2.P_GAMMONS;
  ADD_WIN_RECS.P_BGAMMONS := WIN1.P_BGAMMONS + WIN2.P_BGAMMONS;
End;		(* ADD_WIN_RECS *)
$PAGE UPDATE_STATS_FILE
Procedure UPDATE_STATS_FILE ( FNAME : File_name;
			      WINS  : WIN_REC );

Var TOTAL_WINS : WIN_REC;

Begin

  Update ( STATS_FILE , FNAME , [ Retry , Seekok ] );

  If Iostatus ( STATS_FILE ) <> Io_ok
    Then PR_ERROR ( 0 , '??? Cannot update STATS file "' || FNAME || '"?' )
  Else begin
    If Not Eof ( STATS_FILE )
      Then Begin
	Read ( STATS_FILE , TOTAL_WINS );
	If Iostatus ( STATS_FILE ) <> Io_ok
	  Then PR_ERROR ( 0 , '??? The STATS file "' || FNAME || '" has an illegal format?' )
	Else Begin
	  Seek ( STATS_FILE , 1 );		(* Back to the beginning *)
	  If Iostatus ( STATS_FILE ) <> Io_ok
	    Then PR_ERROR ( 0 , '??? Cannot seek STATS file "' || FNAME || '"?' )
	  Else TOTAL_WINS := ADD_WIN_RECS ( TOTAL_WINS , WINS )
	End
      End
    Else TOTAL_WINS := WINS;		(* For empty file use current value *)

    Write ( STATS_FILE , TOTAL_WINS );
    If Iostatus ( STATS_FILE ) <> Io_ok
      Then PR_ERROR ( 0 , '??? Cannot update STATS file "' || FNAME || '"?' );
    Close ( STATS_FILE );
    If Iostatus <> Io_ok
      Then PR_ERROR ( 0 , '??? Cannot close STATS file "' || FNAME || '"?' )
  End

End;		(* UPDATE_STATS_FILE *)
$PAGE UPD_STATS_FILE
Public Procedure UPD_STATS_FILE ( WINS : WIN_REC );

Begin

  UPDATE_STATS_FILE ( BG_STATS_FILE , WINS );

  UPDATE_STATS_FILE ( USER_STATS_FILE , WINS );

End;		(* UPD_STATS_FILE *)
$PAGE PRINT_STATS
Procedure PRINT_STATS ( FNAME : File_name;
			MSG   : BG_STRING );

Var STR : BG_STRING;
    STATS_WIN_REC : WIN_REC;

Begin

  Reset ( STATS_FILE , FNAME , [ retry ] );

  If Iostatus ( STATS_FILE ) <> Io_ok
    Then PR_ERROR ( 0 , '??? STATS file "' || FNAME || '"  is not accessible?' )
  Else If Eof ( STATS_FILE )
    Then Begin
      PR_ERROR ( 0 , '??? STATS file "' || FNAME || '" has an illegal format?' );
      Close ( STATS_FILE )
    End
  Else Begin
    Read ( STATS_FILE , STATS_WIN_REC );
    If Iostatus ( STATS_FILE ) <> Io_ok
      Then PR_ERROR ( 0 , '??? STATS file "' || FNAME || '"  has been trashed?' )
    Else Begin
      Putstring ( STR , MSG , STATS_WIN_REC.GAMES:0 , ' games:' );
      PR_STATS ( STATS_WIN_REC , STR )
    End;
    Close ( STATS_FILE );
    If Iostatus <> Io_ok
      Then PR_ERROR ( 0 , '??? STATS file "' || FNAME || '" did not close properly?' );
  End

End;		(* DO_EVERYONES_STATS *)
$PAGE DO_STATS
Public Procedure DO_STATS;

Begin

  PR_STATS ( GAME.WINS , 'Your session statistics:' );

  PRINT_STATS ( BG_STATS_FILE , 'Everyone''s statistics over ' );

  PRINT_STATS ( USER_STATS_FILE , 'Statistics accumulated by this user over ' );

End;		(* DO_STATS *)
$PAGE DO_HELP
Public Procedure DO_HELP ( TOK : TOKEN_REC );

Var CMD : CMD_SCL;
    STR : BG_STRING;
    STR2: BG_STRING;
    OUT_STR : BG_STRING;
    CUR_LINE : Y_COORD;
    COUNT : Integer;

Begin

  CUR_LINE := TEXT_LINE_POSN;
  If TOK.CMD = ERROR_CMD
    Then Begin
      COUNT := 0;
      OUT_STR := '';
      For CMD := Succ ( ERROR_CMD ) to Maximum ( CMD_SCL ) Do
	Begin
	  If COUNT = 6
	    Then Begin
	      PRINT_TEXT ( CUR_LINE , OUT_STR );
	      OUT_STR := '';
	      COUNT := 1
	    End
	  Else COUNT := COUNT + 1;

	  Putstring ( STR , Uppercase ( Substr ( CMD_CONST [ CMD ].TXT , 1
	       , CMD_CONST [ CMD ].ABBREV ) ),
	      Lowercase ( Substr ( CMD_CONST [ CMD ].TXT , CMD_CONST [ CMD ].ABBREV+1 ) ) );
	  Putstring ( STR2 , STR : CMD_LEN+1 : L );
	  OUT_STR := OUT_STR || STR2
	End;
      PRINT_TEXT ( CUR_LINE ,'For more information enter HELP followed by a command.' );
    End
  Else Begin		(* HELP <command> *)

    Reset ( F , BG_HELP_FILE );

    If Iostatus ( F ) <> Io_ok
      Then PR_ERROR ( 0 , '??? HELP file is not accessible from this account?' )
    Else If Eof ( F )
      Then Begin
	PR_ERROR ( 0 , '??? The HELP file has been emptied?' );
	Close ( F )
      End
    Else Begin

      (* Find the section describing the desired command *)

      Repeat
	Readln ( F );
	Read ( F , STR );
      Exit if Eof ( F );
	STR := Uppercase ( STR )
      Until STR = CMD_CONST [ TOK.CMD ].TXT;

      If Eof ( F )
	Then PR_ERROR ( 0 , 'No further information is available.' )
      Else Begin
	(* Now print out the text up until a ***** group *)

	 CUR_LINE := TEXT_LINE_POSN;

	Repeat
	  PRINT_TEXT ( CUR_LINE , STR );
	  Readln ( F );
	  Read ( F , STR );
	Until Eof ( F ) Or ( (STR <> '') Andif ( STR [ 1 ] = '*' ) );

      End;
      (* Close that there file for later use *)

      Close ( F )
    End
  End;		(* Else Begin *)

End.		(* DO_HELP *)
    
g¸