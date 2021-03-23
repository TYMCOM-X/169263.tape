$Title BACKGAMMON 1.0
Program BACKGAMMON;

(* Define the board. The board is an array of 24 integers representing the
   points on the backgammon board. The board also has space reserved for
   the white and black players that have been removed from the board,
   and the white and black player that are on the bar. In the board
   players will be represented by positive and negative integers.
   If a given point has the value 0, that indicates that there are no men on
   that point. If the point has a value of -1 it has one of the computers'
   men on that point, and a value of 1 indicates one of the players
   mon on that point. Since there are a total of 15 men of each color, the
   total of men may be checked. *)

$Include BG.INC

$PAGE PUBLICS and EXTERNALS
(* Global variables. *)

Public Var GAME : GAME_REC;

Public Var FACTOR : FACTOR_REC := (
	11,	(* HOME_BLOT *)
	-20,	(* BLOT_IN_HOME_BOARD *)
	20,	(* P_BAR *)
	20,	(* C_BAR *)
	25,	(* BEAROFF *)
	10,	(* BLOT *)
	3,	(* HOME_POINTS_HELD *)
	25,	(* ALL_HOME *)
	11,	(* C_24_23_22_POINTS *)
	9,	(* C_IN_P_OUTBOARD *)
	50,	(* C_IN_P_HOME *)
	6,	(* C_13_TO_18 *)
	25,	(* BUNCH *)
	3,	(* PRIME *)
	5,	(* POINT_5 *)
	5,	(* POINT_6 *)
	9,	(* POINT_7 *)
	3,	(* POINT_17 *)
	10,	(* POINT_18 *)
	11,	(* POINT_19 *)
	13,	(* POINT_20 *)
	4,	(* POINT_21 *)
	3,	(* POINT_22 *)
	9,	(* POINT_19_20 *)
	6,	(* POINT_21_22 *)
	7,	(* POINT_22_20 *)
	14,	(* POINT_23_22_21 *)
	2.25,	(* BLOT_POSN *)
	0.25	(* EVAL *)
	    );
Var X : Real;			(*  For priming the Random number generator *)
    BOARD : BOARD_REC;
$PAGE External Procedure Declarations
(* External Procedure Declarations *)

External Procedure EVALUATE_MOVES ( MOVES : ^MOVE_VALUE );
External Function MAKE_DOUBLE ( BOARD : BOARD_REC ) : Boolean;
External Function ACC_DOUBLE ( BOARD : BOARD_REC ) : Boolean;
External Procedure DO_HELP ( TOKEN_REC );
External Procedure DO_STATS;
External Procedure UPD_STATS_FILE ( WIN_REC );
External Function READ_A_LINE ( BG_STRING ) : BG_STRING;
External Function MY_QUERY ( BG_STRING ) : Boolean;
External Procedure SET_LINE_CHARACTERISTICS;
External Procedure RESTORE_LINE_CHARACTERISTICS;
External Procedure PR_DICE ( DICE_REC );
External Procedure PR_BOARD ( BOARD_REC );
External Procedure UPDATE_BOARD ( BOARD_REC; UPDATE_SCL );
External Procedure PR_ROLL_STATS ( GAME_REC );
External Procedure PR_MESSAGE ( BG_STRING );
External Procedure PR_ERROR ( Integer; BG_STRING );
External Procedure PR_GAME_STATE ( GAME_REC );
External Procedure PR_INSTRUCTIONS;
External Procedure PR_MOVE ( ^MOVE_LIST ; BG_STRING );
External Procedure PR_LIST_OF_MOVES ( ^MOVE_VALUE; Boolean );
External Procedure PR_STATS ( WIN_REC; BG_STRING );
External Procedure RD_FACTORS;
External Procedure WR_FACTORS;
$PAGE CONSTANTS
(* Constant declaration for the COMMANDS *)

Public Const CMD_CONST : CMD_ARY := (
	( '!' ,		1 ),
	( 'ADDS' ,	4 ),
	( 'ADM' ,	3 ),
	( 'BAR' ,	3 ),
	( 'BEAROFF' ,	3 ),
	( 'BOARD' ,	3 ),
	( 'COMPUTER' ,	3 ),
	( 'CUBE' ,	4 ),
	( 'DICE' ,	4 ),
	( 'DISPLAY' ,	3 ),
	( 'DOUBLE' ,	6 ),
	( 'DOUBLING' ,	8 ),
	( 'EVALUATE' ,	4 ),
	( 'HELP' ,	1 ),
	( 'INSTRUCTIONS',3),
	( 'POSSIBLE' ,	3 ),
	( 'QUIT' ,	1 ),
	( 'REGULAR' ,	3 ),
	( 'RESET' ,	3 ),
	( 'ROLL' ,	3 ),
	( 'SEED' ,	4 ),
	( 'START' , 	2 ),
	( 'STATE' , 	5 ),
	( 'STATS' ,	5 ),
	( 'STOP' ,	4 ),
	( 'TEK' ,	3 ),
	( 'TERSE' ,	3 ),
	( 'VERBOSE' ,	3 )
		);
$PAGE Constants defining the BOARD and GAME REC
(* This constant initializes a backgammon board for a typical game.
   The players home board consists of points 106 and the computers
   home board consists of points 19-24. When players are removed from the
   board the computer stores them in point 25 and the player stores
   his in point 0. The BAR is exactly what it indicates. *)

Const INITIAL_BOARD : BOARD_REC := (
	( 0, -2, 0, 0, 0, 0, 5, 0, 3, 0, 0, 0, -5,
	5, 0, 0, 0, -3, 0, -5, 0, 0, 0, 0, 2, 0 ) ,
	0 , 0 );

Const INIT_WIN_REC : WIN_REC := ( 0,0,0,0,0,0,0,0,0 );

$PAGE ROLL_DICE
(* This routine rolls the dice. If the user indicated that he wanted 
   to roll the dice, this routine reads the values from the terminal.
   If not, this routine generates scaled random numbers which are rounded
   into the range of 1..6. *)

Function ROLL_DICE : DICE_REC;

Var TEMP : BG_STRING;
    I : 1..6;
    X : Real;

Begin

  If GAME.AUTO_ROLL
    Then Begin			(* Ok, I'll roll them for the user *)
      ROLL_DICE.DIE1 := Min ( 6 , Trunc ( ( Random * 6 ) + 1 ) );
      For I := 1 to ROLL_DICE.DIE1 Do
	X := Random;
      ROLL_DICE.DIE2 := Min ( 6 , Trunc ( ( Random * 6 ) + 1 ) );
      For I := 1 to ROLL_DICE.DIE2 Do
	X := Random;
    End
  Else Begin
    Repeat
      TEMP := READ_A_LINE ( 'Enter die values: ' );
      Getstring ( TEMP , ROLL_DICE.DIE1 , ROLL_DICE.DIE2 );
      If ( Iostatus <> Io_ok ) Orif Not ( ROLL_DICE.DIE1 in [ 1..6 ] )
      Orif Not ( ROLL_DICE.DIE2 in [ 1..6 ] )
	Then Begin
	  ROLL_DICE := ( 0 , 0 );
	  PR_ERROR ( 0 , 'Bad die values; try again' )
	End
    Until ( ROLL_DICE.DIE1 in [ 1..6 ] ) And ( ROLL_DICE.DIE2 in [ 1..6 ] );

  End;

  If ( GAME.DISPLAY_TYPE <> REGULAR ) Or GAME.AUTO_ROLL
    Then PR_DICE ( ROLL_DICE )

End;	(* ROLL_DICE *)
$PAGE DISPOSE_MOVE_VALUE
Procedure DISPOSE_MOVE_VALUE ( MOVE : ^MOVE_VALUE );

Begin

  If MOVE^.MOVES <> Nil
    Then Dispose ( MOVE^.MOVES );
  Dispose ( MOVE )

End;		(* DISPOSE_MOVE_VALUE *)
$PAGE COMPUTER_WINS
Function COMPUTER_WINS ( BOARD : BOARD_REC ): Boolean;

Var I : SMALL_INT;		(* LOOP_INDEX *)

Begin

  COMPUTER_WINS := BOARD.C_BAR = 0;

  For I := 1 to 24 Do
    COMPUTER_WINS := COMPUTER_WINS And ( BOARD.BOARD [ I ] >= 0 );

  Assert ( COMPUTER_WINS = ( Abs ( BOARD.BOARD [ 25 ] ) = 15 ) )

End;		(* COMPUTER_WINS *)
$PAGE PLAYER_WINS
Function PLAYER_WINS ( BOARD : BOARD_REC ): Boolean;

Var I : SMALL_INT;		(* Loop index *)

Begin

  PLAYER_WINS := BOARD.P_BAR = 0;

  For I := 1 to 24 Do
    PLAYER_WINS := PLAYER_WINS And ( BOARD.BOARD [ I ] <= 0 );

  Assert ( PLAYER_WINS = ( BOARD.BOARD [ 0 ] = 15 ) )

End;		(* PLAYER_WINS *)
$PAGE COMPUTER_GAMMON
Function COMPUTER_GAMMON ( BOARD : BOARD_REC ) : Boolean;

Var I : SMALL_INT;

Begin

  COMPUTER_GAMMON := BOARD.BOARD [ 0 ] = 0;		(* None off *)

  (* If there are any men in the computers home board it is not a GAMMON,
     but rather a BACKGAMMON. *)

  If COMPUTER_GAMMON
    Then For I := 19 to 24 Do
      COMPUTER_GAMMON := COMPUTER_GAMMON And ( BOARD.BOARD [ I ] <= 0 )

End;		(* COMPUTER_GAMMON *)
$PAGE COMPUTER_BACKGAMMON
Function COMPUTER_BACKGAMMON ( BOARD : BOARD_REC ) : Boolean;

Var I : SMALL_INT;

Begin

  COMPUTER_BACKGAMMON := ( BOARD.P_BAR <> 0 ) And ( BOARD.BOARD[0] = 0 );

  If Not COMPUTER_BACKGAMMON
    Then For I := 19 to 24 Do
	COMPUTER_BACKGAMMON := COMPUTER_BACKGAMMON Or ( BOARD.BOARD [ I ] > 0 )

End;		(* COMPUTER_BACKGAMMON *)
$PAGE PLAYER_GAMMON
Function PLAYER_GAMMON ( BOARD : BOARD_REC ) : Boolean;

Var I : SMALL_INT;

Begin

  PLAYER_GAMMON := BOARD.BOARD [ 25 ] = 0;	(* None borne off yet *)

  (* If there are any computers ment in the players home board, then it is
     not a GAMMON, but rather a BACKGAMMON *)

  If PLAYER_GAMMON
    Then For I := 1 to 6 Do
      PLAYER_GAMMON := PLAYER_GAMMON And ( BOARD.BOARD [ I ] >= 0 )

End;		(* PLAYER_GAMMON *)
$PAGE PLAYER_BACKGAMMON
Function PLAYER_BACKGAMMON ( BOARD : BOARD_REC ) : Boolean;

Var I : SMALL_INT;

Begin

  PLAYER_BACKGAMMON := ( BOARD.C_BAR <> 0 ) And ( BOARD.BOARD [ 25 ] = 0 );

  If Not PLAYER_BACKGAMMON
    Then
      For I := 1 to 6 Do
	PLAYER_BACKGAMMON := PLAYER_BACKGAMMON Or ( BOARD.BOARD [ I ] < 0 )

End;		(* PLAYER_BACKGAMMON *)
$PAGE INVERT_BOARD
Public Function INVERT_BOARD ( BOARD : BOARD_REC ): BOARD_REC;

Var I : SMALL_INT;

Begin

  INVERT_BOARD.C_BAR := BOARD.P_BAR;
  INVERT_BOARD.P_BAR := BOARD.C_BAR;

  For I := 0 to 25 Do
    INVERT_BOARD.BOARD [ I ] := -1 * BOARD.BOARD [ 25-I ]

End;		(* INVERT_BOARD.BOARD *)
$PAGE INVERT_MOVE_REC
Function INVERT_MOVE_REC ( MOVE : MOVE_REC ) : MOVE_REC;

Begin

  INVERT_MOVE_REC := MOVE;

  If MOVE.FROM >= 0
    Then INVERT_MOVE_REC.FROM := 25 - MOVE.FROM;

  If MOVE.TARG >= 0
    Then INVERT_MOVE_REC.TARG := 25 - MOVE.TARG

End;		(* INVERT_MOVE_REC *)
$PAGE INVERT_MOVES
Procedure INVERT_MOVES ( Var MOVES : ^MOVE_VALUE );

Var WALK : ^MOVE_VALUE;
    I    : SMALL_INT;

Begin

  WALK := MOVES;

  While WALK <> Nil Do
    Begin
      WALK^.BOARD := INVERT_BOARD ( WALK^.BOARD );
      For I := 1 to Upperbound ( WALK^.MOVES^ ) Do
	WALK^.MOVES^[ I ] := INVERT_MOVE_REC ( WALK^.MOVES^[ I ] );
      WALK := WALK^.NEXT
    End

End;		(* INVERT_MOVES *)
$PAGE SAME_BOARD
(* THis routine checks to see if two board positions are the same,
   and returns true if they are, and false if they are not. *)

Function SAME_BOARD ( BOARD1 : BOARD_REC;
		      BOARD2 : BOARD_REC ) : Boolean;

Var I : SMALL_INT;		(* General loop index *)

Begin


  (* Insure that the number of players on the bar match *)

  SAME_BOARD := ( BOARD1.P_BAR = BOARD2.P_BAR )
		And ( BOARD1.C_BAR = BOARD2.C_BAR );

  (* Now insure the board points are the same *)

  For I := 0 to 25 Do
    Begin
      Exit If Not SAME_BOARD;
      SAME_BOARD := SAME_BOARD And ( BOARD1.BOARD [ I ] = BOARD2.BOARD [ I ] );
    End

End;		(* SAME_BOARD *)
$PAGE LOOKUP_CMD
Function LOOKUP_CMD ( STR : BG_STRING ) : CMD_SCL;

Var CMD : CMD_SCL;

Begin

  LOOKUP_CMD := ERROR_CMD;		(* Pessimistic, error case *)

  For CMD := Minimum ( CMD_SCL ) to Maximum ( CMD_SCL ) Do
    If ( Length ( STR ) <= Length ( CMD_CONST [ CMD ].TXT ) )
    Andif ( Length ( STR ) >= CMD_CONST [ CMD ].ABBREV )
    Andif ( STR = Substr ( CMD_CONST [ CMD ].TXT , 1 , Length ( STR ) ) )
      Then LOOKUP_CMD := CMD

End;		(* LOOKUP_CMD *)
$PAGE TRIM_BLANKS
Procedure TRIM_BLANKS ( STR : BG_STRING;
		    Var IDX : Integer );

Begin

  While ( IDX <= Length ( STR ) )
  Andif ( ( Ord ( STR [ IDX ] ) <= Ord ( ' ' ) )
	Or ( STR [ IDX ] = ',' ) )
    Do IDX := IDX + 1

End;		(* TRIM_BLANKS *)
$PAGE PARSE_TOKEN
Procedure PARSE_TOKEN ( STR : BG_STRING;
		    Var IDX : Integer;
		    Var TOK : BG_STRING;
		    Var ERR_IDX : Integer );

Var IDX2 : Integer;

Begin

  TRIM_BLANKS ( STR , IDX );

  ERR_IDX := IDX;

  If IDX > Length ( STR )
    Then TOK := ''
  Else Begin
    IDX2 := Verify ( Substr ( STR , IDX ) , [ 'A'..'Z' , '0'..'9' ] );

    If IDX2 = 0
      Then Begin
	TOK := Substr ( STR , IDX );
	IDX := Length ( STR ) + 1
      End
    Else If IDX2 = 1
      Then Begin
	TOK := STR [ IDX ];
	IDX := IDX + 1
      End
    Else Begin
      TOK := Substr ( STR , IDX , IDX2 - 1 );
      IDX := IDX + IDX2
    End
  End

End;		(* PARSE_TOKEN *)
$PAGE PARSE_LINE
Function PARSE_LINE ( STR : BG_STRING ) : TOKEN_REC;
Var I , IDX : Integer;
    TOK : BG_STRING;
    CMD : CMD_SCL;
Begin
  IDX := 1;		(* Start at the beginning of the line *)
  PARSE_LINE.KIND := ERROR_TOK;
  PARSE_TOKEN ( STR , IDX , TOK , PARSE_LINE.IDX );
  If TOK = ''
    Then PARSE_LINE.KIND := EOS_TOK
  Else Begin
    (* Lookup the command. If it is found and not [ BAR , BEAROFF ] then
       do the command. [ BAR , BEAROFF ] , # are values else error *)
    CMD := LOOKUP_CMD ( TOK );
    If Not ( CMD in [ BEAROFF_CMD , BAR_CMD , ERROR_CMD ] )
      Then Begin
	If CMD = HELP_CMD
	  Then Begin
	    PARSE_TOKEN ( STR , IDX , TOK , PARSE_LINE.IDX );
	    CMD := LOOKUP_CMD ( TOK );
	    If ( TOK <> '' ) And ( CMD = ERROR_CMD )
	      Then PR_ERROR ( PARSE_LINE.IDX , 'Command expected' )
	    Else PARSE_LINE.KIND := HELP_TOK;
	    PARSE_LINE.CMD := CMD;
	  End
	Else Begin
	  PARSE_LINE.KIND := COMMAND_TOK;
	  PARSE_LINE.CMD := CMD
	End;
	TRIM_BLANKS ( STR , IDX );
	If IDX <= Length ( STR )
	  Then Begin
	    PR_ERROR ( IDX , 'Extraneous input' );
	    PARSE_LINE.KIND := ERROR_TOK
	  End
      End
    Else Begin	(* #|BAR [,] #,BEAROFF, or error case *)
       If ( CMD = BEAROFF_CMD )
       Or ( (CMD<>BAR_CMD) And Not ( TOK [ 1 ] in [ '0'..'9' ] ) )
	Then PR_ERROR ( PARSE_LINE.IDX , 'Invalid Command' )
      Else Begin
	If TOK = '25' Then CMD := BAR_CMD;	(* Fake BAR indication *)
	If CMD = BAR_CMD
	  Then I := -1
	Else Getstring ( TOK , I );
	If ( CMD = BAR_CMD ) Orif ( Iostatus = Io_ok )
	  Then If ( CMD = BAR_CMD ) Orif ( I in [ 1..24 ] )
	    Then begin
	      PARSE_LINE.MOVE.FROM := I;
	      PARSE_TOKEN ( STR , IDX , TOK , PARSE_LINE.IDX );
	      If TOK = ''
		Then PR_ERROR ( PARSE_LINE.IDX , 'Number or BEAROFF expected' )
	      Else Begin
		CMD := LOOKUP_CMD ( TOK );
		If ( TOK [ 1 ] in [ '0'..'9' ] ) Or ( CMD = BEAROFF_CMD )
		  Then Begin
		    If TOK = '0' Then CMD := BEAROFF_CMD;
		    If CMD = BEAROFF_CMD
		      Then I := -2
		    Else Getstring ( TOK , I );
		    If ( CMD = BEAROFF_CMD ) Orif ( Iostatus = Io_ok )
		      Then If ( CMD=BEAROFF_CMD ) Orif ( I in [ 1..24 ] )
			     Then Begin
			       PARSE_LINE.MOVE.TARG := I;
			       (* Check for extraneous input *)
			       TRIM_BLANKS ( STR , IDX );
			       If IDX <= Length ( STR )
				 Then PR_ERROR ( IDX , 'Extraneous input' )
			       Else PARSE_LINE.KIND := MOVE_TOK
			     End
			   Else PR_ERROR ( PARSE_LINE.IDX, 'Number out of range, 1..24' )
		    Else PR_ERROR ( PARSE_LINE.IDX , 'Invalid number format' );
		  End
		Else PR_ERROR ( PARSE_LINE.IDX , 'Number or BEAROFF expected' )
	      End
	    End
	  Else PR_ERROR ( PARSE_LINE.IDX, 'Number out of range, 1..24' )
	Else PR_ERROR ( PARSE_LINE.IDX, 'Invalid number format' )
      End
    End
  End
End;		(* PARSE_LINE *)
$PAGE PROCESS_COMMAND
Procedure PROCESS_COMMAND ( TOK : TOKEN_REC;
			    BOARD : BOARD_REC;
			    DICE  : DICE_REC );

Var STR : BG_STRING;
    I : Integer;
    X : Real;

Begin

  Case TOK.CMD Of
	INSTRUCTIONS_CMD : PR_INSTRUCTIONS;

	BOARD_CMD : PR_BOARD ( BOARD );

	DICE_CMD : PR_DICE ( DICE );

	STOP_CMD : Begin
		     RESTORE_LINE_CHARACTERISTICS;
		     Stop
		   End;

	QUIT_CMD : PR_MESSAGE ( 'QUIT does nothing here. Use STOP to end the session.' );

	DOUBLE_CMD : PR_MESSAGE ( 'You must start the game before you can double.' );

	CUBE_CMD : Begin
		     Putstring ( STR , 'The doubling cube has the value: ',
			GAME.DOUBLE_VALUE:0 , ' and is currently ' );
		     Case GAME.DOUBLING_CUBE of
			CUBE_FREE : PR_MESSAGE ( STR || 'free.' );
			PLAYER_CONTROLS : PR_MESSAGE ( STR || 'yours.' );
			COMPUTER_CONTROLS : PR_MESSAGE ( STR || 'mine.' )
		     End;		(* Case *)
		   End;

	COMPUTER_CMD : Begin
			GAME.PLAY_YOURSELF := Not GAME.PLAY_YOURSELF;
			If GAME.PLAY_YOURSELF
			  Then PR_MESSAGE ( 'I will play against myself.' )
			Else PR_MESSAGE ( 'I will play against you.' )
		       End;

	DOUBLING_CMD : Begin
			 GAME.DOUBLING := Not GAME.DOUBLING;
			 If GAME.DOUBLING
			   Then PR_MESSAGE ( 'Doubling allowed.' )
			 Else PR_MESSAGE ( 'Doubling NOT allowed.' )
		       End;

	POSSIBLE_CMD : Begin
			 GAME.PLAYERS_MOVES := Not GAME.PLAYERS_MOVES;
			 If GAME.PLAYERS_MOVES
			   Then PR_MESSAGE ( 'Displaying your possible moves.' )
			 Else PR_MESSAGE ( 'Not displaying your moves.' )
		       End;

	DISPLAY_CMD : Begin
			GAME.SHOW_EVAL := Not GAME.SHOW_EVAL;
			If GAME.SHOW_EVAL
			  Then PR_MESSAGE ( 'Displaying my moves and evaluations.')
			Else PR_MESSAGE ( 'Not displaying my evaluations.' )
		      End;

	SEED_CMD : Begin
		     STR := READ_A_LINE ( 'Seed: ' );
		     Getstring ( STR , I );
		     If Iostatus <> Io_ok
			Then PR_ERROR ( 0 , 'Bad seed value.' )
		     Else X := Random ( I )
		   End;

	ROLL_CMD : Begin
		     GAME.AUTO_ROLL := Not GAME.AUTO_ROLL;
		     If GAME.AUTO_ROLL
		       Then PR_MESSAGE ( 'I will roll the dice.' )
		     Else PR_MESSAGE ( 'You must roll the dice.' )
		   End;

	EVALUATE_CMD : If GAME.GAME_TYP = EVALUATING_POSN
			 Then Begin
			   GAME.GAME_TYP := NORMAL;
			   PR_MESSAGE ( 'Normal game.' )
			 End
		       Else Begin
			 GAME.GAME_TYP := EVALUATING_POSN;
			 PR_MESSAGE ( 'Evaluate board positions.' )
		       End;

	STATE_CMD : PR_GAME_STATE ( GAME );

	ADM_3A_CMD : Begin
		    GAME.DISPLAY_TYPE := ADM_3A;
		    PR_BOARD ( BOARD )
		  End;

	TEK_4023_CMD : Begin
		    GAME.DISPLAY_TYPE := TEK_4023;
		    PR_BOARD ( BOARD )
		  End;

	ADDS_CMD : Begin
		     GAME.DISPLAY_TYPE := ADDS;
		     PR_BOARD ( BOARD )
		   End;

	REGULAR_CMD : Begin
		    GAME.DISPLAY_TYPE := REGULAR;
		    PR_MESSAGE ( 'Displaying the board for a hardcopy terminal.' );
		  End;

	STATS_CMD : DO_STATS;

	TERSE_CMD : If GAME.TERSE_SETTING = VERBOSE
		      Then Begin
			GAME.TERSE_SETTING := TERSE;
			PR_MESSAGE ( 'Terse mode.' )
		      End
		    Else Begin
		      GAME.TERSE_SETTING := VERY_TERSE;
		      PR_MESSAGE ( 'Very terse mode.' )
		    End;

	VERBOSE_CMD : Begin
			GAME.TERSE_SETTING := VERBOSE;
			PR_MESSAGE ( 'Verbose mode.' )
		      End;

	RESET_CMD : PR_ERROR ( 0 , 'Only allowed when moving.' );

	START_CMD : ;

	Others : PR_ERROR ( 0 , 'Unknown command?' )
  End
End;		(* PROCESS_COMMAND *)
$PAGE START_GAME
Procedure START_GAME ( BOARD : BOARD_REC;
			DICE : DICE_REC );

Var TOK : TOKEN_REC;		(* To get the tokenized line *)
    STR : BG_STRING;		(* To read the line into *)

Begin

  (* Loop reading lines until the user enters START *)

  Repeat

    STR := READ_A_LINE ( 'BG: ' );

    (* Now parse the line *)

    TOK := PARSE_LINE ( STR );

    If TOK.KIND = MOVE_TOK
      Then PR_ERROR ( 0 , 'Enter "START" to begin the game before making any moves' )
    Else If TOK.KIND = HELP_TOK
      Then DO_HELP ( TOK )
    Else If TOK.KIND = COMMAND_TOK
      Then PROCESS_COMMAND ( TOK , BOARD , DICE )

  Until ( TOK.KIND = COMMAND_TOK ) Andif ( TOK.CMD = START_CMD );

End;		(* START_GAME *)
$PAGE FURTHEST_PLAYER_FROM_BAR
Function FURTHEST_PLAYER_FROM_BAR ( BOARD : BOARD_REC;
			     POINT : SMALL_INT ): Boolean;

Var I : SMALL_INT;

Begin

  FURTHEST_PLAYER_FROM_BAR := BOARD.P_BAR = 0;

  For I :=T+1 to 24 Do
    FURTHEST_PLAYER_FROM_BAR := FURTHEST_PLAYER_FROM_BAR And ( BOARD.BOARD [ I ] <= 0 )

End;		(* FURTHEST_PLAYER_FROM_BAR *)
$PAGE LEGAL_PLAYER_MOVE
Procedure LEGAL_PLAYER_MOVE ( Var NEW_BOARD : BOARD_REC;
			      Var DICE  : DICE_REC;
				  BOARD : BOARD_REC;
				  MOVE  : MOVE_REC;
			      Var LEGAL : Boolean );

Var STR : BG_STRING;		(* For constructing error messages *)
    SAVED_DICE : DICE_REC;
    J : Integer;
    ALL_HOME : Boolean;			(* Check legal bearing off *)

Procedure LEGAL_ERROR ( MSG : BG_STRING );

Begin
  PR_ERROR ( 0 , MSG );
  LEGAL := False
End;

Begin
  NEW_BOARD := BOARD;		(* Start with the beginning *)
  SAVED_DICE := DICE;
  LEGAL := True;		(* Assume user entered properly *)

  If MOVE.FROM < 0		(* Indicates movement from bar *)
    Then Begin
      If NEW_BOARD.P_BAR = 0
	Then LEGAL_ERROR ( 'You have no men on the BAR to bring on' )
      Else If Not ( MOVE.TARG in [ 25-DICE.DIE1 , 25-DICE.DIE2 ] )
      Or ( NEW_BOARD.BOARD [ MOVE.TARG ] <= -2 )
	Then LEGAL_ERROR ( 'Target of bearing on is invalid' )
      Else Begin		(* Legal move *)
	NEW_BOARD.P_BAR := NEW_BOARD.P_BAR - 1;
	(* Was a computer point hit? *)

	If NEW_BOARD.BOARD [ MOVE.TARG ] = -1
	  Then Begin
	    NEW_BOARD.C_BAR := NEW_BOARD.C_BAR + 1;
	    NEW_BOARD.BOARD [ MOVE.TARG ] := 1
	  End
	Else NEW_BOARD.BOARD [ MOVE.TARG ] := NEW_BOARD.BOARD [ MOVE.TARG ] + 1;

	(* Fix DIE so that they can't be re-used *)

	If 25-MOVE.TARG = DICE.DIE1
	  Then DICE.DIE1 := DICE.DIE2
	Else DICE.DIE2 := DICE.DIE1
      End
    End
  Else If MOVE.TARG < 0	(* Indicates bearing off *)
    Then Begin
      (* Ckeck to see if bearing off is legal at this time *)
      ALL_HOME := NEW_BOARD.P_BAR = 0;
      For J := 7 to 24 Do
	ALL_HOME := ALL_HOME And ( NEW_BOARD.BOARD[ J ] < 1 );

      If Not ALL_HOME
	Then LEGAL_ERROR ( 'You can''t bear off at this time' )
      Else If ( NEW_BOARD.BOARD [ MOVE.FROM ] < 1 )
	Then Begin
	  Putstring ( STR , 'You have no one on ' , MOVE.FROM );
	  LEGAL_ERROR ( STR )
	End
      Else If ( MOVE.FROM > DICE.DIE1 ) And ( MOVE.FROM > DICE.DIE2 )
	Then LEGAL_ERROR ( 'Nice try! But to bear off from there you need larger die' )
      Else If ( MOVE.FROM in [ DICE.DIE1, DICE.DIE2 ] )
      Orif FURTHEST_PLAYER_FROM_BAR ( BOARD , MOVE.FROM )
	Then Begin
	  (* Fix the dice so that user cannot re-use, doubles won't matter *)
	  If MOVE.FROM = DICE.DIE1
	    Then DICE.DIE1 := DICE.DIE2
	  Else If MOVE.FROM = DICE.DIE2
	    Then DICE.DIE2 := DICE.DIE1
	  Else If DICE.DIE1 > DICE.DIE2
	    Then DICE.DIE1 := DICE.DIE2
	  Else DICE.DIE2 := DICE.DIE2;

	  (* Make the players move *)
	  NEW_BOARD.BOARD [ 0 ] := NEW_BOARD.BOARD [ 0 ] + 1;
	  NEW_BOARD.BOARD [ MOVE.FROM ] := NEW_BOARD.BOARD [ MOVE.FROM ] - 1;
	End
      Else LEGAL_ERROR ( 'Bear-off the higher man first' )
    End
  Else Begin
    If ( NEW_BOARD.BOARD [ MOVE.TARG ] < -1 )
    Or ( NEW_BOARD.BOARD [ MOVE.FROM ] < 1 )
    Or Not ( MOVE.FROM - MOVE.TARG in [ DICE.DIE1,DICE.DIE2 ] )
      Then Begin
	If NEW_BOARD.BOARD [ MOVE.TARG ] < -1 
	  Then Putstring ( STR , 'I control point' , MOVE.TARG:0 )
	Else If NEW_BOARD.BOARD [ MOVE.FROM ] < 1 
	  Then Putstring ( STR , 'You have no one on ' , MOVE.FROM:0 )
	Else Putstring ( STR , 'From ', MOVE.FROM:0, ' to ', MOVE.TARG:0 ,
		' is not equal to the value of either die?' );
	LEGAL_ERROR ( STR )
      End
    Else Begin
      (* Don't allow player to reuse this die *)
      If ( MOVE.FROM - MOVE.TARG ) = DICE.DIE1
	Then DICE.DIE1 := DICE.DIE2
      Else DICE.DIE2 := DICE.DIE1;

      (* MAKE the move *)

      NEW_BOARD.BOARD [ MOVE.FROM ] := NEW_BOARD.BOARD [ MOVE.FROM ] - 1;
      (* Check to see if computer blot was hit *)

      If NEW_BOARD.BOARD [ MOVE.TARG ] = -1
	Then Begin
	  NEW_BOARD.C_BAR := NEW_BOARD.C_BAR + 1;
	  NEW_BOARD.BOARD [ MOVE.TARG ] := 1
	End
      Else NEW_BOARD.BOARD [ MOVE.TARG ] := NEW_BOARD.BOARD [ MOVE.TARG ] + 1
    End;
  End;

  (* Must insure that player moved men from bar, if there were any *)

  If LEGAL And ( NEW_BOARD.P_BAR <> 0 )
  Andif ( NEW_BOARD.P_BAR <> BOARD.P_BAR - 1 )
    Then LEGAL_ERROR ( 'You must remove the men from the BAR first.' );

  If Not LEGAL
    Then DICE := SAVED_DICE

End;		(* LEGAL_PLAYER_MOVE *)
$PAGE READ_PLAYERS_MOVE
Function READ_PLAYERS_MOVE ( BOARD : BOARD_REC;
			     MOVES : ^MOVE_VALUE;
			     DICE  : DICE_REC ) : MOVE_REC;

Const DECLINE_DOUBLE_TOK : TOKEN_REC := ( 0 , MOVE_TOK , (0,-1) );
      QUIT_TOK : TOKEN_REC := ( 0 , MOVE_TOK , ( 0,0 ) );
      RESET_TOK : TOKEN_REC := ( 0 , MOVE_TOK , (-1,-1) );

Var TOK : TOKEN_REC;
    STR : BG_STRING;

Begin

  Repeat
    STR := READ_A_LINE ( 'BG: ' );

    TOK := PARSE_LINE ( STR );

    If TOK.KIND = HELP_TOK
      Then DO_HELP ( TOK )
    Else If TOK.KIND = COMMAND_TOK
      Then Begin
	If TOK.CMD = RESET_CMD
	  Then TOK := RESET_TOK
	Else If TOK.CMD in [ QUIT_CMD, STOP_CMD ]
	  Then Begin
	    If MY_QUERY ( 'Quitting costs you this game. Do you indeed want to quit' )
	      Then TOK := QUIT_TOK
	  End
	Else If TOK.CMD = POSSIBLE_CMD
	  Then PR_LIST_OF_MOVES ( MOVES , GAME.SHOW_EVAL )
	Else If TOK.CMD = DOUBLING_CMD
	  Then PR_ERROR ( 0 , 'Nice try. Doubling may only be enabled or disabled at the start of the game!' )
	Else If TOK.CMD = DOUBLE_CMD
	  Then Begin
	    If Not GAME.DOUBLING
	      Then PR_ERROR ( 0 , 'You disabled doubling, remember!' )
	    Else If GAME.DOUBLING_CUBE = COMPUTER_CONTROLS
	      Then PR_ERROR ( 0 , 'I control the doubling cube.' )
	    Else Begin
	      If ACC_DOUBLE ( MOVES^.BOARD )
		Then Begin
		  GAME.DOUBLING_CUBE := COMPUTER_CONTROLS;
		  GAME.DOUBLE_VALUE := GAME.DOUBLE_VALUE * 2;
		  Putstring ( STR , 'I accept your double! The cube''s value is now: ' , GAME.DOUBLE_VALUE:0 );
		  PR_ERROR ( 0 , STR )
		End
	      Else Begin
		PR_ERROR ( 0 , 'I think you have me this time, therefore I decline the double' );
		TOK := DECLINE_DOUBLE_TOK;
	      End
	    End
	  End
	Else PROCESS_COMMAND ( TOK , BOARD , DICE )
      End
    Else If Not ( TOK.KIND in [ ERROR_TOK , MOVE_TOK ] )
      Then PR_ERROR ( 0 , 'A valid move is required; try again' )

  Until TOK.KIND = MOVE_TOK;

  READ_PLAYERS_MOVE := TOK.MOVE
End;		(* READ_PLAYERS_MOVE *)
$PAGE CLEAR_BOARD
Function CLEAR_BOARD ( WHO_WINS : ROLL_SCL ) : BOARD_REC;

Const EMPTY_BOARD : BOARD_REC := ((15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-15),0,0);

Begin

  CLEAR_BOARD := EMPTY_BOARD;

  If WHO_WINS = PLAYERS
    Then Begin
      CLEAR_BOARD.BOARD [ 24 ] := -1;
      CLEAR_BOARD.BOARD [ 25 ] := -14
    End
  Else Begin
    CLEAR_BOARD.BOARD [ 1 ] := 1;
    CLEAR_BOARD.BOARD [ 0 ] := 14
  End

End;		(* CLEAR_BOARD *)
$PAGE ENTER_PLAYER_MOVE
Function ENTER_PLAYER_MOVE ( MOVES : ^MOVE_VALUE;
			     DICE  : DICE_REC;
			     BOARD : BOARD_REC ) : BOARD_REC;

Var TOK : TOKEN_REC;			(* For the parsed token line *)
    LEGAL : Boolean;			(* Legal move ? *)
    NEW_BOARD : BOARD_REC;		(* For the "new" board from LEGAL *)
    TDICE : DICE_REC;			(* LEGEL will want to change DICE *)
    NUM_MOVES : SMALL_INT;		(* Moves made so far *)
    MOVE_LST  : ^MOVE_LIST;
    NEW_MOVE  : ^MOVE_LIST;
    I        : SMALL_INT;

Begin

  If GAME.PLAYERS_MOVES
    Then PR_LIST_OF_MOVES ( MOVES , False );

  TDICE := DICE;
  ENTER_PLAYER_MOVE := BOARD;
  NUM_MOVES := 0;
  MOVE_LST := Nil;

  While NUM_MOVES < Upperbound ( MOVES^.MOVES^ ) Do
    Begin
      TOK.MOVE := READ_PLAYERS_MOVE ( ENTER_PLAYER_MOVE , MOVES , DICE );

      If ( TOK.MOVE.FROM = 0 ) And ( TOK.MOVE.TARG = 0 )
	Then Begin
	  ENTER_PLAYER_MOVE := CLEAR_BOARD ( COMPUTERS );
	  NUM_MOVES := Upperbound ( MOVES^.MOVES^ )
	End
      Else If ( TOK.MOVE.FROM = 0 ) And ( TOK.MOVE.TARG = -1 )
	Then Begin
	  ENTER_PLAYER_MOVE := CLEAR_BOARD ( PLAYERS );
	  NUM_MOVES := Upperbound ( MOVES^.MOVES^ )
	End
      Else If ( TOK.MOVE.FROM = -1 ) And ( TOK.MOVE.TARG = -1 )
	Then Begin
	  ENTER_PLAYER_MOVE := BOARD;
	  UPDATE_BOARD ( BOARD , ONLY_IF_CRT );
	  PR_MESSAGE ( '' );
	  If MOVE_LST <> Nil
	    Then Begin
	      Dispose ( MOVE_LST );
	      MOVE_LST := Nil
	    End;
	  NUM_MOVES := 0;
	  TDICE := DICE;
	End
      Else Begin
	LEGAL_PLAYER_MOVE ( NEW_BOARD , TDICE , ENTER_PLAYER_MOVE , TOK.MOVE , LEGAL );
	If LEGAL
	  Then Begin
	    NUM_MOVES := NUM_MOVES + 1;
	    New ( NEW_MOVE , NUM_MOVES );
	    If NUM_MOVES > 1
	      Then Begin
		For I := 1 to NUM_MOVES-1 Do
		  NEW_MOVE^[ I ] := MOVE_LST^[ I ];
		Dispose ( MOVE_LST )
	      End;
	    NEW_MOVE^[ NUM_MOVES ] := TOK.MOVE;
	    MOVE_LST := NEW_MOVE;
	    If GAME.DISPLAY_TYPE <> REGULAR
	      Then PR_MOVE ( MOVE_LST , 'Your move: ' );
	    ENTER_PLAYER_MOVE := NEW_BOARD;
	    UPDATE_BOARD ( NEW_BOARD , ONLY_IF_CRT );
	    Break ( tty );
	  End
      End
    End;

  If MOVE_LST <> Nil
    Then Dispose ( MOVE_LST );

End;		(* ENTER_PLAYER_MOVE *)
$PAGE ADD_MOVE_2_LIST
Procedure ADD_MOVE_2_LIST ( Var LIST : ^MOVE_VALUE;
				MOVES: ^MOVE_LIST;
				BOARD : BOARD_REC;
				POINT  : SMALL_INT;
				DIE   : SMALL_INT );

Var VAL_NODE : ^MOVE_VALUE;
    MOVE_LST : ^MOVE_LIST;
    I        : SMALL_INT;
    MOVES_UPD_PLUS_1 : SMALL_INT;

Begin

  If MOVES = Nil
    Then MOVES_UPD_PLUS_1 := 1
  Else MOVES_UPD_PLUS_1 := Upperbound ( MOVES^ ) + 1;

  New ( VAL_NODE );
  New ( MOVE_LST , MOVES_UPD_PLUS_1 );

  If MOVES_UPD_PLUS_1 > 1
    Then For I := 1 to MOVES_UPD_PLUS_1 - 1 Do
	  MOVE_LST^[ I ] := MOVES^[ I ];
  MOVE_LST^[ MOVES_UPD_PLUS_1 ].FROM := POINT;

  VAL_NODE^ := ( 0.0 , BOARD , MOVE_LST , LIST );

  MOVE_LST^[ MOVES_UPD_PLUS_1 ].TARG := POINT + DIE;

  VAL_NODE^.BOARD.BOARD [ POINT ] := VAL_NODE^.BOARD.BOARD [ POINT ] + 1;

  If VAL_NODE^.BOARD.BOARD [ POINT + DIE ] = 1
    Then Begin
      VAL_NODE^.BOARD.BOARD [ POINT + DIE ] := -1;
      VAL_NODE^.BOARD.P_BAR := VAL_NODE^.BOARD.P_BAR + 1
    End
  Else VAL_NODE^.BOARD.BOARD [ POINT + DIE ] :=
	    VAL_NODE^.BOARD.BOARD [ POINT + DIE ] - 1;

  LIST := VAL_NODE
End;		(* ADD_MOVE_2_LIST *)
$PAGE NO_MEN_HIGHER
Function NO_MEN_HIGHER ( BOARD : BOARD_REC;
			 POINT : SMALL_INT;
			 DIE   : SMALL_INT ) : Boolean;

Var I : SMALL_INT;

Begin

  If ( POINT = (25-DIE) ) And ( BOARD.BOARD [ POINT ] < 0 )
    Then NO_MEN_HIGHER := True
  Else Begin
    NO_MEN_HIGHER := BOARD.C_BAR = 0;

    For I := 1 to POINT-1 Do
      NO_MEN_HIGHER := NO_MEN_HIGHER And ( BOARD.BOARD [ I ] >= 0 )
  End

End;		(* NO_MEN_HIGHER *)
$PAGE GEN_MOVES
Function GEN_MOVES ( BOARD : BOARD_REC;
		     MOVES : ^MOVE_LIST;
		     DIE   : SMALL_INT ): ^MOVE_VALUE;

Var I : SMALL_INT;		(* Loop index for walking board *)
    ALL_HOME : Boolean;		(* All men in home board, or borne off *)

Begin
  GEN_MOVES := Nil;			(* Must start somewhere *)

  (* For each of the points from the current position up through
     the last point that could not bear off check to see if a move is 
     legal. If so, add it to the list. BEARING OFF will be handled
     later in the routine. *)

  For I := 1 to 24-DIE Do
    If ( BOARD.BOARD [ I ] < 0 ) And ( BOARD.BOARD [ I+DIE ] <= 1 )
      Then ADD_MOVE_2_LIST ( GEN_MOVES , MOVES , BOARD , I , DIE );

  (* Check to see if the computer can bear off. CAN if No men on bar,
     and all men in home board, or already BORNE off *)

  ALL_HOME := BOARD.C_BAR = 0;
  For I := 1 to 18 Do
    Begin
      Exit If Not ALL_HOME;
      ALL_HOME := ALL_HOME And ( BOARD.BOARD [ I ] > -1 );
    End;

  If ALL_HOME
    Then Begin
      (* Find the greates point that we can BEAR OFF from *)
      I := 25 - DIE;
      While ( BOARD.BOARD [ I ] >= 0 ) And ( I <= 24 )
      And NO_MEN_HIGHER ( BOARD , I , DIE ) Do
	I := I + 1;

      If ( I < 25 ) And ( BOARD.BOARD [ I ] < 0 )
	Then ADD_MOVE_2_LIST ( GEN_MOVES , MOVES , BOARD , I , 25-I )
    End
End;		(* GEN_MOVES *)
$PAGE USED_MAX_DIE
Function USED_MAX_DIE ( WALK : ^MOVE_VALUE;
			DIE  : SMALL_INT ): Boolean;

Var I : SMALL_INT;

Begin

  If WALK^.MOVES^[1].FROM = -1	(* FROM BAR *)
    Then Begin
      USED_MAX_DIE := ( DIE = ( WALK^.MOVES^[1].TARG ) )
	Or ( WALK^.BOARD.BOARD [ DIE ] > 1 )
    End
  Else Begin
    USED_MAX_DIE := DIE = ( WALK^.MOVES^[1].TARG - WALK^.MOVES^[1].FROM );
    If Not USED_MAX_DIE
      Then Begin
	USED_MAX_DIE := True;
	For I := 0 to DIE-1 Do
	  USED_MAX_DIE := USED_MAX_DIE And
	      ( WALK^.BOARD.BOARD [ I ] > -1 );
      End
  End

End;		(* USED_MAX_DIE *)
$PAGE MAX_PIP_MOVED
Function MAX_PIP_MOVED ( MOVES : ^MOVE_VALUE ) : Integer;

Var PIPS_MOVED : Integer;
    I : Integer;

Begin

  MAX_PIP_MOVED := 1;

  For I := 1 to Upperbound ( MOVES^.MOVES^ ) Do
    Begin
      If MOVES^.MOVES^[ I ].FROM = -1	(* From the BAR *)
	Then PIPS_MOVED := MOVES^.MOVES^[ I ].TARG
      Else PIPS_MOVED := MOVES^.MOVES^[ I ].TARG - MOVES^.MOVES^[ I ].FROM;

      MAX_PIP_MOVED := Max ( PIPS_MOVED , MAX_PIP_MOVED )
    End

End;	(* MAX_PIP_MOVED *)
$PAGE REMOVE_ILL_MOVES
Procedure REMOVE_ILL_MOVES ( Var MOVES : ^MOVE_VALUE;
				DICE : DICE_REC );

Var MAX_DIE_USED : SMALL_INT;		(* Max number of die that could be used *)
    MAX_DIE : SMALL_INT;
    WALK, WALK_SAVE ,
    NEXT_WALK    : ^MOVE_VALUE;

Begin
  MAX_DIE_USED := 1;		(* Had to use at least 1 *)
  MAX_DIE := 1;

  WALK := MOVES;
  While WALK <> Nil Do
    Begin
      MAX_DIE := Max ( MAX_DIE , MAX_PIP_MOVED ( WALK ) );
      If Upperbound ( WALK^.MOVES^ ) > MAX_DIE_USED
	Then MAX_DIE_USED := Upperbound ( WALK^.MOVES^ );
      WALK := WALK^.NEXT
    End;

  WALK_SAVE := Nil;		(* For arrangement of "NEXT" pointers *)
  WALK := MOVES;
  While WALK <> Nil Do
    If ( Upperbound ( WALK^.MOVES^ ) < MAX_DIE_USED )
    Or ( ( MAX_DIE_USED = 1 ) Andif Not USED_MAX_DIE ( WALK , MAX_DIE ) )
      Then Begin
	If WALK_SAVE <> Nil
	  Then WALK_SAVE^.NEXT := WALK^.NEXT
	Else MOVES := WALK^.NEXT;
	NEXT_WALK := WALK^.NEXT;
	DISPOSE_MOVE_VALUE ( WALK );
	WALK := NEXT_WALK
      End
    Else Begin
      WALK_SAVE := WALK;
      WALK := WALK^.NEXT
    End
End;		(* REMOVE_ILL_MOVES *)
$PAGE REMOVE_DUPLICATES
(* Remove all moves from a list that represent duplicate moves *)

Procedure REMOVE_DUPLICATES ( Var MOVES : ^MOVE_VALUE );

Var WALK , NEXT_WALK , WALK_SAVE : ^MOVE_VALUE;

Begin
  WALK := MOVES;		(* Start at the beginning of the list *)
  WALK_SAVE := Nil;

  While WALK <> Nil Do
    Begin
      NEXT_WALK := WALK;	(* Only need to check for nodes following *)
      (* Go from the current node to the end checking for a node that
	 matches. If the node matches it must be deleted. *)
      While NEXT_WALK <> Nil Do
	Begin
	  If NEXT_WALK <> WALK	(* It will match but we don't delete *)
	    Then Begin
	      If SAME_BOARD ( WALK^.BOARD , NEXT_WALK^.BOARD )
		Then Begin
		  (* Don't have to worry about WALK_SAVE = Nil because
		     we started at the second node, not the first *)
		  WALK_SAVE^.NEXT := NEXT_WALK^.NEXT;
		  DISPOSE_MOVE_VALUE ( NEXT_WALK );
		  NEXT_WALK := WALK_SAVE^.NEXT
		End
	      Else Begin
		WALK_SAVE := NEXT_WALK;
		NEXT_WALK := NEXT_WALK^.NEXT
	      End
	    End
	  Else Begin
	    WALK_SAVE := NEXT_WALK;
	    NEXT_WALK := NEXT_WALK^.NEXT
	  End
	End;		(* While *)
      WALK := WALK^.NEXT
    End		(* While WALK <> Nil *)
End;		(* REMOVE_DUPLICATES *)
$PAGE GEN_DOUBLE_MOVES
Procedure GEN_DOUBLE_MOVES ( Var MOVES : ^MOVE_VALUE;
			     DIE   : SMALL_INT;
			     GEN   : SMALL_INT );

Var I : SMALL_INT;
    LAST_WALK,
    WALK_HOLD,
    WALK, WALK2 : ^MOVE_VALUE;

Begin

  For I := 1 to GEN Do
    Begin

      (* Remove those duplicate moves from the list. This will increase
	 execution time, but will hopefully reduce  heap size. *)

      REMOVE_ILL_MOVES ( MOVES , (DIE,DIE) );

      REMOVE_DUPLICATES ( MOVES );

      (* For each node generate all moves, and then go back for each die *)

      WALK := MOVES;
      While WALK <> Nil Do
	Begin
	  WALK_HOLD := GEN_MOVES ( WALK^.BOARD , WALK^.MOVES , DIE );
	  If WALK_HOLD = Nil
	    Then WALK := WALK^.NEXT
	  Else Begin
	    WALK2 := WALK_HOLD;
	    While WALK2^.NEXT <> Nil Do
	      WALK2 := WALK2^.NEXT;
	    WALK2^.NEXT := WALK^.NEXT;
	    WALK^.NEXT := WALK_HOLD;
	    WALK := WALK2^.NEXT
	  End;
	End;

    End		(* For loop *)
End;		(* GEN_DOUBLE_MOVES *)
$PAGE REG_MOVES
Function REG_MOVES ( BOARD : BOARD_REC;
		      DICE  : DICE_REC;
		      CAN_SWITCH_DICE : Boolean ) : ^MOVE_VALUE;

Var SWITCHED_DICE : DICE_REC;		(* For switching the dice *)
    ADD_MOVES     : ^MOVE_VALUE;
    WALK, WALK2   : ^MOVE_VALUE;
    I             : SMALL_INT;

Begin

  (* Check to see if we have doubles. If so use a special
     loop because there are 4 moves *)

  If DICE.DIE1 <> DICE.DIE2
    Then Begin
      REG_MOVES := GEN_MOVES ( BOARD , Nil , DICE.DIE1 );

      (* Now walk the list of moves that were generated, using the second
	 dice for the next move. Then we must recursively call this routine 
	 to have things done for the opposite order. *)

      If REG_MOVES <> Nil
	Then Begin
	  WALK := REG_MOVES;
	  While WALK <> Nil Do
	    Begin
	      ADD_MOVES := GEN_MOVES ( WALK^.BOARD , WALK^.MOVES , DICE.DIE2 );
	      If ADD_MOVES <> Nil
		Then Begin
		  (* Merge the new moves into the current list. It is
		     moved in after the current node but before the next
		     node. WALK is set the the next node in the list *)
		  WALK2 := ADD_MOVES;
		  While WALK2^.NEXT <> Nil Do
		    WALK2 := WALK2^.NEXT;
		  WALK2^.NEXT := WALK^.NEXT;
		  WALK^.NEXT := ADD_MOVES;
		  WALK := WALK2^.NEXT
		End
	      Else WALK := WALK^.NEXT
	    End
	End
      Else REG_MOVES := GEN_MOVES ( BOARD , Nil , DICE.DIE2 );

      (* Now switch the dice and try it the other way. *)

      If CAN_SWITCH_DICE
	Then Begin
	  SWITCHED_DICE.DIE1 := DICE.DIE2;
	  SWITCHED_DICE.DIE2 := DICE.DIE1;

	  ADD_MOVES := REG_MOVES ( BOARD , SWITCHED_DICE , False);

	  (* Now connect the lists *)

	  If REG_MOVES = Nil
	    Then REG_MOVES := ADD_MOVES
	  Else Begin
	    WALK := REG_MOVES;
	    While WALK^.NEXT <> Nil Do
	      WALK := WALK^.NEXT;
	    WALK^.NEXT := ADD_MOVES
	  End
	End
    End

  Else Begin		(* DOUBLES PART *)

    (* The algorithm is to generate all moves using two dice ( using
       the move generator, but doing it 4 times. *)

    REG_MOVES := GEN_MOVES ( BOARD , Nil , DICE.DIE1 );

    GEN_DOUBLE_MOVES ( REG_MOVES , DICE.DIE1 , 3 )
  End
End;		(* REG_MOVES *)
$PAGE ONE_FROM_BAR
Function ONE_FROM_BAR ( BOARD : BOARD_REC;
			DIE   : SMALL_INT ) : ^MOVE_VALUE;

Var MOVE_LST : ^MOVE_LIST;

Begin

  ONE_FROM_BAR := Nil;		(* No possible moves *)

  If BOARD.BOARD [ DIE ] < 2
    Then Begin
      New ( MOVE_LST , 1 );
      MOVE_LST^ := ( ( -1 , DIE ) );
      New ( ONE_FROM_BAR );
      ONE_FROM_BAR^ := ( 0.0 , BOARD , MOVE_LST , Nil );
      ONE_FROM_BAR^.BOARD.C_BAR := ONE_FROM_BAR^.BOARD.C_BAR - 1;

      (* Now make a copy of the board properly represented *)

      If ONE_FROM_BAR^.BOARD.BOARD [ DIE ] = 1
	Then Begin
	  ONE_FROM_BAR^.BOARD.P_BAR := ONE_FROM_BAR^.BOARD.P_BAR + 1;
	  ONE_FROM_BAR^.BOARD.BOARD [ DIE ] := -1
	End
      Else ONE_FROM_BAR^.BOARD.BOARD [ DIE ] :=
	    ONE_FROM_BAR^.BOARD.BOARD [ DIE ] - 1
    End
End;		(* ONE_FROM_BAR *)
$PAGE CLEAR_BAR
(* This routine generates the moves for the roll of the dice where
   men are found on the bar, and must be removed. *)

Function CLEAR_BAR ( BOARD : BOARD_REC;
		     DICE  : DICE_REC;
		     MEN_ON_BAR : SMALL_INT ) : ^MOVE_VALUE;

Var MOVE_LST   : ^MOVE_LIST;
    SAVE_MOVE  : MOVE_REC;
    BAR_MEN    : SMALL_INT;
    I          : SMALL_INT;

Begin

  If DICE.DIE1 <> DICE.DIE2
    Then Begin
      CLEAR_BAR := ONE_FROM_BAR ( BOARD , DICE.DIE1 );

      If CLEAR_BAR = Nil	(* Couldn't clear bar with DIE1 *)
	Then Begin
	  CLEAR_BAR := ONE_FROM_BAR ( BOARD , DICE.DIE2 );

	  If ( CLEAR_BAR <> Nil )
	  Andif ( CLEAR_BAR^.BOARD.C_BAR = 0 )
	    Then CLEAR_BAR^.NEXT := GEN_MOVES ( CLEAR_BAR^.BOARD ,
		   CLEAR_BAR^.MOVES , DICE.DIE1 )
	  (* Else we lose the roll *)
	End
      ELse Begin
	If ( CLEAR_BAR^.BOARD.C_BAR <> 0 )
	  Then Begin
	    CLEAR_BAR^.NEXT := ONE_FROM_BAR ( CLEAR_BAR^.BOARD , DICE.DIE2 );

	    (* Now if there was a man cleared, <> nil , then fix up the
	       movelist so it represents all moves *)

	    If CLEAR_BAR^.NEXT <> Nil
	      Then Begin
		SAVE_MOVE := CLEAR_BAR^.NEXT^.MOVES^[1];
		Dispose ( CLEAR_BAR^.NEXT^.MOVES );
		New ( CLEAR_BAR^.NEXT^.MOVES , 2 );
		CLEAR_BAR^.NEXT^.MOVES^[1] := CLEAR_BAR^.MOVES^[1];
		CLEAR_BAR^.NEXT^.MOVES^[2] := SAVE_MOVE
	      End
	  End
	Else Begin
	  CLEAR_BAR^.NEXT := GEN_MOVES ( CLEAR_BAR^.BOARD , CLEAR_BAR^.MOVES ,
		DICE.DIE2 )
	End
      End
    End
  Else Begin		(* DOUBLES *)
    CLEAR_BAR := ONE_FROM_BAR ( BOARD , DICE.DIE1 );

    If CLEAR_BAR <> Nil
      Then Begin
	BAR_MEN := Min ( 4 , MEN_ON_BAR );

	CLEAR_BAR^.BOARD.C_BAR := CLEAR_BAR^.BOARD.C_BAR - BAR_MEN + 1;
	CLEAR_BAR^.BOARD.BOARD [ DICE.DIE1 ] :=
	    CLEAR_BAR^.BOARD.BOARD [ DICE.DIE1 ] - BAR_MEN + 1;

	SAVE_MOVE := CLEAR_BAR^.MOVES^[1];
	Dispose ( CLEAR_BAR^.MOVES );
	New ( CLEAR_BAR^.MOVES , BAR_MEN );
	For I := 1 to BAR_MEN Do
	  CLEAR_BAR^.MOVES^[ I ] := SAVE_MOVE;

	If BAR_MEN < 4
	  Then GEN_DOUBLE_MOVES ( CLEAR_BAR , DICE.DIE1 , 4-BAR_MEN )
      End
  End
End;		(* CLEAR_BAR *)
$PAGE ALL_MOVES
(* Determine all the possible moves given a board position and a roll
   of the dice. *)

Function ALL_MOVES ( BOARD : BOARD_REC;
		     DICE  : DICE_REC ) : ^MOVE_VALUE;

Var SWITCHED_DICE : DICE_REC;
    WALK, TEMP    : ^MOVE_VALUE;

Begin

  (* If there are any men on the bar then remove them *)

  If BOARD.C_BAR <> 0
    Then Begin
      ALL_MOVES := CLEAR_BAR ( BOARD , DICE , BOARD.C_BAR );

      (* Switch the dice and try it in the other order *)

      SWITCHED_DICE.DIE2 := DICE.DIE1;
      SWITCHED_DICE.DIE1 := DICE.DIE2;

      WALK := CLEAR_BAR ( BOARD , SWITCHED_DICE , BOARD.C_BAR );

      If ALL_MOVES = Nil
	Then ALL_MOVES := WALK
      Else Begin
	TEMP := ALL_MOVES;
	While TEMP^.NEXT <> Nil Do
	  TEMP := TEMP^.NEXT;
	TEMP^.NEXT := WALK
      End
    End
  Else ALL_MOVES := REG_MOVES ( BOARD , DICE , True );
End;		(* ALL_MOVES *)
$PAGE DETERMINE_MOVES
(* This routine determines the moves for a given board position with a
   given roll of the dice. This routine uses subroutines to determine
   duplicate moves, and to determine if there are forced moves. *)

Function DETERMINE_MOVES ( BOARD : BOARD_REC;
			   DICE  : DICE_REC ) : ^MOVE_VALUE;

Begin

  (* Call routine to get all the possible moves with the dice *)

  DETERMINE_MOVES := ALL_MOVES ( BOARD , DICE );

  (* Remove all board moves that were accomplished using less than the total
     possible number of die that could be used, i.e. used 1 not 1 and 7 *)

  REMOVE_ILL_MOVES ( DETERMINE_MOVES , DICE );

  (* Now remove all the duplicate moves *)

  REMOVE_DUPLICATES ( DETERMINE_MOVES );

End;		(* DETERMINE_MOVES *)
$PAGE SORT_MOVES
(* Sorts the list of moves in descending order, i.e. best move first *)

Procedure SORT_MOVES ( Var MOVES : ^MOVE_VALUE );

Var WALK,
    WALK2,
    OLD_WALK : ^MOVE_VALUE;

Begin

  If MOVES <> Nil
    Then Begin
      WALK := MOVES^.NEXT;			(* Start with the second node *)
      MOVES^.NEXT := Nil;			(* We now have two lists *)

      (* MOVES is a pointer to a SORTED list *)
      (* WALK points at the list to be sorted *)

      While WALK <> Nil Do
	Begin
	  If MOVES^.VALUE <= WALK^.VALUE
	    Then Begin				(* WALK is a better move *)
	      WALK2 := WALK^.NEXT;		(* Must re-chain properly *)
	      WALK^.NEXT := MOVES;
	      MOVES := WALK;			(* Best move first *)
	      WALK := WALK2			(* To continue through the old list *)
	    End
	  Else Begin				(* Find where it should go *)
	    WALK2 := MOVES;
	    (* Walk the SORTED list looking for the node to slide it into *)

	    While ( WALK2 <> Nil ) Andif ( WALK2^.VALUE > WALK^.VALUE ) Do
	      Begin
		OLD_WALK := WALK2;		(* Don't worry about Nil, skiped 1St *)
		WALK2 := WALK2^.NEXT
	      End;

	    (* Found proper slot *)

	    If WALK2 = Nil			(* Add to end of list *)
	      Then Begin
		OLD_WALK^.NEXT := WALK;
		WALK2 := WALK^.NEXT;
		WALK^.NEXT := Nil;		(* Cant dangle off the end *)
		WALK := WALK2
	      End
	    Else Begin
	      OLD_WALK^.NEXT := WALK;
	      OLD_WALK := WALK^.NEXT;		(* OLD_WALK used as temp *)
	      WALK^.NEXT := WALK2;
	      WALK := OLD_WALK
	    End
	  End		(* Else caluse *)

	End		(* While WALK <> Nil *)
    End
End;		(* SORT_MOVES *)
$PAGE DISPOSE_MOVES
Procedure DISPOSE_MOVES ( Var MOVES : ^MOVE_VALUE );

Var WALK : ^MOVE_VALUE;

Begin

  While MOVES <> Nil Do
    Begin
      WALK := MOVES^.NEXT;
      DISPOSE_MOVE_VALUE ( MOVES );
      MOVES := WALK
    End

End;	(* DISPOSE_MOVES *)
$PAGE ADD_MOVE_TO_BOARD
Procedure ADD_MOVE_TO_BOARD ( MOVE : MOVE_REC;
			Var BOARD : BOARD_REC );

Begin

  If MOVE.FROM = -1
    Then BOARD.C_BAR := MOVE.TARG
  Else If MOVE.FROM in [ 0..25 ]
    Then BOARD.BOARD [ MOVE.FROM ] := MOVE.TARG
  Else PR_ERROR ( 0 , 'Only enter legitimate positions, please!' )

End;		(* ADD_MOVE_TO_BOARD *)
$PAGE VALID_POSN
Function VALID_POSN ( STR : BG_STRING;
		   Var MOVE : MOVE_REC ): Boolean;

Begin

  MOVE := (0 , 0);

  Getstring ( STR , MOVE.FROM , MOVE.TARG );

  VALID_POSN := Iostatus = Io_ok;

End;		(* VALID_POSN *)
$PAGE PLAY_BOARD
(* This routine controls a "game" of backgammon which actually analyses
   given board positions. The board position and dice roll are entered
   by the player. *)

Procedure PLAY_BOARD;

Const EMPTY_BOARD : BOARD_REC := (
	(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ) , 0 , 0  );

Var I : SMALL_INT;			(* For loop index *)
    DICE : DICE_REC;
    MOVE_POSN : MOVE_REC;
    STR  : BG_STRING;
    MOVES : ^MOVE_VALUE;
    BOARD : BOARD_REC;
    BOARD_CORRECT : Boolean;

Begin
  Repeat
    BOARD := EMPTY_BOARD;

    Repeat
      STR := Uppercase ( READ_A_LINE ( 'Pt,men: ' ) );
      If STR <> '' Then
	If STR = 'START'
	  Then BOARD := INITIAL_BOARD
	Else If VALID_POSN ( STR , MOVE_POSN )
	  Then ADD_MOVE_TO_BOARD ( MOVE_POSN , BOARD )
	Else PR_ERROR ( 0 , 'Enter "Point, #of men" please!' );
    Until ( STR = '' ) Or ( STR = 'START' );

    (* Count up the men, so position 0 and 25 are complete *)

    BOARD.BOARD [ 0 ] := BOARD.P_BAR;
    BOARD.BOARD [ 25 ] := BOARD.C_BAR;
    For I := 1 to 24 Do
      If BOARD.BOARD [ I ] < 0
	Then BOARD.BOARD [ 25 ] := BOARD.BOARD [ 25 ] + BOARD.BOARD [ I ]
      Else BOARD.BOARD [ 0 ] := BOARD.BOARD [ 0 ] + BOARD.BOARD [ I ];
    BOARD.BOARD [ 0 ] := - 15 - BOARD.BOARD [ 0 ];
    BOARD.BOARD [ 25 ] := 15 - BOARD.BOARD [ 25 ];

    If ( BOARD.BOARD [ 0 ] <= 0 ) And ( BOARD.BOARD [ 25 ] >= 0 )
      Then Begin
	PR_BOARD ( BOARD );
	BOARD_CORRECT := ( STR = 'START' ) Orif MY_QUERY ( 'Board correct' )
      End
    Else Begin
      PR_ERROR ( 0 , 'Invalid BOARD setup; Try again.' );
      BOARD_CORRECT := False
    End;

  Until BOARD_CORRECT;

  GAME.AUTO_ROLL := False;
  DICE := ROLL_DICE;

  MOVES := DETERMINE_MOVES ( BOARD , DICE );
  If MOVES = Nil
    Then PR_ERROR ( 0 , ' There are no moves for that roll.' )
  Else Begin

    EVALUATE_MOVES ( MOVES );

    SORT_MOVES ( MOVES );

    PR_LIST_OF_MOVES ( MOVES , True );

    If MAKE_DOUBLE ( MOVES^.BOARD )
      Then PR_MESSAGE ( 'The computer would double in this position.' )
    Else PR_MESSAGE ( 'The computer would NOT double in this position.' );
    If ACC_DOUBLE ( MOVES^.BOARD )
      Then PR_MESSAGE ( 'The computer would accept a double in this position.' )
    Else PR_MESSAGE ( 'The computer would NOT accept a double.' );

    DISPOSE_MOVES ( MOVES )
  End

End;		(* PLAY_BOARD *)
$PAGE UPD_FACTORS
Procedure UPD_FACTORS;

Begin

  RD_FACTORS;		(* Dont want it not changing when 2 people are playing *)

  If ( GAME.C_MEN_HIT > GAME.P_MEN_HIT ) And ( GAME.WINS.P_SCORE > GAME.WINS.C_SCORE )
    Then Begin
    End;

  If ( GAME.C_POSN_LOST < GAME.P_POSN_LOST ) And ( GAME.WINS.P_SCORE > GAME.WINS.C_SCORE )
    Then Begin
    End;

  If ( GAME.C_BACK_MEN_NOT_MOVED > GAME.P_BACK_MEN_NOT_MOVED )
  And ( GAME.WINS.P_SCORE > GAME.WINS.C_SCORE )
    Then Begin
    End;

  WR_FACTORS;

End;		(* UPD_FACTORS *)
$PAGE END_GAME
Procedure END_GAME ( BOARD : BOARD_REC );

Var GAME_STATS : WIN_REC;

Begin
  GAME_STATS := INIT_WIN_REC;
  GAME_STATS.GAMES := 1;

  GAME.WINS.GAMES := GAME.WINS.GAMES + 1;

  If COMPUTER_BACKGAMMON ( BOARD )
    Then Begin
      GAME.WINS.C_BGAMMONS := GAME.WINS.C_BGAMMONS + 1;
      GAME.WINS.C_SCORE := GAME.WINS.C_SCORE + 3 * GAME.DOUBLE_VALUE;
      GAME_STATS.C_BGAMMONS := 1;
      GAME_STATS.C_SCORE := 3 * GAME.DOUBLE_VALUE;
      PR_MESSAGE ( 'I have BACKGAMMONED you, thats thrice the points for me!' )
    End
  Else If COMPUTER_GAMMON ( BOARD )
    Then Begin
      GAME.WINS.C_GAMMONS := GAME.WINS.C_GAMMONS + 1;
      GAME.WINS.C_SCORE := GAME.WINS.C_SCORE + 2 * GAME.DOUBLE_VALUE;
      GAME_STATS.C_GAMMONS := 1;
      GAME_STATS.C_SCORE := 2 * GAME.DOUBLE_VALUE;
      PR_MESSAGE ( 'I have GAMMONED you, thats twice the points for me!' )
    End
  Else If COMPUTER_WINS ( BOARD )
    Then Begin
      GAME.WINS.C_WINS := GAME.WINS.C_WINS + 1;
      GAME.WINS.C_SCORE := GAME.WINS.C_SCORE + GAME.DOUBLE_VALUE;
      GAME_STATS.C_WINS := 1;
      GAME_STATS.C_SCORE := GAME.DOUBLE_VALUE;
      PR_MESSAGE ( 'I emerge victorious again!' )
    End
  Else If PLAYER_BACKGAMMON ( BOARD )
    Then Begin
      GAME.WINS.P_BGAMMONS := GAME.WINS.P_BGAMMONS + 1;
      GAME.WINS.P_SCORE := GAME.WINS.P_SCORE + 3 * GAME.DOUBLE_VALUE;
      GAME_STATS.P_BGAMMONS := 1;
      GAME_STATS.P_SCORE := 3 * GAME.DOUBLE_VALUE;
      PR_MESSAGE ( 'I was BACKGAMMONED? I''d like to see you do that again!' )
    End
  Else If PLAYER_GAMMON ( BOARD )
    Then Begin
      GAME.WINS.P_GAMMONS := GAME.WINS.P_GAMMONS + 1;
      GAME.WINS.P_SCORE := GAME.WINS.P_SCORE + 2 * GAME.DOUBLE_VALUE;
      GAME_STATS.P_GAMMONS := 1;
      GAME_STATS.P_SCORE := 2 * GAME.DOUBLE_VALUE;
      PR_MESSAGE ( 'I was GAMMONED? Lets see you do that again!' )
    End
  Else Begin		(* PLAYER_WINS *)
    GAME.WINS.P_WINS := GAME.WINS.P_WINS + 1;
    GAME.WINS.P_SCORE := GAME.WINS.P_SCORE + GAME.DOUBLE_VALUE;
    GAME_STATS.P_WINS := 1;
    GAME_STATS.P_SCORE := GAME.DOUBLE_VALUE;
    PR_MESSAGE ( 'You win. You were lucky that time; try again.' )
  End;

  (* GAME Statistics *)

  PR_ROLL_STATS ( GAME );

  PR_STATS ( GAME.WINS , 'Your session statistics: ' );

  UPD_STATS_FILE ( GAME_STATS );

  UPD_FACTORS;

End;		(* END_GAME *)
$PAGE START_MOVE
Procedure START_MOVE ( Var BOARD : BOARD_REC;
		       Var DICE  : DICE_REC;
		       Var WHOSE_ROLL : ROLL_SCL );

Begin

  If WHOSE_ROLL = FIRST_TIME
    Then Begin
      Repeat
	DICE := ROLL_DICE
      Until DICE.DIE1 <> DICE.DIE2;
      GAME.PLAYER_DIE_USED := 0;
      GAME.COMPUTER_DIE_USED := 0;
      GAME.PLAYER_DOUBLES := 0;
      GAME.COMPUTER_DOUBLES := 0;
      GAME.P_MEN_HIT := 0;
      GAME.C_BACK_MEN_NOT_MOVED := 0;
      GAME.P_BACK_MEN_NOT_MOVED := 0;
      GAME.C_MEN_HIT := 0;
      GAME.P_POSN_LOST := 0;
      GAME.C_POSN_LOST := 0;

      If DICE.DIE1 > DICE.DIE2
	Then Begin
	  WHOSE_ROLL := PLAYERS;
	  GAME.PLAYER_DICE_ROLLS := 1;
	  GAME.PLAYER_DIE_COUNT := DICE.DIE1 + DICE.DIE2;
	  GAME.COMPUTER_DICE_ROLLS := 0;
	  GAME.COMPUTER_DIE_COUNT := 0;
	  If GAME.TERSE_SETTING <> VERY_TERSE
	    Then UPDATE_BOARD ( BOARD , ANYWAY );
	  BOARD := INVERT_BOARD ( BOARD )
	End
      Else Begin
	GAME.COMPUTER_DICE_ROLLS := 1;
	GAME.COMPUTER_DIE_COUNT := DICE.DIE1 + DICE.DIE2;
	GAME.PLAYER_DICE_ROLLS := 0;
	GAME.PLAYER_DIE_COUNT := 0;
	WHOSE_ROLL := COMPUTERS
      End
    End
  Else Begin
    DICE := ROLL_DICE;
    If WHOSE_ROLL = PLAYERS
      Then Begin
	WHOSE_ROLL := COMPUTERS;
	GAME.COMPUTER_DICE_ROLLS := GAME.COMPUTER_DICE_ROLLS + 1;
	If DICE.DIE1 = DICE.DIE2
	  Then Begin
	    GAME.COMPUTER_DOUBLES := GAME.COMPUTER_DOUBLES + 1;
	    GAME.COMPUTER_DIE_COUNT := GAME.COMPUTER_DIE_COUNT + 4*DICE.DIE1
	  End
	Else GAME.COMPUTER_DIE_COUNT := GAME.COMPUTER_DIE_COUNT +
		    DICE.DIE1 + DICE.DIE2
      End
    Else Begin
      GAME.PLAYER_DICE_ROLLS := GAME.PLAYER_DICE_ROLLS + 1;
      If DICE.DIE1 = DICE.DIE2
	Then Begin
	  GAME.PLAYER_DOUBLES := GAME.PLAYER_DOUBLES + 1;
	  GAME.PLAYER_DIE_COUNT := GAME.PLAYER_DIE_COUNT + 4*DICE.DIE1
	End
      Else GAME.PLAYER_DIE_COUNT := GAME.PLAYER_DIE_COUNT + DICE.DIE1 + DICE.DIE2;
      WHOSE_ROLL := PLAYERS;
      BOARD := INVERT_BOARD ( BOARD )	(* Only evaluates Comput *)
    End
  End;
End;		(* START_MOVE *)
$PAGE UPDATE_MEN_HIT
Procedure UPDATE_MEN_HIT ( WHOSE_ROLL : ROLL_SCL;
			   MOVES : ^MOVE_VALUE;
			   BOARD : BOARD_REC );

Var I : SMALL_INT;

Begin

  If MOVES^.BOARD.BOARD [ 1 ] <  0
    Then GAME.C_BACK_MEN_NOT_MOVED := GAME.C_BACK_MEN_NOT_MOVED + 1;

  If MOVES^.BOARD.BOARD [ 24 ] > 0
    Then GAME.P_BACK_MEN_NOT_MOVED := GAME.P_BACK_MEN_NOT_MOVED + 1;

  If WHOSE_ROLL = COMPUTERS
    Then Begin
      If MOVES^.BOARD.P_BAR <> BOARD.P_BAR
	Then Begin
	  GAME.P_MEN_HIT := GAME.P_MEN_HIT + MOVES^.BOARD.P_BAR - BOARD.P_BAR;
	  For I := 1 to 24 Do
	    If ( BOARD.BOARD [ I ] = 1 ) And ( MOVES^.BOARD.BOARD [ I ] < 0 )
	      Then GAME.P_POSN_LOST := GAME.P_POSN_LOST + ( 25 - I )
	End
    End
  Else Begin
    If MOVES^.BOARD.C_BAR <> BOARD.C_BAR
      Then Begin
	GAME.C_MEN_HIT := GAME.C_MEN_HIT + MOVES^.BOARD.C_BAR - BOARD.C_BAR;
	For I := 1 to 24 Do
	  If ( BOARD.BOARD [ I ] = -1 ) And ( MOVES^.BOARD.BOARD [ I ] > 0 )
	    Then GAME.C_POSN_LOST := GAME.C_POSN_LOST + I
      End
  End

End;		(* UPDATE_MEN_HIT *)
$PAGE UPDATE_DIE_UTILIZIED
Procedure UPDATE_DIE_UTILIZIED ( WHOSE_ROLL : ROLL_SCL;
				 MOVES      : ^MOVE_VALUE );

Var I : SMALL_INT;		(* Loop count *)

Begin

  For I := 1 to Upperbound ( MOVES^.MOVES^ ) Do
    Begin
      If WHOSE_ROLL = COMPUTERS
	Then Begin
	  If MOVES^.MOVES^[I].FROM < 0
	    Then GAME.COMPUTER_DIE_USED := GAME.COMPUTER_DIE_USED +
		 ( MOVES^.MOVES^[I].TARG )
	  Else GAME.COMPUTER_DIE_USED := GAME.COMPUTER_DIE_USED +
		 ( MOVES^.MOVES^[I].TARG - MOVES^.MOVES^[I].FROM )
	End
      Else Begin
	If MOVES^.MOVES^[I].FROM < 0
	  Then GAME.PLAYER_DIE_USED := GAME.PLAYER_DIE_USED +
		 ( 25 - MOVES^.MOVES^[I].TARG )
	Else GAME.PLAYER_DIE_USED := GAME.PLAYER_DIE_USED +
		( MOVES^.MOVES^[I].FROM - MOVES^.MOVES^[I].TARG )
      End
    End

End;		(* UPDATE_DIE_UTILIZIED *)
$PAGE PLAY_GAME
Procedure PLAY_GAME;

Var WALK  : ^MOVE_VALUE;
    STR   : BG_STRING;
    DICE  : DICE_REC;
    MOVES : ^MOVE_VALUE;
    WHOSE_ROLL : ROLL_SCL;

Begin

  WHOSE_ROLL := FIRST_TIME;
  GAME.DOUBLE_VALUE := 1;
  GAME.DOUBLING_CUBE := CUBE_FREE;

  Repeat

    START_MOVE ( BOARD , DICE , WHOSE_ROLL );

    MOVES := DETERMINE_MOVES ( BOARD , DICE );
    EVALUATE_MOVES ( MOVES );
    SORT_MOVES ( MOVES );

    (* If the PLAYERS's move, then invert the moves and the board *)

    If WHOSE_ROLL = PLAYERS
      Then Begin
	INVERT_MOVES ( MOVES );
	BOARD := INVERT_BOARD ( BOARD )
      End;

    If MOVES <> Nil
      Then Begin
	UPDATE_DIE_UTILIZIED ( WHOSE_ROLL , MOVES );
	UPDATE_MEN_HIT ( WHOSE_ROLL , MOVES , BOARD );
	If ( WHOSE_ROLL = COMPUTERS ) Or GAME.PLAY_YOURSELF
	  Then Begin
	    BOARD := MOVES^.BOARD;	(* First is the best, after sort *)

	    (* If the user requests print computer's analysis *)

	    If GAME.SHOW_EVAL
	      Then PR_LIST_OF_MOVES ( MOVES , True )
	    Else PR_MOVE ( MOVES^.MOVES , 'My move was: ' )
	  End
	Else If MOVES^.NEXT = Nil	(* Player's move is forced *)
	  Then Begin
	    PR_MOVE ( MOVES^.MOVES , 'Your forced move: ' );
	    If MY_QUERY ( 'Your move is forced. Do you want me to play it for you' )
	      Then BOARD := MOVES^.BOARD
	    Else BOARD := ENTER_PLAYER_MOVE ( MOVES , DICE , BOARD )
	  End
	Else BOARD := ENTER_PLAYER_MOVE ( MOVES , DICE , BOARD );

	If ( GAME.DISPLAY_TYPE in [ TEK_4023 , ADDS , ADM_3A ] )
	Or ( GAME.TERSE_SETTING = VERBOSE )
	Or ( ( GAME.TERSE_SETTING = TERSE ) And ( WHOSE_ROLL = COMPUTERS ) )
	  Then UPDATE_BOARD ( BOARD , ANYWAY );
	DISPOSE_MOVES ( MOVES );

	(* See if the computer is in a position to double the player *)

	If ( WHOSE_ROLL = PLAYERS )
	And Not GAME.PLAY_YOURSELF
	And ( GAME.DOUBLING )
	And ( GAME.DOUBLING_CUBE in [ COMPUTER_CONTROLS , CUBE_FREE ] )
	  Then Begin
	    If MAKE_DOUBLE ( BOARD )
	      Then Begin
		Putstring ( STR , 'The doubling cube''s value is: ',
		  GAME.DOUBLE_VALUE:0 , '. I double, do you accept' );
		If MY_QUERY ( STR )
		  Then Begin
		    GAME.DOUBLING_CUBE := PLAYER_CONTROLS;
		    GAME.DOUBLE_VALUE := GAME.DOUBLE_VALUE * 2
		  End
		Else BOARD := CLEAR_BOARD ( COMPUTERS )
	      End
	  End
      End
    Else If Not ( COMPUTER_WINS ( BOARD ) Or PLAYER_WINS ( BOARD ) )
      Then If WHOSE_ROLL = COMPUTERS
	     Then Begin
		PR_MESSAGE ( 'I can''t move; your turn' );
		If GAME.TERSE_SETTING in [ VERBOSE , TERSE ]
		  Then UPDATE_BOARD ( BOARD , ANYWAY)
	     End
	   Else PR_MESSAGE ( 'You can''t move; my turn.' )
  Until COMPUTER_WINS ( BOARD ) Or PLAYER_WINS ( BOARD );

  END_GAME ( BOARD )
End;		(* PLAY_GAME *)
$PAGE MAIN routine
Begin
  X := Random ( Time );

  (* This is where we start. Welcome the user and determine the type
     of game that is to be played. *)

  Open ( tty );
  Rewrite ( ttyoutput );

  SET_LINE_CHARACTERISTICS;		(* For visual effects *)

  RD_FACTORS;		(* Read the game evaluation factors *)

  Writeln ( tty , '***** Backgammon 1.1  -- ', Compdate,' *****' );
  Writeln ( tty );

  GAME := ( NORMAL , INIT_WIN_REC , True , False , False , False ,
	0,0,0,0,0,0,0,0,1 , CUBE_FREE , True , TERSE , REGULAR ,0,0,0,0,0,0 );


  Loop
    Begin
      Mask ( Attention );
      BOARD := INITIAL_BOARD;

      (* Prompt user until he enters START *)

      START_GAME ( BOARD , (0,0) );

      If GAME.GAME_TYP = EVALUATING_POSN
	Then PLAY_BOARD
      Else PLAY_GAME

      Exception
	ATTENTION : BEGIN
			WRITELN ( TTY , '__' );
			STOP
		    END;
    End;		(* Begin *)
  End;		(* Loop *)

  RESTORE_LINE_CHARACTERISTICS		(* So things work the way they were *)

End.		(* BACKGAMMON 1.0 MAIN Routine *)
  }aà