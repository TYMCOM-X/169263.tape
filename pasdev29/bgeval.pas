$Title EVALUATION of BACKGAMMON Moves
Module BGEVAL;

$Include BG.INC

(* Declare a type record to contain the general game information such
   as game type, ie.e running, blocking, back... *)

Type GAME_TYP = ( START_GAME , NORMAL_GAME , BLOCKING_GAME , BACK_GAME ,
		  RUNNING_GAME );

     DICE_MATRIX = Array [ 1..6 , 1..6 ] of Boolean;


Const INIT_DICE_MATRIX : DICE_MATRIX := (
	False , False , False , False , False , False ,
	False , False , False , False , False , False ,
	False , False , False , False , False , False ,
	False , False , False , False , False , False ,
	False , False , False , False , False , False ,
	False , False , False , False , False , False );
$PAGE EXTERNAL PROCEDURES and VARS
(* External Procedures and VARS *)

External Var GAME : GAME_REC;

External Function INVERT_BOARD ( BOARD : BOARD_REC ) : BOARD_REC;

External Procedure PR_MOVE (^move_list; bg_string);

External Var FACTOR : FACTOR_REC;
$PAGE SET_DIGITS
Procedure SET_DIGITS ( Var DICE_ROLLS : DICE_MATRIX;
			   IDX : SMALL_INT );

Var I : SMALL_INT;

Begin

  For I := 1 to 6 Do
    Begin
      DICE_ROLLS [ I , IDX ] := True;
      DICE_ROLLS [ IDX , I ] := True
   End

End;		(* SET_DIGITS *)
$PAGE GEN_HITS
Procedure GEN_HITS ( Var DICE_ROLLS : DICE_MATRIX;
				 BOARD      : BOARD_REC;
				 BLOT       : SMALL_INT;
				 DIST       : SMALL_INT );

Begin

  If DIST in [ 1..6 ]		(* GEN all 1 die hits *)
    Then SET_DIGITS ( DICE_ROLLS , DIST );

  Case DIST Of
    24: If ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 12 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 18 ] > 0 )
	  Then DICE_ROLLS [ 6 , 6 ] := True;

    20: If ( BOARD.BOARD [ BLOT + 5 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 10 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 15 ] > 0 )
	  Then DICE_ROLLS [ 5 , 5 ] := True;

    18: If ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 12 ] > 0 )
	  Then DICE_ROLLS [ 6 , 6 ] := True;

    16: If ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 8 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 12 ] > 0 )
	  Then DICE_ROLLS [ 4 , 4 ] := True;

    15: If ( BOARD.BOARD [ BLOT + 5 ] > 0 )
	And ( BOARD.BOARD [ BLOT + 10 ] > 0 )
	 Then DICE_ROLLS [ 5 , 5 ] := True;

    12: Begin
	  If BOARD.BOARD [ BLOT + 6 ] > 0
	    Then DICE_ROLLS [ 6 , 6 ] := True;
	  If ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 8 ] > 0 )
	    Then DICE_ROLLS [ 4 , 4 ] := True;
	  If ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 9 ] > 0 )
	    Then DICE_ROLLS [ 3 , 3 ] := True
	End;

    11: If ( BOARD.BOARD [ BLOT + 5 ] > 0 )
	Or ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	  Then Begin
	    DICE_ROLLS [ 5 , 6 ] := True;
	    DICE_ROLLS [ 6 , 5 ] := True
	  End;

    10: Begin
	  If BOARD.BOARD [ BLOT + 5 ] > 0
	    Then DICE_ROLLS [ 5 , 5 ] := True;
	  If ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 4 , 6 ] := True;
	      DICE_ROLLS [ 6 , 4 ] := True
	    End
	End;

    9 : Begin
	  If ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	    Then DICE_ROLLS [ 3 , 3 ] := True;
	  If ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 5 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 4 , 5 ] := True;
	      DICE_ROLLS [ 5 , 4 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 6 , 3 ] := True;
	      DICE_ROLLS [ 3 , 6 ] := True
	    End
	End;

    8 : Begin
	  If BOARD.BOARD [ BLOT + 4 ] > 0
	    Then Begin
	      DICE_ROLLS [ 4 , 4 ] := True;
	      If ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	      And ( BOARD.BOARD [ BLOT + 6 ] > 0 )
		Then DICE_ROLLS [ 2 , 2 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 5 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 3 , 5 ] := True;
	      DICE_ROLLS [ 5 , 3 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 2 , 6 ] := True;
	      DICE_ROLLS [ 6 , 2 ] := True
	    End
	End;

    7 : Begin
	  If ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 3 , 4 ] := True;
	      DICE_ROLLS [ 4 , 3 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 5 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 2 , 5 ] := True;
	      DICE_ROLLS [ 5 , 2 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 1 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 6 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 6 , 1 ] := True;
	      DICE_ROLLS [ 1 , 6 ] := True;
	    End
	End;

    6 : Begin
	  If BOARD.BOARD [ BLOT + 3 ] > 0
	    Then DICE_ROLLS [ 3 , 3 ] := True;
	  If ( BOARD.BOARD [ BLOT + 1 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 5 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 1 , 5 ] := True;
	      DICE_ROLLS [ 5 , 1 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 2 , 4 ] := True;
	      DICE_ROLLS [ 4 , 2 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	    Then DICE_ROLLS [ 2 , 2 ] := True
	End;

    5 : Begin
	  If ( BOARD.BOARD [ BLOT + 1 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 4 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 1 , 4 ] := True;
	      DICE_ROLLS [ 4 , 1 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 3 , 2 ] := True;
	      DICE_ROLLS [ 2 , 3 ] := True
	    End
	End;

    4 : Begin
	  If ( BOARD.BOARD [ BLOT + 1 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	    Then DICE_ROLLS [ 1 , 1 ] := True;
	  If ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	    Then DICE_ROLLS [ 2 , 2 ] := true;
	  If ( BOARD.BOARD [ BLOT + 1 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 3 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 1 , 3 ] := True;
	      DICE_ROLLS [ 3 , 1 ] := True
	    End
	End;

    3 : Begin
	  If ( BOARD.BOARD [ BLOT + 1 ] > 0 )
	  Or ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	    Then Begin
	      DICE_ROLLS [ 1 , 2 ] := True;
	      DICE_ROLLS [ 2 , 1 ] := True
	    End;
	  If ( BOARD.BOARD [ BLOT + 1 ] > 0 )
	  And ( BOARD.BOARD [ BLOT + 2 ] > 0 )
	    Then DICE_ROLLS [ 1 , 1 ] := True
	End;

    2 : If BOARD.BOARD [ BLOT + 1 ] > 0
	  Then DICE_ROLLS [ 1 , 1 ] := True;

    Others :

  End			(* Of the Case statement *)
End;		(* GEN_HITS *)
$PAGE ROLLS_THAT_HIT
Function ROLLS_THAT_HIT ( BOARD : BOARD_REC;
			  BLOT  : SMALL_INT ) : Integer;

Var DICE_ROLLS : DICE_MATRIX;		(* Storing possible rolls *)
    I , J : SMALL_INT;
    COPY_BOARD : BOARD_REC;

Begin

  DICE_ROLLS := INIT_DICE_MATRIX;
  COPY_BOARD := BOARD;
  COPY_BOARD.BOARD [ 25 ] := Abs ( BOARD.P_BAR );

  (* Check to see if the point is a blot *)

  For I := BLOT+1 to 25 Do
    If COPY_BOARD.BOARD [ I ] > 0
      Then GEN_HITS ( DICE_ROLLS , COPY_BOARD , BLOT , I - BLOT );

  (* Now count up the total of rolls that hit *)

  ROLLS_THAT_HIT := 0;

  For I := 1 to 6 Do
    For J := 1 to 6 Do
      If DICE_ROLLS [ I , J ]
	Then ROLLS_THAT_HIT := ROLLS_THAT_HIT + 1;

End;		(* ROLLS_THAT_HIT *)
$PAGE WILLINGNESS_TO_HIT
Function WILLINGNESS_TO_HIT ( BOARD : BOARD_REC;
			      BLOT  : SMALL_INT ) : Real;

Var I : SMALL_INT;			(* For loop index *)
    COMP_HOME_PTS : SMALL_INT;
    COMP_HOME_BLOTS : SMALL_INT;
    PLAYER_POINTS : SMALL_INT;		(* Retain count of consec points *)
    PLAYER_HOME_POINTS   : SMALL_INT;		(* Player home points held *)
    MAX_PRIME_PTS : SMALL_INT;		(* Retain max count *)

Begin
  WILLINGNESS_TO_HIT := 0;
  MAX_PRIME_PTS := 0;
  PLAYER_HOME_POINTS := 0;
  PLAYER_POINTS := 0;
  COMP_HOME_PTS := 0;
  COMP_HOME_BLOTS := 0;

  For I := 1 to 24 Do
    If BOARD.BOARD [ I ] > 1
      Then Begin
	PLAYER_POINTS := PLAYER_POINTS + 1;
	If I <= 6
	  Then PLAYER_HOME_POINTS := PLAYER_HOME_POINTS + 1
      End
    Else Begin
      If ( BOARD.BOARD [ I ] = -1 ) And ( I >= 18 )
	Then COMP_HOME_BLOTS := COMP_HOME_BLOTS + 1;
      If ( BOARD.BOARD [ I ] < -1 ) And ( I >= 19 )
	Then COMP_HOME_PTS := COMP_HOME_PTS + 1;
      If PLAYER_POINTS > MAX_PRIME_PTS
	Then MAX_PRIME_PTS := PLAYER_POINTS;
      PLAYER_POINTS := 0
    End;

  WILLINGNESS_TO_HIT := - ( ( 25 - BLOT ) ** FACTOR.BLOT_POSN )
	+ ( COMP_HOME_PTS * FACTOR.HOME_BLOT )
	- ( COMP_HOME_BLOTS ** FACTOR.HOME_BLOT )
	- ( FACTOR.PRIME ** MAX_PRIME_PTS  )
	- ( FACTOR.HOME_POINTS_HELD ** PLAYER_HOME_POINTS )
End;		(* WILLINGNESS_TO_HIT *)
$PAGE EVAL_BLOTS
Function EVAL_BLOTS ( BOARD : BOARD_REC;
			COMPUTERS_PIP : Integer;
			PLAYERS_PIP : Integer ) : Real;

Var I : SMALL_INT;

Begin

  (* Check for BLOTS. For each blot evaluate probability of beingg hit.
     and multiply that by the OPPONENTS willingness to HIT *)

  EVAL_BLOTS := 0;

  (* Don't start at 1 because we don't really care if we get hit there *)
  For I := 4 to 24 Do
    Begin

      If BOARD.BOARD [ I ] = -1		(* A BLOT *)
	Then EVAL_BLOTS := EVAL_BLOTS + 1 +
	 ROLLS_THAT_HIT ( BOARD , I ) *
		WILLINGNESS_TO_HIT ( BOARD , I ) *
		( ( PLAYERS_PIP / COMPUTERS_PIP ) ** 2 ) / 2;

    End;

End;		(* EVAL_BLOTS *)
$PAGE POINTS_MADE
Function POINTS_MADE ( BOARD : BOARD_REC ) : Integer;

Begin

  POINTS_MADE := 0;

  If BOARD.BOARD [ 5 ] <= -2
    Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_5;

  If BOARD.BOARD [ 6 ] <= -2
    Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_6;

  If BOARD.BOARD [ 7 ] <= -2
    Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_7;

  If BOARD.BOARD [ 17 ] <= -2
    Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_17;

  If BOARD.BOARD [ 18 ] <= -2
    Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_18;

  If BOARD.BOARD [ 19 ] <= -2
    Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_19;

  If BOARD.BOARD [ 20 ] <= -2
    Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_20;

  If BOARD.BOARD [ 21 ] <= -2
    Then If BOARD.BOARD [ 20 ] <= -2
      Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_19_20
    Else POINTS_MADE := POINTS_MADE + FACTOR.POINT_21;

  If BOARD.BOARD [ 22 ] <= -2
    Then If BOARD.BOARD [ 20 ] <= -2
      Then If BOARD.BOARD [ 21 ] <= -2
	Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_23_22_21
      Else POINTS_MADE := POINTS_MADE + FACTOR.POINT_22_20
    Else If BOARD.BOARD [ 21 ] <= -2
      Then POINTS_MADE := POINTS_MADE + FACTOR.POINT_21_22
    Else POINTS_MADE := POINTS_MADE + FACTOR.POINT_22;

End;		(* POINTS_MADE *)
$PAGE BUNCHED_POINTS
Function BUNCHED_POINTS ( BOARD : BOARD_REC ) : Integer;

Var I : SMALL_INT;

Begin

  BUNCHED_POINTS := 0;

  For I := 1 to 24 Do
    If BOARD.BOARD [ I ] < -2
      Then BUNCHED_POINTS := BUNCHED_POINTS - (BOARD.BOARD[I]+2) * FACTOR.BUNCH;

End;		(* BUNCHED_POINTS *)
$PAGE BAD_POINTS
Function BAD_POINTS ( BOARD : BOARD_REC ) : Integer;

Var I : SMALL_INT;

Begin

  BAD_POINTS := 0;

  For I := 1 to 6 Do		(* Player's home board *)
    If BOARD.BOARD [ I ] < 0
      Then BAD_POINTS := BAD_POINTS + FACTOR.C_IN_P_HOME * BOARD.BOARD [ I ];

  For I := 7 to 12 Do		(* Plauers outer board *)
    If BOARD.BOARD [ I ] < 0
      Then BAD_POINTS := BAD_POINTS + FACTOR.C_IN_P_OUTBOARD * BOARD.BOARD [ I ];

  For I := 13 to 18 Do		(* Computer's outer board *)
    If BOARD.BOARD [ I ] < 0
      Then BAD_POINTS := BAD_POINTS + FACTOR.C_13_TO_18 * BOARD.BOARD [ I ];

  For I := 19 to 24 Do		(* Blots in computers home *)
    If BOARD.BOARD [ I ] = -1
      Then BAD_POINTS := BAD_POINTS + FACTOR.BLOT_IN_HOME_BOARD;

  For I := 22 to 24 Do		(* Computer's low points *)
    If ( BOARD.BOARD [ I ] < 0 )
      Then BAD_POINTS := BAD_POINTS + FACTOR.C_24_23_22_POINTS * BOARD.BOARD [ I ];

End;		(* BAD_POINTS *)
$PAGE ALL_HOME
Function ALL_HOME ( BOARD : BOARD_REC ) : Integer;

Var I : SMALL_INT;
    BOOL : Boolean;

Begin

  BOOL := BOARD.C_BAR = 0;

  For I := 1 to 18 Do
    Begin
      BOOL := BOOL And ( BOARD.BOARD [ I ] >= 0 );
  Exit If Not BOOL;
    End;

  If BOOL
    Then ALL_HOME := FACTOR.ALL_HOME
  Else ALL_HOME := 0;

End;
$PAGE DETERMINE_PIP
Procedure DETERMINE_PIP ( BOARD : BOARD_REC;
			Var PLAYERS_PIP : Integer;
			Var COMPUTERS_PIP : Integer );

Var I : SMALL_INT;			(* Loop index *)

Begin

  (* 25 points PIP for each man on the BAR *)

  COMPUTERS_PIP := BOARD.C_BAR * 25;
  PLAYERS_PIP   := BOARD.P_BAR * 25;

  (* For each point on the board determine pip *)

  For I := 1 to 24 Do
    Begin
      If BOARD.BOARD [ I ] < 0		(* Computer's pieces *)
	Then Begin
	  COMPUTERS_PIP := COMPUTERS_PIP - ( (25-I) * BOARD.BOARD [ I ] )
	End
      Else Begin
	PLAYERS_PIP := PLAYERS_PIP + ( I * BOARD.BOARD [ I ] )
      End
    End

End;		(* DETERMINE_PIP *)
$PAGE EVAL_RUNNING
Function EVAL_RUNNING ( BOARD : BOARD_REC;
			PLAYERS_PIP : Integer;
			COMPUTERS_PIP : Integer ) : Real;

Var I : SMALL_INT;		(* For loop variable *)
    J : SMALL_INT;
    C_COUNT : Integer;
    P_COUNT : Integer;

Begin

  C_COUNT := 0;
  P_COUNT := 0;

  For I := 1 to 24 Do
    If BOARD.BOARD [ I ] < 0
      Then Begin
	C_COUNT := C_COUNT - BOARD.BOARD [ I ] * BOARD.P_BAR;
	P_COUNT := P_COUNT - BOARD.BOARD [ I ] * BOARD.P_BAR;
	For J := I to 24 Do
	  If BOARD.BOARD [ J ] > 0
	    Then C_COUNT := C_COUNT - BOARD.BOARD [ J ] * BOARD.BOARD [ I ]
      End
    Else If BOARD.BOARD [ I ] > 0
      Then Begin
	P_COUNT := P_COUNT - BOARD.BOARD [ I ] * BOARD.C_BAR;
	C_COUNT := C_COUNT - BOARD.BOARD [ I ] * BOARD.C_BAR;
	For J := I Downto 1 Do
	  If BOARD.BOARD [ J ] < 0
	    Then P_COUNT := P_COUNT - BOARD.BOARD [ J ] * BOARD.BOARD [ I ]
      End;

  (* Max value at start is 162, we will use 100 cause it is middle *)

  EVAL_RUNNING := ( P_COUNT + C_COUNT ) / ( COMPUTERS_PIP + PLAYERS_PIP )/ 2;

End;		(* EVAL_RUNNING *)
$PAGE EVALUATE_BOARD
Function EVALUATE_BOARD ( BOARD : BOARD_REC ) : Real;

Var PLAYERS_PIP : Integer;
    COMPUTERS_PIP : Integer;		(* Total pip *)
    RUNNING_FACTOR : Real;

Begin

  If ( BOARD.BOARD [ 25 ] = -15 )		(* Computer wins *)
    Then EVALUATE_BOARD := Maximum ( Real )
  Else Begin
    DETERMINE_PIP ( BOARD , PLAYERS_PIP , COMPUTERS_PIP );

    RUNNING_FACTOR := EVAL_RUNNING ( BOARD , PLAYERS_PIP , COMPUTERS_PIP );

    EVALUATE_BOARD := 
	- FACTOR.BEAROFF * BOARD.BOARD [ 25 ]
	+ FACTOR.P_BAR * ( BOARD.P_BAR ** 2 )
	- FACTOR.C_BAR * ( BOARD.C_BAR ** 2 )
	+ PLAYERS_PIP
	- COMPUTERS_PIP
	+ RUNNING_FACTOR * FACTOR.EVAL * EVAL_BLOTS ( BOARD , COMPUTERS_PIP , PLAYERS_PIP )
	+ RUNNING_FACTOR * POINTS_MADE ( BOARD )
	- RUNNING_FACTOR * BUNCHED_POINTS ( BOARD )
	+ BAD_POINTS ( BOARD )
	+ ALL_HOME ( BOARD )
  End

End;		(* EVALUATE_BOARD *)
$PAGE EVALUATE_MOVES
Public Procedure EVALUATE_MOVES ( MOVES : ^MOVE_VALUE );

Var WALK : ^MOVE_VALUE;		(* For walking the list of moves *)

Begin

  (* Evaluate the moves 1 at a time. The caller will call a routine to
     sort the moves based upon value. *)

  WALK := MOVES;

  While WALK <> Nil Do
    Begin
      WALK^.VALUE := EVALUATE_BOARD ( WALK^.BOARD );

      (* Get the next move *)

      WALK := WALK^.NEXT
    End

End;		(* EVALUATE_MOVES *)
$PAGE COMPARE_POSN
Function COMPARE_POSN ( BOARD : BOARD_REC ) : Real;

Var P_SCORE , C_SCORE : Real;

Begin

  P_SCORE := EVALUATE_BOARD ( INVERT_BOARD ( BOARD ) );
  C_SCORE := EVALUATE_BOARD ( BOARD );

  If (C_SCORE = 0) And (P_SCORE = 0)
    Then COMPARE_POSN := 0
  Else Begin
    COMPARE_POSN := (C_SCORE - P_SCORE) / (Abs(C_SCORE) + Abs (P_SCORE))/2
  End;

End;		(* COMPARE_POSN *)
$PAGE ACC_DOUBLE
Public Function ACC_DOUBLE ( BOARD : BOARD_REC ) : Boolean;

Var COMPUTERS_COUNT : Integer;
    PLAYERS_COUNT : Integer;
    POSN_SCORE : Real;

Begin

  DETERMINE_PIP ( BOARD , PLAYERS_COUNT , COMPUTERS_COUNT );
  POSN_SCORE := COMPARE_POSN ( BOARD );

  (* Accept the double as long as the player is not ahead by more than
     17.5%. *)

  If PLAYERS_COUNT = 0		(* Don't accept if the player won *)
    Then ACC_DOUBLE := False
  Else Begin

    ACC_DOUBLE :=  (
	( ( COMPUTERS_COUNT - PLAYERS_COUNT ) / PLAYERS_COUNT ) < .175 )

  End

End;		(* ACC_DOUBLE *)
$PAGE MAKE_DOUBLE
Public Function MAKE_DOUBLE ( BOARD : BOARD_REC ) : Boolean;

Var PLAYERS_COUNT : Integer;
    COMPUTERS_COUNT : Integer;
    POSN_SCORE : Real;

Begin

  DETERMINE_PIP ( BOARD , PLAYERS_COUNT , COMPUTERS_COUNT );
  POSN_SCORE := COMPARE_POSN ( BOARD );

  If ( COMPUTERS_COUNT = 0 ) Or ( GAME.DOUBLE_VALUE >= 16 )
    Then MAKE_DOUBLE := False		(* He won't accept, cause I win *)
  Else If GAME.DOUBLE_VALUE = 1		(* The first double *)
    Then Begin

      If POSN_SCORE < 0
	Then Begin		(* Only double if ahead by 30% *)
	  MAKE_DOUBLE := (PLAYERS_COUNT-COMPUTERS_COUNT)/COMPUTERS_COUNT >= .3
	End
      Else Begin
	(* Now double if ahead in position and by count of 7% *)

	MAKE_DOUBLE := ( POSN_SCORE >= 0 ) And
		(( PLAYERS_COUNT-COMPUTERS_COUNT) / COMPUTERS_COUNT >= .07 )
      End
    End
  Else If POSN_SCORE > 0
    Then Begin
      (* Double if ahead in position and by count of 10% *)

      MAKE_DOUBLE := ( POSN_SCORE >= 0 ) And
		(( PLAYERS_COUNT - COMPUTERS_COUNT) / COMPUTERS_COUNT >= .1 )
    End
  Else MAKE_DOUBLE := False

End.		(* MAKE_DOUBLE *)
    