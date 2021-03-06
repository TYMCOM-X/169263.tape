Type SMALL_INT = -32..32;
     TERMINAL_TYPE = ( REGULAR , ADM_3A , TEK_4023 , ADDS );
     BG_STRING = String [ 80 ];

     BOARD_REC = Record
		  BOARD : Array [ 0..25 ] of SMALL_INT;
		  P_BAR : SMALL_INT;
		  C_BAR : SMALL_INT
		End;


(* Moves are conected together by a pointer chain. The moves themselves
   are represented by an array of 2 or 4 records containing information
   of where the players are moved from and to. *)

     MOVE_REC  = Record
		   FROM : SMALL_INT;
		   TARG : SMALL_INT
		 End;

     MOVE_LIST = Array [ 1..* ] of MOVE_REC;

     MOVE_VALUE = Record
		    VALUE  : Real;
		    BOARD  : BOARD_REC;
		    MOVES  : ^MOVE_LIST;
		    NEXT   : ^MOVE_VALUE
		  End;

     DICE_REC = Record
		  DIE1 : SMALL_INT;
		  DIE2 : SMALL_INT
		End;

(* A game record consists of information which denotes what type of game
   is being played. It conatins information which indicates the session
   is simply one where board positions are evaluated. *)

    WIN_REC = Record
		GAMES : Integer;
		C_SCORE : Integer;
		P_SCORE : Integer;
		C_WINS : Integer;
		P_WINS : Integer;
		C_GAMMONS : Integer;
		C_BGAMMONS : Integer;
		P_GAMMONS : Integer;
		P_BGAMMONS : Integer
	      End;

    GAME_REC = Record
		 GAME_TYP : (NORMAL , EVALUATING_POSN);
		 WINS : WIN_REC;
		 AUTO_ROLL: Boolean;	(* Computer rolling the dice *)
		 PLAY_YOURSELF: Boolean;
		 SHOW_EVAL: Boolean;	(* Show the computers move eval *)
		 PLAYERS_MOVES: Boolean;(* Show all players possible moves *)
		 PLAYER_DICE_ROLLS : Integer;
		 PLAYER_DOUBLES : Integer;
		 PLAYER_DIE_COUNT : Integer;
		 COMPUTER_DICE_ROLLS : Integer;
		 COMPUTER_DOUBLES : Integer;
		 COMPUTER_DIE_COUNT : Integer;
		 COMPUTER_DIE_USED : Integer;
		 PLAYER_DIE_USED   : Integer;
		 DOUBLE_VALUE : Integer;
		 DOUBLING_CUBE:(CUBE_FREE,COMPUTER_CONTROLS,PLAYER_CONTROLS);
		 DOUBLING : Boolean;	(* Use the doubling cube *)
		 TERSE_SETTING : (VERBOSE, TERSE, VERY_TERSE);
		 DISPLAY_TYPE : TERMINAL_TYPE;
		 C_MEN_HIT : Integer;
		 C_POSN_LOST : Integer;		(* HIT * 25-Point *)
		 P_MEN_HIT : Integer;
		 P_POSN_LOST : Integer;
		 C_BACK_MEN_NOT_MOVED : Integer;
		 P_BACK_MEN_NOT_MOVED : Integer
	       End;

    ROLL_SCL = ( COMPUTERS , PLAYERS , FIRST_TIME );

    UPDATE_SCL = ( ANYWAY , ONLY_IF_CRT );

(* Define the command scalars, and the constant which will
   be used to look them up in. *)

Const CMD_LEN := 12;		(* Maximum length of any command *)

Type CMD_REC = Record
		 TXT    : String [ CMD_LEN ];	(* Text of command *)
		 ABBREV : 1..CMD_LEN		(* Minimum abbreviation *)
	       End;

     CMD_SCL = ( ERROR_CMD , ADDS_CMD , ADM_3A_CMD , BAR_CMD , BEAROFF_CMD ,
		 BOARD_CMD , COMPUTER_CMD , CUBE_CMD ,
		 DICE_CMD , DISPLAY_CMD , DOUBLE_CMD , DOUBLING_CMD ,
		 EVALUATE_CMD , HELP_CMD , INSTRUCTIONS_CMD ,
		 POSSIBLE_CMD , QUIT_CMD , REGULAR_CMD ,
		 RESET_CMD , ROLL_CMD , SEED_CMD ,
		 START_CMD , STATE_CMD , STATS_CMD , STOP_CMD ,
		 TEK_4023_CMD , TERSE_CMD , VERBOSE_CMD );

     CMD_ARY = Array [ CMD_SCL ] of CMD_REC;


(* Define the types to define token records. *)

Type TOKEN_KIND = ( ERROR_TOK , EOS_TOK , HELP_TOK , MOVE_TOK , COMMAND_TOK );

     TOKEN_REC = Record
		   IDX : Integer;		(* For the error cursor *)
		   Case KIND : TOKEN_KIND Of
		     HELP_TOK ,
		     COMMAND_TOK : ( CMD  : CMD_SCL );
		     MOVE_TOK    : ( MOVE : MOVE_REC )
		 End;
$PAGE Factor type

Type FACTOR_REC = Record
	HOME_BLOT	: Integer;	(* Blot in home board penalty *)
	BLOT_IN_HOME_BOARD:Integer;
	P_BAR		: Integer;	(* Player bar *)
	C_BAR		: Integer;	(* Computer on bar *)
	BEAROFF		: Integer;	(* Points for bearing off *)
	BLOT		: Integer;	(* Penalty per blot *)
	HOME_POINTS_HELD: Integer;	(* Home points held *)
	ALL_HOME	: Integer;	(* Everyone in 1 to 6 *)
	C_24_23_22_POINTS:Integer;	(* Computer has points24,23,22 *)
	C_IN_P_OUTBOARD	: Integer;	(* Computer in players outer board *)
	C_IN_P_HOME	: Integer;	(* Computer in players home board *)
	C_13_TO_18	: Integer;	(* Computer owns 13..18 *)
	BUNCH		: Integer;	(* Penalty for bunching up men *)
	PRIME		: Integer;	(* Points for a prime built *)
	POINT_5		: Integer;	(* Made point 5 *)
	POINT_6		: Integer;	(* Made point 6 *)
	POINT_7		: Integer;	(* Made point 7 *)
	POINT_17	: Integer;	(* Made point 17 *)
	POINT_18	: Integer;	(* Made point 18 *)
	POINT_19	: Integer;	(* Made point 19 *)
	POINT_20	: Integer;	(* Made point 20 *)
	POINT_21	: Integer;	(* Made point 21 *)
	POINT_22	: Integer;	(* Made point 22 *)
	POINT_19_20	: Integer;	(* Points 19 and 20 made *)
	POINT_21_22	: Integer;	(* Points 21 and 22 made *)
	POINT_22_20	: Integer;	(* Points 22 and 20 made *)
	POINT_23_22_21	: Integer;	(* Points 23 22 and 21 made *)
	BLOT_POSN		: Real;	(* Factors for blot positions *)
	EVAL		: Real		(* Evaluation factor *)
     End;
   