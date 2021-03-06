(* X4MOVE.INC - specification of interface to MOVE module.  SCORE4.TYP and X4MOVE.TYP must
   be included prior to this file.  *)

$IFNOT ADAPT

(* EMPTY BOARD: value of board prior to first move:  *)

  external function empty_board: board_type;


(* UPDATE BOARD: change specified position of given board to given color, checking
   for validity:  *)

  external procedure update_board (var board_type; ball_color_type; ball_coord_type;
				   var boolean);

(* INIT MOVE MODULE: precalculate some things the static evaluation requires: *)

  external procedure init_move_module;


(* BEST MOVE: find the coordinates of the best move: *)

  external function best_move (    board_type;
				   ball_color_type;
				   packed_coef_array;
				   1..number_of_positions;
				   1..number_of_positions;
			       var integer):	 ball_coord_type;


(* STATE OF GAME: determine if game is done, who won, and where: *)

  external procedure state_of_game (board_type; var game_state_type; var ball_coord_type; var ball_coord_type);


$ENDIF
$IF ADAPT

(* PREP STRUCTURE EVALUATION: initialization entry point for the ADAPT program -
   provides parameters necessary for running off machine vs machine games, and
   calls INIT MOVE MODULE: *)

   external procedure prep_structure_evaluation (fitness_parameter_record);


(* EVALUATE STRUCTURE: runs off a game pitting the callers strategy vector against
   the fixed opponent strategy provided by PREP STRUCTURE EVALUATION: *)

   external function evaluate_structure (packed_coef_array): structure_fitness_type;
$ENDIF
 