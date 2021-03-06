(* SCORE4.TYP - specification of types used throughout the score4 program. *)

type
  peg_range = 0..3;				(* coordinate range along any axis *)
  ball_color_type = (no_ball, black_ball, white_ball);
  ball_coord_type =
    packed record
      x_coord,
      y_coord,
      z_coord: peg_range
    end;
  game_state_type = (white_won, black_won, no_more_moves, game_not_over);

const
  reverse_color: array [ball_color_type] of ball_color_type = (no_ball, white_ball, black_ball);
  number_of_positions = 64;

type
  board_position_type = 0..number_of_positions-1;
  board_type = array [black_ball..white_ball] of set of board_position_type;
    