$TITLE x4move
$WIDTH (107)
  
module x4move;

$INCLUDE score4.typ
$INCLUDE x4move.typ

external function card (set of board_position_type): integer;

const
  won = 1000000000;
  ways_to_win = 76;
  infinity = won + number_of_positions;
  all_positions = [minimum (board_position_type)..maximum (board_position_type)];

type
  winning_line_range = 1..ways_to_win;
  board_value_type = -infinity..infinity;
  move_list_type = array [1..16] of 
			record
			  pos: board_position_type;
			  value: board_value_type;
			  critical: boolean
			end;
var
  central_cube,
  corners,
  middle_of_edges,
  middle_of_surfaces: set of board_position_type;
  level: array [peg_range] of set of board_position_type;
$PAGE winning lines

var
  winning_lines: array [winning_line_range] of
    record
      first_ball,
      last_ball: ball_coord_type;
      positions: array [peg_range] of board_position_type;  (* filled in by init routine *)
      position_set: set of board_position_type	            (*   "    "  "   "     "     *)
    end :=

  (  ((0,0,0), (0,0,3), (0,0,0,0), []),		(* vertical lines *)
     ((0,1,0), (0,1,3), (0,0,0,0), []),
     ((0,2,0), (0,2,3), (0,0,0,0), []),
     ((0,3,0), (0,3,3), (0,0,0,0), []),
     ((1,0,0), (1,0,3), (0,0,0,0), []),
     ((1,1,0), (1,1,3), (0,0,0,0), []),
     ((1,2,0), (1,2,3), (0,0,0,0), []),
     ((1,3,0), (1,3,3), (0,0,0,0), []),
     ((2,0,0), (2,0,3), (0,0,0,0), []),
     ((2,1,0), (2,1,3), (0,0,0,0), []),
     ((2,2,0), (2,2,3), (0,0,0,0), []),
     ((2,3,0), (2,3,3), (0,0,0,0), []),
     ((3,0,0), (3,0,3), (0,0,0,0), []),
     ((3,1,0), (3,1,3), (0,0,0,0), []),
     ((3,2,0), (3,2,3), (0,0,0,0), []),
     ((3,3,0), (3,3,3), (0,0,0,0), []),

     ((0,0,0), (3,0,0), (0,0,0,0), []),		(* horizontal left to right *)
     ((0,0,1), (3,0,1), (0,0,0,0), []),
     ((0,0,2), (3,0,2), (0,0,0,0), []),
     ((0,0,3), (3,0,3), (0,0,0,0), []),
     ((0,1,0), (3,1,0), (0,0,0,0), []),
     ((0,1,1), (3,1,1), (0,0,0,0), []),
     ((0,1,2), (3,1,2), (0,0,0,0), []),
     ((0,1,3), (3,1,3), (0,0,0,0), []),
     ((0,2,0), (3,2,0), (0,0,0,0), []),
     ((0,2,1), (3,2,1), (0,0,0,0), []),
     ((0,2,2), (3,2,2), (0,0,0,0), []),
     ((0,2,3), (3,2,3), (0,0,0,0), []),
     ((0,3,0), (3,3,0), (0,0,0,0), []),
     ((0,3,1), (3,3,1), (0,0,0,0), []),
     ((0,3,2), (3,3,2), (0,0,0,0), []),
     ((0,3,3), (3,3,3), (0,0,0,0), []),

     ((0,0,0), (0,3,0), (0,0,0,0), []),		(* horizontal front to back *)
     ((0,0,1), (0,3,1), (0,0,0,0), []),
     ((0,0,2), (0,3,2), (0,0,0,0), []),
     ((0,0,3), (0,3,3), (0,0,0,0), []),
     ((1,0,0), (1,3,0), (0,0,0,0), []),
     ((1,0,1), (1,3,1), (0,0,0,0), []),
     ((1,0,2), (1,3,2), (0,0,0,0), []),
     ((1,0,3), (1,3,3), (0,0,0,0), []),
     ((2,0,0), (2,3,0), (0,0,0,0), []),
     ((2,0,1), (2,3,1), (0,0,0,0), []),
     ((2,0,2), (2,3,2), (0,0,0,0), []),
     ((2,0,3), (2,3,3), (0,0,0,0), []),
     ((3,0,0), (3,3,0), (0,0,0,0), []),
     ((3,0,1), (3,3,1), (0,0,0,0), []),
     ((3,0,2), (3,3,2), (0,0,0,0), []),
     ((3,0,3), (3,3,3), (0,0,0,0), []),

     ((0,0,0), (0,3,3), (0,0,0,0), []),		(* diagonals from front to back *)
     ((0,0,3), (0,3,0), (0,0,0,0), []),
     ((1,0,0), (1,3,3), (0,0,0,0), []),
     ((1,0,3), (1,3,0), (0,0,0,0), []),
     ((2,0,0), (2,3,3), (0,0,0,0), []),
     ((2,0,3), (2,3,0), (0,0,0,0), []),
     ((3,0,0), (3,3,3), (0,0,0,0), []),
     ((3,0,3), (3,3,0), (0,0,0,0), []),

     ((0,0,0), (3,0,3), (0,0,0,0), []),		(* diagonals from left to right *)
     ((0,0,3), (3,0,0), (0,0,0,0), []),
     ((0,1,0), (3,1,3), (0,0,0,0), []),
     ((0,1,3), (3,1,0), (0,0,0,0), []),
     ((0,2,0), (3,2,3), (0,0,0,0), []),
     ((0,2,3), (3,2,0), (0,0,0,0), []),
     ((0,3,0), (3,3,3), (0,0,0,0), []),
     ((0,3,3), (3,3,0), (0,0,0,0), []),

     ((0,0,0), (3,3,0), (0,0,0,0), []),		(* horizontal diagonals *)
     ((0,0,1), (3,3,1), (0,0,0,0), []),
     ((0,0,2), (3,3,2), (0,0,0,0), []),
     ((0,0,3), (3,3,3), (0,0,0,0), []),
     ((0,3,0), (3,0,0), (0,0,0,0), []),
     ((0,3,1), (3,0,1), (0,0,0,0), []),
     ((0,3,2), (3,0,2), (0,0,0,0), []),
     ((0,3,3), (3,0,3), (0,0,0,0), []),

     ((0,0,0), (3,3,3), (0,0,0,0), []),		(* main diagonals *)
     ((0,0,3), (3,3,0), (0,0,0,0), []),
     ((0,3,0), (3,0,3), (0,0,0,0), []),
     ((0,3,3), (3,0,0), (0,0,0,0), [])   );
  
const
  first_vertical_line = 1;
  last_vertical_line = 16;
$PAGE conv_to_board_pos, empty_board

function conv_to_board_pos (ball_pos: ball_coord_type): board_position_type;
  begin
    with ball_pos do
      conv_to_board_pos := x_coord * 16  +  y_coord * 4  +  z_coord
  end;



public function empty_board: board_type;

  begin
    empty_board [white_ball] := [];
    empty_board [black_ball] := []
  end;
$PAGE update_board

public procedure update_board (var board: board_type;
				   color: ball_color_type;
				   ball_pos: ball_coord_type;
			       var ok: boolean);

  var
    height: peg_range;
    change_position,
    free_positions: set of board_position_type;

  begin
    free_positions := all_positions - board [white_ball] - board [black_ball];
    change_position := [conv_to_board_pos (ball_pos)];
    ok := (color = no_ball) <> (change_position <= free_positions);
    if not ok then
      return;

    if (color = no_ball) and (ball_pos.z_coord < maximum (peg_range)) then
      for height := ball_pos.z_coord + 1 to maximum (peg_range) do
	if not (conv_to_board_pos ((ball_pos.x_coord, ball_pos.y_coord, height))
		in free_positions) then
	  ok := false;
    if (color <> no_ball) and (ball_pos.z_coord > minimum (peg_range)) then
      for height := minimum (peg_range) to ball_pos.z_coord - 1 do
	if conv_to_board_pos ((ball_pos.x_coord, ball_pos.y_coord, height)) in free_positions then
	  ok := false;
    if not ok then
      return;
    if color <> no_ball then
      board [color] := board [color] + change_position
    else begin
      board [white_ball] := board [white_ball] - change_position;
      board [black_ball] := board [black_ball] - change_position
    end
  end (* update_board *);
$PAGE init_move_module
  
public procedure init_move_module;

  var
    line_index: winning_line_range;
    ball: peg_range;
    x_incr, y_incr, z_incr: -1..+1;
    x, y, z: peg_range;
    pos: set of board_position_type;

  begin
    for line_index := minimum (winning_line_range) to maximum (winning_line_range) do
      with winning_lines [line_index] do begin
	x_incr := (last_ball.x_coord - first_ball.x_coord) div 3;
	y_incr := (last_ball.y_coord - first_ball.y_coord) div 3;
	z_incr := (last_ball.z_coord - first_ball.z_coord) div 3;
	for ball := 0 to 3 do
	  positions [ball] := (first_ball.x_coord + ball * x_incr) * 16 +
			      (first_ball.y_coord + ball * y_incr) * 4 +
			      (first_ball.z_coord + ball * z_incr);
	position_set := [positions [0], positions [1], positions [2], positions [3]]
      end;

      central_cube := [];
      corners := [];
      middle_of_edges := [];
      middle_of_surfaces := [];
      level := ([], [], [], []);

      for x := 0 to 3 do
	for y := 0 to 3 do
	  for z := 0 to 3 do begin
	    pos := [x * 16 + y * 4 + z];
	    if ((x=1) or (x=2)) and ((y=1) or (y=2)) and ((z=1) or (z=2)) then
	      central_cube := central_cube + pos;
	    if ((x=0) or (x=3)) and ((y=0) or (y=3)) and ((z=0) or (z=3)) then
	      corners := corners + pos;
	    if (((x=0) or (x=3)) and ((y=0) or (y=3)) and ((z=1) or (z=2))) or
	       (((x=0) or (x=3)) and ((y=1) or (y=2)) and ((z=0) or (z=3))) or
	       (((x=1) or (x=2)) and ((y=0) or (y=3)) and ((z=0) or (z=3))) then
	      middle_of_edges := middle_of_edges + pos;
	    if (((x=0) or (x=3)) and ((y=1) or (y=2)) and ((z=1) or (z=2))) or
	       (((x=1) or (x=2)) and ((y=0) or (y=3)) and ((z=1) or (z=2))) or
	       (((x=1) or (x=2)) and ((y=1) or (y=2)) and ((z=0) or (z=3))) then
	      middle_of_surfaces := middle_of_surfaces + pos;
	    level [z] := level [z] + pos
	  end;
  end (* init_move_module *);
$PAGE best_move

public function best_move (    current_board: board_type;
			       whose_move: ball_color_type;
			       cur_strategy: packed_coef_array;
			       primary_max_depth,
			       critical_max_depth: 1..number_of_positions;
			   var positions_evaluated: integer): ball_coord_type;

  var
    temp_board: board_type;
    critical: boolean;
    depth: 0..number_of_positions;
    player_to_move: ball_color_type;
    strategy: array [coef_index_type] of ext_coef_type;
    next_plays: array [first_vertical_line..last_vertical_line]
		   of minimum (peg_range)..maximum (peg_range) + 1;
    next_plays_set: set of board_position_type;
$PAGE static_evaluation  in  best_move

  function static_evaluation: board_value_type;

    var
      black_potential_lines,
      white_potential_lines: array [0..4] of integer;
      line_index: winning_line_range;
      white_card, black_card: 0..4;

    begin
      positions_evaluated := positions_evaluated + 1;
      critical := false;
  
      black_potential_lines := (0,0,0,0,0);
      white_potential_lines := (0,0,0,0,0);
  
      for line_index := minimum (winning_line_range) to maximum (winning_line_range) do
	with winning_lines [line_index] do begin
	  if temp_board [white_ball] * position_set = [] then begin
	    black_card := card (temp_board [black_ball] * position_set);
	    black_potential_lines [black_card] := black_potential_lines [black_card] + 1;
	    if (black_card = 3) and not critical then
	      (* situation isn't really critical unless the free pos. on the line is playable *)
	      if (position_set - temp_board [black_ball]) <= next_plays_set then
		critical := true
	  end
	  else if temp_board [black_ball] * position_set = [] then begin
	    white_card := card (temp_board [white_ball] * position_set);
	    white_potential_lines [white_card] := white_potential_lines [white_card] + 1;
	    if (white_card = 3) and not critical then
	      (* situation isn't really critical unless the free pos. on the line is playable *)
	      if (position_set - temp_board [white_ball]) <= next_plays_set then
		critical := true
	  end;
	end;

      if white_potential_lines [4] > 0 then
	static_evaluation := won + number_of_positions - depth
      else if black_potential_lines [4] > 0 then
	static_evaluation := - (won + number_of_positions - depth)
      else
	static_evaluation :=
		 white_potential_lines [3]                             * strategy [1]
	       + white_potential_lines [2]                             * strategy [2]
	       + white_potential_lines [1]                             * strategy [3]
	       + black_potential_lines [3]                             * strategy [4]
	       + black_potential_lines [2]                             * strategy [5]
	       + black_potential_lines [1]                             * strategy [6]
	       + card (temp_board [white_ball] * central_cube)         * strategy [7]
	       + card (temp_board [black_ball] * central_cube)         * strategy [8]
	       + card (temp_board [white_ball] * corners)              * strategy [9]
	       + card (temp_board [black_ball] * corners)              * strategy [10]
	       + card (temp_board [white_ball] * middle_of_edges)      * strategy [11]
	       + card (temp_board [black_ball] * middle_of_edges)      * strategy [12]
	       + card (temp_board [white_ball] * middle_of_surfaces)   * strategy [13]
	       + card (temp_board [black_ball] * middle_of_surfaces)   * strategy [14]
	       + (card (temp_board [white_ball] * level [0]) -
		  card (temp_board [black_ball] * level [0]))          * strategy [15]
	       + (card (temp_board [white_ball] * level [1]) -
		  card (temp_board [black_ball] * level [1]))          * strategy [16]
	       + (card (temp_board [white_ball] * level [2]) -
		  card (temp_board [black_ball] * level [2]))          * strategy [17]
	       + (card (temp_board [white_ball] * level [3]) -
		  card (temp_board [black_ball] * level [3]))          * strategy [18];
 
      if player_to_move = black_ball then
	static_evaluation := -static_evaluation
    end (* static_evaluation *);
$PAGE available_moves  in  best_move
  
  procedure available_moves (var num_avail: integer;
			     var move_list: move_list_type;
				 next_move_forced: boolean);

    var
      line_index: winning_line_range;
      peg: first_vertical_line..last_vertical_line;
      move_position: board_position_type;
      temp_value: board_value_type;
      i: integer;
      saved_plays_set,
      critical_positions: set of board_position_type;

    begin
      if next_move_forced then begin
	critical_positions := [];
	for line_index := minimum (line_index) to maximum (line_index) do
	  with winning_lines [line_index] do
	    if temp_board [white_ball] * position_set = [] then begin
	      if card (temp_board [black_ball] * position_set) >= 3 then
		critical_positions := critical_positions + (position_set - temp_board [black_ball])
	    end
	    else if temp_board [black_ball] * position_set = [] then begin
	      if card (temp_board [white_ball] * position_set) >= 3 then
		critical_positions := critical_positions + (position_set - temp_board [white_ball])
	    end
      end;
      num_avail := 0;
      for peg := minimum (peg) to maximum (peg) do
	if next_plays [peg] <= maximum (peg_range) then begin
	  move_position := winning_lines [peg].positions [next_plays [peg]];
	  if (not next_move_forced) orif (move_position in critical_positions) then begin
	    temp_board [player_to_move] := temp_board [player_to_move] + [move_position];
	    saved_plays_set := next_plays_set;
	    next_plays_set := next_plays_set - [move_position];
	    if next_plays [peg] < maximum (peg_range) then
	      next_plays_set := next_plays_set + [move_position + 1];
	    temp_value := static_evaluation;
	    next_plays_set := saved_plays_set;
	    temp_board [player_to_move] := temp_board [player_to_move] - [move_position];
	    num_avail := num_avail + 1;
	    i := num_avail - 1;
	    while (i > 0) andif (temp_value > move_list [i].value) do begin
	      move_list [i+1] := move_list [i];
	      i := i - 1
	    end;
	    move_list [i+1] := (move_position, temp_value, critical)
	  end
	end
    end (* available_moves *);
$PAGE negamax  in  best_move

  function negamax (alpha, beta: board_value_type;
		    static_value: board_value_type;
		    next_move_forced: boolean): board_value_type;

    var
      num_avail, index: integer;
      move_list: move_list_type;
      m, temp_value: board_value_type;
      saved_plays_set: set of board_position_type;
      peg: first_vertical_line..last_vertical_line;

    begin
      depth := depth + 1;
      player_to_move := reverse_color [player_to_move];
      available_moves (num_avail, move_list, next_move_forced);

      if num_avail = 0 then
	negamax := static_value
      else begin
	m := alpha;
	for index := 1 to num_avail do
	  with move_list [index] do begin
	    if (abs (value) >= won) or
	       (depth >= critical_max_depth) or
	       ((depth >= primary_max_depth) and not critical) then
	      temp_value := value
	    else begin
	      peg := (pos div 4) + 1;
	      temp_board [player_to_move] := temp_board [player_to_move] + [pos];
	      saved_plays_set := next_plays_set;
	      next_plays_set := next_plays_set - [pos];
	      next_plays [peg] := next_plays [peg] + 1;
	      if pos mod 4 < maximum (peg_range) then
		next_plays_set := next_plays_set + [pos + 1];
	      temp_value := -negamax (-beta, -m, value, critical or (depth >= primary_max_depth));
	      next_plays_set := saved_plays_set;
	      next_plays [peg] := next_plays [peg] - 1;
	      temp_board [player_to_move] := temp_board [player_to_move] - [pos]
	    end;
	    if temp_value > m then
	      m := temp_value;
	   exit if m >= beta
	  end;
	negamax := m
      end;
      player_to_move := reverse_color [player_to_move];
      depth := depth - 1
    end (* negamax *);
$PAGE best_move - body
  
  var
    num_avail, index: integer;
    move_list: move_list_type;
    m, temp_value: board_value_type;
    saved_plays_set: set of board_position_type;
    best_seen: board_position_type;
    coef_index: coef_index_type;
    peg: first_vertical_line..last_vertical_line;
    z_temp: minimum (peg_range)..maximum (peg_range) + 1;

  begin

    (* Unpack and convert strategy parameters. *)

    for coef_index := minimum (coef_index) to maximum (coef_index) do
      strategy [coef_index] := cur_strategy [coef_index] - coef_excess;
    assert (critical_max_depth >= primary_max_depth);

    next_plays_set := [];
    for peg := minimum (peg) to maximum (peg) do
      with winning_lines [peg] do begin
	z_temp := minimum (peg_range);
	while (z_temp <= maximum (peg_range)) andif
	      (positions [z_temp] in (current_board [white_ball] + current_board [black_ball])) do
	  z_temp := z_temp + 1;
        next_plays [peg] := z_temp;
	if z_temp <= maximum (peg_range) then
	  next_plays_set := next_plays_set + [positions [z_temp]]
      end;
 
    depth := 0;
    player_to_move := whose_move;
    positions_evaluated := 0;
    temp_board := current_board;
 
    available_moves (num_avail, move_list, false);
    m := -infinity;
    best_seen := move_list [1].pos;
    for index := 1 to num_avail do
      with move_list [index] do begin
	if (abs (value) >= won) then
	  temp_value := value
	else begin
	  peg := (pos div 4) + 1;
	  temp_board [player_to_move] := temp_board [player_to_move] + [pos];
	  saved_plays_set := next_plays_set;
	  next_plays_set := next_plays_set - [pos];
	  next_plays [peg] := next_plays [peg] + 1;
	  if (pos mod 4) < maximum (peg_range) then
	    next_plays_set := next_plays_set + [pos + 1];
	  temp_value := -negamax (-infinity, -m, value, critical);
	  next_plays_set := saved_plays_set;
	  next_plays [peg] := next_plays [peg] - 1;
	  temp_board [player_to_move] := temp_board [player_to_move] - [pos]
	end;
	if temp_value > m then begin
	  m := temp_value;
	  best_seen := pos
	end
     
    
    best_move := (best_seen div 16, (best_seen mod 16) div 4, best_seen mod 4)
  end (* find_best_move *);
$PAGE state_of_game

public procedure state_of_game (    current_board: board_type;
				var game_state: game_state_type;
				var start_of_line, end_of_line: ball_coord_type);

  var
    line_index: winning_line_range;

  begin
    if (all_positions - current_board [white_ball] - current_board [black_ball]) = [] then
      game_state := no_more_moves
    else
      game_state := game_not_over;
    for line_index := minimum (winning_line_range) to maximum (winning_line_range) do
      with winning_lines [line_index] do begin
	exit if (position_set - current_board [white_ball]) = [] do begin
	  start_of_line := first_ball;
	  end_of_line := last_ball;
	  game_state := white_won
	end;
	exit if (position_set - current_board [black_ball]) = [] do begin
	  start_of_line := first_ball;
	  end_of_line := last_ball;
	  game_state := black_won
	end
      end
  end (* state_of_game *);
$PAGE prep_structure_evaluation

var
  game_parameters: fitness_parameter_record;

public procedure prep_structure_evaluation (fitness_par: fitness_parameter_record);

  begin
    game_parameters := fitness_par;
    init_move_module
  end;
$PAGE evaluate_structure

public function evaluate_structure (structure: packed_coef_array): structure_fitness_type;

  var
    first_player, player_to_move: ball_color_type;
    current_board: board_type;
    game_state: game_state_type;
    current_move_no: 0..number_of_positions;
    desired_move, first_ball, last_ball: ball_coord_type;
    move_ok: boolean;
    evaluation_count: integer;

  begin
    current_board := empty_board;
    current_move_no := 0;
    if random >= 0.5 then
      first_player := white_ball		(* given structure plays first *)
    else
      first_player := black_ball;		(* fixed opponent plays first *)

    repeat
      current_move_no := current_move_no + 1;
      if odd (current_move_no) then
	player_to_move := first_player
      else
	player_to_move := reverse_color [first_player];
      with game_parameters do
	if player_to_move = white_ball then
	  desired_move := best_move (current_board, white_ball, structure, primary_limit,
					critical_limit, evaluation_count)
	else
	  desired_move := best_move (current_board, black_ball, opponents_strategy, 
					opponents_primary_limit, opponents_critical_limit,
					evaluation_count);
      update_board (current_board, player_to_move, desired_move, move_ok);
      assert (move_ok);
      state_of_game (current_board, game_state, first_ball, last_ball)
    until game_state in [white_won, black_won, no_more_moves];

    if game_state = white_won then
      evaluate_structure := 2 * number_of_positions + 2 - current_move_no
    else if game_state = black_won then
      evaluate_structure := current_move_no
    else
      evaluate_structure := number_of_positions + 1
  end (* evaluate_structure *).
 @1¦