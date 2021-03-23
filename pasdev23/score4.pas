$TITLE score4
$WIDTH (106)

program score4;

$INCLUDE score4.typ
$INCLUDE x4io.inc
$INCLUDE x4move.typ
$INCLUDE x4move.inc

type
  command_type = (move_command, bad_command, exit_command, dump_command,
		  refresh_command, backup_command, yes_command, no_command, depth_command,
		  machine_command);

const
  first_verb = exit_command;
  last_verb = machine_command;
  verb_table: array [first_verb..last_verb] of string[3] =
		('EXI', 'DUM', 'REF', 'BAC', 'YES', 'NO', 'DEP', 'MAC');
  strategy: packed_coef_array := (
		 20 + coef_excess,  10 + coef_excess,   5 + coef_excess,
		-20 + coef_excess, -10 + coef_excess,  -5 + coef_excess,
		  0 + coef_excess,   0 + coef_excess,   0 + coef_excess,   0 + coef_excess,
		  0 + coef_excess,   0 + coef_excess,   0 + coef_excess,   0 + coef_excess,
		  0 + coef_excess,   0 + coef_excess,   0 + coef_excess,   0 + coef_excess);

var
  current_move_no: 0..number_of_positions;
  move_sequence: array [1..number_of_positions] of  (* record of game *)
    record
      move: ball_coord_type;
      primary_lookahead,
      critical_lookahead,
      pos_evaluated,
      run:	integer
    end;
  first_player: ball_color_type;
  current_board: board_type;
  game_state: game_state_type;
  primary_search_depth_limit,
  critical_search_depth_limit: 1..number_of_positions;
$PAGE set_depth_limits

procedure set_depth_limits (primary, critical: integer);

  begin
    if (primary > 0) and (critical < primary) then
      message (1, '?critical < primary limit - command ignored.')
    else begin
      if primary > 0 then begin
	primary_search_depth_limit := primary;
	critical_search_depth_limit := critical
      end;
      message (1, 'Current settings of primary and critical lookahead limits are'
		   || conv_int_to_string (primary_search_depth_limit) || ' and'
		   || conv_int_to_string (critical_search_depth_limit) || ' resp.')
    end
  end (* set_depth_limits *);
$PAGE scan

procedure scan (    reply: string[80];
		var kind: command_type;
		var x,y,z: integer);

  var
    com_index: command_type;
    ix: integer;
    temp_str: string[80];

  begin
    ix := verify (reply, [' '], length (reply) + 1);
    temp_str := substr (reply, ix);		(* trim leading blanks *)
    if length (temp_str) = 0 then begin
      kind := bad_command;
      return
    end;

    ix := index (temp_str, ' ', length (temp_str)+1) - 1;
    if temp_str [1] in ['0'..'9'] then begin
      kind := move_command;
      x := maximum (peg_range) + 1;
      y := maximum (peg_range) + 1;
      z := maximum (peg_range) + 1;
      getstring (temp_str, x, y, z);
      if (x > maximum (peg_range)) or (x < minimum (peg_range)) or
	 (y > maximum (peg_range)) or (y < minimum (peg_range)) or
	 (z > maximum (peg_range)) or (z < minimum (peg_range)) then
	kind := bad_command
    end
    else begin
      kind := bad_command;
      for com_index := first_verb to last_verb do begin
	exit if substr (temp_str, 1, min (ix, 3)) = verb_table [com_index] do
	  kind := com_index
      end
    end;

    if kind = exit_command then begin
      reset_terminal;
      stop
    end
    else if kind = depth_command then begin
      temp_str := substr (temp_str, ix+1);
      if temp_str = '' then
	set_depth_limits (0, 0) (* signifies depth command w/o params. *)
      else begin
	x := number_of_positions + 1; (* so we can detect missing parameters *)
	y := number_of_positions + 1;
	getstring (temp_str, x, y);
	if (x > number_of_positions) or (x < 1) or
	   (y > number_of_positions) or (y < 1) then
	  kind := bad_command
	else
	  set_depth_limits (x, y)
      end
    end
  end (* scan *);
$PAGE backoff, refresh_display

procedure backoff (move_to_remove: 1..number_of_positions);

  var
    move_ok: boolean;

  begin
    assert ((move_to_remove = current_move_no) or (move_to_remove = current_move_no - 1));
    update_board (current_board, no_ball, move_sequence [move_to_remove].move, move_ok);
    assert (move_ok);
    add_ball_to_display (no_ball, move_sequence [move_to_remove].move);
    current_move_no := current_move_no - 1
  end (* backoff *);



procedure refresh_display;

  var
    move: 1..number_of_positions;
    player_to_move: ball_color_type;
    move_ok: boolean;

  begin
    current_board := empty_board;
    new_display;
    player_to_move := reverse_color [first_player];
    for move := 1 to current_move_no - 1 do begin
      player_to_move := reverse_color [player_to_move];
      update_board (current_board, player_to_move, move_sequence [move].move, move_ok);
      assert (move_ok);
      add_ball_to_display (player_to_move, move_sequence [move].move)
    end
  end (* refresh_display *);
$PAGE dump_game

procedure dump_game;

  const
    color_label: array [black_ball..white_ball] of string [5] := ('black', 'white');

  var
    player_to_move: ball_color_type;
    move: 1..number_of_positions;
    move_ok: boolean;

  begin
    rewrite (output, 'score4.lpt');
    current_board := empty_board;
    new_display;
    player_to_move := reverse_color [first_player];
    for move := 1 to current_move_no do begin
      if odd (move) then
	page (output);
      player_to_move := reverse_color [player_to_move];
      writeln (output, 'Move', move:3, ':  ',
			conv_coords_to_string (move_sequence [move].move),
			color_label [player_to_move]:7);
      if player_to_move = black_ball then with move_sequence [move] do begin
	writeln (output, ' ':12, 'positions evaluated:', pos_evaluated,
			 '   cpu time:', conv_real_to_string (round (run / 10000.0, -1)) );
	writeln (output, ' ':12, 'primary, critical lookahead limits:',
				 primary_lookahead:3, ',', critical_lookahead:3)
      end;
      update_board (current_board, player_to_move, move_sequence [move].move, move_ok);
      assert (move_ok);
      add_ball_to_display (player_to_move, move_sequence [move].move);
      if (not odd (move)) or (move = current_move_no) then
	dump_display
    end;
    writeln (output); writeln (output);
    case game_state of
      white_won:	writeln (output, 'White won.');
      black_won:	writeln (output, 'Black won.');
      no_more_moves:	writeln (output, 'Game is a draw.')
    end;
    close (output);
    message (2, 'Game is recorded in SCORE4.LPT')
  end (* dump_game *);
$PAGE mainline

var
  reply: string[80];
  kind_of_command: command_type;
  desired_move, first_ball, last_ball: ball_coord_type;
  move_ok: boolean;
  player_to_move: ball_color_type;
  x, y, z,
  cpu_time,
  evaluation_count: integer;
  alpha_move_no: string;

begin
  rewrite (ttyoutput);
  open (tty);

  menu_and_setup_terminal;
  current_move_no := 0;
  refresh_display;
  init_move_module;
  set_depth_limits (2, 3);
  message (0, 'To all queries you may respond "refresh", "backup", "machine", or "depth".');

  repeat
    reply := prompt ('You have white.  Do you want to move first ("yes" or "no")? ');
    scan (reply, kind_of_command, x, y, z);
    if kind_of_command = refresh_command then
      refresh_display
  until kind_of_command in [yes_command, no_command];

  if kind_of_command = yes_command then
    first_player := white_ball 
  else
    first_player := black_ball; 


  repeat

    current_move_no := current_move_no + 1;
    if odd (current_move_no) then
      player_to_move := first_player
    else
      player_to_move := reverse_color [first_player];
    if player_to_move = white_ball then begin
      repeat
	repeat
	  putstring (alpha_move_no, current_move_no:2);
	  reply := prompt ('Enter move ' || alpha_move_no || ' (x,y,z)-- ');
	  scan (reply, kind_of_command, x, y, z);
	  if kind_of_command = refresh_command then
	    refresh_display
	  else if (kind_of_command = backup_command) and (current_move_no >= 3) then begin
	    backoff (current_move_no - 1);
	    backoff (current_move_no - 1)
	  end
	  else if kind_of_command = machine_command then begin
	    desired_move := best_move (current_board, white_ball, strategy,
				       primary_search_depth_limit, critical_search_depth_limit,
				       evaluation_count);
	    message (1, 'The machine''s choice would be ' || conv_coords_to_string (desired_move))
	  end
	until kind_of_command = move_command;
	desired_move := (x, y, z);
	update_board (current_board, white_ball, desired_move, move_ok)
      until move_ok;
    end
    else begin
      cpu_time := runtime;
      desired_move := best_move (current_board, black_ball, strategy, 
				 primary_search_depth_limit, critical_search_depth_limit,
				 evaluation_count);
      cpu_time := runtime - cpu_time;
      message (1, conv_int_to_string (evaluation_count) || ' positions evaluated');
      message (0, conv_real_to_string (round (cpu_time / 10000.0, -1)) || ' cpu time');
      update_board (current_board, black_ball, desired_move, move_ok);
      assert (move_ok)
    end;
    add_ball_to_display (player_to_move, desired_move);
    move_sequence [current_move_no] :=
	   (desired_move, primary_search_depth_limit, critical_search_depth_limit,
	    evaluation_count, cpu_time);

    state_of_game (current_board, game_state, first_ball, last_ball);
    if game_state in [white_won, black_won, no_more_moves] then begin
      if game_state = black_won then begin
	message (2, 'I win!');
	highlight_line (first_ball, last_ball)
      end
      else if game_state = white_won then begin
	message (2, 'You win.');
	highlight_line (first_ball, last_ball)
      end
      else
	message (2, 'No winner.');
      repeat
	reply := prompt ('"backup", "dump", or "exit"-- ');
	scan (reply, kind_of_command, x, y, z)
      until kind_of_command in [backup_command, dump_command];
      if kind_of_command = backup_command then begin
	backoff (current_move_no);
	if player_to_move = black_ball (* machine made last move *) then
	  backoff (current_move_no);
	message (2, '');
	game_state := game_not_over
      end
      else if kind_of_command = dump_command then
	dump_game
    end

  until game_state in [white_won, black_won, no_more_moves];
  reset_terminal

end (* score4 *).
 