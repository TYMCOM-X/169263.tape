$TITLE x4io
$WIDTH (106)

module x4io   options special (word, ptr, coercions);

$INCLUDE score4.typ

type
  terminal_type = (adm_3a, tek_4023, adds_viewpoint, dec_vt100);
  display_x_range = 0..79;			(* characters across line on screen, 0 at left *)
  display_y_range = 0..23;			(* line on screen, 0 at bottom *)
  frame_type =
    packed array [display_x_range, display_y_range] of char;
  display_coord_type = 
    record
      x_coord: display_x_range;
      y_coord: display_y_range
    end;
  control_seq = array [terminal_type] of string[4];


const
  reference_position: display_coord_type = (20, 5); (* space below peg 0,0 *)

  null = char (0b);
  ack = char (6b);
  bs = char (10b);
  ht = char (11b);
  lf = char (12b);
  vt = char (13b);
  ff = char (14b);
  sub = char (32b);
  esc = char (33b);
  fs = char (34b);

  load_cursor_codes: control_seq = (esc || '=', fs, esc || 'Y', esc || '[');
  left_codes: control_seq = (bs, bs, bs, bs);
  right_codes: control_seq = (ff, ht, ack, esc || '[C');
  up_codes: control_seq = (vt, '', sub, esc || '[A');
  down_codes: control_seq = (lf, lf, lf, esc || '[B');
  clear_codes: control_seq = (sub, esc || ff, ff, esc || '[2J');

  first_message_line: display_y_range = 2;
  prompt_line: display_y_range = 1;

var
  terminal: terminal_type;
  display: frame_type;
  cursor_position:
    record
      defined: boolean;
      x_coord: display_x_range;
      y_coord: display_y_range
    end := (false, 0, 0);
$PAGE set_line_characteristics, reset_terminal

$SYSTEM uuocal[31024,320156]

type
  characteristics =
    ( lc_dm1, lc_dm2, lc_ecs, lc_pss, lc_obs, lc_hdx, lc_esc, lc_crd, lc_dfr,
      lc_nop, lc_nfc, lc_brk, lc_axc, lc_ncm, lc_hht, lc_lcp, lc_ptm, lc_hff );

  line_char_record = packed record
    case boolean of
      false: ( bits: set of characteristics;
	       port: 0 .. #o777777 );
      true:  ( value: machine_word );
  end;

const
  auxcal_uuo := #o42; (* perform port i/o functions *)
  ttcall_uuo := #o51; (* perform cmnd port i/o *)
  calli_uuo := #o47;


var
  saved_chars, aux_chars: line_char_record;
  x: machine_word;


procedure set_line_characteristics;

  var
    new_chars: line_char_record;

  begin
    saved_chars.value := -1;
    if uuo_call (ttcall_uuo, 6, 0, address (saved_chars), x) then ;
    new_chars.bits := saved_chars.bits + [lc_nfc, lc_hht, lc_hff];
    if uuo_call (ttcall_uuo, 7, 0, address (new_chars), x) then ;

    aux_chars.value := 0;
    if uuo_call (auxcal_uuo, 1, -#o777750, address (aux_chars), x) then ;
  end (* set_line_characteristics *);


public procedure reset_terminal;

  begin
    close; (* push data out of Pascal's buffers *)
    if uuo_call (calli_uuo, 1, #o41000002, ptr (#o72), x) then ; (* wait for actual output to complete *)
    if uuo_call (ttcall_uuo, 7, 0, address (saved_chars), x) then ;
    if uuo_call (auxcal_uuo, 1, -#o777750, address (aux_chars), x) then ;
  end;
$PAGE menu_and_setup_terminal

public procedure menu_and_setup_terminal;

  const
    terminal_name: array [terminal_type] of string[15] = 
	  ('ADM 3A', 'TEK 4023', 'ADDS VIEWPOINT', 'DEC VT100');

  var
    got_term_kind: boolean;
    term: terminal_type;
    response: string [15];
    len: display_x_range;

  begin
    writeln (ttyoutput);
    writeln (ttyoutput, 'Score4, Version of ', compdate);
    writeln (ttyoutput);

    writeln (ttyoutput, 'Throughout the game, you may use the following commands:');
    writeln (ttyoutput);
    writeln (ttyoutput, '  REFRESH:   will clear and rewrite your screen in case an operator message');
    writeln (ttyoutput, '             or other problem has spoiled it.');
    writeln (ttyoutput, '  MACHINE:   will tell you what move the program would make in your situation.');
    writeln (ttyoutput, '  DEPTH p,c: changes the lookahead limits.  "p" is the primary limit in plies,');
    writeln (ttyoutput, '             "c" is the limit for critical situations.  "c" must be greater');
    writeln (ttyoutput, '             than or equal to "p".');
    writeln (ttyoutput, '  DEPTH:     will tell you the present lookahead limits.');
    writeln (ttyoutput, '  BACKUP:    removes the machine''s last move, and your move preceding it.');
    writeln (ttyoutput, '  EXIT:      terminates the game, putting your line characteristics back');
    writeln (ttyoutput, '             the way they were.');

    writeln (ttyoutput);
    repeat
      write (ttyoutput, 'Enter terminal type (');
      len := 21;
      for term := minimum (terminal_type) to maximum (terminal_type) do begin
	if len > 60 then begin
	  writeln (ttyoutput);
	  write (ttyoutput, ' ':21);
	  len := 21
	end;
	write (ttyoutput, '"', terminal_name [term], '"');
	len := len + length (terminal_name [term]) + 4;
	if term < maximum (terminal_type) then
	  write (ttyoutput, ', ')
      end;

      write (ttyoutput, ')-- ');
      break (ttyoutput);
      readln (tty);
      read (tty, response);
      response := uppercase (response);
      if response = 'EXIT' then
	stop;
      got_term_kind := false;
      for term := minimum (terminal_type) to maximum (terminal_type) do
	exit if response = terminal_name [term] do begin
	  terminal := term;
	  got_term_kind := true
	end
    until got_term_kind;

    set_line_characteristics
  end (* menu_and_setup_terminal *);
$PAGE clear_screen

procedure clear_screen;

  var
    x: display_x_range;
    y: display_y_range;

  begin
    write (ttyoutput, clear_codes [terminal]);
    with cursor_position do begin		(* set to upper left *)
      defined := true;
      x_coord := minimum (display_x_range);
      y_coord := maximum (display_y_range)
    end;
    for x := minimum (x) to maximum (x) do
      for y := minimum (y) to maximum (y) do
	display [x, y] := ' ';
  end;
$PAGE position_cursor

procedure position_cursor (display_pos: display_coord_type);
  function digits (i: integer): string [2];
    var len: 1..2;
    begin
      if i < 10 then len := 1 else len := 2;
      putstring (digits, i:len)
    end;
  begin
    with display_pos do begin
      if terminal = adm_3a then
	write (ttyoutput, load_cursor_codes [terminal], char (ord ('7') - y_coord),
							char (ord (' ') + x_coord))
      else if terminal = tek_4023 then
	write (ttyoutput, load_cursor_codes [terminal], char (ord (' ') + x_coord),
							char (ord (' ') + 23 - y_coord))
      else if terminal = adds_viewpoint then
	write (ttyoutput, load_cursor_codes [terminal], char (ord (' ') + 23 - y_coord),
							char (ord (' ') + x_coord))
      else if terminal = dec_vt100 then
	write (ttyoutput, load_cursor_codes [terminal], digits (24 - y_coord), ';',
							digits (x_coord + 1), 'H')
      else
	assert (false);
      cursor_position := (true, x_coord, y_coord)
    end
  end;
$PAGE left, right, up, down

procedure left;
  begin
    if length (left_codes [terminal]) = 0 then
      with cursor_position do
	position_cursor ((x_coord - 1, y_coord))
    else begin
      write (ttyoutput, left_codes [terminal]);
      with cursor_position do
	cursor_position := (defined, x_coord - 1, y_coord)
    end
  end;

procedure right;
  begin
    if length (right_codes [terminal]) = 0 then
      with cursor_position do
	position_cursor ((x_coord + 1, y_coord))
    else begin
      write (ttyoutput, right_codes [terminal]);
      with cursor_position do
	cursor_position := (defined, x_coord + 1, y_coord)
    end
  end;

procedure up;
  begin
    if length (up_codes [terminal]) = 0 then
      with cursor_position do
	position_cursor ((x_coord, y_coord + 1))
    else begin
      write (ttyoutput, up_codes [terminal]);
      with cursor_position do
	cursor_position := (defined, x_coord, y_coord + 1)
    end
  end;

procedure down;
  begin
    if length (down_codes [terminal]) = 0 then
      with cursor_position do
	position_cursor ((x_coord, y_coord - 1))
    else begin
      write (ttyoutput, down_codes [terminal]);
      with cursor_position do
	cursor_position := (defined, x_coord, y_coord - 1)
    end
  end;
$PAGE convert_to_display_coords

function convert_to_display_coords (ball_pos: ball_coord_type): display_coord_type;
  begin
    convert_to_display_coords := reference_position;
    with convert_to_display_coords do begin
      x_coord := x_coord + ball_pos.x_coord * 17 - 4 * ball_pos.y_coord;
      y_coord := y_coord + 2 * ball_pos.y_coord + 3 * ball_pos.z_coord + 2
    end;
  end;
$PAGE update_screen

procedure update_screen (display_pos: display_coord_type; ch: char);

  begin
    if display [display_pos.x_coord, display_pos.y_coord] <> ch then begin
      display [display_pos.x_coord, display_pos.y_coord] := ch;
      with cursor_position do begin
	if (not defined) orif ((x_coord <> display_pos.x_coord) or
			       (y_coord <> display_pos.y_coord)) then
	  if defined andif ((abs (x_coord - display_pos.x_coord) <= 1) and
			    (abs (y_coord - display_pos.y_coord) <= 1)) then begin
	    if x_coord < display_pos.x_coord then
	      right
	    else if x_coord > display_pos.x_coord then
	      left;
	    if y_coord < display_pos.y_coord then
	      up
	    else if y_coord > display_pos.y_coord then
	      down
	  end
	  else
	    position_cursor (display_pos);
	write (ttyoutput, ch);
	x_coord := min (display_pos.x_coord + 1, maximum (display_x_range));
	y_coord := display_pos.y_coord;
	defined := true
      end
    end
  end (* update_screen *);
$PAGE conv_int_to_string, conv_real_to_string, conv_coords_to_string


public function conv_int_to_string (int: integer): string [80];

  begin
    putstring (conv_int_to_string, int)
  end;


public function conv_real_to_string (r: real): string [80];

  begin
    putstring (conv_real_to_string, r:0:1)
  end;


public function conv_coords_to_string (coords: ball_coord_type): string [80];

  begin
    putstring (conv_coords_to_string, coords.x_coord:1, ',', coords.y_coord:1, ',',
				      coords.z_coord:1)
  end;
$PAGE new_display

public procedure new_display;

  var
    pegx, pegy: peg_range;

  procedure display_peg (peg_x_coord, peg_y_coord: peg_range);
    var
      display_pos: display_coord_type;
      height: display_y_range;
    begin
      display_pos := reference_position;
      with display_pos do begin
	x_coord := x_coord + peg_x_coord * 17;
	if peg_y_coord = minimum (peg_range) then begin
	  if peg_x_coord = maximum (peg_range) then
	    update_screen ((x_coord + 2, y_coord), 'x');
	  update_screen (display_pos, char (ord ('0') + peg_x_coord));
	end;
	x_coord := x_coord - 4 * peg_y_coord;
	y_coord := y_coord + 2 * peg_y_coord;
	if peg_x_coord = minimum (peg_range) then begin
	  if peg_y_coord = maximum (peg_range) then
	    update_screen ((x_coord - 4, y_coord + 2), 'y');
	  update_screen ((x_coord - 2, y_coord + 1), char (ord ('0') + peg_y_coord));
	end;
	for height := 1 to 12 do begin
	  y_coord := y_coord + 1;
	  if (height mod 3) = 2 then
	    update_screen (display_pos, '+')
	  else
	    update_screen (display_pos, ':')
	end;
	if (peg_x_coord = minimum (peg_range)) and (peg_y_coord = minimum (peg_range)) then
	  update_screen ((x_coord, y_coord + 2), 'z')
      end
    end (* display_peg *);


begin
  clear_screen;
  for pegx := 0 to 3 do
    for pegy := 0 to 3 do
      display_peg (pegx, pegy)
end (* new_display *);
$PAGE prompt

public function prompt (msg_text: string[80]): string[80];

  var
    index: display_x_range;

  begin
    for index := length (msg_text) + minimum (display_x_range) to maximum (display_x_range) do
      update_screen ((index, prompt_line), ' ');
    for index := minimum (display_x_range) to length (msg_text) - 1 + minimum (display_x_range) do
      update_screen ((index, prompt_line), msg_text [index - minimum (display_x_range) + 1]);

    position_cursor ((length (msg_text) + minimum (display_x_range), prompt_line));
    break (ttyoutput);
    readln (tty);
    read (tty, prompt);
    prompt := uppercase (prompt);

    for index := 1 to length (prompt) do
      display [length (msg_text) + minimum (display_x_range) + index - 1, prompt_line] := prompt [index];
    cursor_position.defined := false
  end (* prompt *);
$PAGE message

public procedure message (line_no: 0..2; msg_text: string[80]);

  var
    index: display_x_range;

  begin
    for index := minimum (display_x_range) to length (msg_text) - 1 + minimum (display_x_range) do
      update_screen ((index, line_no + first_message_line),
		     msg_text [index - minimum (display_x_range) + 1]);

    for index := length (msg_text) + minimum (display_x_range) to maximum (display_x_range) do
      update_screen ((index, line_no + first_message_line), ' ')
  end (* message *);
$PAGE add_ball_to_display

public procedure add_ball_to_display (ball_color: ball_color_type; ball_pos: ball_coord_type);

  const
    ball_chars: array [ball_color_type, 1..5] of char :=
		((':',' ',':',' ','+'), ('-','|','-','|',' '), ('*','*','*','*','*'));

  var
    display_pos: display_coord_type;

  begin
    display_pos := convert_to_display_coords (ball_pos);
    with display_pos do begin
      update_screen ((x_coord,   y_coord-1), ball_chars [ball_color, 1]);
      update_screen ((x_coord-1, y_coord), ball_chars [ball_color, 2]);
      update_screen ((x_coord,   y_coord+1), ball_chars [ball_color, 3]);
      update_screen ((x_coord+1, y_coord), ball_chars [ball_color, 4]);
      update_screen ((x_coord,   y_coord), ball_chars [ball_color, 5])
    end;
    break (ttyoutput)
  end (* add_ball_to_display *);
$PAGE highlight_line

public procedure highlight_line (first_ball, last_ball: ball_coord_type);

  var
    first_pos, last_pos: display_coord_type;
    x_move, y_move: integer;
    duration: 1..6;
    step, num_parts: integer;
    real_x_pos, real_y_pos,
    desired_x_pos, desired_y_pos,
    x_off, y_off: real;

  begin
    first_pos := convert_to_display_coords (first_ball);
    position_cursor (first_pos);
    last_pos := convert_to_display_coords (last_ball);
    x_move := last_pos.x_coord - first_pos.x_coord;
    y_move := last_pos.y_coord - first_pos.y_coord;
    num_parts := max (abs (x_move), abs (y_move));
    for duration := minimum (duration) to maximum (duration) do begin
      real_x_pos := 0.0;
      real_y_pos := 0.0;
      desired_x_pos := 0.0;
      desired_y_pos := 0.0;
      left; right; right; left; up; down; down; up;
      for step := 1 to num_parts do begin
	desired_x_pos := desired_x_pos + x_move/num_parts;
	desired_y_pos := desired_y_pos + y_move/num_parts;
	x_off := desired_x_pos - real_x_pos;
	y_off := desired_y_pos - real_y_pos;
	if abs (x_off) > 0.5 then
	  if x_off > 0.0 then begin
	    right;
	    real_x_pos := real_x_pos + 1.0
	  end
	  else begin
	    left;
	    real_x_pos := real_x_pos - 1.0
	  end;
	if abs (y_off) > 0.5 then
	  if y_off > 0.0 then begin
	    up;
	    real_y_pos := real_y_pos + 1.0
	  end
	  else begin
	    down;
	    real_y_pos := real_y_pos - 1.0
	  end;
	if (step mod (num_parts div 3)) = 0 then begin
	  left; right; right; left; up; down; down; up
	end
      end;
      x_move := -1 * x_move;
      y_move := -1 * y_move
    end;
    cursor_position.defined := false
  end (* highlight_line *);
$PAGE dump_display

public procedure dump_display;

  var
    x: display_x_range;
    y: display_y_range;
    line: packed array [1..(maximum (x) - minimum (x) + 1)] of char;

  begin
    writeln (output);
    writeln (output);
    for y := maximum (y) downto (first_message_line + 3) do begin
      for x := minimum (x) to maximum (x) do
	line [x+1] := display [x,y];
      writeln (output, line)
    end
  end (* dump_display *).
