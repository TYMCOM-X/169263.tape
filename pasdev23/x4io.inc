(* X4IO.INC - specification of interface to i/o utility routines. SCORE4.TYP must
	      be included prior to this file.  *)

(* MENU AND SETUP TERMINAL: type menu of commands, and then determine type of terminal and
   set line characteristics: *)

  external procedure menu_and_setup_terminal;
  external procedure reset_terminal; (* put characteristics back as they were *)

(* NEW DISPLAY: clear screen and fill in empty pegs: *)

  external procedure new_display;

(* PROMPT: print message at bottom of screen and return user's reply: *)

  external function prompt (string[80]): string[80];

(* MESSAGE: print given message on one of the tnree available message lines: *)

  external procedure message (0..2; string[80]);

(* ADD BALL TO DISPLAY: update display to show given color ball on specified peg: *)

  external procedure add_ball_to_display (ball_color_type; ball_coord_type);

(* HIGHLIGHT LINE: run cursor back and forth over specified line of balls: *)

  external procedure highlight_line (ball_coord_type; ball_coord_type);

(* CONV INT TO STRING: *)

  external function conv_int_to_string (integer): string [80];

(* CONV REAL TO STRING: *)

  external function conv_real_to_string (real): string [80];

(* CONV COORDS TO STRING: *)

  external function conv_coords_to_string (ball_coord_type): string [80];

(* DUMP DISPLAY: copy current display to output file: *)

  external procedure dump_display;
  