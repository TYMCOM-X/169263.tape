$TITLE prtout -- pretty printer output routines
$LENGTH 43

(*  The interface between the input and output modules consists of a text
    buffer and a spacing flag.  The spacing flag may be 'nospace' (the next
    token immediately follows the current token), 'space' (there are one or
    more spaces or new-lines between the current and next tokens), or 'com-
    ment' (there is a comment between the current token and the next token).
    The input routines getinit and get_token set the text of the current
    token in the text buffer.  The input routine get_comment will leave
    the next comment text line in the text buffer, or will return false if
    there is no more text for the current comment.  The input variable
    cmt_column records the starting location of a comment so that the out-
    put routine can position subsequent comment lines correctly.  *)

(*  In addition to the put_token routine, there is a routine putinit, which
    is called to initialize the output module.  Putinit is also responsible
    for printing any initial comments.  *)
$PAGE declarations
$INCLUDE prt.typ
$INCLUDE prtinp.inc
$INCLUDE prtstk.inc

const
    sym_max_text = -500;

type
    sym_index = sym_max_text .. max_text;	(* Symmetric text index type. *)

var
    nsp: sym_index;				(* Number of saved spaces. *)
    col: sym_index;				(* Current output line column number. *)
    out_line: string [max_text];		(* Current output line. *)
    split_token: boolean;			(* Can split line after current token. *)
    split_col: text_index;			(* Latest split symbol location. *)
    cmt_indent: sym_index;
    break_flag: boolean;
$PAGE put_string
(*  PUT_STRING is used to write a string to the output file.  Spaces
    written to the output file are replaced by tabs where appropriate,
    and are deleted at the ends of lines.  *)

procedure put_string ( s: string );

var
    len: text_index;				(* len = length(s) *)
    i:   text_index;				(* i indexes through the string. *)
    ch:  char;					(* ch = s[i] *)
    rem: 0 .. 7;				(* rem is the distance from the last tab stop. *)
    ctr: text_index;

begin
  for i := 1 to length(s) do
    begin
      ch := s[i];
      if ch = ' ' then
	begin
	  if nsp = col
	    then nsp := nsp + 1
	    else out_line := out_line || ' ';
	  col := col + 1;
	end
      else if ch = eol then
	begin
	  col := indentation;
	  nsp := indentation;
	  break_flag := false;
	  len := length(out_line);
	  while (len <> 0) andif (out_line [len] = ' ') do
	    len := len - 1;
	  writeln (output,out_line:len);
	  out_line := '';
	  split_col := 0;
	end
      else if ch = cmt_brk then
	begin
	  col := cmt_indent;
	  nsp := cmt_indent;
	  len := length(out_line);
	  while (len <> 0) andif (out_line [len] = ' ') do
	    len := len - 1;
	  writeln (output,out_line:len);
	  out_line := '';
	  split_col := 0;
	  split_token := false;
	end
      else if ch <> eof_ch then
	begin
	  if col < 0 then col := 0;
	  if nsp < 0 then nsp := 0;
	  rem := col mod 8;
	  if (nsp = col) and (nsp > rem) then begin
	    for ctr := 1 to (nsp - rem + 7) div 8 do
	      out_line := out_line || tab;
	    nsp := rem;
	  end;
	  for ctr := 1 to nsp do
	    out_line := out_line || ' ';
	  nsp := 0;
	  col := col + 1;
	  out_line := out_line || ch;
	end;
    end;
end (* put_string *);
$PAGE put_comment
(*  PUT_COMMENT will output the entire text of a comment.  *)

procedure put_comment;
begin
  if next_token = commentsy
    then cmt_indent := indentation		(* comment *)
    else cmt_indent := 0;			(* dollar line *)
  while get_comment andif (token_text = '') do
    put_string (cmt_brk);
  if cmt_level <> 0 then
    cmt_indent := col - cmt_column		(* comment with code *)
  else if next_token = commentsy then
    cmt_indent := indentation			(* independent comment *)
  else
    cmt_indent := 0;				(* dollar line *)
  put_string (token_text);
  while get_comment do begin
    put_string (cmt_brk);
    put_string (token_text);
    if cmt_level = 0 then
      if next_token = commentsy
	then cmt_indent := indentation		(* comment *)
	else cmt_indent := 0;			(* dollar line *)
  end;
end (* put_comment *);
$PAGE putinit
public procedure putinit;
begin
  nsp := 0;
  col := 0;
  out_line := '';
  split_col := 0;
  break_flag := false;
  if spacing = comment then
    put_comment;
end (* putinit *);
$PAGE put_token
(*  PUT_TOKEN writes the text of the current token to the output, and
    follows it with the appropriate spacing.  Depending on the context,
    this may be nothing, a single space, or a complete comment.  *)

public procedure put_token;

var
    temp_out: string [max_text];
    i: text_index;
    split_token: boolean;

begin
  if break_flag then
    put_string (eol);
  if col + length(token_text) > 80 then begin
      if split_col = 0 then
	split_col := length(out_line) + 1;
      temp_out := substr(out_line,split_col);
      out_line := substr(out_line,1,split_col-1);
      push (one_flag);
      put_string (eol);
      i := verify(temp_out,[' ']);
      if i <> 0 then
	put_string (substr(temp_out,i));
  end;
  put_string (token_text);
  split_token :=
    (this_token in [rparensy,semicolonsy,orsy,andsy,orifsy,andifsy]);
  case spacing of
    nospace:
      (* no spacing *);
    space:
      put_string (' ');
    comment:
      begin
	put_string (' ');
	put_comment;
      end
  end (* case spacing *);
  if split_token then
    split_col := length(out_line) + 1;
end (* put_token *);
$PAGE new_line & break_line
(*  NEW_LINE begins a new output line.  *)

public procedure new_line;
begin
  put_string (eol);
end (* new_line *);



(*  BREAK_LINE forces the next token to appear on a new line.  *)

public procedure break_line;
begin
  break_flag := true;
end (* break_line *).
 