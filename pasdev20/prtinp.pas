$TITLE prtinp -- pretty printer input routines
$LENGTH 43
$INCLUDE prt.typ
$INCLUDE prtout.inc


(*  Variables for the output module.  *)

public var
    spacing: space_codes;
    token_text: string [max_text];
    cmt_column: text_index;
    cmt_level: 0 .. 1000;

(*  Variables for the analyzer module.  *)

public var
    this_token: token_kind;
    next_token: token_kind;

(*  Variables just for us.  *)

var
    next_text: string [max_text];
    nspaces: text_index;
    nlines: 0 .. 1000;
    cur_column: text_index;
$PAGE get_char
var saved_char: char;


(*  GET_CHAR will return the next input character, both as its return
    value and in its parameter.  If a character has been saved by PUT_BACK,
    then that character will be returned; otherwise, a character will be
    read.  *)

function get_char ( var c: char ): char;
begin
  if saved_char <> nul then
    begin
      c := saved_char;
      saved_char := nul;
    end
  else if eof(input) then
    c := eof_ch
  else if eoln(input) then
    begin
      c := eol;
      readln (input);
      cur_column := 0;
    end
  else
    begin
      read (input,c);
      cur_column := cur_column + 1;
      if (c = '$') and (cur_column = 1)
	then c := dollar_flag;
    end;
  get_char := c;
end (* get_char *);
$PAGE put_back
(*  PUT_BACK will save its parameter character in the saved character
    buffer, so that it can be returned by GET_CHAR again.  *)

procedure put_back ( c: char );
begin
  saved_char := c;
end (* put_back *);
$PAGE lookup
(*  LOOKUP returns the token kind of a reserved word, or 'etc' for
    anything else.  *)

type string10 = string [10];

$INCLUDE prtrw.inc

function lookup ( s: string10 ): token_kinds;

var
    i: 1 .. maxrw;
    len: 1 .. 10;

begin
  len := length(s);
  if len > max_rw_len then begin
    lookup := etc;
    return;
  end;
  rw[nrw[len+1]-1] := s;
  i := nrw[len];
  while rw[i] <> s do i := i + 1;
  lookup := rwsym [i];
end (* lookup *);
$PAGE get_next
(*  GET_NEXT will pick up the next token from the input, setting next_text,
    next_token, and spacing.  If the next input token is a comment, then
    nlines will be the number of new-lines preceding it.  *)

procedure get_next;

var
    build_buffer: packed array [1..max_text] of char;
    count: text_index;
    c: char;

label
    (* got_something *) 100;

begin
  nlines := 0;
  nspaces := 0;
  loop						(* Find a non-blank character.  *)
    if get_char(c) = eol then
      nlines := nlines + 1
    else if c = ' ' then
      nspaces := nspaces + 1
    else
      goto (* got_something *) 100
  end;

(* got_something *) 100:

  next_text := c;
  case uppercase(c) of

    'A'..'Z','_':
      begin
	count := 1;
	build_buffer [1] := c;
	while uppercase(get_char(c)) in ['A'..'Z','0'..'9','_'] do begin
	  count := count + 1;
	  build_buffer [count] := c;
	end;
	put_back (c);
	next_text := substr(build_buffer,1,count);
	next_token := lookup(uppercase(next_text));
      end;

    '0'..'9','.':
      begin
	count := 1;
	build_buffer [1] := c;
	while get_char(c) in ['0'..'9','.'] do begin
	  count := count + 1;
	  build_buffer [count] := c;
	end;
	if uppercase(c) = 'E' then begin
	  count := count + 1;
	  build_buffer [count] := c;
	  if get_char(c) in ['+','-'] then begin
	    count := count + 1;
	    build_buffer [count] := c;
	  end
	  else put_back (c);
	  while get_char(c) in ['0'..'9','.'] do begin
	    count := count + 1;
	    build_buffer [count] := c;
	  end;
	end;
	put_back (c);
	next_text := substr(build_buffer,1,count);
	next_token := etc;
      end;

    '''':
      begin
	count := 0;
	repeat
	  count := count + 1;
	  build_buffer [count] := '''';
	  repeat
	    count := count + 1;
	    build_buffer [count] := get_char(c);
	    if c = eol then
	      error ('END-OF-LINE IN QUOTED STRING');
	  until c = '''';
	until get_char(c) <> '''';
	put_back (c);
	next_text := substr(build_buffer,1,count);
	next_token := etc;
      end;

    ':':
      if get_char(c) = '=' then
	begin
	  next_text := ':=';
	  next_token := etc;
	end
      else
	begin
	  next_token := colonsy;
	  put_back (c);
	end;

    ',':
      next_token := commasy;

    ';':
      next_token := semicolonsy;

    '(':
      if get_char(c) = '*' then
	begin
	  next_token := commentsy;
	  cmt_column := cur_column - 2;
	end
      else
	begin
	  next_token := lparensy;
	  put_back (c);
	end;

    ')':
      next_token := rparensy;

    '%':
      begin
	next_token := commentsy;
	cmt_column := cur_column - 1;
      end;

    '#':
      begin
	next_text := '<>';
	next_token := etc;
      end;

    '<':
      begin
	next_token := etc;
	if get_char(c) = '=' then
	  next_text := '<='
	else if c = '>' then
	  next_text := '<>'
	else
	  put_back (c);
      end;

    '>':
      begin
	next_token := etc;
	if get_char(c) = '=' then
	  next_text := '>='
	else
	  put_back (c);
	end;

    '|':
      begin
	next_token := etc;
	if get_char(c) = '|' then
	  next_text := '||'
	else
	  put_back (c);
      end;

    '[':
      next_token := lbracketsy;

    ']':
      next_token := rbracketsy;

    dollar_flag:
      next_token := dollarsy;

    eof_ch:
      next_token := eofsy;

    others:
      next_token := etc

  end (* case *);

  if next_token in [commentsy,dollarsy] then
    spacing := comment
  else if (nspaces <> 0) or (nlines <> 0) then
    spacing := space
  else
    spacing := nospace;

end (* get_next *);
$PAGE getinit
public procedure getinit;
begin
  saved_char := nul;				(* No characters saved by put_back. *)
  cur_column := 0;
  cmt_level := 0;
  get_next;					(* Get the first token or comment. *)
end;
$PAGE get_comment
(*  GET_COMMENT will load the next line of comment text into the
    token text buffer.  If there is no more comment text, it will
    return false.  *)

public function get_comment: boolean;

var
    build_buffer: packed array [1..max_text] of char;
    count: text_index;
    c: char;
    i: text_index;

label
    (* scan line *) 100;

begin
  get_comment := (next_token in [commentsy,dollarsy]);
  if not get_comment then begin
    for count := 1 to nlines - 1 do new_line;
    if nlines <> 0 then break_line;
    nlines := 0;
    return;
  end;
  if nlines <> 0 then
    begin
      nlines := nlines - 1;
      token_text := '';
      return;
    end;
  if next_token = dollarsy then begin
    count := 1;
    build_buffer [1] := '$';
    while get_char(c) <> eol do begin
      count := count + 1;
      build_buffer [count] := c;
    end;
    get_next;
    token_text := substr(build_buffer,1,count);
    return;
  end;
  if cmt_level = 0 then
    begin					(* First line of this comment. *)
      count := 2;
      build_buffer [1:2] := '(*';
      cmt_level := 1;
    end
  else
    count := 0;					(* Subsequent line of this comment. *)

(* scan line *) 100:

  while cmt_level <> 0 do begin
  exit if get_char(c) = eol;
    count := count + 1;
    build_buffer [count] := c;
    if c = '%' then
      begin
	build_buffer [count:2] := '(*';
	count := count + 1;
	cmt_level := cmt_level + 1;
      end
    else if c = '\' then
      begin
	build_buffer [count:2] := '*)';
	count := count + 1;
	cmt_level := cmt_level - 1;
      end
    else if c = '(' then
      if get_char(c) = '*' then
	begin
	  count := count + 1;
	  build_buffer [count] := '*';
	  cmt_level := cmt_level + 1;
	end
      else
	put_back (c)
    else if c = '*' then
      if get_char(c) = ')' then
	begin
	  count := count + 1;
	  build_buffer [count] := ')';
	  cmt_level := cmt_level - 1;
	end
      else
	put_back (c)
    else if c = dollar_flag then
      build_buffer [count] := '$'
    else if c = eof_ch then
      error ('EOF IN COMMENT');
  end;
  if cmt_level = 0 then begin
    get_next;
    if next_token in [commentsy,dollarsy] then
      if nlines = 0 then begin
	for i := count+1 to count+nspaces do
	  build_buffer [i] := ' ';
	build_buffer [count+nspaces+1:2] := '(*';
	count := count + nspaces + 2;
	cmt_level := 1;
	goto (* scan line *) 100;
      end
      else
	nlines := nlines - 1;
  end;
  token_text := substr(build_buffer,1,count);
end (* get_comment *);
$PAGE get_token
(*  GET_TOKEN is the primary input routine.  It copies the data about the
    next token (if there is any) for the current token, and then gets data
    for the next token.  *)

public procedure get_token;
begin
  this_token := next_token;
  token_text := next_text;
  get_next;
end (* get_token *).
 