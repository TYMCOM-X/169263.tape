$TITLE prtfmt -- pretty	printer	formatting algorithm
$LENGTH	43

(*  The formatting algorithm has access to the	types of the current and next
    tokens, via	the symbols this_token and next_token.	These are initialized
    by a call to getinit, and advanced by a call to get_token.	This_token
    is always the type of the current token.  Next_token may be the type of
    the	next token, or it may be 'commentsy' if	there is an intervening
    comment.  *)

(*  The formatting algorithm performs input actions, output actions, and
    stack actions.  The only input action is get_token, which obtains the
    next token type from the input.  The output actions are put_token,	which
    causes the current token to	be written to the output; break_line, which
    informs the	output routine that this is the	last token to be placed	on
    this line; and new_line, which causes a blank line to be generated after
    a break.  *)

(*  The stack actions are push, mark, pop_until, and pop_while.  Push	will
    push a flag	on the stack along with	the current indentation	level, and
    will increment the indentation level.  The	mark operation is the same,
    except that	the indentation	level is not incremented.  Pop_until will
    pop	flags off of the stack until a specified symbol	is popped, and
    pop_while will pop flags off of the	stack until the	top of the stack
    doesn't contain a specified	symbol.	 Whenever the stack is	popped,	the
    indentation	level is restored to its saved value.  The stack flags	are
    one_flag, pushed when a single statement is	to be indented,	many_flag,
    pushed when	all the	statements up to some terminator are to	be indented,
    and	then_flag, pushed when the statement following an 'if .. then' is
    to be indented.  *)
$PAGE declarations
$INCLUDE prt.typ
$INCLUDE prtinp.inc
$INCLUDE prtstk.inc
$INCLUDE prtout.inc


var key_tokens: set of token_kind :=
		[procedursy,initprocsy,functionsy,labelsy,constsy,typesy,
		 varsy, beginsy,staticsy,externalsy,publicsy,eofsy];
$PAGE format_hdr
procedure format_hdr ( ext_flag: boolean );

var plevel: text_index;

begin
  plevel := 0;
  repeat
    get_token;
    case this_token of
      lparensy:
	begin
	  plevel := plevel + 1;
	  put_token;
	end;
      rparensy:
	begin
	  if plevel = 0 then
	    error ('EXTRA RIGHT PAREN IN PROCEDURE OR FUNCTION HEADER');
	  plevel := plevel - 1;
	  put_token;
	end;
      eofsy:
	error ('EOF IN PROCEDURE OR FUNCTION HEADER');
      others:
	put_token
    end;
  until (plevel = 0) and (this_token = semicolonsy);
  if ext_flag andif (next_token in [fortransy,pascalsy]) then
    repeat
      if next_token = eofsy then
	error ('EOF IN PROCEDURE OR FUNCTION HEADER');
      get_token;
      put_token;
    until this_token = semicolonsy;
  pop_while ([one_flag]);
end (* format_hdr *);
$PAGE format_labels
procedure format_labels;
begin
  get_token;
  put_token;
  push (one_flag);
  push (one_flag);
  break_line;
  repeat
    get_token;
    case this_token of
      commasy:
	begin
	  put_token;
	  break_line;
	end;
      eofsy:
	error ('EOF IN LABELS SECTION');
      others:
	put_token
    end;
  until this_token = semicolonsy;
  pop_while ([one_flag]);
end (* format_labels *);
$PAGE format_consts
procedure format_consts;
begin
  get_token;
  put_token;
  push (then_flag);
  push (then_flag);
  break_line;
  while not (next_token in key_tokens) do begin
    get_token;
    put_token;
    if this_token = semicolonsy then begin
      pop_while ([one_flag]);
      break_line;
    end;
  end;
  pop_while ([then_flag]);
end (* format_consts *);
$PAGE format_decls
procedure format_decls;

var
    case_level: 0 .. 100;
    plevel: array [0..100] of text_index;
    case_flag: boolean;

begin
  get_token;
  put_token;
  push (then_flag);
  push (then_flag);
  break_line;
  case_level := 0;
  plevel [0] := 0;
  case_flag := false;
  while not (next_token in key_tokens) do begin
    get_token;
    case this_token of

      recordsy:
	begin
	  put_token;
	  push (many_flag);
	  break_line;
	end;

      casesy:
	begin
	  put_token;
	  case_flag := true;
	end;

      ofsy:
	begin
	  put_token;
	  if case_flag then begin
	    push (many_flag);
	    break_line;
	    case_level := case_level + 1;
	    plevel [case_level] := 0;
	    case_flag := false;
	  end;
	end;

      lparensy:
	begin
	  put_token;
	  if (case_level <> 0) andif (plevel [case_level] = 0) then begin
	    push (many_flag);
	    break_line;
	  end;
	  plevel [case_level] := plevel [case_level] + 1;
	end;

      rparensy:
	begin
	  put_token;
	  if plevel [case_level] = 0 then begin
	    pop_until ([many_flag]);
	    if case_level <> 0 then
	      case_level := case_level - 1;
	  end;
	  plevel [case_level] := plevel [case_level] - 1;
	  if (case_level <> 0) andif (plevel [case_level] = 0) then
	    pop_until ([many_flag]);
	end;

      endsy:
	begin
	  if case_level <> 0 then
	    pop_until ([many_flag]);
	  pop_until ([many_flag]);
	  break_line;
	  put_token;
	  case_level := 0;
	  plevel [0] := 0;
	end;

      semicolonsy:
	begin
	  put_token;
	  pop_while ([one_flag]);
	  break_line;
	end;

      others:
	put_token

    end (* case this_token *);
  end;
  pop_while ([then_flag]);
end (* format_decls *);
$PAGE format_body
(*  FORMAT_BODY is called when the next symbol is a (zero-level)
    begin.  It	performs the formatting	of the body of a procedure
    or program,	up through the symbol following	the matching (zero-
    level) end.	 *)

procedure format_body;

var
  begin_level: text_index;
  plevel: text_index;

begin
  begin_level := 0;
  plevel := 0;
  repeat
    get_token;
    case this_token of

      beginsy,
      ofsy,
      loopsy,
      repeatsy:
	begin
	  put_token;
	  push (many_flag);
	  break_line;
	  begin_level := begin_level + 1;
	end;

      endsy,
      untilsy:
	begin
	  pop_until ([many_flag]);
	  break_line;
	  put_token;
	  begin_level := begin_level - 1;
	end;

      dosy,
      colonsy:
	begin
	  put_token;
	  if (next_token <> beginsy) and (plevel = 0) then begin
	    push (one_flag);
	    break_line;
	  end;
	end;

      exitsy:
	begin
	  pop_until ([many_flag]);
	  put_token;
	  push (many_flag);
	end;

      semicolonsy:
	begin
	  put_token;
	  pop_while ([one_flag,then_flag]);
	  break_line;
	end;

      thensy:
	begin
	  put_token;
	  if next_token	= beginsy then
	    mark (then_flag)
	  else begin
	    push (then_flag);
	    break_line;
	  end;
	end;

      elsesy:
	begin
	  pop_until ([then_flag]);
	  break_line;
	  put_token;
	  if not (next_token in	[ifsy,beginsy])	then begin
	    push (one_flag);
	    break_line;
	  end;
	end;

      lparensy,
      lbracketsy:
	begin
	  put_token;
	  plevel := plevel + 1;
	end;

      rparensy,
      rbracketsy:
	begin
	  put_token;
	  if plevel <> 0 then
	    plevel := plevel - 1;
	end;

      eofsy:
	error ('EOF IN PROCEDURE BODY');

      others:
	put_token

    end	(* case	this_token *);
  until	begin_level = 0;
  get_token;
  put_token;
end (* format_body *);
$PAGE format
(*  FORMAT examines selected key words and invokes the primary formatting
    routines to format the various declaration and statement parts of
    the program.  *)

public procedure format;

var
    proc_level: text_index;
    no_break_flag: boolean;
    ext_flag: boolean;

begin
  proc_level := 0;
  no_break_flag := false;
  ext_flag := false;
  while next_token <> eofsy do begin
    if not no_break_flag then begin
      new_line;
      new_line;
    end;
    case next_token of

      procedursy,
      initprocsy,
      functionsy:
	begin
	  if not ext_flag then begin
	    if proc_level <> 0 then begin
	      push (many_flag);
	      break_line;
	    end;
	    proc_level := proc_level + 1;
	  end;
	  format_hdr (ext_flag);
	  ext_flag := false;
	  no_break_flag := false;
	end;

      labelsy:
	begin
	  format_labels;
	  ext_flag := false;
	  no_break_flag := false;
	end;

      constsy:
	begin
	  format_consts;
	  ext_flag := false;
	  no_break_flag := false;
	end;

      typesy,
      varsy:
	begin
	  format_decls;
	  ext_flag := false;
	  no_break_flag := false;
	end;

      beginsy:
	begin
	  format_body;
	  if proc_level <> 0 then
	    proc_level := proc_level - 1;
	  if proc_level <> 0 then pop_until ([many_flag]);
	  ext_flag := false;
	  no_break_flag := false;
	end;

      semicolonsy:
	begin
	  get_token;
	  put_token;
	  no_break_flag := false;
	  break_line;
	end;

      others:
	begin
	  get_token;
	  put_token;
	  if this_token = externalsy then
	    ext_flag := true
	  else if this_token in [forwardsy,externsy] then begin
	    if proc_level <> 0 then proc_level := proc_level - 1;
	    if proc_level <> 0 then pop_until ([many_flag]);
	  end;
	  no_break_flag := true;
	end

    end (* case next_token *);
  end;
  new_line;
  if proc_level <> 0 then
    error ('INCOMPLETE FUNCTION OR PROCEDURE');
end (* format *).
   