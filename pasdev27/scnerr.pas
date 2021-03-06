$TITLE SCNERR -- SCANNR Error Module

module scnerr;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N E R R                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  1 October 1979
     
     PURPOSE:  This module handles  the  recording  and  printing  of
        error messages.
     
     ENTRY POINTS:
     
        ERR_INIT    is called to initialize the error module.
     
        ERR_LOC (line, column, message)
                    is  called  with  the  location of an error and a
                    message describing it.  It adds this  information
                    to an internal error list.
     
        ERR_TOKEN (token, message)
                    is  called  with  the  token  on  which  an error
                    occurred and a message  describing  it.  It  adds
                    the   token  location  and  the  message  to  its
                    internal error list.
     
        ERR_PRINT (line)
                    is called with a line number.  If there  are  any
                    errors  on  the specified line, it will print the
                    error messages on the terminal and on the listing
                    file.
     
        ERR_COUNT   is  the  number  of error messages that have been
                    recorded since ErrInit was called.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE scannr.typ
$INCLUDE scnlit.typ
$INCLUDE scntok.typ

$INCLUDE scnlst

public var err_count: number;


type
    err_ptr = ^ err_record;

    err_record = record
	next: err_ptr;
	err_line: number;
	err_column: line_index;
	err_message: packed array [1..*] of char;
    end;

var errors: err_ptr;
$PAGE err_init

(*  ERR INIT initializes the error module.  *)

public procedure err_init;

begin
  errors := nil;
  err_count := 0;
end;
$PAGE err_loc

(*  ERR LOC records an error message with a specified line and column number.  *)

public procedure err_loc ( line_no: number; column_no: line_index;
			   msg: parm_string );

var err, e1, e2: err_ptr;

begin
  err_count := err_count + 1;

  new (err, length (msg));
  with err^ do begin
    err_line := line_no;
    err_column := column_no;
    err_message := msg;
  end;

  e1 := errors;
  e2 := nil;
  while (e1 <> nil) andif
	( (e1^.err_line < line_no) or
	  ( (e1^.err_line = line_no) and (e1^.err_column <= column_no) ) ) do begin
    e2 := e1;
    e1 := e1^.next;
  end;

  if e2 = nil
    then errors := err
    else e2^.next := err;
  err^.next := e1;
end (* err_loc *);
$PAGE err_token

(*  ERR TOKEN records an error message with the line and column number of a
    specified token.  *)

public procedure err_token ( tok: sym_value; msg: parm_string );

begin
  err_loc (tok.line_no, tok.column_no, msg);
end;
$PAGE err_print

(*  ERR PRINT prints all the error messages pertaining to a specified line.  *)

public procedure err_print ( line_no: number; line: parm_string );

var e, e_last: err_ptr;
    tag_line: str_ptr;

begin
  if (errors = nil) orif (errors^.err_line <> line_no) then
    return; (* <---- No errors on this line. *)

  echo_line; (* Print the bad line on the terminal. *)

  e_last := errors^.next; (* Find the last error on this line. *)
  e := errors;
  while (e_last <> nil) andif (e_last^.err_line = line_no) do begin
    e := e_last;
    e_last := e_last^.next;
  end;

  new (tag_line, e^.err_column); (* Print the marker line. *)
  tag_line^ := '';
  e := errors;
  while e <> e_last do begin
    if e^.err_column <> 0 then
      tag_line^[e^.err_column] := '^';
    e := e^.next;
  end;
  prt_message (tag_line^);
  dispose (tag_line);

  while errors <> e_last do begin (* Print the error messages. *)
    prt_message (errors^.err_message);
    e := errors^.next;
    dispose (errors);
    errors := e;
  end;

  prt_message ('');
end (* err_print *).
    