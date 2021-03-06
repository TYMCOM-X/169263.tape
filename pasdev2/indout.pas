module INDOUT;
$TITLE INDOUT Outputting module for INDENT
$HEADER INDOUT.HDR
$PAGE GLOBALS and other declarations

$INCLUDE INDENT.TYP

$INCLUDE INDOUT.INC

const
  PRI_MAX := 3;				(* number of levels of priority *)

var
  OUTIND,				(* output indent. of line *)
  EXTRA_IND,				(* to indent split-off lines a quan. *)
  COM_ADJUST,				(* adjustment count for CONT_COMs *)
  LSTART: LINE_IDX;			(* array index of first token *)
  COM_END,				(* to remember end of comments following
					   splitpoints (below) *)
  SPLIT: array[1..PRI_MAX] of LINE_IDX;	(* for split point indices by priority *)
  COM_FOL: array[1..PRI_MAX]of boolean;	(* true if comment follows splitpoint *)

  NEVERBEFORES: SYMSET;			(* set of tokens before which we
					   never split a line (can change) *)

const
  PREFERRED: SYMSET :=			(* set of tokens marking 'preferred'
					split points *)
    [SEMISY, ASSIGNSY, LOGCONSY, THENSY, DOSY];

$PAGE WR_LINE helper -- does comment alignment
procedure WR_LINE ( LIN_END: LINE_IDX;
		    COM_END: LINE_IDX;
		    TERMINALCOMMENT: boolean);

(* WR_LINE writes an output line in the packed array LINE going from
   LSTART to LIN_END. if TERMINALCOMMENT is true, WR_LINE assumes that
   a terminal comment starts at LIN_END, and extends to COM_END.
   This comment is aligned to COM_COL if ALIGN_COM is true (alignment 
   of comments has been specified by user). *)

var
  I: LINE_IDX;				(* loop temp *)

  begin
  for I := 1 to (OUTIND + EXTRA_IND) div 8 do (* write tabs for indentation *)
    write(OUTFILE, chr(9) );
  write (OUTFILE, ' ': (OUTIND + EXTRA_IND) mod 8); (* and spaces for remainder *)

  write (OUTFILE, substr(LINE, LSTART, LIN_END - LSTART));

  (* text of line now written out -- write spaces before comment if needed *)

  if TERMINALCOMMENT and ALIGN_COM then
    begin
    if (THIS_SYM <> CONT_COM) and	(* CONT_COM and CLOSE_COM had ADJ... *)
       (THIS_SYM <> CLOSE_COM) then	(* set by previous OPEN_COM *)
      COM_ADJUST := max(0,COM_COL - LIN_END + LSTART - OUTIND - EXTRA_IND);
    write (OUTFILE, ' ': COM_ADJUST)	(* write them out *)
    end;

  if TERMINALCOMMENT then
    write (OUTFILE, substr(LINE,LIN_END, COM_END - LIN_END + 1));

    (* now line is written out. Tack on a writeln *)

  writeln (OUTFILE)
  end (* procedure WR_LINE *);
$PAGE INDOUT the WRITER coroutine entry point
public procedure INDOUT;

(* INDOUT is the coroutine living in the environment WRITER. It writes
   output lines as they are developed by the other coroutines, and
   performs the following formatting functions:
      Splits lines for length
      Aligns comments if the option has been selected.

   It is the responsibility of the caller (the 'wrapper' routine or
   its caller) to open and close the output file. INDOUT will never
   detach except after its initial creation. *)


var
  I: LINE_IDX;				(* temp for for loops, etc. *)
  SATMP: SAPTR;				(* temp for walking comment list *)

label 2;				(* permits jump to comment catcher *)

  begin
  for I := 1 to PRI_MAX do		(* initialize split variables *)
    begin
    SPLIT[I] := 0;
    COM_END[I] := 0;
    COM_FOL[I] := false
    end;
  EXTRA_IND := 0;
  NEVERBEFORES := [COMMASY,SEMISY,COLONSY,OPEN_COM];

  detach;

  (* now we have the first token *)

  OUTIND := OUT_IND;
  LSTART := CURSTART;
  goto 2;				(* to catch any starting comments *)

    loop
      if THIS_SYM in [LPARENSY,RPARENSY] then
	NEVERBEFORES := NEVERBEFORES + [THIS_SYM]
      else NEVERBEFORES := NEVERBEFORES - [LPARENSY,RPARENSY];

      if not (NEXT_SYM in NEVERBEFORES) (* we can split before next one *)
	and not ((THIS_SYM in		(* and if we're not looking at...*)
		  [OPEN_COM,CLOSE_COM,CONT_COM])
	    and (EOL_CT > 0)) then	  (* an end-of-line imbedded comment *)
	begin				(* we are allowed to split after this *)
	if CURSTART - LSTART + TOK_LEN + OUTIND  + EXTRA_IND >= OUT_MAX then
	  begin				(* writing this token will exceed len *)

	  if LSTART <> CURSTART then	(* if not single token on line *)
	    for I := 1 to PRI_MAX do
	      exit if SPLIT[I] > 0 do	(* pick out highest pri. split pt. *)
		begin
		WR_LINE (SPLIT[I],COM_END[I],COM_FOL[I]);
		EXTRA_IND := QUANTUM;
		LSTART := verify (substr(LINE,SPLIT[I],CURSTART - SPLIT[I]),
				  [' '], CURSTART-SPLIT[I]+1) + SPLIT[I] - 1
		end

	  else begin			(* single token, too long *)
	    WR_LINE (CURSTART + TOK_LEN, 0, false);
	    EOL_CT := max (EOL_CT - 1, 0);
	    EXTRA_IND := 0;
	    LSTART := 0
	    end;

	  for I := 1 to PRI_MAX do	(* reset split info *)
	    begin
	    SPLIT[I] := 0;
	    COM_FOL[I] := false
	    end
	  end (* output line too long *);

	if LSTART <> 0 then
	  begin				(* didn't write THIS_SYM *)
	  if THIS_SYM in PREFERRED then (* accumulate split points *)
	    I := 1			(* by getting priority of THIS_SYM *)
	  else if CUREND-CURSTART > TOK_LEN - 1 then
	    I := 2			(* blanks follow, higher than... *)
	  else I := 3;			(* token with no blanks *)
	  SPLIT[I] := CURSTART + TOK_LEN;
	  if (NEXT_SYM = OPEN_COM) and (EOL_CT = 0) then
	    begin
	    SPLIT[I] := CUREND;		(* we know line won't be split here *)
	    COM_FOL[I] := true;
	    resume(READER);		(* get to comment *)
	    COM_END[I] := CURSTART + TOK_LEN
	    end
	  end
	end;

      if EOL_CT > 0 then
	begin
	if LSTART <> 0 then
	  begin				(* must write out rest of line *)
	  if THIS_SYM in [OPEN_COM,CONT_COM,CLOSE_COM] then
	    loop			(* write out comment and any followers*)
	      WR_LINE (CURSTART, CUREND, true);
	    exit if not (NEXT_SYM in [CONT_COM,CLOSE_COM]);
	      resume (READER);		(* still a comment coming, get it *)
	      OUTIND := OUT_IND;	(* get proper indentation *)
	      LSTART := CURSTART	(* pony up start of line *)
	    end
	  else WR_LINE (CUREND + 1, 0, false);
	  EOL_CT := EOL_CT - 1;
	  LSTART := 0
	  end;
	for I := 1 to EOL_CT do
	  writeln(OUTFILE);
	for I := 1 to PRI_MAX do
	  begin				(* reinit. split points *)
	  SPLIT[I] := 0;
	  COM_FOL[I] := false
	  end
	end;

      resume (READER);

      2: if SAFIRST <> nil then		(* there are comments to write *)
	loop
	  with SAFIRST^ do
	    begin			(* write out the line *)
	    for I := 1 to (COM_IND + SAOFFSET) div 8 do
	      write (OUTFILE, chr(9));
	    write (OUTFILE, ' ': (COM_IND + SAOFFSET) mod 8);
	    write (OUTFILE, substr (SATEXT, 1, SALEN));
	    for I := 1 to SAEOL do
	      writeln (OUTFILE)
	    end;
	  SATMP := SAFIRST^.SANEXT;	(* walk down to next record *)
	  dispose (SAFIRST);
	exit if SATMP = nil do
	  begin				(* we're done, reset pointers *)
	  SAFIRST := nil;
	  SALAST := nil
	  end;

	  SAFIRST := SATMP		(* else get next and go again *)
	end;

      if LSTART = 0 then
	begin
	OUTIND := OUT_IND;
	LSTART := CURSTART;
	EXTRA_IND := 0
	end
    end (* loop *)
  end (* procedure INDOUT and module *).
  