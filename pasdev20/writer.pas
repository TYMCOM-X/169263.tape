$LENGTH 43
                  (*$&+*********************
                   *                       *
                   * SCRIBE-10 LINE WRITER *
                   *                       *
                   * PERFORMS  UNDERLINING *
                   *                       *
		   *  AND OTHER  CHARACTER *
		   *			   *
		   *       PROCESSING      *
		   *                       *
                   *************************)
 
module writer;

$include stdtyp.inc

type script_char = array[1..2] of char; (* for commands controlling
					   subscripting/superscripting *)

const (* various and sundry special characters *)
  esc := chr(33b); (* diablo command character *)
  supchar: script_char := ( esc, 'D' ); (* to superscript *)
  subchar: script_char := ( esc, 'U' ); (* to subscript *)
  cr := chr(15b); (* to return carriage *)
  bs := chr(10b); (* to backspace one position *)
  sp := ' '; (* to advance one position *)
  ul := '_'; (* the under/overline character *)
  lf := chr(12b); (* to advance carriage one line *)
  revlf: script_char := ( esc, lf );  (* to move carriage up one line *)
$PAGE wrtline -- heading and local variables of wrtline

public procedure wrtline( var f: text; (* the output file *)
			   var inline: line; (* the input line *)
			   innum: lineptr; (* length of input line *)
			   dounder: boolean; (* flags $underline on/off *)
			   backspace: boolean; (* flags /b switch *)
			   map: trans_table; (* $translate table *)
			   terminal: terminal_type ); (* /t switch setting *)

  var
    outbuf, (* character buffer for line text *)
    underbuf, (* character buffer for underline text *)
    overbuf: array[1..maxlinelen] of char; (* for overline text *)
  static var (* for efficiency of access *)
    undernum, overnum: lineptr; (* lengths of the buffers *)
    inptr: lineptr; (* pointer to current character in inline *)
    ch: chtype; (* for holding input characters *)
    super_or_sub, (* flags any super/subscripting in input *)
    special_ter: boolean; (* flags diablo terminal *)
    scrstate: (normal, super, sub); (* for remembering carriage state *)
$PAGE process_ch -- character writer for subscripting and/or superscripting

  procedure rest_state;

  (* This routine returns the carriage to its normal position. *)

    begin
    case scrstate of
      super: write(f,subchar);
      sub: write(f,supchar)
    end (* case *);
    scrstate := normal
    end (*rest_state*);


  procedure process_ch ( ch: char;  (* the character to write *)
			 attr: attribute_set );	(* and its attributes *)

  (* This routine writes a character, performing any necessary
     subscripting and/or superscripting *)

  begin
  if ch <> sp then  (* and don't worry about mode for SP *)
    begin	    (* change mode if necessary *)
    if [superscript,subscript] <= attr then
      rest_state    (* super and sub is normal *)
    else if superscript in attr then
      begin
      if scrstate <> super then
	begin	    (* must change to super *)
	rest_state;
	write(f, supchar);
	scrstate := super
	end
      end
    else if subscript in attr then
      begin	    (* same deal for subscript *)
      if scrstate <> sub then
	begin
	rest_state;
	write(f, subchar);
	scrstate := sub
	end
      end
    else rest_state (* neither sub or super, goto normal *)
    end (* if special processing needed *);

  write(f,ch)
  end (*process_ch*);
$PAGE do_backspace -- performs underlining by backspacing

  procedure write_ch( ch: char; (* character to write *)
		      attr: attribute_set ); (* its attributes (ignored) *)

  (* This procedure is passed to do_backspace as its output routine
     if no super or subscripting is to be performed.  It merely writes
     the character passed. *)

  begin
    write(f, ch)
  end (*write_ch*);


  type formal = procedure ( char; attribute_set );

  procedure do_backspace( writer: formal );

  (* This procedure writes the underline line (underbuf) and the
     line text (outbuf) by backspacing to print the underlining.
     It uses a passed routine to output characters, which performs
     the super and/or subscripting, if any. *)

    var ch, chu: char;
	inptr, index1, index2: lineptr;

  begin
    inptr:= 1;
    ch:= underbuf[1];

    repeat
      index1:= inptr; (* remember index of current character *)
      chu:= ch; (* and its value *)

      (* write characters from outbuf until a character different from
	 the one saved in chu is found in underbuf.  At this point, fall
	 out of loop with its index in inptr *)

      while (chu=ch) and (inptr<=innum) do begin
	writer ( outbuf[inptr], inline[inptr].attr );
	inptr:= inptr + 1;
	ch:= underbuf[inptr]
      end;

      (* if chu indicates underlining is in order, back off to the first
	 character written above (index saved in index1), and then
	 write the underlining. *)

      if chu=ul then begin
	for index2:= index1 to inptr-1 do write(f,bs); (* back up *)
	for index2:= index1 to inptr-1 do
	  writer(underbuf[index2], inline[index2].attr)
      end
    until inptr > innum
  end (*do_backspace*);
$PAGE wrtline_2 -- body of wrtline routine

begin

  if innum<=0 then return; (* a rather expensive nop *)

  undernum:= 0; (* initialize lengths of various buffers *)
  overnum:= 0;
  super_or_sub:= false; (* and the two flags *)
  special_ter := (terminal = diablo);

  (* loop through the input line, and set up outbuf, underbuf, overbuf *)

  for inptr:= 1 to innum do begin
    ch:= inline[inptr]; (* fetch the current input character *)
    outbuf[inptr]:= map[ch.value]; (* perform translation *)

    (* store either a space or an underbar in underbuf *)

    if (ch.value<>sp) andif (underline in ch.attr) (* don't underline spaces *)
    then begin
      underbuf[inptr]:= ul;
      undernum:= inptr (* save highest underlined position *)
    end
    else underbuf[inptr]:= sp;

    (* if terminal is diablo, check for overlining and scripting *)

    if special_ter then begin
      if (ch.value<>sp) andif (overline in ch.attr) then begin
	overbuf[inptr]:= ul;
	overnum:= inptr (* remember highest index here too *)
      end
      else overbuf[inptr]:= sp;
      super_or_sub:= super_or_sub orif
		     ( ([subscript,superscript]*ch.attr) <> [] )
    end
  end (*for*);

  scrstate:= normal; (* assume we are at ground zero *)

  if super_or_sub then begin (* print character by character *)
    if (not dounder) or (undernum=0) then (* no underlining, print text *)
      for inptr:= 1 to innum do process_ch(outbuf[inptr], inline[inptr].attr)
    else if not backspace then begin (* underline normally *)
      for inptr:= 1 to innum do process_ch(outbuf[inptr], inline[inptr].attr);
      write(f,cr); (* return the carriage *)
      for inptr:= 1 to undernum do (* print the underline line *)
	process_ch(underbuf[inptr], inline[inptr].attr)
    end
    else do_backspace( process_ch );
    if overnum>0 then begin (* do the overlining *)
      write(f,cr,revlf); (* position above first character in line *)
      for inptr:= 1 to overnum do (* this we only do by overprinting *)
	process_ch(overbuf[inptr], inline[inptr].attr);
      write(f, lf) (* and reposition carriage *)
    end;
    rest_state (* and finally, return carriage to normal position *)
  end

  else begin (* if not scripting, can print full lines *)
    if (not dounder) or (undernum=0) then
      write(f, outbuf:innum)
    else if not backspace then
      write(f, outbuf:innum, cr, underbuf:undernum)
    else do_backspace( write_ch );

    if overnum>0 then (* overline *)
      write(f, cr, revlf, overbuf:overnum, lf)
  end
end (*wrtline*).
