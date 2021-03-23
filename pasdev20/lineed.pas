$PAGE INCLUDES
$INCLUDE QSTR.TYP
$PAGE declarations

(*Symbols for all interesting characters*)

const
  cntrla = chr(1);	cntrlb = chr(2);	cntrlc = chr(3);
  cntrld = chr(4);	cntrle = chr(5);	cntrlf = chr(6);
  cntrlg = chr(7);	cntrlh = chr(10b);	tab = chr(11b);
  lf = chr(12b);	cntrlk = chr(13b);	cntrll = chr(14b);
  cr = chr(15b);	cntrln = chr(16b);	cntrlo = chr(17b);
  cntrlp = chr(20b);	cntrlq = chr(21b);	cntrlr = chr(22b);
  cntrls = chr(23b);	cntrlt = chr(24b);	cntrlu = chr(25b);
  cntrlv = chr(26b);	cntrlw = chr(27b);	cntrlx = chr(30b);
  cntrly = chr(31b);	cntrlz = chr(32b);	escape = chr(33b);
  cntrlshftl = chr(34b);cntrlshftm = chr(35b);	cntrlshftn = chr(36b);
  cntrlshfto = chr(37b);delete = chr(177b);	null = chr(0);

type
  cmndchars = cntrla..escape;			(*Spans range of characters which may be
						  commands, except for DELETE*)

(* General classifications of characters in CMNDCHARS *)

type ctypes = (
  notcmnd,					(*Normal character*)
  editprev,					(*Command affecting previous line*)
  editnew,					(*Command affecting new line, i.e., "normal"
						  delete char or line editing characters*)
  editfin);					(*Command terminating editing mode*)

(*Mapping arrays for command characters.  CT contains type, CA, whether
  or not command requires argument character.  Note that CA will never
  be accessed unless CT indicates character is command, hence, entries
  which are not commands are not initialized.  Access to these arrays
  is mediated by routines CMNDTYPE and CMNDHASARG, to handle DELETE.*)

type
  cttype = array[cmndchars] of ctypes;
  catype = array[cmndchars] of boolean;

const ct: cttype :=
     (	editnew, editprev, editprev, editprev, editprev,
	editprev, editfin, editprev, notcmnd, editfin,
	notcmnd, notcmnd, editfin, editprev, editprev,
	editprev, editnew, editprev, editprev, notcmnd,
	editnew, editprev, notcmnd, editprev, editprev,
	editprev, editfin  );

const ca: catype :=
      (	false, false, false, false, false,
	false, false, false, false, false,
	false, false, false, false, true,
	true, false, false, false, false,
	false, true, false, true, true,
	true, false  );

(* External routines to perform (character by character) I/O *)

external procedure incio;
external procedure outc(ch: char);
external procedure inc(var ch: char);
external procedure endcio;
external procedure intcio;
$PAGE lineedit
public function lineedit(original: qstring): qstring;	(*The editor*)

var
  prevline,					(*Previous line (being edited against)*)
  newline: qstring;					(*New line being constructed*)

  previdx,					(*Index of current character in PREVLINE
						  which has not yet been skipped or copied*)
  prevposn,					(*Current character "position" in PREVLINE,
						  which may be different than PREVIDX due
						  to TABs in previous line*)
  outposn: 0..4095;				(*Current output character position*)

  deletion_limit: qstringidx;			(* Limit of character/line erase processing, used to
						   limit action when inserting with control-E *)

  (* Array parallel to PREVLINE, giving position of character in line *)

  prevchposn: array[1..qstringlen] of qstringidx;

  (* Variables used in main command loop *)

  cmndch,					(*Current command character*)
  argch: char;					(*Its argument character, if it has one*)
  done: boolean;				(*Control variable of edit loop*)

  (* Routines to access mapping arrays *)

  function cmndtype(ch: char): ctypes;		(*Returns character type*)
  begin
    if (ch>=minimum(cmndchars)) and (ch<=maximum(cmndchars)) then
      cmndtype:= ct[ch]
    else if ch=delete then cmndtype:= editnew
    else cmndtype:= notcmnd
  end (*CMNDTYPE*);

  function cmndhasarg(ch: char): boolean;	(*TRUE if argument char required*)
  begin
    if ch=delete then cmndhasarg:= false
    else
      cmndhasarg:= (cmndtype(ch)<>notcmnd) andif ca[ch]
  end (*CMNDHASARG*);

  (* Utility routine to determine next tab position *)

  function tabpad(currposn: qstringidx): qstringidx;	(*Returns # chars to position*)
  begin
    if (currposn mod 8) = 0 then tabpad:= 1
    else tabpad:= 9 - (currposn mod 8)
  end (*TABPAD*);

  (*Teletype output primitives*)

  procedure error;				(*Signal an error*)
  begin
    outc(cntrlg)				(*No affect on OUTPOSN*)
  end (*ERROR*);

  procedure echo(s: qstring);			(*Echo character(s) to teletype*)
    var i, chidx: qstringidx; ch: char;
  begin
    for chidx:= 1 to length(s) do begin
      ch:= s[chidx];
      case ch of

      cntrla..cntrlf,cntrlh,cntrlk,cntrll,cntrln..cntrlz: begin
	outc('&'); outc(chr(ord(ch)+ord('A')-ord(cntrla)));
	outposn:= outposn+2
      end;

      cntrlshftl..cntrlshfto: begin		(*Echo for TI-735 instead of ASR-33*)
	outc('&'); outc(chr(ord(ch)+ord('\')-ord(cntrlshftl)));
	outposn:= outposn+2
      end;

      tab: begin
	for i:= 1 to tabpad(outposn) do outc(' ');
	outposn:= outposn + tabpad(outposn)
      end;

      cr: begin					(*This is only EDITFIN character which should be echoed*)
	outc(cr);
	outposn:= 1				(*and reset the carraige position*)
      end;

      lf,escape,null,cntrlg: begin		(*type obnoxious message on bad chars*)
	outc(cntrlg); outc('?'); outc(cntrlg)
      end;

      others: begin
	outc(ch); outposn:= outposn+1
      end

      end					(*CASE*)
    end (*FOR*)
  end (*ECHO*);

  (*Routine to setup for previous line editing*)

  procedure setprev(s: qstring);
    var curridx, currposn: qstringidx;
  begin
    prevline := s;
    currposn:= 1;				(*Set up PREVCHPOSN array*)
    for curridx:= 1 to length(prevline) do begin
      prevchposn[curridx]:= currposn;
      if prevline[curridx]=tab then
	currposn:= currposn+tabpad(currposn)
      else currposn:= currposn+1
    end;
    prevposn:= 1;
    previdx:= 1
  end (*SETPREV*);

	(* Now for the routines which help with the editing process *)

(* Routine to add a character to newline.  Aborts if newline is overflowed. *)

procedure concat (ch: char; echofg: boolean);
 begin
  if length (newline) = upperbound (newline)
    then error
    else begin
      newline := newline || ch;
      if echofg then echo (ch)
    end
 end;


  (* Routine to add character (entered directly from teletype) to NEWLINE *)

  procedure appnew(ch: char);
  begin
    concat (ch, true);
    if previdx <= length (prevline) then begin
      if ch=tab then prevposn:= prevposn+tabpad(prevposn)
      else prevposn:= prevposn+1;
  
      (*Now for the sly part.  Search for next character in PREVLINE whose
        position is greater than the one we just computed.  This allows
        multiple characters to be bypassed by entering a TAB, and also
        allows the position to be in the middle of a tab.  However, do
        not go beyond the carraige return at the end of the line.*)
  
      while (previdx<length(prevline)) andif (prevposn>=prevchposn[previdx+1]) do
        previdx:= previdx+1
    end
  end (*APPNEW*);

  (*Routine to search for character in previous line, starting at current
    index.  Returns index if found, zero if not.*)

  function find(ch: char): qstringidx;
  begin
    find:= previdx;
    while find <= length (prevline) do begin
      if prevline[find] = ch then return;
      find := find + 1
    end;
    if ch <> cr then find := 0
  end (*FIND*);

  (*Routine to copy characters from previous line into new line through
    passed index - 1.  Positions to passed index afterwards.  If passed
    implies nothing to be copied, routine prints error message. If CR
    is copied, DONE is set.*)

  procedure copyupto(nextidx: qstringidx; echofg: boolean);
    var ch: char;
  begin
    if nextidx<=previdx then error
    else begin
      while previdx < nextidx do begin
	if previdx <= length (prevline) then begin			(*Do not echo or concatenate CR's!!!*)
	  ch := prevline[previdx];
	  concat (ch, echofg);
	end;
	previdx:= previdx+1
      end;
      if previdx <= length (prevline) then prevposn := prevchposn[previdx]
    end
  end (*COPYUPTO*);

  (*Routine to skip characters in previous line through passed index - 1.*)

  procedure delupto(nextidx: qstringidx);
    var i: qstringidx;
  begin
    if (nextidx<previdx) or (previdx > length (prevline)) then error
    else begin
      while previdx < nextidx do begin
	if prevline[previdx]=tab then
	for i:= 1 to tabpad(outposn) do echo('%')

	(* Following test assumes line does not contain invalid control chars *)

	else if prevline[previdx] < ' ' then echo('%%')
	else echo('%');
	previdx:= previdx+1
      end;
      if previdx <= length (prevline) then prevposn:= prevchposn[previdx]
    end
  end (*DELUPTO*);

  (*Routine to perform "normal" editing according to passed character
    on passed line.*)

  function normaledit(ch: char): boolean;
  begin
    if cmndtype(ch)=editnew then begin
      if (ch=cntrla) or (ch=delete) then begin
	if length(newline)=deletion_limit then error
	else begin
	  newline:= substr(newline,1,length(newline)-1);
	  echo('^')
	end
      end
      else begin
	echo('_'); echo(cr);
	newline:= substr (newline, 1, deletion_limit)
      end;
      normaledit:= true
    end
    else normaledit:= false
  end (*NORMALEDIT*);

  (*Routine covering character input to perform any special processing*)

  procedure nextch(var ch: char);
    label 1;
  begin
  1:inc(ch);
    if ch=cntrlc then begin
      endcio;
      open(tty); rewrite(tty);
      intcio;
      close(tty); close(ttyoutput);
      incio;
      goto 1
    end
  end (*NEXTCH*);


begin						(*The main control character editor package*)

  close(tty); close(ttyoutput);			(*Initialize char by char input*)
  incio;

  newline:= '';					(*Initialize for the main loop*)
  deletion_limit := 0;
  setprev(original);
  outposn:= 1;
  done:= false;

  while not done do begin			(*The main editing loop*)

    nextch(cmndch);				(*Get a new character*)

    (*Perform normal editing (delete char or line) now.  This is
      done here primarily to avoid having DELETE case in command
      CASE statement, as this would make branch table too large.*)

    while normaledit(cmndch) do begin
      if (cmndch=cntrlu) or (cmndch=cntrlq) then begin
	prevposn:= 1;				(*Reset for edit of previous line*)
	previdx:= 1
      end;
      nextch(cmndch)
    end;

    (*Get argument character if command has one*)

    if cmndhasarg(cmndch) then begin
      nextch(argch);
      if (cmndtype(argch)=editfin) or (argch=null) then
	cmndch:= null				(*EOLN or NULL character bad, kill command*)
    end;

    case cmndch of				(*Process command characters*)

	(* Bad characters come here *)

    null, cntrlg, escape, lf: error;

	(* Commands to copy from previous line.
	   DONE set by COPYUPTO if CR copied. *)

    cntrlc, cntrln: if previdx >= length (prevline)
		      then done := true
		      else copyupto (previdx + 1, true);
    cntrlo, cntrly: copyupto(find(argch),true);
    cntrlz: copyupto(find(argch)+1,true);
    cntrlb, cntrlh: copyupto(length(prevline)+1,true);
    cntrld: begin copyupto (length(prevline)+1,true); done := true end;
    cntrlf: copyupto(find(cr)+1,false);		(*No echo*)

	(* Commands to delete (skip) characters in previous line. *)

    cntrls: delupto(previdx+1);
    cntrlp: delupto(find(argch));
    cntrlx: delupto(find(argch)+1);

	(* Command to print edited line thus far *)

    cntrlr: echo(cr || newline);

	(* Command to insert text into new line without affecting previous *)

    cntrle: begin
      echo ('<'); deletion_limit := length (newline);
      nextch(cmndch);
      while cmndch<>cntrle do begin
	while normaledit(cmndch) do begin
	  if (cmndch=cntrlq) or (cmndch=cntrlu) then
	    echo(newline || '<');
	  nextch(cmndch)
	end;
	if (cmndtype(cmndch)=editfin) or (cmndch=null) then error
	else if cmndtype(cmndch)=notcmnd then begin
	  concat (cmndch, true);
	end
	else if cmndch=cntrlv then begin
	  nextch(argch);
	  if (cmndtype(argch)=editfin) or (argch=null) then error
	  else begin
	    concat (argch, true);
	  end
	end
	else if cmndch<>cntrle then error;
	if cmndch<>cntrle then nextch(cmndch)
      end;
      echo('>'); deletion_limit := 0;
    end;

	(* Explicit termination of edit mode *)

    cr: done:= true;

	(* Verbatim addition of character to new line *)

    cntrlv: appnew(argch);

	(* All other characters are merely added to new line *)

    others: appnew(cmndch)

    end						(*CASE*)
  end (*MAIN LOOP*);

  echo(cr);
  lineedit:= newline;

  endcio;					(* Terminate special input mode *)
  open(tty); rewrite(tty)
end (*LINEEDIT*).
  