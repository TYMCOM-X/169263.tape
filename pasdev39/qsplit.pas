$PAGE qsplitlines
module qsplit
  options special;
public function qsplitlines			(* split selected lines into smaller ones *)
(	var buffer: qbuffer;			(* working buffer *)
	lineno: qlineno;			(* line to split *)
	pat: spattern;				(* where to split each line *)
	opts: split_opt_set;			(* various action-controllers *)
	var cnt: qlineno;			(* number of splits done, i.e. number of new lines *)
	var err: qerrcode			(* error report *)
		): boolean;			(* set true if a match for pat found *)

  function numtochar (num: qlineno): qstring;
  var value: qlineno;
  begin
    numtochar := '';
    value := num;
    repeat
      numtochar := chr (ord ('0') + (value mod 10)) || numtochar;
      value := value div 10
    until value = 0
  end;						(* numtochar *)

label 1;

var
  pos,
  len:		qstringidx;			(* for SPATMATCH *)
  source:	qstring;			(* text of lines to be split *)
  tempstr:	qstring;			(* temporary for building lines *)
  doit,
  didit,
  numberit:	boolean;			(* conditionals *)
  newlineno:	qlineno;			(* line counters *)
  idx:		qstringidx;			(* position at which to search for pat *)

begin
  cnt := 0;
  err := qok;
  didit := false;
  qsplitlines := false;
  newlineno := lineno;
  idx := 1;
  source := qgetline (buffer, lineno, err);
  if err <> qok then return;
  numberit := (number_splitop in opts);
  loop
    tempstr := substr (source, idx);
    doit := spatmatch (tempstr, pat, pos, len, err);
  exit if (not doit) or (err <> qok);
    qsplitlines := true;
    if numberit
      then begin
	 ttwrite (numtochar (lineno));
	numberit := false
      end;
    if confirm_splitop in opts
      then begin
        ttwrite (substr (source, 1, idx + pos - 1 - 1));
        ttwrite ('\');
        ttwrite (substr (source, idx + pos - 1, len));
        ttwtln;
	doit := query ('OK')
      end;
    if doit
      then begin

	(* we take care here to complete this single split operation, so
	   that in the event of a escape being issued to the next
	   confirmation prompt, everything is as it should be up to the
	   point that the escape is issued *)

	if delete_splitop in opts
	  then tempstr := substr (source, 1, idx + pos - 1 - 1)
	  else tempstr := substr (source, 1, idx + pos - 1 + len - 1);
	source := substr (source, idx + pos - 1 + len);
	qaddline (buffer, newlineno, source, err);
	if err <> qok then return;
	qmodline (buffer, newlineno, tempstr, err);
	if err <> qok then return;
	didit := true;
	newlineno := newlineno + 1;
	cnt := cnt + 1;
	idx := 1
      end
      else idx := idx + pos - 1 + len;
    if not ((all_splitop in opts) and (pat.stype in [simple, token]))
      then goto 1
  end;						(* loop *)
1:
  if (print_splitop in opts) and didit
    then termlstlns (buffer, lineno, newlineno, true, false, true, err);
end.						(* qsplitlines *)
