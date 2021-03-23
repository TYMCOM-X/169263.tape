$title pasopt - PASCAL compiler option and support routines
$OPTIONS SPECIAL, NOCHECK

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASLEX.INC

$INCLUDE CMDUTL.TYP[,320220]
$INCLUDE LOOKUP.TYP[,320220]
$SYSTEM DTIME.TYP[,320220]
$SYSTEM DTIME.INC[,320220]
external procedure readfileidentifier (var filnamestr);
external var rfiderror: boolean;
$PAGE declarations
type
    options = (allocopt,checkopt,commentopt,debugopt,defaultopt,disableopt,(*$Y4 DOUBLEOPT, *)enableopt,incoreopt,infoopt,inpopt,
      ka10opt,ki10opt,kl10opt,lengthopt,listopt,mainopt,ncheckopt,ninfoopt,
	nlistopt, nprogopt,nsourceopt,ntraceopt,outpopt,overopt,progopt,
	  sourceopt, specopt,storeopt,terseopt,traceopt,ttyopt,verboseopt,
	    virtualopt,widthopt,xrefopt);


type
    option_list = array[options] of cmdlist;

external function
  lookup_option (cmdline;var cmdlineidx; option_list; options; var options): boolean;

const
    option: option_list := (
    (* allocopt *)('ALLOC',2),
    (* CHECKOPT *)('CHECK',2),
    (* commentopt *)('COMMENT',2),
    (* DEBUGOPT *)('DEBUG',3),
    (* DEFAULTOPT *)('DEFAULT',3),
    (* DISABLEOPT*)('DISABLE',2),
(*$Y4 (*DOUBLEOPT *)('DOUBLE',	*)2),
    (* ENABLEOPT *)('ENABLE',2),
    (* INCOREOPT *)('INCORE',3),
    (* INFOOPT *)('INFO',3),
    (* INPOPT *)('INPUT',3),
    (* KA10OPT *)('KA10',2),
    (* KI10OPT *)('KI10',2),
    (* KL10OPT *)('KL10',2),
    (* LENGTHOPT *)('LENGTH',2),
    (* LISTOPT *)('LIST',2),
    (* MAINOPT *)('MAIN',2),
    (* NCHECKOPT *)('NOCHECK',3),
    (* NINFOOPT *)('NOINFO',3),
    (* NLISTOPT *)('NOLIST',3),
    (* NPROGOPT *)('NOPROGRESS',3),
    (* NSOURCEOPT*)('NOSOURCE',3),
    (* NTRACEOPT*)('NOTRACE',3),
    (* OUTPOPT *)('OUTPUT',3),
    (* OVEROPT *)('OVERLAY',2),
    (* PROGOPT *)('PROGRESS',1),
    (* SOURCEOPT *)('SOURCE',2),
    (* SPECOPT *)('SPECIAL',2),
    (* STOREOPT *)('STORAGE',2),
    (* TERSEOPT *)('TERSE',2),
    (* TRACEOPT *)('TRACE',2),
    (* TTYOPT *)('TTY',2),
    (* VERBOSEOPT*)('VERBOSE',2),
    (* virtualopt *)('VIRTUAL',2),
    (* WIDTHOPT*)('WIDTH',1),
    (* XREFOPT *)('XREF',1));

public var
    idlength: integer;

$PAGE  readidentifier
public procedure readidentifier (var id : alfa);

begin
  idlength := 0;
  id := '          ';
  if ch in alphabetic then begin
    while (ch in lettersdigitsorleftarrow) and not eol do begin
      if idlength < 10 then begin
	idlength := idlength + 1;
	id[idlength] := ch
      end;
      nextch
    end
  end
end;

$PAGE ivalue, skipblanks
public function ivalue: integer;

(* EATS DIGITS *)

var
    i : integer;

begin
  i := 0;
  while (ch in digits) do begin
    i := (i*10) + ord(ch) - ord('0');
    nextch
  end;
  ivalue := i
end;

public procedure skipblanks;

begin
  while (ch = ' ') and not eol do
    nextch
end;

$PAGE  errskip
public procedure errskip (errn: integer);

begin
  error (errn);
  while not eol do
    nextch
end;

$PAGE optionlist
public procedure optionlist;

var
    optionix : options;
    i, lower, upper : integer;
    setting : boolean;
    opt_id: alfa;

begin
  nextch;					(*SKIP BLANK OR '/'  *)
  optline := chcnt;
  while not eol do begin
    skipblanks;
    readidentifier (opt_id);
    idlength := 1;
    if lookup_option (opt_id, idlength, option, maximum (options), optionix) then begin
      case optionix of
	incoreopt: begin
	  if swflag then incore := true;
	end;
	virtualopt: begin
	  if swflag then virtual := true;
	end;
	allocopt: begin
	  skipblanks;
	  allocation := ivalue;
	  if (allocation > max_allocation) orif (allocation < min_allocation) then begin
	    (* errskip ? *)
	    allocation := min_allocation;
	  end;
	end;
	commentopt: begin
	  pass_percent := false;
	end;
	checkopt,ncheckopt:
	  runtmcheck := optionix = checkopt;
	debugopt: begin
	  if swflag then begin
	    debug := true;
	  end;
	end;
	defaultopt: begin
	  skipblanks;
	  if ch in ['=', ':'] then
	    nextch;	(* skip separator *)
	  src_defaults := 0;	(* start with no active search directories *)
	  loop
	    skipblanks;
	  exit if eol or (ch = ',');
	  exit if src_defaults = src_max do
	    errskip (567);
	    readfileidentifier (src_directories[src_defaults + 1]);
	  exit if rfiderror do
	    errskip (566);
	    src_defaults := src_defaults + 1;
	  end;
	end;
	infoopt,ninfoopt:
	  listinfo := optionix = infoopt;
	inpopt:
	  if swflag then
	    ininuse := true;
	listopt,nlistopt:
	  listcode := optionix = listopt;
(*$Y4	    DOUBLEOPT: IF SWFLAG THEN
	  DBLREAL := TRUE;	*)
	enableopt,disableopt: begin
	  skipblanks;
	  setting := optionix = enableopt;
	  if ch = 'A' then begin
	    readidentifier(id);
	    if id = 'ALL       ' then
	      for i := 0 to 99 do
		compsw[i] := setting
	    else if id = 'ANY       ' then
	      debugcomments := setting
	    else
	      errskip (203)
	  end
	  else if (ch = ',') or eol then
	    compsw[0] := setting
	  else if ch = '(' then begin
	    while (ch <> ')') and not eol do begin
	      nextch;
	      lower := 0;
	      skipblanks;
	      if ch in digits then
		lower := ivalue
	      else if not (ch in [',','.',')']) then
		errskip (203);
	      upper := lower;
	      skipblanks;
	      if ch = '.' then begin
		nextch;
		if ch = '.' then begin
		  nextch;
		  skipblanks;
		  if ch in digits then
		    upper := ivalue
		  else
		    errskip (255)
		end
		else
		  errskip (175)
	      end
	      else
		upper := lower;
	      if lower > upper then begin
		i := lower;
		lower := upper;
		upper := i
	      end;
	      if (lower < 0) or (upper > 99) then
		errskip (223)
	      else if not eol then		(*IF NO ERRORS*)
		for i := lower to upper do
		  compsw[i] := setting
	    end;
	    if ch = ')' then
	      nextch
	  end
	end;
	ka10opt,ki10opt,kl10opt:
	  if swflag then begin
	    kl10sw := optionix = kl10opt;
	    dmove := optionix <> ka10opt
	  end;
	mainopt:
	  if swflag then
	    main := true;
	outpopt:
	  if swflag then
	    outinuse := true;
	overopt:
	  if swflag then
	    overlaysw := true;
	progopt,nprogopt:
	  progresssw := optionix = progopt;
	sourceopt,nsourceopt:
	  if glistsource then
	    listsource := optionix = sourceopt;
	specopt:
	  specialsw := true;
	terseopt,verboseopt:
	  shorterrors := optionix = terseopt;
	traceopt,ntraceopt:
	  tracer := optionix = traceopt;
	xrefopt: begin
	  xref := true;
	end;
	ttyopt:
	  if swflag then
	    ttyinuse := true;
	storeopt, lengthopt, widthopt: begin
	  skipblanks;
	  if ch = '=' then begin
	    nextch;
	    skipblanks;
	  end;
	  if ch in digits then begin
	    i := ivalue;
	    if optionix = storeopt then
	      corsiz := i
	    else if optionix = lengthopt then begin
	      if (i>=minlength) orif (i=0) then
		pagelength := i
	      else
		errskip (221);
	    end
	    else if (i<minwidth) orif (i>maxwidth) then
	      errskip (222)
	    else
	      linewidth := i;
	  end;
	end
      end					(*CASE*)
    end						(*IF lookup ok*)
    else if opt_id <> ' ' then
      errskip (219)
    else if (ch <> ',') and not eol then
      errskip (203);
      (*READID STUCK ON NON-ALPHABETIC CHAR*)
    if (ch = ',') orif (ch = '/') then
      ch := ' '					(*SKIP COMMA OR SLASH *)
  end;						(*WHILE NOT EOL*)
  optlen := chcnt-optline+1;
  if not incompilation then
    chcnt := 0
end;						(*OPTIONLIST*)

$PAGE width
public function width (val: integer): integer;

begin
  width := 0;
  repeat
    val := val div 10;
    width := width + 1
  until val = 0
end;

$PAGE entitle
public procedure entitle (forced: boolean);

begin
  with filedata[inclnest] do begin
    if not forced then begin
    if incompilation then
      pagecnt := pagecnt + 1;
    localline := 1;
    lastline := 0;
    end;
    if listsource then begin
      write ('Page ',pagecnt:width(pagecnt));
      write (' ':3,day:9,' ':3);
      write (' ':3,dc_ext(daytime));
      write (' ':4,header,dateofcompilation,' ':4,'File # ');
      write (fileid:width(fileid),': ');
      if source = reading then
	writeln (srcnam[filenum-1])
      else
	writeln (inclname[inclnest]);
      linesthispage := 2;			(*HEADER LINE AND BLANK LINE*)
      if titlelength > 0 then begin
	writeln(' ':15,title[1:titlelength]);
	linesthispage := linesthispage + 1
      end;
      if lensubtitle > 0 then begin
	writeln (' ':15,subtitle[1:lensubtitle]);
	linesthispage := linesthispage + 1
      end;
      writeln
    end
  end
end.
 