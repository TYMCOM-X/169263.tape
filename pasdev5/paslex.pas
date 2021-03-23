$title paslex - PASCAL compiler input, listing, error and misc support routines

$OPTIONS SPECIAL, NOCHECK

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASOPT.INC

$INCLUDE PRGDIR.INC[,320220]
$INCLUDE LOOKUP.TYP[,320220]

$INCLUDE PASRW.PAS

$INCLUDE PASCG.INC
$PAGE declarations

external procedure setnest;

external procedure zeronest;

external procedure incnest;

external procedure decnest;

external function seoln: boolean;

external function seof: boolean;

external procedure sread;

external procedure sreadln;

external procedure sgetlinenr (var linenrtype);

external procedure litch;

external procedure nextch;

type
    updateform = (c,d);
    etp = ^ errorupdate;
    errorupdate = packed record
      number: integer;
      next: etp;
      case form: updateform of
	c: (
	  cstring: alfa);
	d: (
	  intval: integer)
    end;

public var
    savedp: boolean;
    saveic: addrrange;
    errorflag: boolean;
    errorinline: boolean;
    err_cnt : integer;
    followerror: boolean;
    source: inputstate;
    inclnest: 0..maxinclusion;			(*NEST LEVEL OF INCLUDE FILES*)
    inclname: array[1..maxinclusion] of filnamestr;   (*NAMES OF INCLUDE FILES*)

    filedata: array[0..maxinclusion] of fileinfo;   (* MISC FILE/LISTING/DEBUG INFO *)
    filecnt: integer;				(* SEQUENTIAL COUNT OF FILES READ *)
    fileic: addrrange;				(* IC OF LAST FILE BLOCK *)
    chcnt : 0..chcntmax;			(* LISTING BUFFERS *)
    buffer: packed array[1..chcntmax] of char;
    errline : packed array[1..chcntmax] of char;

static var
    i: integer;
    comment_set: set of char;
    errmptr: etp;				(* TO LIST OF ERROR UPDATES *)
    errinx: 0..maxerr ;				(* NO OF ERRORS IN CURRENT LINE *)
    errlist: array [1..maxerr] of packed record
      arw: 1..4;
      pos: 1..chcntmax;
      nmr: 1..600;
      tic: char
    end;

static var
    comment: boolean;				(* START OF COMMENT FOUND *)
    linenr: linenrtype;

public var
    pass_percent: boolean;
    nestlv: integer;				(* INDENTATION LEVEL *)
    pnestlv: integer;				(* NEST LV OF FIRST CONSTRUCT ON LINE *)
    firstnestsetonline: boolean;
    commentlvl: integer;			(* LEVEL OF NESTED COMMENTS *)
    origch: char;				(* CH BEFORE UPPERCASE *)
    inclfile: text;
    incl2file: text;
    (*INCL3FILE: TEXT;
     INCL4FILE: TEXT;*)
     lines_read: integer;	(* count input lines read *)
     incl_lines_read: integer;	(* count lines from include files *)
     in_system: boolean;	(* true if reading system file *)

const
  src_max = 3;

public var
  src_defaults: 0..src_max;
  src_directories: array[1..src_max] of file_name;

$PAGE lexinit
public procedure lexinit;

var
    i: integer;
    sup: supports;

  procedure recover (v : ctp);

  var
      i : integer;

  begin
    if v <> nil then
      with v^ do begin
	recover (llink);
	if (klass=vars) andif (vclass=externalsc) then
	  vaddr := 0
	else if (klass=proc) orif (klass=func) then
	  if pfdeckind = declared then begin
	    pfaddr := 0;
	    for i := 0 to maxlevel do
	      linkchain[i] := 0;
	  end;
	recover (rlink)
      end
  end;

begin
  if errorflag then begin
    recover (display[0].fname);			(*SINCE WRITEC WON'T RECOVER AFTER ERRORS*)
    for sup := firstsupport to lastsupport do
      rnts.link[sup] := 0;
    end;
  errorflag := false;
  errorinline := false;
  followerror := false;
  errmptr := nil;
  errinx := 0;
  comment := false;
  commentlvl := 0;
  fileic := 0;
  chcnt := 0;
  nestlv := 0;
  inclnest := 0;
  lines_read := 0;
  incl_lines_read := 0;
  if pass_percent then
    comment_set := ['*','(','/']
  else comment_set := ['*','(','/','%','\'];
  for i := 1 to chcntmax do
    errline[i] := ' ';
end;

external procedure cnvtor(s: stringarray; var r:real);

external procedure readfileidentifier (var filename: filnamestr);

external var
    rfiderror: boolean;				(* SET BY ABOVE TO INDICATE ERROR *)

$PAGE next_source_file

function next_source_file (var f: text; fname: filnamestr): boolean;

(* opens the file for input, applying any specifies search directories
   until it is found or the search list is exhausted. returns the EOF
   of the file. *)

var
  name: filnamestr;
  ix: 1..src_max;

begin
  name := pas || fname;
  next_source_file := false;	(* assume file exists until we know otherwise *)
  reset (f, name);
  if eof (f) then begin
    for ix := 1 to src_defaults do begin
      reset (f, name || ' ' || src_directories[ix]);
      if not eof (f) then
	return;	(* found one *)
    end;
    next_source_file := true;	(* not found *)
  end;
end;
$PAGE nextfile
public procedure nextfile;			(*GET NEXT SOURCE FILE *)

begin
  if filenum<filemax then begin
    if filenum > 1 then begin
      close(input);
    end;
    for i := 1 to 6 do
      entry[i] := srcnam[filenum,i];
    if next_source_file (input, srcnam[filenum]) then;
    filenum:=filenum+1;
    with filedata[0] do begin
      fileid := filecnt;
      filecnt := filecnt + 1;
      fwdfile := 0;
      fwdpage := 0;
      laststmt := 0;
      lastpage := 777777b;
      lastline := 0;
      pagecnt := 0;
      linecnt := 1;
      localline := 1
    end
  end;
end;

(* FOLLOWING PROCEDURES ARE INPUT ROUTINES USED BY THE COMPILER
   WHICH READ FROM APPROPRIATE FILES ACCORDING TO THE VALUE OF THE
   GLOBAL SWITCH 'SOURCE' *)

$PAGE writelinenr
public procedure writelinenr (var f:text);

begin
  with filedata[inclnest] do begin
    if fileid = 0 then
      write (f,' ':2)
    else
      write (f, fileid:2);
    if source = including then
      write (f, '-')
    else
      write (f, ' ');
    write (f, linecnt:5)
  end
end;

$PAGE wrbuffer, writebuffer
procedure wrbuffer;

(*	Listing line format:

	Column		Usage
	------		-----

	  1-6		Address
	    7		Blank
	  8-9		File number
	    10		'-' if including, else blank
	 11-15		Line number within file
	 16-20		Line number within page
	    21		'*' if stmt_block emitted, else blank
	    22		Blank
	 23-24		Nesting level
	 25-26		Blank
	 27...		Source line			*)

begin
  if chcnt > linewidth then
    chcnt := linewidth;
  if savedp then
    write ('       ')
  else
    write (saveic:6:o,' ');
  writelinenr (output);
  write (filedata[inclnest].localline:5);
  if dp or (pnestlv <= 0) then
    write (' ':6)
  else
    write (stmt_indicator,' ',pnestlv:2,' ':2);
  writeln(buffer:chcnt)
end;

public procedure writebuffer;

begin
  if listcode then begin
    wrbuffer;
    for chcnt := 1 to 17 do
      buffer[chcnt] := ' ';
    chcnt := 17
  end
end;

$PAGE getnextline
public procedure getnextline;

var
    i: 1..5;
    lcnt: integer;

begin
  firstnestsetonline := true;
  pnestlv := nestlv;
  loop
    sgetlinenr(linenr);
  exit if linenr <> '     ';
    if listsource then
      page;					(* WRITES FORM-FEED *)
    lensubtitle := 0;
    entitle (false);
    sreadln;					(*TO OVERREAD SECOND CARRIAGE RETURN IN PAGE MARK*)
  end;
  if (pagelength <> 0) andif (linesthispage >= pagelength) and listsource then begin
    page;
    entitle (true);
  end;
  if listsource then
    linesthispage := linesthispage + 1;
  if linenr <> '-----' then begin
    lcnt := 0;
    for i := 1 to 5 do
      lcnt := (lcnt * 10) + (ord(linenr[i]) - ord ('0'));
    with filedata[inclnest] do begin
      linecnt := lcnt;
      localline := lcnt;
    end;
  end;
  savedp := dp;
  saveic := ic;
  stmt_indicator := ' ';
end;

$PAGE finderror
procedure finderror (nmr: integer);

var
    index: integer;
    ch1: char;
    found: boolean;

begin
  if shorterrors then begin
    if incompilation then
      writeln(' ERROR: ',nmr:3);
    writeln(tty,' ERROR: ',nmr:3)
  end
  else begin
    if not incompilation then
      write (tty,' ERROR: ');
    open(pasmsg,errmsgfile||prgm_dir);
    found := false;
    repeat
      readln(pasmsg);
      if eoln(pasmsg) andif not eof(pasmsg) then begin
	readln(pasmsg);
	read(pasmsg,index);
	if index=nmr then begin
	  found := true;
	  repeat
	    read(pasmsg,ch1);
	    if incompilation then
	      write(ch1);
	    write(tty,ch1);
	    if eoln(pasmsg) then begin
	      writeln (tty);
	      if incompilation then begin
		writeln;
		linesthispage := linesthispage + 1
	      end;
	      readln(pasmsg);
	      if not eoln(pasmsg) then begin
		if incompilation then begin
		  writeln;
		  write (' ':13);
		end;
		write (tty,' ':12)
	      end
	    end
	  until eoln(pasmsg)
	end
      end
    until found or eof(pasmsg);
    if not found then
      writeln (tty,' WARNING: ERROR ',nmr:3,' NOT FOUND');
    close(pasmsg)
  end
end;

$PAGE endofline
public procedure endofline;

var
    i,k,posn: integer;
    errm: etp;

begin
  if listsource or errorinline then
    wrbuffer;
  if errorinline then				(*OUTPUT ERROR MESSAGES*)
  begin
    err_cnt := err_cnt + errinx;
    writeln (tty);
    listcode := false;
    if not eofn then begin
      write (tty,'***File ');
      if source = reading then write (tty,uppercase(srcnam[filenum-1]))
      else write (tty,uppercase(inclname[inclnest]));
      with filedata[inclnest] do begin
	write (tty,' line ',linecnt:width(linecnt));
	if pagecnt <> 0 then begin
	  write (tty,', page ');
	  i := 0;
	  if lensubtitle > 0 then
	    i := verify (substr(subtitle,1,lensubtitle),lettersdigitsorleftarrow,
		    lensubtitle+1)-1;
	  if i = 0 then write (tty,pagecnt:width(pagecnt))
	  else write (tty,uppercase(substr(subtitle,1,i)));
	  write (tty,' line ',localline:width(localline));
	end;
	writeln (tty);
      end;
      writeln (tty, ' ':3, buffer:chcnt);	(* WRITE LINE TO TTY *)
      posn := 0;
      linesthispage := linesthispage + 1;
      for k := 1 to errinx do
	with errlist[k] do
	  if (pos > posn) and (tic <> ' ') then
	    posn := pos;
      if posn > linewidth then
	posn := linewidth;
      writeln (' ':26,errline:posn);
      writeln (tty, ' ':3,errline:posn)
    end;
    for k := 1 to errinx do
      with errlist[k] do begin
	write (' *****',arw:3,tic,':  ');
	write (tty,'Error ',nmr:3,':  ');
	if errmptr <> nil then begin
	  errm := errmptr;
	  while errm <> nil do
	    with errm^ do begin
	      if nmr = number then begin
		case form of
		  c: begin
		    write(cstring:10,' --> ');
		    write(tty,cstring:10,' --> ')
		  end;
		  d: begin
		    write(intval:5,' --> ');
		    write(tty,intval:5,' --> ')
		  end
		end;
		number := 0;
		errm := nil
	      end
	      else
		errm := next
	    end
	end;
	finderror (nmr);
      end;
    break(tty);
    errinx := 0;
    errorinline := false;
    for i := 1 to chcnt do
      errline [i] := ' ';
    errmptr := nil
  end;
  chcnt := 0;
  with filedata[inclnest] do begin
    linecnt := linecnt + 1;
    localline := localline + 1
  end;
  eol := false
end (*ENDOFLINE*);

$PAGE error
public procedure error (ferrnr: integer) ;

var
    lpos,larw : integer;

begin
  if not incompilation then
    finderror (ferrnr)
  else begin
    if not followerror then begin
      errorflag := true ;
      if errinx >= maxerr then begin
	errlist[maxerr].nmr := 410;
	errinx := maxerr
      end
      else begin
	errinx := errinx + 1;
	with errlist[errinx] do begin
	  nmr := ferrnr;
	  tic := '^'
	end
      end;
      followerror := true;
      errorinline := true;
      if (ferrnr <> 215) and (ferrnr <> 356) and (ferrnr <> 405) and
	(ferrnr <> 464) then begin
	  if chcnt > 0 then
	    if seoln then
	      errline [chcnt] := '^'
	    else
	      errline [chcnt-1] := '^'
	end
	else
	  errlist[errinx].tic := ' ';
      if errinx > 1 then
	with errlist [ errinx-1] do begin
	  lpos := pos;
	  larw := arw
	end;
      with errlist [errinx] do begin
	pos := chcnt;
	if errinx = 1 then
	  arw := 1
	else if lpos = chcnt then
	  arw := larw
	else
	  arw := larw + 1
      end;
    end;
  end
end (*ERROR*);

$PAGE errorwithtext
public procedure errorwithtext ( ferrnr: integer; ftext: alfa ) ;

var
    errm: etp;

begin
  error(ferrnr);
  new(errm,c);
  with errm^ do begin
    number := ferrnr;
    cstring := ftext;
    next := errmptr
  end;
  errmptr := errm
end (*ERROR WITH TEXT*);

$PAGE insymbol
public procedure insymbol;
(*READ NEXT BASIC SYMBOL OF SOURCE PROGRAM AND RETURN ITS
 DESCRIPTION IN THE GLOBAL VARIABLES SY, OP, ID, VAL AND LGTH*)

const
    digmax = 12;
    max8 = 37777777777b;
    test8 = 40000000000b;
    min8 = 400000000000b;
    max10 = 3817748707;
    max16 = 17777777777b;
    maxexp = 35;
    elipnext = 0;				(* CH SET TO THIS ON .. AFTER INT CONSTANT *)

var
    i,k,ival: integer;
    stringtoolong: boolean;
    digit: array [1..digmax] of char;
    string1: stringarray;
    realstring: stringarray;
    lvp: csp;
    lch: char;
    comlvl: integer;

$PAGE dodirect
  procedure dodirect;				(* SCANS AND PROCESSES DIRECTIVE LINES *)

  var
      k: integer;
      diridx: directives;
      ls1, ls2: boolean;
      ifile: filnamestr;

label 10;

  begin
    nextch;
    readidentifier (drctvs[baddir]);		(* SCAN DIRECTIVE, ASSUME ONLY '$' READ BY INSYMBOL *)
    diridx := include (* FIRST DIRECTIVES VALUE *);
    while drctvs [baddir] <> drctvs [diridx] do
      diridx := succ (diridx);
    case diridx of
      baddir: begin
	errskip (176);
	nextch
      end;					(*LISTS LINE, GETS 1ST CHAR OF NEXT *)
      pagelist: begin
	skipblanks;				(* THESE PRECISE STEPS PRODUCE GOOD LISTING *)
	lensubtitle := 0;
	ch := origch;				(* DON'T UPPERCASE FIRST CHARACTER! *)
	while not eol do begin
	  if lensubtitle < maxtitlelen then begin
	    lensubtitle := lensubtitle + 1;
	    subtitle[lensubtitle] := ch
	  end;
	  litch
	end;
	if listsource and (linesthispage > 0) then
	  page;
	entitle (false);
	if listsource then
	  linesthispage := linesthispage + 1;
	10: chcnt := 0;
	sreadln;
	endofline;
	if not seof then
	  getnextline;
	nextch
      end;
      system, heading, include: begin
	if (diridx=heading) andif not listsource then
	  goto 10;				(* NEXT LINE IF IGNORING HEADER DIRECTIVE *)
	if inclnest >= maxinclusion then begin
	  errskip (412);
	  nextch
	end
	else begin
	  skipblanks;
	  readfileidentifier (ifile);
	  if not rfiderror then begin
	    k := inclnest + 1;
	    case k of
	      1: begin
		rfiderror := next_source_file (inclfile, ifile);
	      end;
	      2: begin
		rfiderror := next_source_file (incl2file, ifile);
	      end				(*;
	      3: begin
		rfiderror := next_source_file (incl3file, ifile);
	      end;
	      4: begin
		rfiderror := next_source_file (incl4file, ifile);
	      end	*)
	    end
	  end;
	  if rfiderror then begin
	    errskip (264);
	    nextch
	  end
	  else begin				(* INCLUDE SUCCESSFUL *)
	    while not eol do
	      nextch;
	    sreadln;
	    endofline;
	    source := including;
	    inclnest := k;
	    inclname[inclnest] := ifile;
	    with filedata[inclnest] do begin
	      was_in_system := in_system;
	      was_listing := listsource;
	    end;
	    in_system := in_system or (diridx = system);
	    listsource := listsource and not in_system;
	    with filedata[inclnest] do begin
	      fwdfile := 0;
	      fwdpage := 0;
	      laststmt := 0;
	      lastpage := 777777b;
	      lastline := 0;
	      fileid := filecnt;
	      filecnt := filecnt + 1;
	      pagecnt := 0;
	      linecnt := 1;
	      localline := 1
	    end;
	    eofn := false;			(* KNOW INCL FILE IS NON-NULL*)
	    getnextline;			(* FIRST LINE OF INCLUDE FILE *)
	    nextch				(* FIRST CHAR *)
	  end
	end
      end;
      doptions: begin
      (*IF NOSOURCE SPECIFIED AND CURRENTLY EMITTING A
       SOURCE LISTING, FORCE PRINTING OF THIS LINE*)
	ls1 := listsource;
	optionlist;
	ls2 := listsource;
	if ls1 then
	  listsource := true;
	nextch;					(*PRINTS THE LINE IF NEC.*)
	listsource := ls2
      end;
      dtitle: begin
	skipblanks;
	titlelength := 0;
	ch := origch;				(*DON'T UPPERCASE IT*)
	while not eol do begin
	  if titlelength < maxtitlelen then begin
	    titlelength := titlelength + 1;
	    title[titlelength] := ch
	  end;
	  litch
	end
      end;
      dwidth, dlength: begin
	skipblanks;
	if ch='=' then begin
	  nextch;
	  skipblanks;
	end;
	if ch in digits then begin
	  k := ivalue;
	  if diridx = dlength then
	    if (k<>0) and (k<minlength) then
	      error (221)
	    else
	      pagelength := k
	  else if (k<minwidth) or (k>maxwidth) then
	    error (222)
	  else
	    linewidth := k
	end
	else
	  errskip (255)
      end
    end						(* CASE *)
  end;						(* DODIRECT *)

$PAGE options
  procedure options;

  var
      lch : char;
      conditional, accumulation, lswitch : boolean;
      i : integer;
      charset: set of char;

  begin
    conditional := false;
    accumulation := true;
    charset := comment_set + [' ',','];
    repeat
      nextch;
      lch := ch;
      if (ch <> '\') and (ch <> '*') then
	nextch;
      if not (ch in ['+','-']) then begin
	if (lch = 'S') andif (ch = '=') then begin
	  nextch;
	  corsiz := ivalue
	end
	else if ((lch = 'X') orif (lch = 'Y')) andif (ch in [' ',',','0'..'9'])
	  then begin
	    i := ivalue;			(* RETURNS 0 IF CH NOT IN DIGITS *)
	    if (i<minconditional) or (i>maxconditional) then
	      error (223)
	    else begin
	      lswitch := compsw[i];
	      if lch = 'Y' then
		lswitch := not lswitch;
	      accumulation := accumulation and lswitch;
	      if debugcomments then
		conditional := true
	    end
	  end
	  else begin
	    error (203);
	    nextch
	  end
      end
      else begin
	lswitch := ch = '+';
	if lch = '&' then
	  specialsw := lswitch
	else
	  case lch of
	    'C':
	      runtmcheck := lswitch;
	    'D': begin
	      if lswitch then
		debug := true
	    end;
	    'L':
	      listcode := lswitch;
	    'M':
	      if swflag then
		main := lswitch;
	    'O':
	      if swflag then
		outinuse := lswitch;
	    'T':
	      if swflag then
		ttyinuse := lswitch;
	X':
	      compsw[0] := lswitch
	  end;
      end;
      if not (ch in charset) then
	nextch
    until ch <> ',';
    if conditional and accumulation then begin
      comment := false;
      commentlvl := commentlvl + 1
    end
  end (*OPTIONS*);

$PAGE insymbol - body
begin
(*INSYMBOL*)
  2:
    loop
      while ch = ' ' do begin
	if eofn then begin
	  sy := eofsy;
	  goto 3
	end;
	nextch
      end;
    exit if not comment andif (pass_percent orif (ch <> '%'));
      comment := true;
      nextch;
      if ch = '$' then begin
	options;
	if not comment				(* TURNED OFF IF IN DEBUG COMMENT *)
	then
	  goto 2
      end;
      comlvl := commentlvl + 1;
      repeat
	while not (ch in comment_set) do begin
	  if eofn then begin
	    sy := eofsy;
	    error (216);
	    goto 3
	  end;
	  nextch
	end;
	if ch = '*' then begin
	  nextch;
	  if ch in [')', '/'] then begin
	    comlvl := comlvl - 1;
	    nextch
	  end
	end
	else if ch in ['(', '/'] then begin
	  nextch;
	  if ch = '*' then begin
	    comlvl := comlvl + 1;
	    nextch
	  end
	end
	else if ch = '%' then begin
	  comlvl := comlvl + 1;
	  nextch
	end
	else if ch = '\' then begin
	  comlvl := comlvl - 1;
	  nextch
	end
      until comlvl <= commentlvl;
      comment := false
    end;
  case ch of
    'A','B','C','D','E','F','G','H','I', 'J','K','L','M','N','O','P','Q','R',
      'S','T','U','V','W','X','Y','Z': begin
	idlength := 0;
	id := '          ';
	while ch in (lettersdigitsorleftarrow) do begin
	  if idlength < alfaleng then begin
	    idlength := idlength + 1;
	    id[idlength] := ch;
	  end;
	  nextch;
	end;
	for i := frw[idlength] to frw[idlength+1] - 1 do
	  if rw[i] = id then begin
	    sy := rsy[i];
	    op := rop[i];
	    goto 1
	  end;
	sy := ident;
	op := noop;
	1:
      end;
    '0','1','2','3','4','5','6','7','8','9': begin
      sy := intconst;
      op := noop;
      i := 0;
      repeat
	if i < digmax then begin
	  i:= i+1;
	  digit[i]:= ch
	end
	else
	  error(174) ;
	nextch
      until not (ch in digits);
      ival := 0;
      if ch = 'B' then begin
	for k := 1 to i do
	  if ival <= max8 then begin
	    if digit[k] in ['8','9'] then
	      error(252);
	    ival := 8*ival + ord(digit[k])-ord('0')
	  end
	  else if (ival =test8) and (digit[12] = '0') then
	    ival := min8
	  else begin
	    error(204);
	    ival := 0
	  end;
	val.ival := ival;
	nextch
      end
      else begin
	for k := 1 to i do
	  if ival <= max10 then
	    ival := 10*ival + ord(digit[k])-ord('0')
	  else begin
	    error(204);
	    ival := 0
	  end;
	if ch = '.' then begin
	  nextch;
	  if ch = '.' then
	    ch := chr (elipnext)
	  else begin
	    sy:= realconst;
	    for k:= 1 to i do
	      realstring[k]:= digit[k];
	    i:= i+1;
	    realstring[i]:= '.';
	    while ch in digits do begin
	      i:= i+1;
	      realstring[i]:= ch;
	      nextch
	    end
	  end
	end;
	if ch = 'E' then begin
	  if sy<>realconst then begin		(*NO PRECEEDING POINT*)
	    for k:= 1 to i do
	      realstring[k]:= digit[k];
	    sy:= realconst
	  end;
	  i:= i+1;				(*STORE 'E'*)
	  realstring[i]:= ch;
	  nextch;
	  if (ch='+') or (ch='-') then begin
	    i:= i+1;
	    realstring[i]:= ch;
	    nextch
	  end;
	  if not (ch in digits) then
	    error(205)
	  else
	    repeat
	      i:= i+1;
	      realstring[i]:= ch;
	      nextch
	    until not (ch in digits)
	end;
	if sy = intconst then
	  val.ival := ival
	else begin				(*CONVERT REAL NUMBER*)
	  realstring[i+1]:= ' ';
	  new(lvp,reel);
	  cnvtor(realstring,lvp^.rval);
	  (* Assume new record zeroed, so LVP^.RVAL2 := 0 not required *)
	  lvp^.selfcsp := nil;
	  val.valp:= lvp
	end
      end
    end;
    '"': begin
      sy := intconst;
      op := noop;
      ival := 0;
      nextch;
      while ch in hexadigits do begin
	if ival <= max16 then
	  if ch in digits then
	    ival := 16*ival + ord(ch) - ord('0')
	  else
	    ival := 16*ival + ord(ch) - 67b
	else begin
	  error(174);
	  ival := 0
	end;
	nextch
      end;
      val.ival := ival;
      if not specialsw then
	error (466)
    end;
    '''': begin
      lgth := 0;
      sy := stringconst;
      op := noop;
      stringtoolong := false;
      repeat
	repeat
	  litch;
	  if lgth < strglgth then begin
	    lgth := lgth + 1;
	    string1[lgth] := ch
	  end
	  else
	    stringtoolong := true
	until (eol) or (ch = '''');
	if stringtoolong then
	  error(301);
	if eol andif (ch<>'''') then
	  error(351)
	else
	  nextch
      until ch <> '''';
      lgth := lgth - 1;				(*NOW LGTH = NR OF CHARS IN STRING*)
      if lgth = 1 then
	val.ival := ord(string1[1])
      else begin
	new(lvp,strg:lgth);
	with lvp^ do begin
	  slgth := lgth;
	  sval[1:lgth] := string1[1:lgth];
	  selfcsp := nil;
	end;
	val.valp := lvp
      end
    end;
    ':': begin
      op := noop;
      nextch;
      if ch = '=' then begin
	sy := becomes;
	nextch
      end
      else
	sy := colon
    end;
    '.': begin
      op := noop;
      nextch;
      if (ch = '.') andif (not eofn) then begin
	sy := elipsis;
	nextch
      end
      else
	sy := period
    end;
    '%','?','+','-', '@','=', ')','[',']',',',';','^','_': begin
      5: sy := ssy[ch];
      op := sop[ch];
      nextch
    end;
    '&','#': begin
      sy := ssy[ch];
      op := sop [ch];
      nextch;
      if not specialsw then
	error (466);
    end;
    '$': begin
      if chcnt = 1 then begin
	dodirect;
	goto 2
      end
      else begin
	sy := othersy;
	op := noop;
	nextch
      end
    end;
    '\','*': begin
      if (ch = '%') and pass_percent then goto 5;
      if ch = '*' then
	nextch;
      if ch in [')', '/', '\'] then begin
	commentlvl := commentlvl - 1;
	if commentlvl < 0 then begin
	  error (216);
	  commentlvl := 0
	end;
	nextch;
	goto 2
      end
      else if ch='*' then begin
	nextch;
	sy:=mulop;
	op:=expn
      end
      else begin
	sy := mulop;
	op := mul
      end
    end;
    '(', '/': begin
      lch := ch;
      nextch;
      if ch = '*' then begin
	comment := true;
	goto 2
      end
      else begin
	sy := ssy[lch];
	op := sop[lch]
      end
    end;
    '<','>': begin
      sy := ssy[ch];
      op := sop[ch];
      nextch;
      if (op=ltop) and (ch='>') then begin
	op:= neop;
	nextch
      end
      else if ch = '=' then begin
	if op = ltop then
	  op := leop
	else
	  op := geop;
	nextch
      end
    end;
    '!','|': begin
      lch := ch;
      nextch;
      if ch=lch then begin
	nextch;
	sy:=addop;
	op:=concat
      end
      else begin
	error(166);
	goto 2
      end
    end;
    others: begin
      if ch = chr (elipnext) then begin
	sy := elipsis;
	op := noop;
	nextch
      end
      else begin
	error (166);
	nextch;
	goto 2
      end
    end
  end;						(*CASE*)
  3:
end;						(*INSYMBOL*)

$PAGE enterid
public procedure enterid(fcp: ctp);
(*ENTER ID POINTED AT BY FCP INTO THE NAME-TABLE,
 WHICH ON EACH DECLARATION LEVEL IS ORGANISED AS
 AN UNBALANCED BINARY TREE*)

var
    nam: alfa;
    lcp, lcp1: ctp;
    lleft: boolean;

begin
  nam := fcp^.name;
  lcp := display[top].fname;
  if lcp = nil then
    display[top].fname := fcp
  else begin
    repeat
      lcp1 := lcp;
      if lcp^.name <= nam then begin
	if lcp^.name = nam then
	  error(302) (*NAME CONFLICT*);
	lcp := lcp^.rlink;
	lleft := false
      end
      else begin
	lcp := lcp^.llink;
	lleft := true
      end
    until lcp = nil;
    if lleft then
      lcp1^.llink := fcp
    else
      lcp1^.rlink := fcp
  end;
  with fcp^ do begin
    llink := nil;
    rlink := nil;
    selfctp := nil
  end
end (*ENTERID*);

$PAGE srchsection
public procedure srchsection(fcp: ctp; var fcp1: ctp);
(*TO FIND RECORD FIELDS AND FORWARD DECLARED PROCEDURE ID'S
 --> PROCEDURE PROCEDUREDECLARATION
 --> PROCEDURE SELECTOR*)

begin
  while fcp <> nil do
    with fcp^ do begin
      if name = id then
	goto 1;
      if name < id then
	fcp := rlink
      else
	fcp := llink
    end;
  1:
    fcp1 := fcp
end (*SEARCHSECTION*);

$PAGE srchid
public procedure srchid(fidcls: setofids; var fcp: ctp);

var
    lcp: ctp;

begin
  for disx := top downto 0 do begin
    lcp := display[disx].fname;
    while lcp <> nil do
      with lcp^ do begin
	if name = id then
	  if klass in fidcls then
	    goto 1
	  else begin
	    if prterr then
	      error(401);
	    lcp := rlink
	  end
	else begin
	  if name < id then
	    lcp := rlink
	  else
	    lcp := llink
	end
      end
  end;
  (*SEARCH NOT SUCCSESSFUL; SUPPRESS ERROR MESSAGE IN CASE
   OF FORWARD REFERENCED TYPE ID IN POINTER TYPE DEFINITION
   --> PROCEDURE SIMPLETYPE*)
  if prterr then begin
    error(253);
    (*TO AVOID RETURNING NIL, REFERENCE AN ENTRY
     FOR AN UNDECLARED ID OF APPROPRIATE CLASS
     --> PROCEDURE ENTERUNDECL*)
    if types in fidcls then
      lcp := utypptr
    else if vars in fidcls then
      lcp := uvarptr
    else if field in fidcls then
      lcp := ufldptr
    else if konst in fidcls then
      lcp := ucstptr
    else if proc in fidcls then
      lcp := uprcptr
    else
      lcp := ufctptr;
  end;
  1:
    fcp := lcp
end (*SEARCHID*);

$PAGE getbounds
public procedure getbounds(fsp: stp; var fmin,fmax: integer);
(*GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE*)
(*ASSUME (FSP # NIL) AND (FSP^.FORM <= SUBRANGE)
	 AND  NOT COMPTYPES(REALPTR,FSP)*)

begin
  with fsp^ do
    if form = subrange then begin
      fmin := min.ival;
      fmax := max.ival
    end
    else if fsp = intptr then begin
      fmin := - maximumint;
      fmax := maximumint
    end
    else begin
      fmin := 0;
      if fsp = charptr then
	fmax := 177b
      else if fconst <> nil then
	fmax := fconst^.values.ival
      else
	fmax := 0
    end
end (*GETBOUNDS*);

$PAGE skipiferr, iferrskip, errandskip
public procedure skipiferr(fsyinsys:setofsys; ferrnr:integer;
  fskipsys: setofsys);

var
    i,oldchcnt,oldlinecnt : integer;

begin
  if not (sy in fsyinsys) then
    with filedata[inclnest] do begin
      error(ferrnr);
      oldlinecnt := linecnt;
      oldchcnt := chcnt;
      while not (sy in fskipsys or fsyinsys or [eofsy]) do begin
	if oldlinecnt <> linecnt then
	  oldchcnt := 1;
	for i := oldchcnt to chcnt-1 do
	  if i <= chcntmax then
	    errline [i] := '*';
	oldchcnt := chcnt;
	oldlinecnt := linecnt;
	errorinline := true;
	insymbol
      end;
      (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)
    end;
  followerror := false
end;

public procedure iferrskip(ferrnr: integer; fsys: setofsys);

begin
  skipiferr(fsys,ferrnr,fsys)
end;

public procedure errandskip(ferrnr: integer; fsys: setofsys);

begin
  skipiferr([ ],ferrnr,fsys)
end;

$PAGE dcllabel, searchlabel, findlabel
public function dcllabel (labno: integer; kind: declkind): labp;

var
    lab: labp;
    topp: disprange;

begin
  topp := top;
  while display[topp].occur <> blck do
    topp := topp - 1;
    (* DECLARED LABEL IN INNERMOST BLOCK *)
  new (lab);
  with lab^, display[topp] do begin		(* GENERATE NEW LABEL *)
    lkind := kind;
    labval := labno;
    labchain := 0;
    defined := false;
    labaddr := 0;
    nextlab := flabel;
    flabel := lab
  end;
  dcllabel := lab
end;

public function searchlabel (labno: integer; dl: disprange): labp;

var
    found: boolean;
    lab: labp;

begin
  found := false;
  with display [dl] do begin
    lab := flabel;
    while (lab <> nil) and (not found) do begin
      if lab^.labval = labno then
	found := true
      else
	lab := lab^.nextlab
    end;
    searchlabel := lab
  end
end;

public function findlabel (labno: integer; var rellev: disprange): labp;

var
    lab: labp;
    topp: disprange;

begin
  topp := top;
  rellev := 0;
  repeat
    lab := nil;
    with display[topp] do
      if occur = blck then begin
	lab := searchlabel (labno, topp);
	if lab = nil then
	  rellev := rellev + 1
      end;
    topp := topp - 1
  until (lab <> nil) or (topp <= 0);
  findlabel := lab
end.
 S'%ñ