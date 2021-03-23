$OPTIONS MAIN, NOCHECK, SPECIAL, STORAGE=6000

$LENGTH 44

(* CONDITIONAL COMPILATION SWITCHES FOR THIS MODULE:

	X1:  IF ENABLED, SUPPRESSES LISTING OF CONST, TYPE, AND VAR INCLUDE FILES
	Y2:  UNLESS ENABLED, OPENS SYMBOL TABLE FILE UNDER DEBUG OPTION  
	Y4:  SUPPORT DOUBLE PRECISION REALS UNLESS DISABLED	
	X5:  IF ENABLED, SUPPORT INITPROCEDURES
								*)

(*HINTS FOR INTERPRETING ABBREVIATED IDENTIFIERS
       BRACK  : BRACKET "[ ]"          IX  : INDEX
       C  : CURRENT                    L  : LOCAL
       C  : COUNTER                    L  : LEFT
       CST  : CONSTANT                 PARENT  : "( )"
       CTP  : IDENTIFIER POINTER       P/PTR  : POINTER
       EL  : ELEMENT                   P/PROC  : PROCEDURE
       F  : FORMAL                     R  : RIGHT
       F  : FIRST                      S  : STRING
       F  : FILE                       SY  : SYMBOL
       F/FUNC  : FUNCTION              V  : VARIABLE
       G  : GLOBAL                     V  : VALUE
       ID  : IDENTIFIER
       REL  : RELATIVE                 REL  : RELOCATION*)

$PAGE includes
const

(*$x1
$OPTIONS NOSOURCE
 *)
$INCLUDE PASCON.INC

type

$INCLUDE PASTYP.INC

public var

$INCLUDE PASVAR.INC

$INCLUDE PRGDIR.INC[,320220]

$INCLUDE PASINI.PAS

$INCLUDE PASLEX.INC

$INCLUDE PASOPT.INC

$INCLUDE PASBLK.INC

$INCLUDE PASSTR.INC

$INCLUDE PASMC.INC

$INCLUDE PASCG.INC
$INCLUDE DTIME.TYP[,320220]
$INCLUDE DTIME.INC[,320220]
$INCLUDE PASHAK.INC
$INCLUDE PASLOG.TYP

$PAGE declarations
static var
    heap_ptr: integer;
    tempstring: string60;
    j: integer;

public var
    rfiderror: boolean;				(* SET BY FOLLOWING TO FLAG ERROR *)
    i: integer;

external function tmpcor: boolean;	(* returns true if tmpcor file *)

external var
    dateofcompilation: packed array[1..9] of char;  (* "COMPDATE" *)
    auto_run: boolean;	(* true if run from compil *)

type 
    jobnum_string = packed array[1..3] of char;

external function jobnum: jobnum_string;
$PAGE readfileidentifier
public procedure readfileidentifier (var str: filnamestr);

begin
  str := '';
  while not (eol or (ch in ['[',',',' ','/','!','=','_'])) do begin
    str := str || ch;
    nextch
  end;
  if ch = '[' then begin
    str := str || ch;
    repeat
      nextch;
      if ch = ')' then
	ch := ']';
      if ch in [',','-',']','0'..'7'] then
	str := str || ch
    until eol or not (ch in [',','-','0'..'7']);
    if (ch = ']') then
      nextch
  end;
  if ch = '_' then
    ch := '=';
  breakch := ch;
  rfiderror := not (ch in [',','!',' ','/','='])
end;

external procedure link (string60); fortran;
$PAGE log_stats

(* LOG_STATS determines if logging statistics are to be recorded and does so
   if required.	*)


static var log_file: file of log_file_stats;

external procedure wrt_log (filblock; log_file_stats; integer);

procedure log_stats;

var log: log_file_stats;
    log_file_info: filblock;

type
  fake_block = record
    case boolean of 
      true:  (i1: filblock);
      false: (i2: packed array[0..7] of halfword)
  end;

const
  init_file_block: fake_block := (false, (
    446353b,0,	(* dsk *)
    604163b,434154b, (* pascal *)
    545747b,0,0,0));	(* .log *)


begin
  log_file_info := init_file_block.i1;
  prgm_ppn (log_file_info[3]);
      with log do begin
	file_name := fileb$text (input);
	version := rversion;
	run_time := rtime;
	no_lines := lines_read;
	no_incl_lines := incl_lines_read;
	no_errors := err_cnt;
	users_ppn := user_ppn;
	date_and_time := daytime;
	lowseg_size := statlc;
	hiseg_size := highestcode;
	ka10_ki10 := dmove;
	kl10 := kl10sw;
	opt_debug := debug;
	opt_double := dblreal;
	opt_main := main;
	opt_check := runtmcheck;
	opt_overlay := overlaysw;
	opt_progress := progresssw;
	opt_source := glistsource;
	opt_special := specialsw;
	opt_terse := shorterrors;
	opt_trace := tracer;
	opt_auto_run := auto_run;
	opt_tmpcor := cortmp;
	opt_hash := not (cortmp or realtty);
	opt_virtual := virtual;
	opt_incore := incore;
	opt_xref := xref;
  end;
  wrt_log (log_file_info, log, size (log_file_stats));
  (* restore breaks *)
end (* log_stats *);
$PAGE PASCAL body
public procedure open_xref_file;
begin
  rewrite (symfile,srcnam[1] || '.SYM[,]');
  xref_open := true;
end;
procedure fileerr;

begin
  if realtty then
    write(tty,'*');
  break(tty);
  source := prompting;
  sreadln;
end (*FILEERR*);

begin
  rewrite(ttyoutput);				(*OUTPUT ONLY*)
  realtty:=false;

  (*ENTER STANDARD NAMES AND STANDARD TYPES:*)
  (******************************************)

  rtime := runtime;
  level := 0;
  top := 0;
  with display[0] do begin
    fname := head_symbol;
    occur := blck;
    flabel := nil;
    blkid := nil;
  end;
  init_0;
(*$X5   SFGLOBPTR:=FGLOBPTR; *)
  slastbtp:=lastbtp;

  (*INITIAL HEADER*)
  (****************)

  writeln (tty,header,dateofcompilation);
  writeln (tty);
  break (tty);

  (*OPEN COMPILER FILES*)
  (*********************)

  if auto_run then
    cortmp := tmpcor	(* check for tmpcor:pas file *)
  else cortmp := false;
  if auto_run and not cortmp then
    reset (ttyi,jobnum||'PAS.TMP');			(*LOOK FOR COMMAND FILE*)
  if not cortmp andif (not auto_run orif eof (ttyi)) then begin
    write(tty,'*');
    break(tty);
    realtty:=true;
    reset(ttyi,'TTY:'||jobnum||'PAS.TMP')
  end;
  1:
    source := prompting;
  if seoln orif seof then
    goto 3;

    (*INITIALIZE COMPILER FLAGS, POINTERS, ETC. *)

  incompilation := false;			(* WHILE READING FILENAMES *)
  listcode := false;				(* L- *)
  runtmcheck := true;				(* C+ *)
  ttyinuse := false;				(* T- *)
  outinuse := false;				(* O- *)
  ininuse := false;
  specialsw := false;				(* &- *)
  debug := false;
  xref := false;
  xref_open := false;
  tracer := true;				(* LIMITED DEBUG INFO *)
  debugcomments := true;			(* WAS X+, NOW IS ENABLE ANY/DISABLE ANY *)
  listinfo := false;
  overlaysw := false;
  progresssw := false;
  relout := true;
  listsource := false;
  glistsource := true;
  main := false;
  shorterrors := false;
  swflag := true;
  kl10sw := true;
  dmove := true;
(*$Y4 DBLREAL := FALSE;	*)
  corsiz := 4000b;
  virtual := false;
  incore := false;
  mask_offset := 0;
  v_nil := 0;
  area_mask := 0;
  ovfl_mask := 0;
  shl_area := 0;
  shr_area := 0;
  allocation := min_allocation;	(* i. e., quick_fit *)
  alc_used := false;	(* set true only if allocation routines required *)
  pass_percent := true;
  in_system := false;
  src_defaults := 0;	(* no active search directories *)
  filenum := 1;
  filemax := 1;
  filecnt := 0;
  eol := false;
  eofn := false;
  for i := minconditional to maxconditional do
    compsw[i] := false;				(* CLEAR CONDITIONAL COMPILATION FLAGS*)
  linewidth := maxwidth;			(* DEFAULT *)
  titlelength := 0;				(* NO TITLE *)
  lensubtitle := 0;				(* NO SUBTITLE, EITHER *)
  pagelength := 0;				(* NO PAGINATION *)
  linesthispage := 0;
  optline := 0;					(* SET BY OPTIONLIST IF CALLED*)
  err_cnt := 0;
  chcnt := 0;
  nextch;
  readfileid(relnam);
  if (relnam='') andif ((breakch=',') orif (breakch='=')) then begin
    relout := false;
    relnam := 'SINK:'
  end
  else if rfiderror then begin
    writeln(tty,'?INVALID REL FILE NAME, REENTER COMMAND');
    fileerr;
    goto 1
  end;
  if breakch='!' then begin
    if not (cortmp or realtty) then begin
      rewrite (ttyi,jobnum||'PAS.TMP');
      close (ttyi);
    end;
    link('SYS:' || relnam || '.' (* OFF EXTENSION *) );
  end;
  listnam := '';
  if breakch = '/' then
    optionlist;
  if seoln then		(* old style command *)
  begin
    srcnam[1] := relnam;
    listnam := relnam||'.LST[,]';		(* FORCE .LST EXTENSION WITH USER'S PPN *)
    relnam := relnam||'.REL[,]';		(* FORCE .REL EXTENSION *)
    filemax:=2;
    goto 2
  end;
  if breakch=',' then begin
    nextch;
    readfileid(listnam);
    if rfiderror then begin
      writeln(tty,'?INVALID LISTING FILE NAME, REENTER COMMAND');
      fileerr;
      goto 1
    end;
    listsource := listnam <> '';
    listnam := lst || listnam
  end
  else if breakch='=' then
    glistsource := false;
  if breakch<>'=' then begin
    writeln(tty,'?INVALID COMMAND FORMAT, REENTER COMMAND');
    fileerr;
    goto 1
  end;
  while not seoln do begin
    nextch;
    readfileid(srcnam[filemax]);
    if rfiderror then begin
      writeln(tty,'?INVALID SOURCE FILE NAME');
      fileerr;
      goto 1
    end;
    if breakch = '/' then begin
      optionlist;
    end;
    filemax:=filemax+1;
  end;

  (*  HAVING SCANNED THE COMMAND, WE CAN NOW LOOK UP ALL THE FILES *)

  2:
    if not (glistsource and listsource) then begin
      listsource := false;
      glistsource := false;
      listnam := 'SINK:';
    end;
  source := reading;
  nextfile;
  if seof then begin
    writeln (tty, 'SOURCE FILE IS NULL OR INVALID');
    writeln (tty);
    break (tty);
    goto 4
  end;
  rewrite(output,listnam);
  if glistsource andif not eof(output) then begin
    writeln(tty,'?CAN''T CREATE LISTING FILE');
    fileerr;
    goto 1
  end;
  rewrite(relfile,rel||relnam);
  if relout andif not eof(relfile) then begin
    writeln(tty,'?CAN''T CREATE REL FILE');
    fileerr;
    goto 1
  end;
  tempstring := substr (srcnam[1],index(srcnam[1],':')+1);
  j := verify (tempstring,lettersdigitsorleftarrow,length(tempstring)+1)-1;
  if j > 6 then
    j := 6;
  entry := '          ';
    entry[1:j] := tempstring[1:j];

    (*WRITE HEADER*)
    (**************)

  entitle (false);
  if listsource then begin
    if optline > 0 then begin
      writeln (' ':14,'*',' ':11,substr(buffer,optline,optlen));
      linesthispage := linesthispage + 1
    end
  end;
  fwptr := nil;
  lastbtp := slastbtp;
(*$X5  FGLOBPTR := SFGLOBPTR ; *)
  fileptr := sfileptr ;
  localpfptr:=nil;
  globtestp := nil;
  firstkonst := nil;
  int_str.selfstp := nil;
  real_str.selfstp := nil;
  char_str.selfstp := nil;
  bool_str.selfstp := nil;
  true_id.selfctp := nil;
  false_id.selfctp := nil;
  nil_str.selfstp := nil;
  text_str.selfstp := nil;
  alfa_str.selfstp := nil;
  onetoten.selfstp := nil;
  tty_id.selfctp := nil;
  ttyout_id.selfctp := nil;
  output_id.selfctp := nil;
  input_id.selfctp := nil;
  deb_words := 0;
  pflist := nil;
  relwords := 0;
  wordswritten := 0;
  cbpssaddr := 0;
  loadnoptr := true;
(*$X5   INITGLOBALS := FALSE ;	*)
  dp := true;
  prterr := true;
  entrydone := false;
  progchain := 0;				(*BACKLINK CHAIN OF FWD REFS TO FILE INFO BLOCK IN D+*)
  libix := 0;
  eol := false;
  eofn := false;
  top := 1;
  level := 1;
  ic := highstart;
  lexinit;
  relblock.count:= 0;
  with display[1] do begin
    fname := nil;
    occur := blck;
    flabel := nil;
    blkid := nil
  end;

  (************* BEGIN COMPILATION ***************)

  incompilation := true;
  writeln (tty);
  getnextline;					(*GETS FIRST LINENUMBER IF ANY*)
  ch := ' ';
  insymbol;
  swflag := false;
  if incore and virtual then error (179);
  if incore then virtual := true;
(*$Y4   IF DBLREAL THEN BEGIN
    IF NOT DMOVE THEN BEGIN
      ERROR (511);	(* NOT AVAILABLE ON THE KA10 *)
      DMOVE := TRUE;
    END;
    REAL_STR.SIZE := 2;
  END
  ELSE BEGIN
    REAL_STR.SIZE := 1;
  END;
 *)
  if virtual then
    nil_id.values.ival := -1
  else nil_id.values.ival := 377777b;
  if xref and not xref_open then
    open_xref_file;
  if debug then begin
    mark (heap_ptr);
    (*$Y2
	REWRITE (SMBFILE,RELNAM || '.DEB[,]');
      *)
  end
(*$Y2 ELSE IF RELOUT THEN BEGIN
    RESET (SMBFILE,RELNAM || '.DEB[,]');
    IF NOT EOF (SMBFILE) THEN BEGIN	(* IF FILE EXISTS *)
      REWRITE (SMBFILE,RELNAM || '.DEB[,]');
      CLOSE (SMBFILE);	(* EMPTIES OLD SYMBOL TABLE FILE *)
    END;
    END	*) ;
  if (sy = programsy) orif (sy = modulesy) then begin
    main := sy = programsy;
    insymbol;
    if sy <> ident then
      error(209)				(*IDENTIFIER EXPECTED*)
    else begin
      if progresssw then begin
	write (tty,substr(id,1,idlength),' ')
      end;
      entry := id;
      insymbol
    end;
    if main and (sy = lparent) then begin
      repeat
	insymbol;
	if sy = ident then insymbol
	else error (209);
      until sy <> comma;
      if sy = rparent then insymbol
      else error (152);
    end;
    if sy = semicolon then
      insymbol
    else
      error (156);
  end;
  break;
  cix := -1;
  lc := progrst;
  statlc := progrst;
  if not entrydone then begin
  (* IN CASE STRUCTURED CONSTANTS EMITTED BEFORE CODE *)
    entrydone := true;
    writemc (writeentry);
    writemc (writename);
    writemc (writehiseg)
  end;
  fullword (no,0,0);				(* fixup address of program block later if debug on *)
  codeend := ic;
  writemc (writeconstants);			(* force it out *)
  block(nil,blockbegsys or statbegsys-[casesy],[period]);
  insymbol;
  if (sy <> eofsy) then begin
    error (369);
  end;
  if errorinline then
    endofline;
  if progresssw then
    writeln (tty);
  writeln;
  writeln;
  if not errorflag then begin
    write('No error detected');
    write (tty,'No error detected')
  end 
  else begin
    if err_cnt = 1 then begin
      writeln (tty,'?One error detected');
      writeln ('?One error detected');
    end
    else begin
	i := width (err_cnt);
	writeln (tty,'?',err_cnt: i,' errors detected');
	writeln ('?',err_cnt: i,' errors detected');
    end;
  end;
  writeln;
  writeln(tty);
  i := (highestcode - 400000b + 1023) div 1024;
  if listinfo then
    writeln(tty,'HIGHSEG: ',i:3,'K (',highestcode:6:o,')');
  writeln('HIGHSEG: ',i:3,'K');
  if listinfo then begin
    i := statlc;
    j := 0;
    repeat
      i := i div 10b;
      j := j + 1;
    until i = 0;
  end;
  i := (statlc + 1023) div 1024;
  if listinfo then
    writeln(tty,'LOWSEG : ',i:3,'K (',statlc:j:o,')');
  writeln('LOWSEG : ',i:3,'K');
  break(tty);
  rtime := runtime - rtime;
  if listinfo then begin
    write(tty,'Runtime ',rtime/1000:7:3);
    writeln(tty)
  end;
  log_stats;
  rtime := runtime;
  if debug then
    release (heap_ptr);
  4:
    if realtty then
      write(tty,'*');
  break(tty);
  close(output);				(*CLOSE OUTPUT FILES*)
  close(relfile);
  if xref then
    close (symfile);
  source := prompting;
  sreadln;
  goto 1;
  3:
  if not cortmp then
    if not realtty then begin
      rewrite (ttyi,jobnum||'PAS.TMP');
      close (ttyi);
    end
end.
