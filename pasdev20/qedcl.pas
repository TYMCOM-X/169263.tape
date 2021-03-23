$PAGE includes and externals
$INCLUDE QERR.TYP
$INCLUDE QSTR.TYP
$INCLUDE QSPAT.TYP
$INCLUDE QSPRED.TYP
$INCLUDE RLB:CMDUTL.TYP
$INCLUDE QEDLN.TYP
$INCLUDE QLD.TYP
$INCLUDE QED.TYP
$INCLUDE QSPLIT.TYP
$INCLUDE RLB:FILUTL.INC
$INCLUDE RLB:IORESE.INC
$INCLUDE RLB:QUERY.INC
$INCLUDE QSPAT.INC
$INCLUDE QSPRED.INC
$INCLUDE QEDERR.INC
$INCLUDE QLD.INC
$INCLUDE QREAD.INC
$INCLUDE QEDLN.INC
$INCLUDE QMARK.INC
$INCLUDE QPRINT.INC
$INCLUDE QSUBST.INC
$INCLUDE QJOIN.INC
$INCLUDE QSPLIT.INC

external procedure passcc;
external function onescape: boolean;
external function on_heap_overflow: boolean;
external procedure escoff;
$PAGE command processing tables
type
  rangetypes =
    ( one, dollar, dot, dotp1, lb, lbp1);

$INCLUDE RLB:LOOKUP.TYP

type
  rangelist =
    record
      lbound, hbound1, hbound2: rangetypes;
      required, permitted: ldcount
    end;


  qcmdlist = array [qedcmds] of cmdlist;

  sub_opt_list = array [sub_options] of cmdlist;

  set_par_list = array [set_params] of cmdlist;

  split_op_list = array [split_options] of cmdlist;

  defrangelist = array [qedcmds] of rangelist;


external const
  qcmds: qcmdlist;
  sops: sub_opt_list;
  setparams: set_par_list;
  splitops: split_op_list;
  defrange: defrangelist;

type
  qdatarec =	(* SET and other parameter variables *)
    record
      linecount,	(* number of lines to print in a <cr> *)
      maxdel: qlineno;	(* maximum no. of lines to delete without confirm *)
      tabprint: boolean;	(* controls printing of tab character *)
      wildswitch: boolean;	(* enables/disables wildcarding *)
      openfileopen: boolean;	(* is the file open? *)
      markstring: cmdline;	(* the mark string for BOUND matching *)
      lasterr: qerrcode;
      errlevel: cmdlineidx
    end;

static var
  qdata: qdatarec;
  openfile: text;
  list_file: text;
  saved_bounds: boolean;			(* to push and pop buffer bounds *)
  lbound_save,
  hbound_save: qlineno;
  offset_save: qlineno;

  procedure pushbounds(var buffer: qbuffer; var err: qerrcode);
  begin
    offset_save := buffer.offset;
    lbound_save := buffer.lbound;
    hbound_save := buffer.hbound;
    saved_bounds := true;
    qunbound (buffer, err)
  end;

  procedure popbounds(var buffer: qbuffer);
  var derr: qerrcode;				(* use local code since called from error reporting utils *)
  begin
    if saved_bounds then begin			(* called at errors, check if bounds pushed *)
      qunbound (buffer, derr);			(* save linenos are in terms of whole buffer *)
      if lbound_save <= hbound_save then		(* must be empty buffer - qunbound suffices to set
						   bounds properly; qsetbound fouls up *)
        begin
          buffer.offset := offset_save;
	  qsetbounds (buffer, lbound_save, hbound_save, true, derr)
	end
    end;
    saved_bounds := false
  end;


$PAGE lookup procedures
external function lookupcmds
  (  line: cmdline; var idx: cmdlineidx;
     list: qcmdlist; maxscalar: qedcmds;
     var cmd: qedcmds                   ): boolean;

external function lookupsops
  (  line: cmdline; var idx: cmdlineidx;
     list: sub_opt_list; maxscalar: sub_options;
     var sop: sub_options               ): boolean;

external function lookupsetparams
  (  line: cmdline; var idx: cmdlineidx;
     list: set_par_list; maxscalar: set_params;
     var setopt: set_params		): boolean;

external function lookupsplitops
  (  line: cmdline; var idx: cmdlineidx;
     list: split_op_list; maxscalar: split_options;
     var splitop: split_options		): boolean;
$PAGE qinit
public procedure qsetmarkdefault (line: cmdline);
begin
  qdata.markstring := line
end;

public procedure qsettabdefault (default: boolean);
begin
  qdata.tabprint := default
end;

public procedure qinit (var buffer: qbuffer);
  var didx: qstringidx;  derr: qerrcode;
begin with qdata do
  begin
    linecount := 1;
    maxdel := 10;
    wildswitch := true;
    openfileopen := false;
    lasterr := qok;
    errlevel := 1
  end;
  didx := 1;
  if spredparse (qdata.markstring, didx, buffer.mark, false, derr) then ;
end;
$PAGE qinitexec
(* initialization routine which calls four other initialization
   routines in the mandantory order *)

public procedure qinitexec( var buffer: qbuffer);

begin
  qinitbuf( buffer );
  qsetmarkdefault( ':$PAGE:');
  qsettabdefault( true );
  qinit( buffer )
end;
$PAGE qexecute
public procedure qexecute
(       var buffer:     qbuffer;		(* working buffer *)
	line:           cmdline;		(* command line to parse *)
	var lindex:     cmdlineidx;		(* place marker *)
	var execrange:  ldrange;		(* limits of execution *)
	var ble:        qlineno;		(* bottommost line examined *)
	findflag:       boolean;		(* running under find? *)
	allowed_cmds:   qed_cmd_set;		(* which commands are legal? *)
	var err:        qerrcode);		(* anything wrong? *)

label 1, 2, 100;

const
  confirm_file := true;				(* new/old file prompting desired *)

var						(* parsing routine args, etc. *)
  nld:          ldcount;			(* number of la's in ld *)
  ld:           ldchain;			(* pointer to parsed ld linked list *)
  cmd:          qedcmds;			(* parsed command *)
  cmdrange:     ldrange;			(* value of parsed ld *)

var						(* command routine identifiers *)
  fid:          file_id;			(* for file reading and writing *)
  cnt:          qlineno;			(* line count for APPENDs *)
  confirm,
  doit:         boolean;			(* for conditional tests throughout *)
  lp:           qlinep;				(* for debugging *)
  sop:		sub_options;			(* substitute storage *)
  sop_set:	sub_opt_set;			(* for option parsing *)
  splitop:	split_options;			(* split option parsing *)
  splitop_set:	split_opt_set;			(* ditto *)
  idx:		qlineno;			(* counter for running through buffer *)
  pat:		spattern;			(* pattern parsing for substitute, find *)
  pred:		spred;				(* for SET parsing & bound *)
  repstr:	qstring;			(* replacement string in substitute request *)
  total:	qlineno;			(* to keep track of changes made *)
  source:	qstring;			(* place to keep text of looked-up line *)
  pos,
  stridx,
  len:		qstringidx;			(* indicies into QED-type strings *)
  fno,
  lno:		qlineno;			(* boundary markers *)
  tmprange:	ldrange;			(* for additional LD parsing *)
  findble:	qlineno;			(* for FIND to keep track with *)
  find_cmds:	qed_cmd_set;			(* legal commands under FIND *)
  optcmd:	qedcmds;			(* for option parsing in MOVE, COPY *)
  setopt:	set_params;			(* for SET parameter parsing *)
  have_file:	boolean;			(* true if file parameter present *)
  old_cmdrange: ldrange;			(* temp used in find command *)
  on_opt:	boolean;                              (* for on/off parsing *)
  joinstring:	qstring;			(* JOIN continuation mark *)
$PAGE utilities
  procedure chkerr;
  begin
    if err <> qok then begin
      popbounds(buffer);
      goto 100
    end;
  end;						(* chkerr *)

  procedure seterr (newerr: qerrcode);
  begin
    err := newerr;
    chkerr
  end;						(* seterr *)


  procedure skipblanks;
  begin
    while (lindex <= length (line)) andif (ord (line[lindex]) <= ord (' '))
      do lindex := lindex + 1
  end;

  procedure ck_extra_txt;
  begin
    skipblanks;
    if (lindex <= length(line)) andif (line[lindex] <> ';') then
      seterr(qextratxt);
  end;						(* ck_extra_txt *)

  function checkpunct (ch: char): boolean;
   begin
    skipblanks;
    if (lindex <= length (line)) andif (ch = line[lindex])
      then begin
	checkpunct := true;
	lindex := lindex + 1
      end
    else checkpunct := false
   end;


  function parsenum (var value: qlineno; var err: qerrcode): boolean;
  begin
    skipblanks;
    value := 0;
    parsenum := false;
    while (lindex <= length (line)) andif (line[lindex] in ['0'..'9']) do begin
     if (value * 10) > maximum (qlineno) then seterr (qtoobig);
     value := value * 10 + (ord (line[lindex]) - ord ('0'));
     lindex := lindex + 1;
     parsenum := true
    end
  end (* parsenum *) ;

function parseonoff (var on_opt: boolean;   (* flag for on option *)
                     var err: qerrcode
                                    ): boolean; (* flag for good parse *)
var str: qstring;
begin
  parseonoff := false;
  skipblanks;
  str := '';

  while (lindex <= length(line)) do begin
    if line[lindex] <> ' ' then
      str := str||line[lindex];
    lindex := lindex +1
  end;     (* while *)

    if uppercase(str) = 'ON' then on_opt := true
      else if uppercase(str) = 'OFF' then on_opt := false
        else err := qbadparam;

    if err = qok then parseonoff := true
end;     (* parseonoff *)


  procedure chkrng (cmd: qedcmds; var nld: ldcount; var range: ldrange;
    findflag: boolean; var err: qerrcode);

    function decode (arg: rangetypes): qlineno;
    begin
      case arg of
	one:    decode := qfirst_val (buffer);
        dollar: decode := qdollar_val (buffer);
	dot:    decode := buffer.curlineno;
	dotp1:  decode := buffer.curlineno + 1;
	lb:     decode := cmdrange.lbound;
	lbp1:   decode := cmdrange.lbound + 1
      end					(* case *)
    end;					(* decode *)

  begin
    err := qok;
    if (nld = 0) and (defrange[cmd].permitted = 0) then return;
    if (nld > defrange[cmd].permitted) or (nld < defrange[cmd].required)
      and (not findflag) then err := qbadargno
    else begin
      if nld = 0 then
	if findflag then
	begin
	  range.lbound := buffer.curlineno;
	  range.hbound := buffer.curlineno;
	  nld := 1				(* suppress NLD=0 special cases when executing under
						   FIND, e.g. LIST *)
	end
	else
	begin
	  range.lbound := decode (defrange[cmd].lbound);
	  range.hbound := decode (defrange[cmd].hbound1)
	end
      else if nld = 1 then range.hbound := decode (defrange[cmd].hbound2);
      if not ( ((nld = 0) and (cmd in [bound, writecmd, save])) or
	       ((range.lbound = 0) and (cmd = append))		)
	then with range do begin
	  if lbound > hbound
	    then if qbuflength (buffer) = 0
	      then err := qempty
	      else err := qbadrn
	  else if lbound < qfirst_val (buffer)
	    then err := qbadlb
          else if hbound > qdollar_val (buffer)
	    then err := qbadub
	end
    end
  end;						(* chkrng *)


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


  function discard_changes: boolean;
  begin
    if buffer.changes
      then discard_changes := query ('Unwritten changes, OK')
      else discard_changes := true
  end;


  procedure close_open_file;
  begin
    if qdata.openfileopen then close (openfile);
    qdata.openfileopen := false
  end;


  function file_parameter (var fid: file_id): boolean;
   var l: cmdlineidx;
   begin
    fid := '';
    file_parameter := false;
    skipblanks;
    if lindex > length (line) then return;	(* nothing on line - don't check for semicolon since
						   on some systems, semi is part of filename *)
    l := lindex;
    if pr_file_id (line, lindex, fid)		(* see if file here *)
      then file_parameter := true
      else if l = lindex			(* bad file, if cursor ... *)
	then file_parameter := false		(* ... didn't move, no filename there *)
	else seterr (qbadfile)			(* ... did move, error *)
   end;


  procedure do_append (fid: file_id; lno: qlineno);
   var cnt: qlineno;
   begin
    if fid = ''
      then qttyappend (buffer, lno, cnt, err)
      else qfileappend (buffer, fid, lno, cnt, err);
    execrange.hbound := execrange.hbound + cnt;
    if lno < ble then ble := ble + cnt;
    buffer.curlineno := lno + cnt;
    chkerr
   end;


  function do_delete (range: ldrange): boolean;
   var cnt: qlineno;
   begin
    with range do begin
      cnt := hbound - lbound + 1;
      do_delete := (cnt < qdata.maxdel) orif query (numtochar (cnt) || ' lines, OK');
      if do_delete then begin
	qdellines (buffer, lbound, hbound, err);
	chkerr;
	if ble > hbound
	  then ble := ble - cnt
	  else if ble >= lbound then ble := lbound - 1;
	execrange.hbound := execrange.hbound - cnt;
	buffer.curlineno := lbound - 1
      end
    end
   end;


  procedure print_times (num: qlineno);
   begin
    write (tty, numtochar (num), ' time');
    if num <> 1 then write (tty, 's');
    writeln (tty, '.')
   end;


$PAGE substitute command
procedure substcmd;

var
     nth, n_par : qlineno;  (* variables for nth occurrance matching *)

      begin
        if not parsenum(n_par, err) then n_par := 1;
        chkerr;
	if not spatparse (line, lindex, pat, qdata.wildswitch, err) then err := qbadsubst;
	chkerr;
	if ((pat.stype <> simple) and (pat.stype <> token)) andif (n_par <> 1) then
	  err := qbadnth;	(* Nth occurrance is illegal with ::,@@, or ## *)
	chkerr;
	pos := index (substr (line, lindex), substr (line, lindex - 1, 1));
	if pos = 0 then seterr (qbadsubst);
	repstr := substr (line, lindex, pos - 1);
	lindex := lindex + pos;
	skipblanks;
	sop_set := [];
	while lookupsops (line, lindex, sops, maximum (sub_options), sop) do
	begin
	  sop_set := sop_set + [sop];
	  if checkpunct (',') then ;
	end;
	skipblanks;
	if (lindex <= length (line)) andif (line[lindex] <> ';') then
	begin
	  err := qbadopt;
	  return
	end;
	total := 0;
	for idx := cmdrange.lbound to cmdrange.hbound do
	begin
	  source := qgetline (buffer, idx, err);
	  chkerr;
	  if qsubstitute
           (source, idx, pat, repstr, sop_set, cnt, cmd, nth, n_par, err) then
	  begin
	    buffer.curlineno := idx;		(* set for a matching line *)
	    chkerr;
	    if cnt <> 0 then begin		(* modify only if subs made *)
	      qmodline (buffer, idx, source, err);
	      chkerr;
	    end;
	    total := total + cnt
	  end;
	  chkerr
	end;
	if (cmdrange.lbound <> cmdrange.hbound) or (total = 0) or (all_sop in sop_set)
	  then print_times (total);
	spatdispose (pat)
      end;					(* substitute *)
$PAGE bound command
    procedure boundcmd;
     
      var tempat: spattern;       (* save var for defpat on NEXT option *)
          next: boolean;          (* flag indicating a NEXT option *)
          backward: boolean;      (* flag for a backward search *)

      function nextparse: boolean;(* returns true on a NEXT option *)
      var next_str: qstring;      (* the word NEXT *)
          start: qlineno;         (* start position of option *)

      begin
        next_str := 'NEXT';
        nextparse := true;
        start := lindex - 1;

	(* take 'NEXT' or any abbreviation *)
        while ((lindex <= length(line)) and ((lindex-start) <= 4))
	  and (line[lindex] <> ';') do begin
          if (uppercase(line[lindex]) <> next_str[lindex-start]) then
            nextparse := false;
          lindex := lindex + 1
        end;
        if ((lindex-start) = 1) then nextparse := false; (* pointer didn't advance *)

        if nextparse then begin
          start := 1;
          nextparse := spredparse('::', start, pred, qdata.wildswitch, err);
          end
        else if ((lindex-start) <> 1) then err := qbadopt
						   else err := qok;

        next := nextparse

      end;     (* nextparse *)


      begin
	next := false;
	idx := buffer.curlineno + buffer.lbound - 1;	(* save curlineno in terms of unbounded linenos *)
        if (line[lindex] = '^') then begin
          backward := true;
          lindex := lindex + 1
          end
        else backward := false;
	if spredparse (line, lindex, pred, qdata.wildswitch, err) orif nextparse then
	begin
          chkerr;
          ck_extra_txt;
	  if nld <> 0 then seterr (qbadargno);
	  pushbounds(buffer, err);
	  qunbound (buffer, err);
	  if qbuflength (buffer) = 0 then seterr (qempty);
	  if (idx = qdollar_val (buffer)) and (not backward) then idx := 0
	  else if (idx = qfirst_val(buffer)) and backward then idx :=
						qdollar_val (buffer) + 1;
	  if next then begin (* to save the defpat first *)
	    tempat := spatgetdefault;
	    if backward then
	      qmarkmatch (buffer, buffer.mark, pred, idx, fno, lno, backward, false, err)
	    else
	      qmarkmatch (buffer, buffer.mark, pred, idx+1, fno, lno, backward, false, err);
	    spatsetdefault(tempat);	(* restore defpat *)
	    spatdispose(tempat);	(* clean up *)
	    if (err = qbadln) then err := qnomark
	    end
	  else if backward then
		qmarkmatch (buffer, buffer.mark, pred, idx-1, fno, lno, backward, true, err) 
	        else qmarkmatch (buffer, buffer.mark, pred, idx+1, fno, lno, backward, true, err);
	  chkerr;
	  qsetbounds (buffer, fno, lno, true, err);
	  chkerr;
	  saved_bounds := false;			(* we have reset bounds after push *)
	  buffer.curlineno := qfirst_val (buffer)
	end
	else if nld = 0
	  then begin
	    if backward then err := qbadopt; (* no pattern or NEXT!! *)
            chkerr;
            ck_extra_txt;
	    buffer.curlineno := idx;
	    qunbound (buffer, err);
	    chkerr
	  end
	else
	begin
          ck_extra_txt;
	  if nld <> 2 then seterr (qbadargno);
	  (* a FALSE argument here for non-absolute line address on BOUND *)
	  qsetbounds (buffer, cmdrange.lbound, cmdrange.hbound, false, err);
	  chkerr;
	  buffer.curlineno := qfirst_val (buffer)
	end;

	(* Update the execrange bounds to permit accesses to the new
	   bounded section.  We assume here that execrange is never smaller
	   than the bounded section, unless bound is prohibited. *)

	execrange.lbound := 0;
	execrange.hbound := qdollar_val (buffer);
      end;					(* boundcmd *)
$PAGE indent command
    procedure indentcmd;
      var way: (left, column, right);
	  ind, parm, i, pos, col: qstringidx;
	  source, indstr: qstring;
	  ln: qlineno;
      begin
	if checkpunct ('-') then way := left
	else if checkpunct ('+') then way := right
	else way := column;
	if not parsenum (parm, err) then seterr (qnonumf);
        ck_extra_txt;

	for ln := cmdrange.lbound to cmdrange.hbound do begin
	  buffer.curlineno := ln;			(* set so if error occurs, line is error line *)source := qgetline (buffer, ln, err); chkerr;
	  pos := 1;  col := 1;	(* derive first non-white column *)
	  while (pos <= length (source)) andif (source[pos] <= ' ') do  begin
	    if source [pos] = ' '
	      then col := col + 1
	      else if source[pos] = chr(11b) then repeat col := col + 1 until (col mod 8) = 1;
	    pos := pos + 1
	  end;
	  if pos > length (source) then source := ''	(* line is blank, truncate *)
	  else begin
	    case way of					(* check if indentation okay *)
	      left:  if parm >= col then seterr (qtooshort)
		       else ind := col-parm-1;		(* and derive length of indentation *)
	      right: ind := col + parm - 1;
	      column: ind := parm
	    end;
	    indstr := '';				(* build indentation string *)
	    for i := 1 to (ind div 8) do indstr := indstr || chr(11b);
	    indstr := indstr || substr ('        ', 1, ind mod 8);
	    if (length (source) - col + 1 + length (indstr)) > upperbound (source)
	      then seterr (qlnlong);
	    source := indstr || substr (source, pos);
	  end;
	  qmodline (buffer, ln, source, err); chkerr
	end
      end;
$PAGE begin main command loop
begin						(* begin body of qexecute *)
  saved_bounds := false;

1:						(* each pass over following code parses and executes one
						   command in the command line. *)
  qldparse (line, lindex, nld, ld, qdata.wildswitch, err);
  chkerr;
  if not lookupcmds (line, lindex, qcmds, maximum (qedcmds), cmd) then
    if (lindex > length (line)) orif (line[lindex] = ';')
      then begin				(* just <eoln> *)
	if nld = 0
	  then begin				(* no ld - print next linecount lines *)
	    if findflag then return;		(* but don't do anything under find *)
	    if buffer.curlineno = qdollar_val (buffer)
	      then seterr (qbadln);		(* nothing to print *)
	    fno := buffer.curlineno + 1;	(* get range of lines to print *)
	    lno := buffer.curlineno + qdata.linecount;
	    if lno > qdollar_val (buffer) then lno := qdollar_val (buffer);   (* use remainder if too few *)
	    qlistlines (buffer, fno, lno, ttyoutput, true, false, qdata.tabprint, err);
            chkerr;
	    buffer.curlineno := lno;
	    goto 2;				(* to exit interpretation loop *)
	  end
	else cmd := print			(* Ld <eoln> - assume print and eval Ld normally *)
      end
    else seterr (qbadcmd);			(* no known command name, not eoln *)
  if not (cmd in allowed_cmds) then seterr (qnocmd);
  qldeval (buffer, ld, execrange, cmdrange, err);
  qlddispose (ld);
  chkerr;
  chkrng (cmd, nld, cmdrange, findflag, err);
  chkerr;
$PAGE command dispatcher and action routines
  skipblanks;
  case cmd of

    after, before:
      begin
        substcmd
      end;

    append:
      begin
	if file_parameter (fid) then ;
	ck_extra_txt;
	do_append (fid, cmdrange.lbound)
      end;

    bound:
      begin
	boundcmd;
      end;					(* bound *)

    change:
      begin
        if file_parameter( fid ) then;
        ck_extra_txt;
	if do_delete (cmdrange) then 
	  do_append (fid, cmdrange.lbound - 1)
      end;

    delete:
      begin
        ck_extra_txt;
        if do_delete (cmdrange) then 
      end;

    closecmd:
      begin
        ck_extra_txt;
        close_open_file
      end;

    edit, modify:
      writeln(tty,'No longer implemented - use SUBSTITUTE');

    list:
      begin
        have_file := file_parameter( fid );
        ck_extra_txt;
	if have_file then begin
	  if not open_file (list_file, fid, '', output_mode, [confirm_open])
	    then seterr (qbadfile)
	end
	else rewrite (list_file, 'TTY:');

	(* listing of lines within section *)
	if nld > 0 then begin
	  qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, list_file, false, true, true, err);
          chkerr;
	  buffer.curlineno := cmdrange.hbound
	end

	(* list entire file, numbered by section *)
	else begin
	  pushbounds(buffer, err);				(* save current bounds *)
	  idx := 0; fno := 1;
	  loop					(* one pass certain, since no addr error => nonzero line cnt *)
	    repeat
	      idx := idx + 1
	    until (idx > qdollar_val (buffer)) orif
		    spredmatch (qgetline (buffer, idx, err), buffer.mark, err);
	    lno := idx - 1;
	    if lno >= fno then begin		(* may have zero length section at start *)
	      qsetbounds (buffer, fno, lno, true, err);   (* address new region *)
	      chkerr;
	      if fno <> 1 then page (list_file);    (* page between parts, not at begin and end *)
	      qlistlines (buffer, 1, lno-fno+1, list_file, false, true, true, err);
              chkerr;
	      qunbound (buffer, err); chkerr;
	    end;
	  exit if idx > qdollar_val (buffer);
	    fno := idx
	  end;
	  popbounds(buffer);
	end;

	close (list_file)
      end;

    find:
      begin
	find_cmds := allowed_cmds - [why, quit, exitcmd, resetcmd, load, save, writecmd,
				     opencmd, closecmd, setcmd, bound, uparrow];
	cnt := 0;
	if not spredparse (line, lindex, pred, qdata.wildswitch, err) then err := qnofindpred;
	chkerr;
	confirm := query ('Confirm');
	idx := cmdrange.lbound;
	while idx <= cmdrange.hbound do
	begin
	  source := qgetline (buffer, idx, err);
	  chkerr;
	  if spredmatch (source, pred, err) then
	  begin
	    pat := spatgetdefault;
	    findble := idx;
	    buffer.curlineno := idx;		(* set if matched *)
	    doit := true;
	    if confirm then begin
	      writeln (tty, source);
	      doit := query ('OK')
	    end;
	    if doit then
	    begin
	      cnt := cnt + 1;			(* count confirmed matches, not executions *)
	      stridx := lindex;
              old_cmdrange := cmdrange;
	      qexecute (buffer, line, stridx, cmdrange, findble, true, find_cmds, err);
	      execrange.hbound := execrange.hbound + cmdrange.hbound -
				  old_cmdrange.hbound;
	      spatsetdefault (pat);
	      spatdispose (pat);
	      buffer.curlineno := findble;
	      chkerr;
	    end;
	    cmdrange.lbound := findble + 1;
	    idx := cmdrange.lbound;
	  end
	  else begin
	    chkerr;
	    idx := idx + 1
	  end
	end;
	spreddispose(pred);
        if confirm 
          then write (tty, 'Found and confirmed ')
          else write (tty, 'Found ');
        write (tty, numtochar (cnt), ' time' );
        if cnt <> 1 then write ( tty, 's' );
        writeln ( tty, '.' );
	lindex := length (line) + 1;		(* find uses rest of line *)
      end;					(* find *)

    gotocmd:
      begin
        ck_extra_txt;
        buffer.curlineno := cmdrange.lbound
      end;

    insert:
      begin
	if file_parameter (fid) then ;
        ck_extra_txt;
	do_append (fid, cmdrange.lbound - 1)
      end;

    indent: indentcmd;

    join:
      begin
	if not spatparse (line, lindex, pat, false, err)
	  then joinstring := ''
	  else joinstring := pat.list^.sstring;
	chkerr;
        ck_extra_txt;
	qjoinlines (buffer, cmdrange.lbound, cmdrange.hbound, joinstring, err);
	chkerr;
	with cmdrange do begin
	  buffer.curlineno := lbound;
	  execrange.hbound := execrange.hbound - (hbound - lbound);
	  if ble > hbound
	    then ble := ble - (hbound - lbound)
	    else if ble >= lbound then ble := lbound
	end
      end;					(* join *)

    load:
      begin
	if not file_parameter (fid) then seterr (qnofile);
        ck_extra_txt;
	if discard_changes then begin
	  close_open_file;
	  qdelbuf (buffer);
	  qinitbuf (buffer);
          execrange.lbound := 0;
          execrange.hbound := 0;
	  qinit (buffer);
	  do_append (fid, 0)
	end
      end;

    move,
    transfer,
    copy:
      begin
	if lookupcmds (line, lindex, qcmds, maximum (qedcmds), optcmd) then begin
	  if not (optcmd in [append, insert]) then seterr (qbadopt)
	end
	else optcmd := insert;
	ld := nil;
	qldparse (line, lindex, nld, ld, qdata.wildswitch, err);
	if err = qok then begin
	  if nld <> 1
	    then err :=  qnomovela
	    else qldeval (buffer, ld, execrange, tmprange, err)
	end;
	qlddispose (ld);			(* take care to dispose even if errors *)
	chkerr;
        ck_extra_txt;
	if optcmd = insert then tmprange.lbound := tmprange.lbound - 1;
	cnt := cmdrange.hbound - cmdrange.lbound + 1;
	if cmd in [move, transfer]
	  then begin
	    qmovelines (buffer, cmdrange.lbound, cmdrange.hbound, tmprange.lbound, err);
	    execrange.hbound := execrange.hbound - cnt;
	    with cmdrange do begin
	      if ble > hbound
		then ble := ble - cnt
		else if ble >= lbound
		  then ble := lbound - 1;	(* in range moved *)
	      if tmprange.lbound > hbound
		then tmprange.lbound := tmprange.lbound - cnt
	    end;
	  end
	  else qcopylines (buffer, cmdrange.lbound, cmdrange.hbound, tmprange.lbound, err);
	execrange.hbound := execrange.hbound + cnt;
	if ble > tmprange.lbound then ble := ble + cnt;
	buffer.curlineno := tmprange.lbound + cnt
      end;					(* move *)

    number,
    eqcmd:
      begin
        ck_extra_txt;
        writeln (tty, numtochar (cmdrange.lbound))
      end;

    opencmd:
      begin
	if file_parameter (fid) then begin
          ck_extra_txt;
          close_open_file;
	  if open_file (openfile, fid, '', output_mode, [confirm_open])
	    then qdata.openfileopen := true
	    else seterr (qbadfile)
	end
	else seterr (qnofile)
      end;					(* opencmd *)

    outputcmd:
      begin
        ck_extra_txt;
	if not qdata.openfileopen then
	begin
	  err := qnotopen;
	  return
	end;
	qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, openfile, false, false, false, err);
        chkerr
      end;					(* outputcmd *)

    print:
      begin
        ck_extra_txt;
	qlistlines (buffer, cmdrange.lbound, cmdrange.hbound, ttyoutput, true, false, qdata.tabprint, err);
        chkerr;
	buffer.curlineno := cmdrange.hbound
      end;					(* print *)

    quit,
    exitcmd:
      begin
        ck_extra_txt;
	err := qquit				(* caller must decide whether to discard changes *)
      end;

    resetcmd:
      begin
        ck_extra_txt;
	if discard_changes then begin
	  close_open_file;
	  qdelbuf (buffer);
	  qinitbuf (buffer);
	  qinit (buffer)
	end
      end;					(* reset *)

    setcmd:
      begin
	if lookupsetparams (line, lindex, setparams, maximum (set_params), setopt) then
	begin
	skipblanks;
	case setopt of
	  del_param:
	    begin
	      if not parsenum (idx, err) then seterr (qnoparamval);
              ck_extra_txt;
	      qdata.maxdel := idx
	    end;				(* del_param *)

	  lcnt_param:
	    begin
	      if not parsenum (idx, err) then seterr (qnoparamval);
              ck_extra_txt;
	      qdata.linecount := idx
	    end;				(* lcnt_param *)

	  mark_param:
	    begin
	      if not spredparse (line, lindex, pred, qdata.wildswitch, err) then return;
              ck_extra_txt;
	      spreddispose (buffer.mark);
	      buffer.mark := pred
	    end;					(* mark_param *)

          tab_param:
            begin
              if not parseonoff(on_opt, err) then seterr(err)
              else if on_opt then qdata.tabprint := true
                   else qdata.tabprint := false
            end;               (* tab_param *)

	  wild_param:
	    begin
		if not parseonoff(on_opt, err) then seterr(err)
		else if on_opt then qdata.wildswitch := true
		     else qdata.wildswitch := false
	    end			(* wild_param *)
	end					(* case setopt *)
	end
	else seterr (qbadparam)
      end;					(* setcmd *)

    split:
      begin
	if not spatparse (line, lindex, pat, qdata.wildswitch, err)
	  then if err <> qok
	    then return
	    else seterr (qnosplitpat);
	skipblanks;
	splitop_set := [];
	while lookupsplitops (line, lindex, splitops, maximum (split_options), splitop) do
	begin
	  splitop_set := splitop_set + [splitop];
	  if checkpunct (',') then ;
	end;
	if (lindex <= length (line)) andif (line[lindex] <> ';')
	  then seterr (qbadopt);
	idx := cmdrange.lbound;
	total := 0;
	repeat					(* loop over cmdrange, adjusting hbound for splits *)
	  if qsplitlines (buffer, idx, pat, splitop_set, cnt, err)
	    then begin
	      if ble > idx then ble := ble + cnt;
	      execrange.hbound := execrange.hbound + cnt;
	      cmdrange.hbound := cmdrange.hbound + cnt;
	      idx := idx + cnt;			(* this adjusts for splits, does not increment *)
	      total := total + cnt;
	      buffer.curlineno := idx;
	    end;
	  chkerr;
	  idx := idx + 1
	until idx > cmdrange.hbound;
	if (cmdrange.lbound <> (cmdrange.hbound - total)) or (total = 0) or (all_splitop in splitop_set)
	  then print_times (total)
      end;					(* qsplit *)

    substitute:
      begin
	substcmd
      end;

    uparrow:
      begin
        ck_extra_txt;
	if buffer.curlineno <= 1 then
	begin
	  err := qbadlb;
	  goto 100
	end;
	buffer.curlineno := buffer.curlineno - 1;
	qlistlines (buffer, buffer.curlineno, buffer.curlineno, ttyoutput, true, false, qdata.tabprint, err);
        chkerr
      end;					(* uparrow *)

    why:
      with qdata do
      begin
        ck_extra_txt;
	errlevel := errlevel + 1;
	qederr (ttyoutput, lasterr, errlevel);
      end;					(* why *)

    writecmd,
    save:
      begin
	if not file_parameter (fid) then begin
	  if buffer.curfileok
	    then fid := buffer.curfile
	    else seterr (qbadfile)
	end;
        ck_extra_txt;
	if nld > 0
	  then qfilewrite (buffer, fid, cmdrange.lbound, cmdrange.hbound, confirm_file, err)
	  else begin				(* assume user wants whole file *)
	    pushbounds(buffer, err);
	    qfilewrite (buffer, fid, 1, qdollar_val (buffer), confirm_file, err);
	    popbounds(buffer)
	  end;
          chkerr
      end					(* write/save *)

  end;						(* case *)
$PAGE end main command loop
2:
  if buffer.curlineno > ble then ble := buffer.curlineno;
  if lindex <= length (line) then
    if line[lindex] = ';' then
    begin
      lindex := lindex + 1;
      goto 1
    end;
100:
  if (err <> qok) and (err <> qquit) then begin	(* save error code for why *)
    qdata.lasterr := err;
    qdata.errlevel := 1
  end;
end;						(* qexecute *)

$PAGE qedcl
public procedure qedcl
(       var buffer:     qbuffer;		(* working buffer *)
	allowed_cmds:   qed_cmd_set);		(* only commands allowed *)

var
  line: cmdline;
  lindex: cmdlineidx;
  execrng: ldrange;
  ble: qlineno;
  err: qerrcode;
  lp: qlinep;

begin
  line := '';
  passcc;

  saved_bounds := false;

  if onescape then begin
    ioreset;
    open (tty); rewrite (tty);			(* make sure everything is kosher *)
    writeln (tty, '__');
    break
  end;

  if on_heap_overflow then begin
    err := qheapfull;
    buffer.curfileok := false;
    buffer.curfile := '';  (* don't let him ruin his file unless he wnats to *)
    
    writeln (tty,'?Error -- the heap has overflowed.');
    writeln (tty,'The buffer may no longer be consistent.');
    writeln (tty,'Save any unwritten changes in a new file.')
  end;

  if saved_bounds then popbounds(buffer);


  loop
    write (tty, '*'); break;
    line := qread;
    execrng.lbound := 0;
    execrng.hbound := qdollar_val (buffer);
    lindex := 1;
    ble := buffer.curlineno;
    qexecute (buffer, line, lindex, execrng, ble, false, allowed_cmds, err);
  exit if err = qquit;
    if err <> qok then begin
      ireset;
      qederr (ttyoutput, err, 1)
    end;
  end;

  escoff;
end.						(* qedcl *)
   RAe¸