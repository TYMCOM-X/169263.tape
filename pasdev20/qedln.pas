$options special
(*   +--------------------------------------------------------------+
     i                                                              i
     i                        b q e d l n                           i
     i                        - - - - - -                           i
     i                                                              i
     +--------------------------------------------------------------+

     mdsi, company confidential

     started: 21-apr-77

     purpose: this package contains the basic routines which manage a
	qed text buffer.

     usage:
	entry points...
	  qinitbuf.....initialize a text buffer.
		      this routine should only be called once with a
		      buffer, prior to its first use.
	  qdelbuf......delete (release) the contents of a text buffer.
		      whenever a buffer is to be reused, this routine
		      should be called, rather than qinitbuf.
	  qgetline.....return the text of a line.
	  qmodline.....change the text of a line.
	  qaddline.....add a new line to the buffer.
	  qdellines....delete line(s) from the buffer.
	  movelines...move line(s) in the buffer.
	  copylines...copy (duplicate) line(s) in the buffer.

     requirements: this  package  uses  the qed string routines,  and
	the text returned by these routines  is  in  the  qed  string
	form.

     algorithm: line  descriptors  are  maintained in a doubly linked
	list with header information in the passed buffer descriptor.
	the line descriptors in turn contain qed strings.

     notes: since  any  of  the qed line operations may be broken out
	of,  care is taken in  the  management  of  the  line  chain.
	while  storage  may  be  lost  as  the result of breaks,  the
	following strategy insures that  these  routines  will  never
	leave  the line chain in a confused state,  i.e.,  will never
	partially complete operations.

	1.  lastlineno is always correct.

	2.  getlinep and lastlinep may  not  be  defined  (are  nil),
	even  if  the  buffer  contains  text.  whenever an operation
	changes getlineno or lastlineno,  the corresponding  xxxlinep
	is niled first,  then updated after the xxxlineno is changed.

	3.  the back line chain may be broken,  i.e.,  a qline  other
	than  the  first  one  may have prevlinep=nil.  however,  the
	forward chain is always complete.

	4.  the definitive test of an empty buffer  is  lastlineno=0,
	in  which  case,  the other contents of the buffer are assumed
	to be arbitrary.

     responsible: a. kortesoja

     changes: 
     	12/11/78 smr changed qfilewrite to check for write errors.
        12/11/78 smr added parameter to qfilewrite which indicates
                     whether new/old file prompting is desired.

	7/30/79 P.Lee  Changed QSETBOUNDS & MAP to use an offset in
			the buffer for bounded line addressing. Also
			the added option of using the entire buffer
			or the bounded buffer in bounding .
			New heap overflow handling by qfileappend and
			qttyappend.

     ---------------------------------------------------------------- *)


$include qerr.typ		(* QED error codes *)
$include qstr.typ		(* QED string definitions *)
$include qspat.typ		(* search pattern definitions *)
$include qspred.typ		(* string predicate definitions *)
$include cmdutl.typ		(* file_id definitions, etc. *)
$INCLUDE QEDLN.TYP
$INCLUDE rlb:filutl.inc
$INCLUDE qread.inc
$INCLUDE qspat.inc
$INCLUDE qspred.inc

external procedure mask;
external procedure unmask;
external function pushescape: boolean;		(* file write escape handler *)
external procedure escpop;
external procedure firescape;

$PAGE move
(* MOVE is the ultimate procedure for all line manipulations, including
   additions, deletions, and real moves.  It handles all the bookkeeping
   for updating the special line numbers and pointers.  It is assumed that
   all line numbers and pointers passed to this routine are reasonable. *)


procedure move
     (	var buf: qbuffer;			(* buffer to manipulate *)
	fln: qlineno;				(* addr of first line of section to be moved, if
						   zero, lines are new additions from garblist *)
	flp: qlinep;				(* ptr to above line *)
	lln: qlineno;				(* addr of last line to be moved, if fln = 0 then
						   this is #lines - 1 to yield proper count *)
	llp: qlinep;				(* ptr to above line *)
	tln: qlineno;				(* addr of line after which text is to be moved *)
	tlp: qlinep   );			(* ptr to above line, if nil, lines are added to
						   the list of lines to be discarded *)

var cnt: qlineno;
    tlno: qlineno;				(* tln adjusted for movements *)
begin
 with buf do begin
  cnt := lln - fln + 1;				(* count of lines to move *)
  tlno := tln;
  mask;						(* escape might leave buffer inconsistent *)


  (* slice out source lines from buffer if move or delete *)

  if fln <> 0 then begin			(* check for lines in buffer *)
    flp^.prevlinep^.nextlinep := llp^.nextlinep;    (* take off chain *)
    if llp^.nextlinep <> nil then 
      llp^.nextlinep^.prevlinep := flp^.prevlinep;

    if fln <= lbound then			(* adjust special line numbers *)
      if lbound > lln then lbound := lbound - cnt
	else begin				(* lbound in lines moved, new lbound follows *)
	  lbound := fln;
	  lboundp := llp^.nextlinep
	end;

    if fln <= getlineno then
      if getlineno > lln then getlineno := getlineno - cnt
	else begin				(* getlineno in lines move, new before them *)
	  getlineno := fln - 1;
	  getlinep := flp^.prevlinep
	end;

    if fln <= hbound then
      if hbound > lln then hbound := hbound - cnt
	else begin				(* hbound in lines moved, new before them *)
	  hbound := fln - 1;
	  hboundp := flp^.prevlinep
	end;

    if fln <= lastlineno then
      if lastlineno > lln then lastlineno := lastlineno - cnt
	else begin				(* last line in lines moved, new at new end of buffer *)
	  lastlineno := fln - 1;
	  lastlinep := flp^.prevlinep
	end;

    if fln < tlno then tlno := tlno - cnt	(* addr of target may be affected too *)
  end


  (* if appending new lines, remove from garb list *)

  else if llp = garblinep			(* quick check to see that line is on list *)
    then garblinep := flp^.prevlinep;


  (* if deleting, add to list to be discarded *)

  if tlp = nil then begin
    flp^.prevlinep := garblinep;		(* garb chain is backwards *)
    garblinep := llp
  end


  (* if moving or appending, add after target line *)

  else begin
    llp^.nextlinep := tlp^.nextlinep;		(* thread source to target *)
    flp^.prevlinep := tlp;

    if tlp^.nextlinep <> nil then		(* thread target to source *)
      tlp^.nextlinep^.prevlinep := llp;
    tlp^.nextlinep := flp;

    if lastlineno = tlno then lastlinep := llp;	(* adjust special hooks *)
    lastlineno := lastlineno + cnt;
    if tlno <= hbound then begin
      if tlno = hbound then hboundp := llp;
      hbound := hbound + cnt;
      if tlno = lbound - 1 then lboundp := flp
	else if tlno < lbound then lbound := lbound + cnt
    end
  end;

  changes := true;
  unmask;
 end
end;
$PAGE cleangarb
(* CLEANGARB removes deleted (or unused) lines from the so-called garb list.
   The list is scanned backwards and one line at a time is deleted. This
   code runs unmasked; if interrupted, at most one line will be lost (i.e.
   unchained, but not disposed.) *)

procedure cleangarb ( var buffer: qbuffer );
 var lp: qlinep;
 begin
  with buffer do begin
   while garblinep <> nil do begin		(* scan list and delete one at a time *)
     lp := garblinep;				(* save current ptr in temp *)
     garblinep := garblinep^.prevlinep;
     dispose (lp)				(* now delete, after stepping over it in chain *)
   end
  end
 end;
$PAGE findlinep
(*    internal procedure to find the pointer to a passed lineno    *)

procedure findlinep(var buf: qbuffer; lno: qlineno; var lp: qlinep);

(* assumes that l is a good number *)


  procedure setlp(tp: qlinep);			(* sets findlinep return value *)
  begin
    with buf do begin				(*update buf info first*)
      mask;
      getlineno := lno;
      getlinep := tp;
      unmask
    end (*with*);
    lp:= tp					(*now return pointer*)
  end (*setlp*);


  procedure search(beglineno: qlineno; beglinep: qlinep;
		   endlineno: qlineno; endlinep: qlinep);
    var tp: qlinep; i: qlineno; (*used in line search*)
  begin
    (*determine search direction*)
    if ((endlineno - lno) <= (lno - beglineno)) and (endlinep <> nil)
      then begin (*search backward from endlineno to lno*)
	tp:= endlinep;
	for i:= endlineno-1 downto lno do
	  tp:= tp^.prevlinep
      end
      else begin (*search forward from beglineno to lno*)
	tp:= beglinep;
	for i:= beglineno+1 to lno do
	  tp:= tp^.nextlinep
      end;
    setlp(tp) (*update getline information*)
  end (*search*);


begin						(*findline*)
  with buf do
    if lno < getlineno
      then if lno < lbound
	then search (0, firstlinep, getlineno, getlinep)
	else if lno < hbound
	  then search (lbound, lboundp, getlineno, getlinep)
	  else search (hbound, hboundp, getlineno, getlinep)
      else if lno < lbound
	then search (getlineno, getlinep, lbound, lboundp)
	else if lno <= hbound
	  then search (getlineno, getlinep, hbound, hboundp)
	  else search (getlineno, getlinep, lastlineno, lastlinep)
end (*findline*);
$PAGE utilities
(*********** text buffer manager utility routines ***********)

(* function to transform bounded linenos into absolute ones *)

function map
(	var buffer: qbuffer;			(* working buffer *)
	line: qlineno				(* line number to transform *)
		): qlineno;			(* mapped result *)

begin
  map := line + buffer.lbound - buffer.offset
end;						(* map *)

(*    procedure to check a line number    *)

function chkline(var buf: qbuffer; l: qlineno; var err: qerrcode): boolean;
begin
  err:= qok;
  if (l < buf.lbound) or (l > buf.hbound) then err:= qbadln;
  chkline:= (err=qok)
end (*chkline*);


(*    procedure to check a line range    *)

function chkrange(var buf: qbuffer; f,l: qlineno; var err: qerrcode): boolean;
begin
  err:= qok;
  if f > l then err:= qbadrn
  else if l > buf.hbound then err:= qbadub
  else if f < buf.lbound then err:= qbadlb;
  chkrange:= (err=qok)
end (*chkrange*);
$PAGE makeline
(* procedure to create a qed line record, does not chain it in *)

function makeline (var buf: qbuffer; line: qstring): qlinep;
 type
   synlinep = ^synline;				(* synthetic line *)
   synline =
     record
       prevlinep, nextlinep: qlinep;
       slgth: qstringidx;			(* overlay for string *)
       strng: packed array[1..qstringlen] of char
     end;
 var synp: synlinep;
     np: qlinep;
 begin
  (*$&+ *)                              (* here goes the kludgery *)
  new (synp: length (line));			(* alloc line of appropriate length *)
  synp^.slgth := length (line);
  synp^.strng[1:length(line)]:=line;		(* copy only to length allocated *)
  np := address (synp^);			(* coerce the pointer *)
  (*$&- *)                              (* done with hacks *)
  with np^ do begin
    nextlinep := nil;
    prevlinep := buf.garblinep;			(* add to dispose list, in case we lose it *)
    if buf.garblinep <> nil then
      buf.garblinep^.nextlinep := np;
    buf.garblinep := np
  end;
  makeline := np
 end;						(* makeline *)
$PAGE qdelbuf

(*    procedure to delete a buffer    *)

public procedure qdelbuf(var buf: qbuffer);
  var err: qerrcode;				(*we need it but we ignore them*)
begin						(*qdelbuf*)
  with buf do begin
    if lastlineno>0 then begin			(*something to release*)
      move (buf, 1, firstlinep^.nextlinep, lastlineno, lastlinep, 0, nil);  (* move lines to garb list *)
      cleangarb (buf);				(* dispose the lines *)
    end;
    dispose (firstlinep);			(* get rid of zeroth line *)
    spreddispose (mark);			(* dispose mark predicate *)
    mark := nil					(* for good measure *)
  end						(*with*)
end (*qdelbuf*);

$PAGE qinitbuf
(*    procedure to initialize buffer for first time    *)

public procedure qinitbuf(var buf: qbuffer);
begin
  with buf do
  begin
    lastlineno := 0;
    getlineno := 0;
    lbound := 1;
    offset := 1;
    oldoffset := 1;
    hbound := 0;
    curlineno := 0;
    new (firstlinep);				(* dummy zeroth line to make things easier *)
    with firstlinep^ do begin
      prevlinep := nil;
      nextlinep := nil
    end;
    lastlinep := firstlinep;
    getlinep := firstlinep;
    lboundp := nil;
    hboundp := firstlinep;
    garblinep := nil;
    curfile := '';
    curfileok := false;
    changes := false;
    mark := nil;
  end
end;						(* qinitbuf *)
$PAGE qgetline
(*    function to return text of line    *)

public function qgetline(var buf: qbuffer; l: qlineno; var err: qerrcode): qstring;

var
  lp: qlinep;
  lno: qlineno;

begin
  lno := map (buf, l);
  if not chkline(buf, lno, err) then qgetline:= ''
  else begin
    findlinep(buf, lno, lp);
    qgetline := substr (lp^.source, 1, length (lp^.source))
  end
end (*qgetline*);

$PAGE qmodline

public procedure qmodline(var buf: qbuffer; l: qlineno; newtext: qstring;
  var err: qerrcode);

var
  lp,np: qlinep;
  lno: qlineno;

begin
  lno := map (buf, l);
  if chkline(buf, lno, err) then begin
    findlinep(buf, lno, lp);
    np := makeline (buf, newtext);
    mask;
    with np^ do begin
      buf.garblinep := prevlinep;		(* remove new from garb list *)
      prevlinep := lp^.prevlinep;		(* chain new line to neighbors of old line *)
      nextlinep := lp^.nextlinep
    end;
    with lp^ do begin				(* chain neighbors to new line *)
      prevlinep^.nextlinep := np;		(* make forward chain *)
      if nextlinep <> nil			(* build backward chain *)
	then nextlinep^.prevlinep := np
	else buf.lastlinep := np;
      if lno = buf.lbound then buf.lboundp := np;   (* if this was special line, reset ptr *)
      if lno = buf.hbound then buf.hboundp := np;
      if lno = buf.getlineno then buf.getlinep := np;
    end;
    lp^.prevlinep := buf.garblinep;		(* put old on garb list to dispose *)
    buf.garblinep := lp;
    buf.changes := true;
    unmask;
    cleangarb (buf)				(* dispose old line *)
  end
end (*qmodline*);
$PAGE qaddline
(*    procedure to add a line to a buffer    *)

public procedure qaddline(var buf: qbuffer; l: qlineno; text:qstring;
  var err: qerrcode);

var
  lno: qlineno;
  np, lp: qlinep;

begin
  err := qok;					(* assume success *)
  lno := map (buf, l);
  if lno > buf.hbound then err := qbadln
  else begin
    findlinep (buf, lno, lp);			(* find line to append to *)
    np := makeline (buf, text);			(* create a line with text *)
    move (buf, 0, np, 0, np, lno, lp);		(* move from garblist to buffer *)
  end
end (*qaddline*);
$PAGE qdellines
(*    procedure to delete line(s) from buffer    *)

public procedure qdellines (var buf: qbuffer; f,l: qlineno; var err: qerrcode);

var
  fp,
  lp: qlinep;
  fno,
  lno: qlineno;
begin
  fno := map (buf, f);
  lno := map (buf, l);
  if chkrange (buf, fno, lno, err) then begin
    findlinep (buf, fno, fp);			(* find addressed lines *)
    findlinep (buf, lno, lp);
    move (buf, fno, fp, lno, lp, 0, nil);	(* move to garb list *)
    cleangarb (buf);				(* and dispose *)
    err := qok
  end
end (*qdellines*);
$PAGE qbuflength
public function qbuflength ( var buf: qbuffer ): qlineno;
 begin
   with buf do begin
     qbuflength := hbound - lbound + 1
   end
 end;

public function qdollar_val ( var buf: qbuffer ): qlineno;
  begin
    qdollar_val := qbuflength (buf) + buf.offset - 1
  end;    (* qdollar_val *)

public function qfirst_val ( var buf: qbuffer ): qlineno;
  begin
    qfirst_val := buf.offset
  end;    (* qfirst_val *)
$PAGE qmovelines
public procedure qmovelines
(	var buffer: qbuffer;			(* working buffer *)
	first, last: qlineno;			(* range of lines to be moved *)
	dest: qlineno;				(* where to move them to *)
	var err: qerrcode);			(* error report *)

var
  fno,
  lno,
  dno:		qlineno;			(* for line number mapping *)
  firstp,
  lastp:	qlinep;				(* temporary pointers *)
  destp:	qlinep;				(* where to re-attach lines *)

begin
  fno := map (buffer, first);
  lno := map (buffer, last);
  dno := map (buffer, dest);
  if not chkrange (buffer, fno, lno, err) then return;
  if not ((dno = buffer.lbound - 1) orif (chkline (buffer, dno, err))) then return;
  if (fno <= dno) and (dno <= lno) then begin	(* target within lines to be moved *)
    err := qbadmovela;
    return
  end;
  findlinep (buffer, fno, firstp);
  findlinep (buffer, lno, lastp);
  findlinep (buffer, dno, destp);
  move (buffer, fno, firstp, lno, lastp, dno, destp);	(* do it *)
end;
$PAGE qcopylines
public procedure qcopylines
(	var buffer: qbuffer;			(* working buffer *)
	first, last: qlineno;			(* range of lines to copy *)
	dest: qlineno;				(* where to copy them to *)
	var err: qerrcode);			(* error report *)

var
  source:	qstring;			(* to hold text of lines to be copied *)
  idx:		qlineno;			(* counter for lines *)
  fno,
  lno,
  dno:		qlineno;			(* for line number mapping *)
  firstp,
  lastp,
  destp:	qlinep;				(* working pointers *)

begin
  fno := map (buffer, first);
  lno := map (buffer, last);
  dno := map (buffer, dest);
  if not chkrange (buffer, fno, lno, err) then return;
  if not ((dno = buffer.lbound - 1) orif (chkline (buffer, dno, err))) then return;

  (* construct copy of lines to move on garb list *)

  cleangarb (buffer);				(* not really necessary, but good form *)
  firstp := nil;				(* to check if first line copied *)
  for idx := first to last do begin		(* copy lines, use relative #s with qgetline *)
    source := qgetline (buffer, idx, err);	(* get text of line *)
    if err <> qok then return;
    lastp := makeline (buffer, source);		(* append copy to garb list *)
    if firstp = nil then firstp := lastp	(* remember start *)
  end;

  (* move copy of lines into buffer *)

  findlinep (buffer, dno, destp);
  move (buffer, 0, firstp, lno-fno, lastp, dno, destp)

end;						(* qcopylines *)
$PAGE bounding utilities
(* routine to set thfer offset for addressing bounded lines *)
public procedure qsetoffset (newoffset: qlineno; var buffer: qbuffer);
begin
  buffer.oldoffset := buffer.offset;
  buffer.offset := newoffset
end;

public procedure qsetbounds (var buffer: qbuffer; low, high: qlineno;
	absolute: boolean; var err: qerrcode);

var
  tempoffset: qlineno;
  tempp: qlinep;				(* temporary storage *)
  fno,
  lno: qlineno;					(* for bound conversion *)

begin
  tempoffset := buffer.offset;
  if absolute then buffer.offset := buffer.lbound
  else buffer.offset := buffer.oldoffset;
  fno := map (buffer, low);
  lno := map (buffer, high);
  if chkrange (buffer, fno, lno, err) then
  with buffer do
  begin
    mask;
    findlinep (buffer, fno, tempp);
    findlinep (buffer, lno, hboundp);
    lboundp := tempp;
    lbound := fno - offset + 1;
    hbound := lno - offset + 1;
    unmask
  end;
  buffer.offset := tempoffset
end (* qsetbounds *);



public procedure qunbound (var buffer: qbuffer; var err: qerrcode);
begin
  err := qok;
  mask;
  with buffer do
  begin
    lbound := 1;
    lboundp := firstlinep^.nextlinep;
    hbound := lastlineno;
    offset := 1;
    hboundp := lastlinep
  end;
  unmask
end (* qunbound *);
$PAGE qfileappend

var
  f: text;					(* kludge around brain-damage *)

public procedure qfileappend
(	var buffer: qbuffer;			(* working buffer *)
	filename: file_id;			(* file to read text from *)
	where: qlineno;				(* where to append text *)
	var cnt: qlineno;			(* number of lines appended *)
	var err: qerrcode);			(* error report *)

label 1;

var
  line: packed array [1..qstringlen] of char;
  idx: qstringidx;
  ch: char;
  whereno: qlineno;				(* mapped address *)
  tline: qstring;
  first, last, wherep: qlinep;

begin
  err := qok;
  whereno := map (buffer, where);
  if whereno > buffer.hbound then begin
    err := qbadln;
    return
  end;

  cnt := 0;					(* leave cnt of zero, if open fails *)
  if open_file (f, filename, '   ', input_mode, []) then
  begin
    first := nil;
    loop
      idx := 0;
      if eoln (f) then readln (f);
    exit if eof (f) or (err <> qok);
      while not eoln (f) do
      begin
	if idx < length (line) then
	begin
	  idx := idx + 1;
	  line[idx] := f^;
	  get (f)
	end
	else goto 1
      end;
1:    tline := substr (line, 1, idx);
      last := makeline (buffer, tline);		(* alloc on garb list *)
      if first = nil then first := last;	(* remember first *)
      cnt := cnt + 1
    end;
    if cnt > 0 then begin			(* don't move zero lines *)
      findlinep (buffer, whereno, wherep);	(* find line at which to insert text *)
      move (buffer, 0, first, cnt-1, last, whereno, wherep);	(* move lines into buffer *)
    end;
    if (buffer.lastlineno = cnt) and (err = qok) then begin	(* append into empty buffer *)
      buffer.changes := false;
      buffer.curfile := pathname (f);
      buffer.curfileok := true
    end;
    close (f)
  end
  else err := qnofile
end;						(* qfileappend *)
$PAGE qttyappend
public procedure qttyappend
(	var buffer: qbuffer;			(* working buffer *)
	where: qlineno;				(* where to append text *)
	var cnt: qlineno;			(* number of lines appended *)
	var err: qerrcode);			(* error report *)

var
  line: qstring;
  ch: char;
  linenum: qlineno;
  done: boolean;

begin
  break;
  linenum := where;
  err := qok;
  if where = 0 then line := ''			(* get text of previous line to edit *)
  else line := qgetline (buffer, where, err);
  if err <> qok then return;
  done := false;
  while (err = qok) and (not done) do
  begin
    line := qread ;
    if (length (line) = 1) andif (line [1] = '.') then done := true
    else
    begin
      qaddline (buffer, linenum, line, err);
      linenum := linenum + 1
    end;
  end;
  if linenum > where then buffer.changes := true;
  cnt := linenum - where
end (* qttyappend *);
$PAGE qfilewrite

public procedure qfilewrite			(* write text to file *)
(       var buffer: qbuffer;			(* buffer to write from *)
	filename: file_id;			(* file to write to *)
	fn, ln: qlineno;			(* range to write *)
        confirm: boolean;			(* new/old file prompting? *)
	var err: qerrcode);			(* error report *)

var
  fno, lno: qlineno;
  flp, llp: qlinep;
  lineno: qlineno;
  line: qstring;
  options_set: io_option_set;

begin
  fno := map (buffer, fn);  lno := map (buffer, ln);
  if not chkrange (buffer, fno, lno, err) then return;
  if confirm then options_set := [confirm_open]
  else options_set := [];
  if open_file (f, filename, '   ', output_mode, options_set) then
  begin

  if pushescape then begin		(* file write truncation occurs! *)
    mask;
    writeln(tty,'Warning -- output file write incomplete'); break;
    escpop;
    firescape;
    unmask
    end (* file write escape handler *);
    findlinep (buffer, fno, flp);
    findlinep (buffer, lno, llp);
    loop
      writeln (f, flp^.source);
    exit if not eof(f) do err := qwrterr;
    exit if flp = llp;
      flp := flp^.nextlinep
    end;
    if (fno = 1) and (lno = buffer.lastlineno) and (err = qok) then
    begin
      buffer.changes := false;
      buffer.curfile := pathname (f);
      buffer.curfileok := true
    end;
    close (f)
  end
  else err := qnofile
end.						(* qfilewrite *)
   ,.&m