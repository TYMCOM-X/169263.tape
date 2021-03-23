$PAGE QEDLN -- Text Buffer Manager for QED
MODULE QEDLN OPTIONS SPECIAL;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                          q e d l n                           |
     |                          - - - - -                           |
     |                                                              |
     +--------------------------------------------------------------+

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

	9/17/81 djm    Replaced old MASK/UNMASK/PUSHESCAPE/ESCPOP/FIRESCAPE
		       attention handling with the new Exception Handling
		       constructs available in Pascal.

	9/21/81 djm    Removed $IF ANC code in procedure makeline, and removed
		       procedures QTAG, QTAGSET, and QTAGCLEAR.  This code is
		       still present in the ANC version.

	9/24/81 djm    Removed duplicate const declarations of lf and cr, 
		       which are now declared in QED.INC.

	9/25/81 djm    Added VAX code to QFILEAPPEND to read in lines from
		       a text file that contains control characters.

	9/28/81 djm    Added $IF P10 switches around certain system dependent
		       portions of the 940 file handling code.

       10/01/81 djm    Added more $IF P10 switches around more system dependent
		       portions of the 940 file handling code.

	5/17/82 djm    Added initialization of S940 flag in qfilewrite for
		       non-P10 code.

     ---------------------------------------------------------------- *)

$PAGE
(* The following constants help make QED device independent *)

const
$IFANY (VAX,M68)
  str_uppr_bnd = 65535;     (* max. length of a string *)
  units_per_len_word = 2;      (* how long is a length field *)
  chars_per_unit = 1;          (* 1 char per byte on VAX *)
$ENDIF

$IF P10
  str_uppr_bnd = maximum(integer);     (* max. length of a string *)
  units_per_len_word = 1;      (* how long is a length field *)
  chars_per_unit = 5;          (* 5 bytes per word on P10 *)
$ENDIF

(* SYNLINE IS USED FOR ITS PACKED ARRAY IT WILL BECOME A NODE ON 
   THE LINKED LIST *)

type
  str_word_len = packed 0..str_uppr_bnd;
  synlinep = ^synline;
  synline =
    packed record
      prevlinep,nextlinep:qlinep;
      strng: packed array[1..*] of char
    end;
$PAGE BUFF_TO_TMP1
PROCEDURE BUFF_TO_TMP1(var buffer:qbuffer; nbr_to_write:qlineno;
		       var err:qerrcode);

  var
    x:qlineno;
    p:qlinep;
    len_to_write: str_word_len;
    Q: SYNLINEP;
    STORAGE_UNITS_PER_STRING: QLINENO;

begin
  with buffer do begin
    for x := 1 to nbr_to_write do begin
      mask(attention);
      Q := PTR(ORD(FIRSTLINEP));
      len_to_write := length(firstlinep^.source);
      STORAGE_UNITS_PER_STRING := (LEN_TO_WRITE+CHARS_PER_UNIT-1) DIV CHARS_PER_UNIT;
      write(TMP1,len_to_write,q^.strng:storage_units_per_string,len_to_write);

      (* IS THIS THE LAST ONE IN THE BUFFER ? *)

      if firstlineno = lastlineno then
	empty_buff := true
      else begin
	firstlineno := firstlineno + 1;
	p := firstlinep;
	firstlinep := firstlinep^.nextlinep;
	firstlinep^.prevlinep := nil;
	unmask(attention);
	dispose(p)
      end
    end (* for *)

    EXCEPTION
      IO_ERROR:
      BEGIN
	UNMASK(ATTENTION);
	ERR := QBADSLINK
      END
  end  (* with *)
end;  (* buff_to_TMP1 *)
$PAGE TMP2_TO_BUFF
PROCEDURE TMP2_TO_BUFF(var buffer:qbuffer; nbr_to_write:qlineno;
		       var err:qerrcode);

  var
    x:qlineno;
    p:qlinep;
    q:synlinep;
    str_len:str_word_len;
    storage_units_per_string:  qlineno;

begin
  with buffer do begin
    for x := 1 to nbr_to_write do begin
      mask(attention);
      readrn(TMP2,cursor(TMP2)-units_per_len_word,str_len);
      storage_units_per_string := (str_len+chars_per_unit-1)
			 div chars_per_unit;
      seek(TMP2,cursor(TMP2)-storage_units_per_string-units_per_len_word);
      new(q,str_len);
      p := ptr(ord(q));
      read(TMP2,q^.strng:storage_units_per_string);
      seek(TMP2,cursor(TMP2)-storage_units_per_string);
      p^.nextlinep := nil;
      if empty_buff then begin
	empty_buff := false;
	if firstlineno <> 1 then  begin
	  firstlineno := firstlineno +  1;
	  lastlineno := lastlineno + 1
	end;
	firstlinep := p;
	DISPOSE(LASTLINEP);
	p^.prevlinep := nil
      end
      else begin
	p^.prevlinep := lastlinep;
	lastlineno := lastlineno + 1;
	lastlinep^.nextlinep := p
      end;
      lastlinep := p;
      if lastlineno = tmp2_last then 
	tmp2_last := 0;
      unmask(attention)
    end (* for *)

    EXCEPTION
      IO_ERROR:
      BEGIN
	UNMASK(ATTENTION);
	ERR := QBADSLINK
      END;
  end  (* with *)
end;  (* TMP2_TO_BUFF *)
$PAGE TMP2_TO_TMP1
PROCEDURE TMP2_TO_TMP1(var buffer:qbuffer; nbr_to_write:qlineno; var err:qerrcode);

  var
    x:qlineno;
    tline:packed array [1..254] of char;
    str_len:str_word_len;
    storage_units_per_string: qlineno;

begin
  with buffer do begin
    for x := 1 to nbr_to_write do begin
      mask(attention);
      readrn(TMP2,cursor(TMP2)-units_per_len_word,str_len);
      storage_units_per_string := (str_len+chars_per_unit-1) div chars_per_unit;
      readrn(TMP2,cursor(TMP2)-storage_units_per_string-units_per_len_word,
	    tline:storage_units_per_string);
      seek(TMP2,cursor(TMP2)-storage_units_per_string);
      write(TMP1,str_len,tline:storage_units_per_string,str_len);
      firstlineno := firstlineno + 1; (* keep track of first and last line numbers *)
      lastlineno := lastlineno + 1;
      if tmp2_last = lastlineno then
	tmp2_last := 0;
      unmask(attention)
    end (* for *)

    EXCEPTION
      IO_ERROR:
      BEGIN
	UNMASK(ATTENTION);
	ERR := QBADSLINK
      END;
  end  (* with *)
end;  (* TMP2_TO_TMP1 *)
$PAGE BUFF_TO_TMP2
PROCEDURE BUFF_TO_TMP2(var buffer:qbuffer; nbr_to_write:qlineno;
		       var err:qerrcode);

  var
    x:qlineno;
    p:qlinep;
    len_to_write: str_word_len;
    q: synlinep;
    storage_units_per_string:  qlineno;

begin
  with buffer do begin
    for x := 1 to nbr_to_write do begin
      mask(attention);
      q := ptr(ord(lastlinep));
      len_to_write := length(lastlinep^.source);
      storage_units_per_string := (len_to_write+chars_per_unit-1)
			div chars_per_unit;
      write(TMP2,q^.strng:storage_units_per_string,len_to_write);
      if tmp2_last = 0 then
	tmp2_last := lastlineno;
      if firstlineno = lastlineno then 
	empty_buff := true
      else begin
	lastlineno := lastlineno - 1;
	p := lastlinep;
	lastlinep := lastlinep^.prevlinep;
	lastlinep^.nextlinep := nil;
	unmask(attention);
	dispose(p)
      end
    end (* for *)

    EXCEPTION
      IO_ERROR:
      BEGIN
	UNMASK(ATTENTION);
	ERR := QBADSLINK
      END;
  end  (* with *)
end;  (* buff_to_tmp2 *)
$PAGE TMP1_TO_BUFF
PROCEDURE TMP1_TO_BUFF(var buffer:qbuffer; nbr_to_write:qlineno;
		       var err:qerrcode);

  var
    x:qlineno;
    q:synlinep;
    str_len:str_word_len;
    p:qlinep;
    storage_units_per_string: qlineno;

begin
  with buffer do begin
    for x := 1 to nbr_to_write do begin
      mask(attention);
      readrn(TMP1,cursor(TMP1)-units_per_len_word,str_len);
      new(q,str_len);
      storage_units_per_string := (str_len+chars_per_unit-1) div chars_per_unit;
      readrn(TMP1,cursor(TMP1)-storage_units_per_string-units_per_len_word,
	      q^.strng:storage_units_per_string);
      p := ptr(ord(q));
      p^.prevlinep := nil;
      if empty_buff then begin
	EMPTY_BUFF := FALSE;
	LASTLINENO := LASTLINENO - 1;
	p^.nextlinep := nil;
	lastlinep := p;
	IF FIRSTLINEP <> NIL THEN
	  DISPOSE(FIRSTLINEP);
      end
      else begin
	p^.nextlinep := firstlinep;
	firstlinep^.prevlinep := p
      end;
      seek(TMP1,cursor(TMP1)-storage_units_per_string-units_per_len_word);
      firstlinep := p;
      firstlineno := firstlineno - 1;
      unmask(attention)
    end (* for *)

    EXCEPTION
      IO_ERROR:
      BEGIN
	UNMASK(ATTENTION);
	ERR := QBADSLINK
      END;
  end  (* with *)
end;  (* TMP1_TO_BUFF *)
$PAGE TMP1_TO_TMP2
PROCEDURE TMP1_TO_TMP2(var buffer:qbuffer; nbr_to_write:qlineno;
		       var err:qerrcode);

  var
    x:qlineno;
    str_len:str_word_len;
    tline: packed array[1..254]  of char;
    storage_units_per_string: qlineno;

begin
  with buffer do begin
    for x := 1 to nbr_to_write do begin
      mask(attention);
      readrn(TMP1,cursor(TMP1)-units_per_len_word,str_len);
      storage_units_per_string := (str_len+chars_per_unit-1) div chars_per_unit;
      readrn(TMP1,cursor(TMP1)-storage_units_per_string-units_per_len_word,
	     tline:storage_units_per_string);
      seek(TMP1,cursor(TMP1)-storage_units_per_string-units_per_len_word);
      write(TMP2,tline:storage_units_per_string,str_len);
      if tmp2_last = 0 then 
	tmp2_last := lastlineno + 1;
      firstlineno := firstlineno - 1;
      lastlineno := lastlineno - 1;
      unmask(attention)
    end (* for *)

    EXCEPTION
      IO_ERROR:
      BEGIN
	UNMASK(ATTENTION);
	ERR := QBADSLINK
      END;
  end  (* with *)
end;  (* TMP1_TO_TMP2 *)
$PAGE FINDLINEP -- Find the line number of a node in the buffer

PROCEDURE findlinep(var buffer:qbuffer; linenbr:qlineno; var linep:qlinep;
		    var err:qerrcode);





  var x:qlineno;
      FILE_NAME: QSTRING;
$PAGE BEFORE_BUFF_SLINK -- Slink from TMP1 to in_core_buffer

  PROCEDURE BEFORE_BUFF_SLINK(var buffer:qbuffer; linenbr:qlineno; var err:qerrcode); 

    (* BEFORE_BUFF_SLINK is called by FINDLINEP when line < firstlineno.  The line
       wanted will become the first line in the buffer (i.e. firstlineno = line *)

    var
      save_firstno,
      nbr_to_write,
      now_in_buff:qlineno;

  begin
    with buffer do begin
      if TMP2 = NILF then begin
	update(TMP2,TEMP_FILE_NAME('QT2')||'[]',[SEEKOK]);
	tmp2_last := 0
      end;
      save_firstno := firstlineno;
      now_in_buff := lastlineno-firstlineno+1;

      (* EITHER CLEAR THE BUFFER OR SLINK ENOUGH ROOM FOR LINENBR *)

      nbr_to_write := min(now_in_buff,max(0,(firstlineno-linenbr-
			    (buffer_limit-now_in_buff))));
      BUFF_TO_TMP2(buffer,nbr_to_write,err);
      if err <> qok then return;

      (* GET LINENBR TO WITHIN A BUFFER_LIMIT'S DISTANCE *)

      nbr_to_write := max(0,(save_firstno-linenbr-buffer_limit));
      TMP1_TO_TMP2(buffer,nbr_to_write,err);
      if err <> qok then return;

      (* LINENBR IS NOW WITHIN A BUFFER_LIMIT'S DISTANCE.  GET IT IN THERE. *)

      nbr_to_write := firstlineno-linenbr;
      TMP1_TO_BUFF(buffer,nbr_to_write,err);
      if err <> qok then return;
    end (* with *)
  end;     (* BEFORE_BUFF_SLINK *)
$PAGE BEYOND_BUFF_SLINK -- Slink from TMP2 and/or INPUT_FILE to buffer
  PROCEDURE BEYOND_BUFF_SLINK(var buffer:qbuffer; linenbr:qlineno;
			      var err:qerrcode);

    (* BEYOND_BUFF_SLINK is called when the wanted line is beyond the last line
       contained in the buffer. The line wanted will become the last line in the
       buffer (i.e. lastlineno = linenbr).  *)

    var
      save_lastno,
      now_in_buff,
      nbr_to_write:qlineno;

  begin
    with buffer do begin
      if TMP1 = NILF then  
	update(TMP1,TEMP_FILE_NAME('QT1')||'[]',[SEEKOK]);
        FILE_NAME := FILENAME(TMP1);
      save_lastno := lastlineno;
      now_in_buff := lastlineno-firstlineno+1;
      if empty_buff then now_in_buff := 0;

      (* either clear the buffer or slink enough room to get linenbr *)

      nbr_to_write := max(0,min(now_in_buff,(linenbr-lastlineno-
			  (buffer_limit-now_in_buff))));
      BUFF_TO_TMP1(buffer,nbr_to_write,err);
      IF ERR <> QOK THEN RETURN;

      (* get linenbr to within a buffer_limit's distance *)

      IF EMPTY_BUFF AND (TMP2_LAST <> 0) THEN BEGIN
	IF TMP2_LAST < LINENBR THEN
	  NBR_TO_WRITE := TMP2_LAST-LASTLINENO
	ELSE IF (LINENBR-LASTLINENO) > BUFFER_LIMIT THEN
	  NBR_TO_WRITE := LINENBR-LASTLINENO-BUFFER_LIMIT
	ELSE NBR_TO_WRITE := 0;
	TMP2_TO_TMP1(buffer,nbr_to_write,err);
	IF ERR <> QOK THEN RETURN
      END;

      (* linenbr is now within a buffer_limit's distance.   get it in there *)

      IF TMP2_LAST <> 0 THEN BEGIN
	NBR_TO_WRITE := MIN(TMP2_LAST,LINENBR)-LASTLINENO;
	TMP2_TO_BUFF(buffer,nbr_to_write,err);
	IF ERR <> QOK THEN RETURN
      END  (* IF *)
    END (* WITH *)
  END;  (* BEYOND_BUFF_SLINK *)



begin (* mainstream of findlinep *)
  with buffer do begin

    (* GET LINENBR INTO THE IN-CORE BUFFER *)

    if (linenbr < firstlineno) then 
      BEFORE_BUFF_SLINK(buffer,linenbr,err)
    else if (linenbr > lastlineno) then
      BEYOND_BUFF_SLINK(buffer,linenbr,err);
    IF ERR <> QOK THEN RETURN;

    (* SEARCH  FOR LINENBR FROM CLOSEST END OF LIST *)

    if (linenbr - firstlineno) < (lastlineno - linenbr) then begin
      linep := firstlinep;
      IF LINENBR <> FIRSTLINENO THEN
	for x := firstlineno to linenbr-1 do
	  linep := linep^.nextlinep
    end
    else begin
      linep := lastlinep;
      IF LINENBR <> LASTLINENO THEN
	for x := lastlineno downto linenbr+1 do
	  linep := linep^.prevlinep
    end;  (* if *)
    mask(attention);
    getlineno := linenbr;
    getlinep := linep;
    unmask(attention)
  end (* with *)
end;   (* findlinep *)
$PAGE UTILITIES
(*********** text buffer manager utility routines ***********)

(* function to transform bounded linenos into absolute ones *)

FUNCTION MAP
  (       VAR BUFFER: QBUFFER;                    (* working buffer *)
	  LINE: QLINENO                           (* line number to transform *)
	  ): QLINENO;                     (* mapped result *)

BEGIN
  MAP := LINE + BUFFER.LBOUND - BUFFER.OFFSET
END;                                            (* map *)

(*    procedure to check a line number    *)

FUNCTION CHKLINE(VAR BUF: QBUFFER; L: QLINENO; VAR ERR: QERRCODE): BOOLEAN;
BEGIN
  ERR:= QOK;
  IF (L < BUF.LBOUND) OR (L > BUF.HBOUND) THEN ERR:= QBADLN;
  CHKLINE:= (ERR=QOK)
END (*chkline*);


(*    procedure to check a line range    *)

FUNCTION CHKRANGE(VAR BUF: QBUFFER; F,L: QLINENO; VAR ERR: QERRCODE): BOOLEAN;
BEGIN
  ERR:= QOK;
  IF F > L THEN ERR:= QBADRN
  ELSE IF L > BUF.HBOUND THEN ERR:= QBADUB
  ELSE IF F < BUF.LBOUND THEN ERR:= QBADLB;
  CHKRANGE:= (ERR=QOK)
END (*chkrange*);
$PAGE QTMPSCRATCH
(* PROCEDURE TO SCRATCH ALL THE TEMP FILES *)

PUBLIC PROCEDURE QTMPSCRATCH ( VAR BUFFER: QBUFFER);

BEGIN
  WITH BUFFER DO BEGIN
    MASK(ATTENTION);
    IF TMP1 <> NILF THEN
      SCRATCH(TMP1);
    IF TMP2 <> NILF THEN
      SCRATCH(TMP2);
    TMP1 := NILF;
    TMP2 := NILF;
    TMP2_LAST := 0;
    UNMASK(ATTENTION)
  END 
END;  (* QTMPSCRATCH *)
$PAGE MAKELINE
(* procedure to create a qed line record, does not chain it in *)

FUNCTION MAKELINE (VAR BUF: QBUFFER; LINE: QSTRING): QLINEP;

  VAR SYNP: SYNLINEP;
      NP: QLINEP;

BEGIN
  NEW (SYNP, LENGTH (LINE));                    (* alloc line of appropriate length *)
  SYNP^.STRNG[1:LENGTH(LINE)]:=LINE;            (* copy only to length allocated *)
  NP := ADDRESS (SYNP^);                        (* coerce the pointer *)
  MAKELINE := NP
END;                                           (* makeline *)
$PAGE QDELBUF
(*    procedure to delete a buffer    *)

PUBLIC PROCEDURE QDELBUF(VAR BUF: QBUFFER);

  VAR ERR: QERRCODE;                            (*we need it but we ignore them*)
      x: qlineno;
      p: qlinep;

BEGIN                                           (*qdelbuf*)
  WITH BUF DO BEGIN
    mask(attention);
    if firstlinep <> NIL then begin
      while firstlinep^.nextlinep <> NIL do begin
	p := firstlinep;
	firstlinep := firstlinep^.nextlinep;
	disp)
      end;
      dispose(firstlinep);
      firstlinep := NIL
    end;
    if FILENAME(TMP1) <> '' THEN
      scratch(TMP1);
    IF FILENAME(TMP2) <> '' THEN
      SCRATCH(TMP2);
    TMP1 := NILF;
    TMP2 := NILF;
    tmp2_last := 0;
    unmask(attention);
    SPREDDISPOSE (MARK);                        (* dispose mark predicate *)
    MARK := NIL                                 (* for good measure *)
  END                                           (*with*)
END (*qdelbuf*);
$PAGE QINITBUF
(*    procedure to initialize buffer for first time    *)

PUBLIC PROCEDURE QINITBUF(VAR BUF: QBUFFER);
BEGIN
  WITH BUF DO BEGIN
    firstlineno := 0;
    LASTLINENO := 0;
    GETLINENO := 0;
    LBOUND := 1;
    OFFSET := 1;
    OLDOFFSET := 1;
    HBOUND := 0;
    nbr_in_entire_file := 0;
    CURLINENO := 0;
    firstlinep := nil;
    buffer_limit := 500;
    tmp2_last := 0;
    NEXT_INPUT := 1;
    IF FILENAME(TMP1) <> '' THEN
      SCRATCH(TMP1);
    IF FILENAME(TMP2) <> '' THEN
      SCRATCH(TMP2);
    TMP1 := NILF;
    TMP2 := NILF;
    TMP3 := NILF;
    F := NILF;
    LASTLINEP := FIRSTLINEP;
    GETLINEP := FIRSTLINEP;
    LBOUNDP := NIL;
    HBOUNDP := FIRSTLINEP;
    GARBLINEP := NIL;
    CURFILE := '';
    CURFILEOK := FALSE;
    CHANGES := FALSE;
    empty_buff := true;
    MARK := NIL;
  END
END;                                            (* qinitbuf *)
$PAGE QGETLINE
(*    function to return text of line    *)

PUBLIC FUNCTION QGETLINE(VAR BUF: QBUFFER; L: QLINENO; VAR ERR: QERRCODE): QSTRING;

  VAR
    LP: QLINEP;
    LNO: QLINENO;

BEGIN
  LNO := MAP (BUF, L);
  IF NOT CHKLINE(BUF, LNO, ERR) THEN QGETLINE:= ''
  ELSE BEGIN
    FINDLINEP(BUF, LNO, LP, ERR);
    QGETLINE := SUBSTR (LP^.SOURCE, 1, LENGTH (LP^.SOURCE))
  END
END (*qgetline*);
$PAGE QMODLINE

PUBLIC PROCEDURE QMODLINE(VAR BUF: QBUFFER; L: QLINENO; NEWTEXT: QSTRING;
			  VAR ERR: QERRCODE);

  VAR
    LP,NP: QLINEP;
    LNO: QLINENO;

BEGIN
  LNO := MAP (BUF, L);
  IF CHKLINE(BUF, LNO, ERR) THEN BEGIN
    FINDLINEP(BUF, LNO, LP, ERR);
    NP := MAKELINE (BUF, NEWTEXT);
    MASK(ATTENTION);
    np^.prevlinep := lp^.prevlinep;
    np^.nextlinep := lp^.nextlinep;
    if lp^.prevlinep <> NIL then
      lp^.prevlinep^.nextlinep := np;
    if lp^.nextlinep <> NIL then
      lp^.nextlinep^.prevlinep := np;
    if lno = buf.getlineno then
      buf.getlinep := np;
    if lno = buf.firstlineno then
      buf.firstlinep := np
    else if lno = buf.lastlineno then
      buf.lastlinep := np;
    BUF.CHANGES := TRUE;
    UNMASK(ATTENTION);
  END
END (*qmodline*);
$PAGE QADDLINE
(*    procedure to add a line to a buffer    *)

PUBLIC PROCEDURE QADDLINE(VAR BUF: QBUFFER; L: QLINENO; TEXT:QSTRING;
			  VAR ERR: QERRCODE);

  VAR
    LNO: QLINENO;
    NP, LP: QLINEP;
    ZEROADD: BOOLEAN;

BEGIN
  with buf do begin
    zeroadd := false;
    ERR := QOK;                                   (* assume success *)
    lno := l;
    if lno = 0 then begin
      zeroadd := true;
      lno := 1
    end;
    LNO := MAP (BUF, LNO);
    (* NO QBADLN ERR IF HBOUND<LBOUND  (I.E EMPTY BOUNDED SECTION)  *)
    IF (LNO > HBOUND) AND (HBOUND >= LBOUND) THEN BEGIN
      ERR := QBADLN;
      RETURN
    END;
    IF NBR_IN_ENTIRE_FILE <> 0 THEN  (* FINDLINEP only if file has been loaded *)
      FINDLINEP (BUF, LNO, LP, ERR);                   (* find line to append to *)
    NP := MAKELINE (BUF, TEXT);                 (* create a line with text *)

    (* if buffer is full then slink one node off the list, away from the line 
       being appended to (in case that line is firstlineno or lastlineno) *)

    if buffer_limit <= (lastlineno-firstlineno+1) then begin
      if lno = firstlineno then begin
	if TMP2 = NILF then
	  update(TMP2,TEMP_FILE_NAME('QT2')||'[]',[SEEKOK]);
	BUFF_TO_TMP2(buf,1,err)
      end
      else begin
	if TMP1 = NILF then
	  update(TMP1,TEMP_FILE_NAME('QT1')||'[]',[SEEKOK]);
	BUFF_TO_TMP1(buf,1,err)
      end
    end;
    mask(attention);
    if zeroadd then begin
      np^.nextlinep := lp;
      np^.prevlinep := lp^.prevlinep;
      if lp^.prevlinep <> nil then
	lp^.prevlinep^.nextlinep := np;
      lp^.prevlinep := np;
      if lno = firstlineno then 
        firstlinep := np;
      if empty_buff then begin
	lastlinep := np;
	empty_buff := false
      end
    end
    else begin
      np^.nextlinep := lp^.nextlinep;
      np^.prevlinep := lp;
      lp^.nextlinep := np;
      if np^.nextlinep <> NIL then
	np^.nextlinep^.prevlinep := np;
      if lno = lastlineno THEN 
	lastlinep := np
    end;
    lastlineno := lastlineno + 1;
    if lno <= HBOUND then begin
      HBOUND := HBOUND + 1;
      if lno < LBOUND then
	LBOUND := LBOUND + 1
    end;
    (* if the bounded section was empty before the add set things straight *)
    if hbound < lbound then
      hbound := lbound;   (* now they both point to the added record *)
    if HBOUND = 0 then
      HBOUND := HBOUND + 1;
    if LBOUND = 0 then
      LBOUND := LBOUND + 1;
    if tmp2_last <> 0 then
      tmp2_last := tmp2_last + 1;
    nbr_in_entire_file := nbr_in_entire_file + 1;
    changes := true;
    unmask(attention);
  end  (* with *)
END (*qaddline*);
$PAGE QDELLINES - Delete one or more lines, one at a time.

PUBLIC PROCEDURE QDELLINES (var buf:qbuffer; f,l:qlineno; var err:qerrcode);

  var
    fp,lp,linep:qlinep;
    fno,lno,x:qlineno;

begin
  fno := map(buf,f);
  lno := map(buf,l);
  if not chkrange (buf,fno,lno,err) then 
    return;
  with buf do begin
    (* Insure buffer will never be left empty by getting fno-1 into buffer *)
    if firstlineno <> 1 then 
      findlinep(buf,fno-1,linep,err);
    for x := 1 to (lno-fno+1) do begin   (* delete lno-fno+1 records *)
      findlinep(buf,fno,linep,err);
      mask(attention);
      if linep = firstlinep then begin  
	if firstlinep^.nextlinep <> NIL then begin
	  firstlinep := firstlinep^.nextlinep;
	  firstlinep^.prevlinep := nil
	end
      end
      else if linep = lastlinep then begin
	lastlinep := lastlinep^.prevlinep;
	lastlinep^.nextlinep := nil
      end
      else begin
	linep^.nextlinep^.prevlinep := linep^.prevlinep;
	linep^.prevlinep^.nextlinep := linep^.nextlinep
      end;
      IF (FNO=FIRSTLINENO) AND (FNO=LASTLINENO) THEN BEGIN
	empty_buff := true;
	firstlineno := firstlineno - 1
      end
      else
	dispose(linep);
      lastlineno := lastlineno - 1;
      if fno <= HBOUND then
	HBOUND := HBOUND - 1;
      if fno < LBOUND then 
	LBOUND := LBOUND - 1;
      if fno <= GETLINENO then
	GETLINENO := GETLINENO - 1;
      if tmp2_last <> 0 then 
	tmp2_last := tmp2_last - 1;
	 (*if tmp2_last = 1 then
	  tmp2_last := 0
      end;  *)
      if fno < curlineno then
	curlineno := curlineno -1;
      nbr_in_entire_file := nbr_in_entire_file - 1;
      unmask(attention);
    end;  (* for *)
    changes := true
  end
end;   (* qdellines *)
$PAGE QBUFLENGTH
PUBLIC FUNCTION QBUFLENGTH ( VAR BUF: QBUFFER ): QLINENO;
BEGIN
  WITH BUF DO BEGIN
    QBUFLENGTH := HBOUND - LBOUND + 1
  END
END;

PUBLIC FUNCTION QDOLLAR_VAL ( VAR BUF: QBUFFER ): QLINENO;
BEGIN
  QDOLLAR_VAL := QBUFLENGTH (BUF) + BUF.OFFSET - 1
END;                                          (* qdollar_val *)

PUBLIC FUNCTION QFIRST_VAL ( VAR BUF: QBUFFER ): QLINENO;
BEGIN
  QFIRST_VAL := BUF.OFFSET
END;                                          (* qfirst_val *)
$PAGE QMOVELINES
PUBLIC PROCEDURE QMOVELINES
  (       VAR BUFFER: QBUFFER;                    (* working buffer *)
	  FIRST, LAST: QLINENO;                   (* range of lines to be moved *)
	  DEST: QLINENO;                          (* where to move them to *)
	  VAR ERR: QERRCODE);                     (* error report *)

  VAR
    nbr_moved,
    FNO,
    LNO,
    DNO:          QLINENO;                        (* for line number mapping *)
    FIRSTP,
    LASTP:        QLINEP;                         (* temporary pointers *)
    DESTP:        QLINEP;                         (* where to re-attach lines *)

BEGIN
  FNO := MAP (BUFFER, FIRST);
  LNO := MAP (BUFFER, LAST);
  DNO := MAP (BUFFER, DEST);
  IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR) THEN RETURN;
  IF NOT ((DNO = BUFFER.LBOUND - 1) ORIF (CHKLINE (BUFFER, DNO, ERR))) THEN RETURN;
  IF (FNO <= DNO) AND (DNO <= LNO) THEN BEGIN   (* target within lines to be moved *)
    ERR := QBADMOVELA;
    RETURN
  END;
  qcopylines(buffer,fno,lno,dno,err);
  if dno < fno then begin    (* adjust line numbers if necessary *)
    nbr_moved := lno-fno+1;
    fno := fno + nbr_moved;
    lno := lno + nbr_moved
  end;
  QDELLINES(buffer,fno,lno,err)
END;
$PAGE QCOPYLINES
PUBLIC PROCEDURE QCOPYLINES
  (       VAR BUFFER: QBUFFER;                    (* working buffer *)
	  FIRST, LAST: QLINENO;                   (* range of lines to copy *)
	  DEST: QLINENO;                          (* where to copy them to *)
	  VAR ERR: QERRCODE);                     (* error report *)

  VAR
    SOURCE:       QSTRING;                        (* to hold text of lines to be copied *)
    IDX:          QLINENO;                        (* counter for lines *)
    FNO,
    LNO,
    DNO:          QLINENO;                        (* for line number mapping *)
    FIRSTP,
    LASTP,
    DESTP:        QLINEP;                         (* working pointers *)
    current_nbr,
    nbr_to_write,
    nbr_free,
    cnt,
    x:             qlineno;
    str_len: str_word_len;
    len_to_write: str_word_len;
    FA_NAME,    (* NAME OF TMP3 TO PASS TO QFILEAPPEND *)
    f_name: qstring;




BEGIN
  with  buffer do begin
    FNO := MAP (BUFFER, FIRST);
    LNO := MAP (BUFFER, LAST);
    DNO := MAP (BUFFER, DEST);
    IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR) THEN RETURN;
    IF NOT ((DNO = LBOUND - 1) ORIF (CHKLINE (BUFFER, DNO, ERR))) THEN RETURN;
    mask(attention);
    rewrite(TMP3,TEMP_FILE_NAME('QT3')||'[,]');
    F_NAME := FILENAME(TMP3);

    (* HOLD THE LINES TO BE COPIED IN A TEMP FILE (TMP3) *)

    for x := fno to lno do begin
      findlinep(buffer,x,firstp,err);
      writeln(TMP3,firstp^.source);
    end;
    findlinep(buffer,dno,destp,err);
    cnt := lno-fno+1;
    F_NAME := CURFILE;    (* REMEMBER CURRENT FILE NAME *)
    FA_NAME := FILENAME(TMP3);  (* PASS TMP3 NAME TO QFILEAPPEND *)
    close(TMP3);
    TMP3 := NILF;
    (* Pass relative line nbr. (dest) not absolute (dno)  *)
    QFILEAPPEND(BUFFER,FA_NAME,'',DEST,CNT,ERR);
    CURFILE := F_NAME;
    OPEN(TMP3,FA_NAME);
    F_NAME := FILENAME(TMP3);
    SCRATCH(TMP3);
    TMP3 := NILF;
    
    EXCEPTION
      IO_ERROR:
      BEGIN
        UNMASK(ATTENTION);
        ERR := QBADSLINK
      END
  end (* with *)
END;                                            (* qcopylines *)
$PAGE BOUNDING UTILITIES
(* routine to set the buffer offset for addressing bounded lines *)

PUBLIC PROCEDURE QSETOFFSET (NEWOFFSET: QLINENO; VAR BUFFER: QBUFFER);
BEGIN
  BUFFER.OLDOFFSET := BUFFER.OFFSET;
  BUFFER.OFFSET := NEWOFFSET
END;

PUBLIC PROCEDURE QSETBOUNDS (VAR BUFFER: QBUFFER; LOW, HIGH: QLINENO;
			     ABSOLUTE: BOOLEAN; VAR ERR: QERRCODE);

  VAR
    TEMPOFFSET: QLINENO;
    TEMPP: QLINEP;                                (* temporary storage *)
    FNO,
    LNO: QLINENO;                                 (* for bound conversion *)

BEGIN
  TEMPOFFSET := BUFFER.OFFSET;
  IF ABSOLUTE THEN BUFFER.OFFSET := BUFFER.LBOUND
  ELSE BUFFER.OFFSET := BUFFER.OLDOFFSET;
  FNO := MAP (BUFFER, LOW);
  LNO := MAP (BUFFER, HIGH);
  IF CHKRANGE (BUFFER, FNO, LNO, ERR) THEN
    WITH BUFFER DO
      BEGIN
	MASK(ATTENTION);
	LBOUND := FNO - OFFSET + 1;
	HBOUND := LNO - OFFSET + 1;
	UNMASK(ATTENTION)
      END;
  BUFFER.OFFSET := TEMPOFFSET;
  BUFFER.CURLINENO := QFIRST_VAL (BUFFER)
END (* qsetbounds *);



PUBLIC PROCEDURE QUNBOUND (VAR BUFFER: QBUFFER; VAR ERR: QERRCODE);
BEGIN
  ERR := QOK;
  MASK(ATTENTION);
  WITH BUFFER DO
    BEGIN
      LBOUND := 1;
      LBOUNDP := FIRSTLINEP;
      HBOUND := nbr_in_entire_file;
      OFFSET := 1;
      HBOUNDP := LASTLINEP
    END;
  UNMASK(ATTENTION)
END (* qunbound *);
$PAGE QREADINPUT
PROCEDURE QREADINPUT (VAR BUFFER: QBUFFER; VAR TLINE: QSTRING;
		      VAR WERR: WCODES;  VAR ERR: QERRCODE;  VAR THE_FILE: TEXT);

  VAR
    JUNKLINE: BOOLEAN;
    WCHAN: WCHANNEL;
    RLINE: QSTRING;

BEGIN
  WITH BUFFER DO BEGIN
    MASK(ATTENTION);
$IF P10
    IF S940                                     (* 940 files have special read routine *)
    THEN WINLINE (WCHAN, WERR, TLINE)
    ELSE BEGIN (* NORMAL FILES *)
      TLINE := '';                              (* Start out fresh      *)

      (*
      *       Although there are a number of characters which will end lines
      *       as far as Pascal is concerned, the only acceptable line end
      *       characters for us are a CR-LF pair and CR by itself (not
      *       recognized by PDP-10 I/O as a line end).  Accordingly, we
      *       watch out for all other cases and force them to wrap around
      *       for up to QSTRINGLEN characters.  EOF forces a halt, regardless
      *       of the character in the buffer.
      *
      *       Note that a line of QSTRINGLEN characters should not generate a
      *       zero length line following it.  Thus, if the character in F^ is a
      *       CR, the line must be thrown out if the previous line had QSTRINGLEN
      *       characters and the current line has only one (the CR).  This is
      *       accomplished with a hack.  The hack can be removed if the run time
      *       code ever treats CR-LF as a line end.
      *)

      REPEAT                                  (* Until we fill a QED line     *)
	JUNKLINE := FALSE;                      (* Line looks good so far       *)
	IF EOLN (THE_FILE) THEN BEGIN                             (* Fetch a line, if needed      *)
	  IF THE_FILE^ <> CR THEN                           (* Force nonstandard lines to wrap *)
	    TLINE := TLINE || THE_FILE^;
	  READLN (THE_FILE)
	END
	ELSE JUNKLINE := TRUE;                  (* This might be a phony null line *)
	READ (THE_FILE, RLINE: QSTRINGLEN - LENGTH (TLINE));   (* Don't get too much   *)
	IF EOF (THE_FILE) THEN
	  THE_FILE^ := CR;                          (* Always stop on EOF   *)
	IF LENGTH (RLINE) > 0 THEN BEGIN
	  IF EOLN (THE_FILE) AND (THE_FILE^=LF) AND (RLINE[LENGTH(RLINE)] = CR) THEN BEGIN
	    THE_FILE^ := CR;                           (* and set CR as fake line end  *)
	    IF LENGTH (RLINE) > 1 THEN BEGIN
	      TLINE := TLINE || SUBSTR (RLINE, 1, LENGTH (RLINE) - 1);
	      JUNKLINE := FALSE
	    END
	  END
	  ELSE BEGIN
	    TLINE := TLINE || RLINE;            (* Bad line end. Will wrap      *)
	    JUNKLINE := FALSE                   (* Don't eat the line   *)
	  END
	END
	ELSE IF (LENGTH (TLINE) = 0) AND EOF (THE_FILE)
	THEN WERR := WEOF                       (* Absolutely all done  *)
      UNTIL (EOLN (THE_FILE) AND (THE_FILE^ = CR) AND NOT JUNKLINE) OR
	(LENGTH (TLINE) = QSTRINGLEN)
    END;
$END
$IFANY (VAX, M68)
    READLN(THE_FILE);
    IF EOF(THE_FILE) THEN
      WERR := WEOF
    ELSE
      READ(THE_FILE,TLINE);
$END
    UNMASK(ATTENTION);
  END  (* WITH *)
END;  (* QREADINPUT *)
$PAGE QFILELOAD -- Load Buffer Text from a File
PUBLIC PROCEDURE QFILELOAD
  (       VAR BUFFER: QBUFFER;                    (* working buffer *)
	  S940ID: FILE_ID;                        (* file to read text from       *)
	  WMOD: WMODIFIER;                        (* 940 file modifier    *)
	  WHERE: QLINENO;                         (* where to append text *)
	  VAR CNT: QLINENO;                       (* number of lines appended *)
	  VAR ERR: QERRCODE);                     (* error report *)

  VAR
    IDX: QSTRINGIDX;
    CH: CHAR;
    WHERENO: QLINENO;                             (* mapped address *)
    TLINE, RLINE: QSTRING;
    FIRST, LAST, WHEREP: QLINEP;
    PDP10ID: FILE_ID;                       (* Converted name of file       *)
    RFLAG:  BOOLEAN;                        (* Kludgy documentation revision switch *)
    WERR:   WCODES;                         (* Return codes from 940 I/O routines   *)
    S940:   BOOLEAN;                        (* True if file is 940 file     *)
    WCHAN:  WCHANNEL;                       (* Channel number from which to read    *)
    JUNKLINE: BOOLEAN;                      (* True if line should be ditched *)
    x: qlineno;
    p: qlinep;
    storage_units_per_string,
    len_to_write: qlineno;
    packline: packed array [1..254] of char;
    filesize: 0..999999;

BEGIN
  with buffer do begin
    ERR := QOK;                                     (* Start with a clean slate     *)
    WHERENO := MAP (BUFFER, WHERE);
    CNT := 0;                                       (* No lines read yet    *)
    S940 := FALSE;
    PDP10ID := S940ID;
$IF P10

    (*
    *       Convert the file name and figure out what kind of a file we're
    *       dealing with.  If it is a 940 file, use the special 940 file
    *       opening routine; otherwise, just use standard I/O.
    *)

    MASK(ATTENTION);
    WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
    UNMASK(ATTENTION);
    IF WERR = WBADNAME THEN
      ERR := QNOFILE                             (* Stop obvious garbage early   *)
    ELSE IF WERR = WOK THEN BEGIN
      WCHAN := GETCHANNEL;
      WOPEN (WCHAN, WERR, WINPUT, PDP10ID);
      IF WERR <> WOK THEN BEGIN
	FREECHANNEL (WCHAN);                        (* Open failed. Return channel to pool  *)
	ERR := QNOFILE
      END
      ELSE S940 := TRUE;                            (* Open succeeded. Leave the word       *)
    END
    ELSE
$END
      QOPENFILE (F, PDP10ID, '', QINPUT_MODE, [QIO_ASCII], ERR);

    (* IF OPEN WENT OKAY THEN READ THE INPUT FILE *)

    IF ERR = QOK THEN BEGIN
      FIRST := NIL;                                 (* Initialize new chain *)
      WERR := WOK;
      IF NOT S940 THEN
	F^ := CR;                                (* Initialize normal file buffer        *)

      (* read until EOF or cnt>buffer_limit *)

      repeat
	QREADINPUT(BUFFER,TLINE,WERR,ERR,F);
      EXIT IF WERR <> WOK;                        (* Either EOF or error  *)
	MASK(ATTENTION);
	p := MAKELINE (BUFFER, TLINE);           (* Chain line into list *)
	p^.nextlinep := NIL;
	p^.prevlinep := NIL;
	if first = NIL then begin
	  first := p;
	  last := p
	end
	else begin
	  p^.prevlinep := last;
	  last^.nextlinep := p;
	  last := p
	end;
	CNT := CNT + 1;
	UNMASK(ATTENTION);
      until EOF(F) or (cnt >= buffer_limit)
    END; (* IF *)

    (*
    *       The file has now been  read in.  If there was anything
    *       there, chain the new blob of lines into the proper slot.  Indicate
    *       that the buffer is unchanged if it was initially fresh.  Close
    *       the file, reenable attention interrupts, and leave.
    *)

    IF CNT > 0 THEN BEGIN
      firstlinep := first; 
      firstlineno := 1;
      lastlinep := last;
      EMPTY_BUFF := FALSE;
      lastlineno := cnt;
      hbound := cnt;
      next_input := next_input + cnt;
      nbr_in_entire_file := cnt
    END;
    FILESIZE := 1;

    (* ADD UP NBR. OF STORAGE UNITS LEFT IN THE FILE.  *)

    WHILE NOT EOF(f) DO BEGIN
      QREADINPUT(BUFFER,TLINE,WERR,ERR,F);
      IF NOT EOF(F) THEN BEGIN
	LEN_TO_WRITE := LENGTH(TLINE);
	storage_units_per_string := (len_to_write+chars_per_unit-1)
				div chars_per_unit;
	filesize := filesize + storage_units_per_string + units_per_len_word;
	nbr_in_entire_file := nbr_in_entire_file + 1
      END  (* IF *)
    end;  (* while *)
    hbound := nbr_in_entire_file;
    tmp2_last := nbr_in_entire_file;

    (*  CLOSE AND REOPEN THE INPUT FILE.  GETTING READY TO READ *)

    IF S940 THEN BEGIN
$IF P10
      WCLOSE(WCHAN,WERR);
      FREECHANNEL(WCHAN)
$END
    END
    ELSE BEGIN
      CLOSE(F);
      F := NILF
    END;
    IF FILESIZE <> 0 THEN BEGIN
$IF P10
      MASK(ATTENTION);
      WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
      UNMASK(ATTENTION);
      IF WERR = WBADNAME THEN
	ERR := QNOFILE                             (* Stop obvious garbage early   *)
      ELSE IF WERR = WOK THEN BEGIN
	WCHAN := GETCHANNEL;
	WOPEN (WCHAN, WERR, WINPUT, PDP10ID);
	IF WERR <> WOK THEN BEGIN
	  FREECHANNEL (WCHAN);                        (* Open failed. Return channel to pool  *)
	  ERR := QNOFILE
	END
	ELSE S940 := TRUE;                            (* Open succeeded. Leave the word       *)
      END
      ELSE
$END
	QOPENFILE (F, PDP10ID, '', QINPUT_MODE, [QIO_ASCII], ERR);

      (*  IF OPEN WENT OKAY THEN DO :  *)

      IF ERR = QOK THEN BEGIN
	FIRST := NIL;                                 (* Initialize new chain *)
	WERR := WOK;
	IF NOT S940 THEN
	  F^ := CR;                                (* Initialize normal file buffer        *)

	(*  1 - READ TO THE HIGH WATER MARK OF THE INPUT FILE  *)

	FOR X := 1 TO NEXT_INPUT-1 DO BEGIN
	  QREADINPUT(BUFFER,TLINE,WERR,ERR,F);
	  IF WERR <> WOK THEN RETURN;
	end;  (* for *)
	IF TMP2 <> NILF THEN BEGIN
	  SCRATCH(TMP2);
	  TMP2 := NILF
	END;
	UPDATE(TMP2,TEMP_FILE_NAME('QT2')||'[]',[SEEKOK]);
	SEEK(TMP2,FILESIZE);

	(*  2 - WRITE REST OF INPUT TO TMP2 (backwards)  *)

	FOR X := NEXT_INPUT TO NBR_IN_ENTIRE_FILE DO BEGIN
	  QREADINPUT(BUFFER,TLINE,WERR,ERR,F);
	  IF NOT EOF(F) THEN BEGIN
	    PACKLINE := TLINE;
	    LEN_TO_WRITE := LENGTH(TLINE);
	    STORAGE_UNITS_PER_STRING := (LEN_TO_WRITE+CHARS_PER_UNIT-1)
				       DIV CHARS_PER_UNIT;
	    SEEK(TMP2,CURSOR(TMP2)-STORAGE_UNITS_PER_STRING-UNITS_PER_LEN_WORD);
	    WRITE(TMP2,PACKLINE:STORAGE_UNITS_PER_STRING,LEN_TO_WRITE);
	    SEEK(TMP2,CURSOR(TMP2)-STORAGE_UNITS_PER_STRING-UNITS_PER_LEN_WORD)
	  END (* IF *)
	END; (* FOR *)
	CLOSE(F);
	F := NILF;
	SEEK(TMP2,FILESIZE)
      END (* IF *)
    END; (* IF *)

    IF ERR = QOK THEN BEGIN
      CHANGES := FALSE;                    (* Buffer starts out clean      *)
      IF S940 THEN BEGIN
$IF P10
	WFILENAME (WCHAN, CURFILE, RFLAG);
	S940 := TRUE                       (* This is now a 940 buffer     *)
$END
      END
      ELSE BEGIN
	CURFILE := PDP10ID;
	S940 := FALSE
      END;
      CURFILEOK := TRUE
    END;

    EXCEPTION
      OTHERS: BEGIN
		MASK(ATTENTION);
		IF S940 THEN BEGIN
$IF P10
		  WCLOSE (WCHAN, WERR);
		  FREECHANNEL (WCHAN)
$END
		END
		ELSE BEGIN                           (* Standard close for most things       *)
		  CLOSE (F);
		  F := NILF
		END;
		UNMASK(ATTENTION);
		SIGNAL();
	      END
  end  (* with *)
END;                                            (* qfileload *)
$PAGE QFILEAPPEND 

VAR
  FA: TEXT;                                      (* kludge around brain-damage *)

PUBLIC PROCEDURE QFILEAPPEND
  (       VAR BUFFER: QBUFFER;                    (* working buffer *)
	  S940ID: FILE_ID;                        (* file to read text from       *)
	  WMOD: WMODIFIER;                        (* 940 file modifier    *)
	  WHERE: QLINENO;                         (* where to append text *)
	  VAR CNT: QLINENO;                       (* number of lines appended *)
	  VAR ERR: QERRCODE);                     (* error report *)

  VAR
    IDX: QSTRINGIDX;
    CH: CHAR;
    WHERENO: QLINENO;                             (* mapped address *)
    TLINE, RLINE: QSTRING;
    FIRST, LAST, WHEREP: QLINEP;
    PDP10ID: FILE_ID;                       (* Converted name of file       *)
    RFLAG:  BOOLEAN;                        (* Kludgy documentation revision switch *)
    WERR:   WCODES;                         (* Return codes from 940 I/O routines   *)
    S940:   BOOLEAN;                        (* True if file is 940 file     *)
    WCHAN:  WCHANNEL;                       (* Channel number from which to read    *)
    JUNKLINE: BOOLEAN;                      (* True if line should be ditched *)
    len_to_write: qlineno;
    p: qlinep;
    storage_units_per_string:  qlineno;
    packline: packed array [1..254] of char;
    nbr_to_write: qlineno;
    zero_add: boolean;

BEGIN
  with buffer do begin
    ERR := QOK;                                     (* Start with a clean slate     *)
    WHERENO := MAP (BUFFER, WHERE);
    IF WHERENO > HBOUND THEN BEGIN
      ERR := QBADLN;
      RETURN
    END;
    CNT := 0;
    if where=0 then begin
      whereno := whereno + 1;
      zero_add := true
    end
    else zero_add := false;
    S940 := FALSE;
    PDP10ID := S940ID;
$IF P10

    (*
    *       Convert the file name and figure out what kind of a file we're
    *       dealing with.  If it is a 940 file, use the special 940 file
    *       opening routine; otherwise, just use standard I/O.
    *)

    MASK(ATTENTION);
    WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
    UNMASK(ATTENTION);
    IF WERR = WBADNAME THEN
      ERR := QNOFILE                             (* Stop obvious garbage early   *)
    ELSE IF WERR = WOK THEN BEGIN
      WCHAN := GETCHANNEL;
      WOPEN (WCHAN, WERR, WINPUT, PDP10ID);
      IF WERR <> WOK THEN BEGIN
	FREECHANNEL (WCHAN);                        (* Open failed. Return channel to pool  *)
	ERR := QNOFILE
      END
      ELSE S940 := TRUE;                            (* Open succeeded. Leave the word       *)
    END
    ELSE
$END
      QOPENFILE (FA, PDP10ID, '', QINPUT_MODE, [QIO_ASCII], ERR);
    IF ERR = QOK THEN BEGIN
      FIRST := NIL;                                 (* Initialize new chain *)
      WERR := WOK;
      IF NOT S940 THEN
	FA^ := CR;                                (* Initialize normal file buffer        *)
      if TMP1 = NILF then
	update(TMP1,TEMP_FILE_NAME('QT1')||'[]',[seekok]);

      FINDLINEP(BUFFER,WHERENO,WHEREP,ERR);
      IF ERR <> QOK THEN RETURN;
      IF NOT ZERO_ADD THEN
	BUFF_TO_TMP1(BUFFER,WHERENO-FIRSTLINENO+1,ERR);
      IF ERR <> QOK THEN RETURN;
      WHILE NOT EOF(FA) DO BEGIN
	QREADINPUT(BUFFER,TLINE,WERR,ERR,FA);
	IF (WERR <> WOK) AND (WERR <> WEOF) THEN BEGIN
	  ERR := QBADSLINK;
	  RETURN
	END;
	IF NOT EOF(FA) THEN BEGIN
	  LEN_TO_WRITE := LENGTH(TLINE);
	  STORAGE_UNITS_PER_STRING := (LEN_TO_WRITE+CHARS_PER_UNIT-1)
				DIV CHARS_PER_UNIT;
	  PACKLINE := TLINE;
	  WRITE(TMP1,LEN_TO_WRITE,PACKLINE:STORAGE_UNITS_PER_STRING,LEN_TO_WRITE);
	  CNT := CNT + 1
	END  (* IF *)
      END; (* WHILE *)


      (* update counter fields *)

      IF CNT > 0 THEN BEGIN
	HBOUND := HBOUND + CNT;
	IF WHERENO < LBOUND THEN
	  LBOUND := LBOUND + CNT;
	FIRSTLINENO := FIRSTLINENO + CNT;
	lastlineno := lastlineno + cnt;
	if tmp2_last <> 0 then
	  tmp2_last:= tmp2_last + cnt;
	nbr_in_entire_file := nbr_in_entire_file + cnt;
      END; (* if cnt > 0 *)

      (* DON'T LEAVE THE BUFFER EMPTY *)

      IF EMPTY_BUFF THEN BEGIN
	FIRSTLINENO := FIRSTLINENO + 1;
	LASTLINENO := LASTLINENO + 1;
	TMP1_TO_BUFF(BUFFER,1,ERR);
	IF ERR <> QOK THEN RETURN
      END;

      changes := true;
      if err = qok then begin
	curfileok := true;
	if s940 then begin
	  wfilename(wchan,curfile,rflag);
	  s940 := true
	end
	else begin
	  curfile := filename(fa);
	  s940 := false
	end
      end;
      IF S940 THEN BEGIN
$IF P10
	WCLOSE (WCHAN, WERR);
	FREECHANNEL (WCHAN)
$END
      END
      ELSE BEGIN                               (* Normal close for others      *)
	CLOSE(FA);
	FA := NILF
      END
    END
  end  (* with *)

  EXCEPTION
    OTHERS: BEGIN
	      MASK(ATTENTION);
	      IF S940 THEN BEGIN
$IF P10
		WCLOSE (WCHAN, WERR);
		FREECHANNEL (WCHAN)
$END
	      END
	      ELSE BEGIN                           (* Standard close for most things       *)
		CLOSE (FA);
		FA := NILF
	      END;
	      UNMASK(ATTENTION);
	      SIGNAL();
	    END;
END;                                            (* qfileappend *)
$PAGE QTTYAPPEND
PUBLIC PROCEDURE QTTYAPPEND
  (       VAR BUFFER: QBUFFER;                    (* working buffer *)
	  WHERE: QLINENO;                         (* where to append text *)
	  VAR CNT: QLINENO;                       (* number of lines appended *)
	  VAR ERR: QERRCODE);                     (* error report *)

  VAR
    LINE: QSTRING;
    CH: CHAR;
    LINENUM: QLINENO;
    DONE: BOOLEAN;

BEGIN
  BREAK;
  LINENUM := WHERE;
  ERR := QOK;

  DONE := FALSE;
  WHILE (ERR = QOK) AND (NOT DONE) DO
    BEGIN
      LINE := QREAD ;
      IF (LENGTH (LINE) = 1) ANDIF (LINE [1] = '.') THEN DONE := TRUE
      ELSE
	BEGIN
	  QADDLINE (BUFFER, LINENUM, LINE, ERR);
	  LINENUM := LINENUM + 1
	END;
    END;
  IF LINENUM > WHERE THEN BUFFER.CHANGES := TRUE;
  CNT := LINENUM - WHERE
END (* qttyappend *);
$PAGE QFILEWRITE
PUBLIC PROCEDURE QFILEWRITE                     (* write text to file *)
  (       VAR BUFFER: QBUFFER;                    (* buffer to write from *)
	  S940ID: FILE_ID;                        (* file to write to     *)
	  WMOD: WMODIFIER;                        (* 940 file modifier    *)
	  FN, LN: QLINENO;                        (* range to write *)
	  CONFIRM: BOOLEAN;                       (* new/old file prompting? *)
	  VAR ERR: QERRCODE);                     (* error report *)

  VAR
    FNO, LNO: QLINENO;
    FLP, LLP: QLINEP;
    LINENO: QLINENO;
    LINE: QSTRING;
    OPTIONS_SET: QIOOPTION_SET;
    PDP10ID: FILE_ID;
    S940, RFLAG: BOOLEAN;
    WERR: WCODES;
    WCHAN: WCHANNEL;
    cnt,
    save_cursor: qlineno;
    tline,rline:  qstring;
    str_len: str_word_len;
    save_ptr: qlinep;
    storage_units_per_string: qlineno;
    len_to_write: str_word_len;
    packline: packed array [1..254] of char;
    FI: TEXT;  (* the file we are editing opened a second time *)
    FO: TEXT;  (* the output file, passed by caller *)


BEGIN
  with buffer do begin
    FNO := MAP (BUFFER, FN);
    LNO := MAP (BUFFER, LN);
    IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR)
    THEN RETURN;                                    (* Can't write outside legal range      *)
    IF CONFIRM
    THEN OPTIONS_SET := [QIO_CONFIRM]
    ELSE OPTIONS_SET := [];

      (* open the file we will be writing to. use the name provided by caller.
	 it's okay if it's the same name as the file were editing  *)

$IF P10

    (*
    *       If we have a saved file name, use it.  Otherwise, convert a possible
    *       940 file to a PDP-10 name.  In either case, open the file for
    *       output, prompting for old/new file as required.  If the user confirms
    *       the prompt, back off the open and reopen it using the special 940
    *       I/O if it's a 940 file.
    *)

    MASK(ATTENTION);
    IF WMOD = '*' THEN BEGIN                                   (* If 940 name was already converted *)
      PDP10ID := S940ID;                            (* Don't bother to reconvert    *)
      WERR := WOK                                   (* But remember it was 940      *)
    END
    ELSE WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
    IF WERR = WBADNAME THEN
      ERR := QNOFILE
    ELSE QOPENFILE (FO, PDP10ID, '', QOUTPUT_MODE, OPTIONS_SET, ERR);
    UNMASK(ATTENTION);
$END
$IFANY (VAX, M68)
    QOPENFILE (FO, S940ID, '', QOUTPUT_MODE, OPTIONS_SET, ERR);
    S940 := FALSE;
$END
    IF ERR = QOK THEN BEGIN
$IF P10
      IF WERR = WOK THEN BEGIN
	CLOSE (FO);
	FO := NILF;
	WCHAN := GETCHANNEL;
	WOPEN (WCHAN, WERR, WOUTPUT, PDP10ID);
	IF WERR <> WOK THEN BEGIN
	  FREECHANNEL (WCHAN);
	  ERR := QNOFILE;
	  RETURN;
	END;
	S940 := TRUE                                (* Success. Remember it *)
      END
      ELSE S940 := FALSE;                           (* Normal file signal   *)
$END

      (* read from TMP1 if required *)

      if fno < firstlineno then begin
	save_cursor := cursor(TMP1);
	seek(TMP1,1);
	if fno <> 1 then begin
	  for lineno := 1 to fno-1 do begin
	    read(TMP1,str_len);
	    storage_units_per_string := (str_len+chars_per_unit-1)
			  div chars_per_unit;
	    seek(TMP1,cursor(TMP1)+str_len+units_per_len_word)
	  end
	end;
	for lineno := fno to min(firstlineno-1,lno) do begin
	  read(TMP1,str_len);
	  storage_units_per_string := (str_len+chars_per_unit-1)
			div chars_per_unit;
	  read(TMP1,packline:storage_units_per_string);
	  seek(TMP1,cursor(TMP1)+units_per_len_word);
	  writeln(FO,substr(packline,1,str_len));
	end;
	seek(TMP1,save_cursor);
      end;

      (* now read (again only if required) from the buffer *)

      if (fno <= lastlineno) and (lno >= firstlineno) then begin
	cnt := firstlineno;
	save_ptr := firstlinep;
	if fno > firstlineno then begin
	  for lineno := firstlineno  to fno-1 do begin
	    save_ptr := save_ptr^.nextlinep;
	    cnt := cnt + 1
	  end
	end;
	for lineno := cnt to min(lastlineno,lno) do begin
	  writeln(FO,save_ptr^.source);
	  save_ptr := save_ptr^.nextlinep
	end
      end;

      (* read from TMP2 if required *)

      if (fno <= tmp2_last) and (lno > lastlineno) then begin
	cnt := lastlineno + 1;
	save_cursor := cursor(TMP2);
	if fno > lastlineno + 1 then begin
	  for lineno := (lastlineno+1) to fno-1 do begin
	    readrn(TMP2,cursor(TMP2)-units_per_len_word,str_len);
	    storage_units_per_string := (str_len+chars_per_unit-1) div chars_per_unit;
	    seek(TMP2,cursor(TMP2)-storage_units_per_string-units_per_len_word);
	    cnt := cnt + 1
	  end
	end;
	for lineno := cnt to min(tmp2_last,lno) do begin
	  readrn(TMP2,cursor(TMP2)-units_per_len_word,str_len);
	  storage_units_per_string := (str_len+chars_per_unit-1) div chars_per_unit;
	  seek(TMP2,cursor(TMP2)-storage_units_per_string-units_per_len_word);
	  read(TMP2,packline:storage_units_per_string);
	  seek(TMP2,cursor(TMP2)-storage_units_per_string);
	  writeln(FO,substr(packline,1,str_len));
	end;
	seek(TMP2,save_cursor);  (* get back to original starting position *)
      end;


      (*
      *       The file (or a hunk of it) has now been completely written.
      *       if it was, in fact, the whole file, save the file name and
      *       type for later defaulting.  Then close the file.
      *)

      IF (FNO=1) AND (LNO=NBR_IN_ENTIRE_FILE) AND (ERR=QOK) THEN BEGIN
	CHANGES := FALSE;                    (* Buffer is now clean  *)
	IF S940 THEN BEGIN
$IF P10
	  RFLAG := FALSE;                           (* avoiding DOC revision kludge *)
	  WFILENAME (WCHAN, CURFILE, RFLAG);
	  S940 := TRUE                       (* All hail, XDS        *)
$END
	END
	ELSE BEGIN
	  CURFILE := FILENAME(FO);
	  S940 := FALSE
	END;
	CURFILEOK := TRUE                    (* We now have a saved name     *)
      END;
      IF S940 THEN BEGIN
$IF P10
	WCLOSE (WCHAN, WERR);                       (* The usual special 940 close  *)
	FREECHANNEL (WCHAN)
$END
      END
      ELSE BEGIN
	CLOSE(FO);
	FO := NILF
      END;
    END
    ELSE IF ERR = QNOFILE THEN                           (* Don't treat confirm failure as error *)
      ERR := QOK;

    EXCEPTION
      OTHERS: BEGIN
		MASK(ATTENTION);
		IF S940
		THEN
		  BEGIN                                     (* File is 940 file. Special close      *)
$IF P10
		    WCLOSE (WCHAN, WERR);
		    FREECHANNEL (WCHAN)
$END
		  END
		ELSE BEGIN                             (* Standard close for normal file       *)
		  CLOSE (FO);
		  FO := NILF
		END;
		WRITELN (TTY, 'Warning--output file write incomplete.');
		BREAK;
		UNMASK(ATTENTION);
		SIGNAL();
	      END;
  end;  (* with *)
END.                                            (* QFILEWRITE   *)
   7@zf