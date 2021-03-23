(*   MODULE VBUF   *)

$LENGTH 44

$INCLUDE VBUF.TYP

external procedure MASK;
external procedure UNMASK;
external procedure OPENFILE (FILENAME; var WORD; var boolean; boolean; boolean);
external procedure CLOSEFILE (WORD; boolean);
external procedure READPAGE (WORD; WORD; WORD; HALFWORD; var boolean);
external procedure WRITEPAGE(WORD; WORD; WORD; HALFWORD; var boolean);
external procedure ZAPERR(VERROR);

public var
  VCTL: VBUFCTL := nil;			(* the only one!! *)

const
  DEFAULTF: FILENAME := 'VHP.TMP';	(* default paging file name *)
$PAGE CHAINDISPOSE for disposing the MRU chain of buffers
procedure CHAINDISPOSE (var FOO: VPAGEPTR);

(* a convenient way to dispose a list of VIRTUALPAGEs *)

var
  TEMPTR: VPAGEPTR;

  begin
  while FOO <> nil do
    begin
    TEMPTR := FOO^.VNEXT;
    dispose(FOO);
    FOO := TEMPTR
    end
  end;	(* PROCEDURE CHAINDISPOSE *)
$PAGE GETFILE to find an unused temp paging file on user's account
function GETFILE: FILENAME;

(* GETFILE returns a string of the format XXXVHP.TMP for use as a default
   paging file, where XXX is 000,001,002..., and the entire string is
   the first such file not already in use.  This permits a single user
   running multiple jobs to use the package in each job.
*)

var
  TEMPNAME: string[30];
  TEMPCHANNEL: WORD;
  FIDX: WORD;
  IS_OPEN: boolean;

  begin
  FIDX := 0;
  TEMPNAME := '000' || DEFAULTF;

    loop
      OPENFILE (TEMPNAME, TEMPCHANNEL, IS_OPEN, true, false);

      (* try to open the filename for input only.  If it does
      not fail, then the file exists, and hence is in use, so
      try the next name in sequence.  Otherwise we got it. *)

    exit if not IS_OPEN;		(* not open--we found it *)
      CLOSEFILE(TEMPCHANNEL, false);	(* don't want to delete it! *)
      FIDX := FIDX + 1;			(* get next XXX *)

    exit if FIDX >= 1000 do 
      ZAPERR(VBADFILE);			(* blow up after XXX=999 *)

      (* now construct the next filename as a string *)

      TEMPNAME := chr(ord('0') + (FIDX div 100))	(* 100's digit *)
		||chr(ord('0') + ((FIDX div 10) mod 10))(* 10's digit *)
		||chr(ord('0') + (FIDX mod 10))		(* 1's digit *)
		||DEFAULTF			(* and rest of name *)
    end (* loop *);

  GETFILE := TEMPNAME
  end (* FUNCTION GETFILE *);
$PAGE VBUFCLS to close down the address space
public procedure VBUFCLS(KEEP: boolean);

(* VBUFCLS shuts down the address space whose control record is
   in VCTL. All associated storage is released. *)

var
  BUFTMP: VBUFCTL;		(* used for the storage release *)
  PAGETMP: VPAGEPTR;		(* used to flush dirty pages *)
  IOOK: boolean;		(* return code from I/O op'ns *)

  begin
  if VCTL = nil then return;		(* nothing to do *)
  if (VCTL^.VPAGEFILE <> '') and KEEP then	(* must flush before close *)
    begin
    PAGETMP := VCTL^.VMRUHEAD;

  (* notice that if the paging file was never opened, then VMRUHEAD
  will be nil (i.e. never did anything), so loop does nothing *)

    while (PAGETMP <> nil) andif PAGETMP^.VDIRTY do
      with PAGETMP^ do
      begin			(* got a dirty page *)
      WRITEPAGE (ord(VSPACE), WORDCOUNT div WORDSPERDBLOCK, 
	(VBLOCK*WORDCOUNT div WORDSPERDBLOCK) + NUM_HEAD_BLOCKS,
	VCTL^.VCHANNEL, IOOK);
      if not IOOK then ZAPERR(VINTERR);
      VDIRTY := false;		(* mark as clean in case of ^C *)
      PAGETMP := VNEXT		(* and get next *)
      end;

    (* now write out current value of VHPAGE into word zero, block zero *)

    with VCTL^ do
    if VFILEOPEN then
      begin				(* write only if file was opened *)
      VGARBBUF^[0] := VHPAGE;
      WRITEPAGE (ord(VGARBBUF), WORDCOUNT div WORDSPERDBLOCK,
	0, VCHANNEL, IOOK);
      if not IOOK then ZAPERR(VINTERR)
      end 				(* flushing of KEEP file *)
    end;

  MASK;
    with VCTL^ do
      begin
      if VFILEOPEN then		(* close old file if open *)
	CLOSEFILE (VCHANNEL,		(* delete if temp file or not KEEP *)
	  (VPAGEFILE = '') or not KEEP);
      VFILEOPEN := false;
      BUFTMP := VCTL		(* assign temp for release *)
      end;
    VCTL := nil;		(* and zap the old info *)
  UNMASK;			(* now it looks like no address space *)

  with BUFTMP^ do		(* release storage -- notice that a  *)
    begin			(* break would only lose heap storage *)
    if VMRUHEAD <> nil then
      CHAINDISPOSE(VMRUHEAD);
    dispose(VGARBBUF);
    dispose(BUFTMP)
    end
  end;	(* PROCEDURE VBUFCLS *)
$PAGE VBUFINIT to set up address space
public procedure VBUFINIT (
	NUMBUF: WORD;
	FILENM: FILENAME;
	var ERR: boolean);

var
  BUFTMP: VBUFCTL;		(* set up, then assign to VCTL *)
  TEMPOPEN: boolean;		(* remember if successful opening *)

  begin
  ERR := true;
  if NUMBUF <= 0 then ERR := false
  else begin
    new(BUFTMP);		(* get a new control record *)
    with BUFTMP^ do		(* and initialize it *)
      begin
      VBUFNUM := NUMBUF;
      VBUFUSED := 0;
      VMRUHEAD := nil;
      new(VGARBBUF);
      VLOCKCT := 0;
      VHPAGE := 0;
      if FILENM = '' then
	VPAGEFILE := GETFILE
      else VPAGEFILE := FILENM;
      VFILEOPEN := false;
      VCHANNEL := 0
      end;

    if VCTL <> nil then
      VBUFCLS(false);			(* now no address space exists *)

    MASK;
      with BUFTMP^ do
	OPENFILE(VPAGEFILE, VCHANNEL, TEMPOPEN, false, false);
	(* open file, don't delete contents, not input only *)
      VCTL := BUFTMP;
    UNMASK;			(* now new record in place, open or not *)

    if not TEMPOPEN then
      ERR := false
    else with VCTL^ do
      begin
      READPAGE(ord(VGARBBUF), WORDCOUNT div WORDSPERDBLOCK, 0,
	VCHANNEL, TEMPOPEN);
      if not TEMPOPEN then
	ERR := false
      else begin
	VHPAGE := VGARBBUF^[0];	(* first word as start, 0 if new file *)
	VPAGEFILE := FILENM;	(* filename else '' if default *)
	VFILEOPEN := true	(* and let the record now be valid *)
	end
      end
    end
  end (* PROCEDURE VBUFINIT *);
$PAGE TOPMRU to place a buffer on top of the MRU chain
procedure TOPMRU (HERE, PREV: VPAGEPTR);

(* Places HERE on the top of the MRU chain.  HERE must already be
   on the chain at some point.  PREV is the previous VIRTUALPAGE on
   the chain.  TOPMRU uses MASK, and can be considered as an
   uninterruptible operation. *)

  begin
  with VCTL^ do
    begin
    if HERE <> VMRUHEAD then
      begin				(* notice that if HERE is not the head, *)
      MASK;				(* then it must have a predecessor PREV *)
	PREV^.VNEXT := HERE^.VNEXT;
	HERE^.VNEXT := VMRUHEAD;
	VMRUHEAD := HERE;
      UNMASK				(* just switch links, HERE now on top *)
      end
    end
  end;					(* PROCEDURE TOPMRU *)
$PAGE PAGEONTOP to place a page, if in core, on top of MRU chain
function PAGEONTOP (PAGE: VPAGEID; var LAST,NEXTTOLAST: VPAGEPTR): boolean;

(* Returns true if PAGE is in a core buffer, placing that buffer on
   top of the MRU list.  Returns false if PAGE is not found, with
   LAST pointing to the LRU page, and NEXTTOLAST to its predecessor. *)

  begin
  PAGEONTOP := false;
  with VCTL^ do
    begin
    LAST := VMRUHEAD;
    NEXTTOLAST := nil;
    if LAST <> nil then
      loop
	PAGEONTOP := (LAST^.VBLOCK = PAGE);
      exit if (PAGEONTOP) or (LAST^.VNEXT = nil);
	NEXTTOLAST := LAST;
	LAST := LAST^.VNEXT
      end;

    if (PAGEONTOP)			(* meaning we got something *)
    andif (NEXTTOLAST <> nil) then	(* and it's not on top *)
      TOPMRU(LAST,NEXTTOLAST)		(* then put it on top *)
    end
  end;					(* FUNCTION PAGEONTOP *)
$PAGE BRINGINPAGE to bring in a new virtual page
procedure BRINGINPAGE (PAGE: VPAGEID; LAST,NEXTTOLAST: VPAGEPTR);

(* Places PAGE in a core buffer.  If not all buffers have been 
   obtained, it gets a NEW one.  If all buffers are allocated, it
   writes out the LRU one. *)

var
  TEMPPAGE: VPAGEPTR;
  TEMPLINK: SPACEPTR;
  IOOK: boolean;

  begin
  with VCTL^ do
    begin
    if VBUFUSED = VBUFNUM then
      begin				(* page out tail *)
      if LAST^.VREFCT > 0 then ZAPERR(VLASTLOCKED);
      if LAST^.VDIRTY then
	begin
	WRITEPAGE ( ord(LAST^.VSPACE), WORDCOUNT div WORDSPERDBLOCK,
	    (LAST^.VBLOCK*WORDCOUNT div WORDSPERDBLOCK) + NUM_HEAD_BLOCKS,
    VCHANNEL, IOOK);
	if not IOOK then ZAPERR(VIOERR)
	end;
      LAST^.VDIRTY := false;		(* may as well mark him clean *)
      TOPMRU (LAST, NEXTTOLAST)
      end;

    (* we can go ahead and read the new page into the garbage buffer *)

    READPAGE( ord(VGARBBUF), WORDCOUNT div WORDSPERDBLOCK,
	(PAGE*WORDCOUNT div WORDSPERDBLOCK) + NUM_HEAD_BLOCKS, VCHANNEL, IOOK);
    if not IOOK then ZAPERR(VIOERR);

    MASK;				(* now things get critical *)

      if VBUFUSED < VBUFNUM then
	begin				(* make a new virtualpage *)
	new(TEMPPAGE);
	VBUFUSED := VBUFUSED + 1;	(* bump up global count *)
	with TEMPPAGE^ do
	  begin
	  VNEXT := VMRUHEAD;
	  VSPACE := VGARBBUF
	  end;
	VMRUHEAD := TEMPPAGE;
	new(VGARBBUF)
	end

      else with VMRUHEAD^ do
	begin				(* simply link the garbbuf to VMRUHEAD *)
	TEMPLINK := VSPACE;
	VSPACE := VGARBBUF;
	VGARBBUF := TEMPLINK
	end;

      (* common code to change identity of VIRTUALPAGE *)

      with VMRUHEAD^ do
	begin
	VREFCT := 0;
	VBLOCK := PAGE;
	VDIRTY := false
	end;

    UNMASK
    end
  end;					(* PROCEDURE BRINGINPAGE *)
$PAGE VHOLD
public procedure VHOLD (VP: VIRTUALPOINTER;
			var  RP: WORD;
			DIRTY: boolean);

var
  LAST, NEXTTOLAST: VPAGEPTR;

  begin
  if (VCTL = nil) orif (not VCTL^.VFILEOPEN) then ZAPERR(VNOTINIT);
  with VCTL^ do
    begin
    if VLOCKCT >= VBUFNUM then ZAPERR(VNOBUF);

    if not PAGEONTOP (VP.VPAGE,LAST,NEXTTOLAST) then
      BRINGINPAGE (VP.VPAGE,LAST,NEXTTOLAST);

    (* desired PAGE is now on top of the MRU *)

    MASK;
      with VMRUHEAD^ do
	begin				(* update control info *)
	VREFCT := VREFCT + 1;
	if DIRTY then VDIRTY := true;
	RP := ord(VSPACE) + VP.VINDEX
	end;
      VLOCKCT := VLOCKCT + 1;
    UNMASK;
    end
  end;					(* PROCEDURE VHOLD *) 
$PAGE VFREE
public procedure VFREE (RP: WORD);

var WALK: VPAGEPTR;

  begin
  if (VCTL = nil) orif (not VCTL^.VFILEOPEN) then ZAPERR(VNOTINIT);
  with VCTL^ do
    begin
    WALK := VMRUHEAD;
    while (WALK <> nil) andif		(* while not right page, try next *)
      ((ord(WALK^.VSPACE) > RP) or ((RP - ord(WALK^.VSPACE)) >= WORDCOUNT)) do
	WALK := WALK^.VNEXT;	(* get next page control rec *)

    if WALK = nil then ZAPERR(VBADRP);	(* never found correct page record *)

    MASK;				(* else decrement ref counts *)
      WALK^.VREFCT := WALK^.VREFCT - 1;
      VLOCKCT := VLOCKCT - 1;
    UNMASK
    end
  end;					(* PROCEDURE VFREE *)
$PAGE VFREEALL
public procedure VFREEALL;

var WALK: VPAGEPTR;

  begin
  MASK;
    WALK := VCTL^.VMRUHEAD;		(* start at top of chain *)
    while WALK <> nil do
      begin
      WALK^.VREFCT := 0;		(* and zap the page's ref count *)
      WALK := WALK^.VNEXT
      end;
    VCTL^.VLOCKCT := 0;			(* reset global lock count *)
  UNMASK
  end.					(* PROCEDURE FREEALL *)
  