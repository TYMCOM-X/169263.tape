$LENGTH 44

(* Virtual memory allocation package.  Contains entries:
     VNEW -- allocate a block, returning virtual pointer.
     VDISPOSE -- deallocate a block.
     VFETCH -- retrieve and return contents of a previously allocated block.
     VCHANGE -- modify contents of previously allocated block.

   This package $INCLUDEs, and hence shares, state information kept by the
   virtual address translation routines VHOLD and VFREE to optimize allocation.
   Allocation is first attempted in blocks currently maintained in core
   by the address translation routines. *)

$INCLUDE vbuf.typ

external var vctl: vbufctl;

(* Type definitions for this package.
   Free blocks are kept on a linked list within each page.  The list head
   pointer is kept in a block at index 0 within the page of (alleged) length
   zero, which prevents it from ever being allocated or concatenated with
   any other free blocks.  Allocation algorithm is a first fit on pages
   currently held in core by the address translation routines.
   Dirty pages are examined first, and then clean ones.  If no block of
   sufficient length is found in any page in core, then a page at an
   address which has never been referenced (vhpage in control information)
   is held, intialized as a single block, and allocated from. *)

type
  block_ptr= ^block_header;
  block_header= packed record
		  next_block: block_ptr;
		  block_length: halfword
		end;

external procedure vhold( virtualpointer; var block_ptr; boolean);
external procedure vfree( block_ptr );
external procedure zaperr(verror);
external procedure mask;		(*prevent breaks*)
external procedure unmask;		(*allow breaks*)
$PAGE GETPAGE  grabs an unused virtual page and initializes free chain

function getpage: vpageptr;

  (* Holds page whose index is given by vhpage in master control block,
     and places entire data area in one free block minus header block,
     which is block of length zero at index 0 in page. *)

  var
    new_page: virtualpointer;
    first_block: block_ptr;

begin					(*getpage*)
  with vctl^ do begin
    with new_page do begin		(*pony up virtual ptr to next available page*)
      vpage:= vhpage;
      vindex:= 0
    end;
    vhpage:= vhpage+1			(*increment for next time*)
  end;
  vhold(new_page, first_block, true);	(*force new page into core*)

  mask;					(*BEGIN critical*)
    with first_block^ do begin
      next_block:= ptr(1);	(*init zero length block*)
      block_length:= 0
    end;
    first_block := ptr(ord(first_block)+1);(* hard pointer to non-zero block *)
    first_block^.next_block:= nil;	(*init block containing rest of page*)
    first_block^.block_length:= maxindex;  (*maxindex 1 less than page length*)
  unmask;				(*END critical*)
  vfree(first_block);

  (* Return pointer to page control information which will be on
     top of mru chain. *)

  getpage:= vctl^.vmruhead
end (*getpage*);
$PAGE FIRSTFIT  attempts to allocate a block of passed size in passed page

function firstfit( page_info: vpageptr;	(*control info on page to search*)
		   var vp: virtualpointer;  (*pointer to allocated space*)
		   desired_length: halfword): boolean;	(*true if allocated*)

  (* Searches free list in page for first block larger than desired length.
     Length must be larger to accomodate length word which is left before
     allocated data block so vdispose knows how much storage to return.
     Storage is torn off bottom of block on chain so link information
     can be preserved. *)

  var
    cur_block, first_block, new_block, prev_block: block_ptr;
    remaining_length: halfword;

begin					(*firstfit*)
  first_block:= ptr( ord(page_info^.vspace) );	(*always at beginning of data*)
  cur_block:= first_block;

  (* Search the free list.  No need to initialize prev_block, as zero first
     block always forces loop to go at least once. *)

  firstfit:= false;
  while cur_block <> nil do begin
    exit if cur_block^.block_length > desired_length	(*must have room for length*)
      do firstfit:= true;
    prev_block:= cur_block;
    if cur_block^.next_block = nil then
      cur_block := nil
    else cur_block := ptr(ord(cur_block^.next_block)+ord(first_block))

    (* keep cur_block as a HARD pointer within the block, and then
       prev_block and new_block will also be hard pointers *)

  end;

  if firstfit then begin
    mask;				(*BEGIN critical*)
      with cur_block^ do
	if block_length = (desired_length+1) then begin	(*unchain, and allocate all*)
	  prev_block^.next_block:= next_block; (* next_block is relative ptr *)
	  new_block:= cur_block
	end
	else begin			(*split new block off end of current*)
	  remaining_length:= block_length - desired_length - 1;
	  block_length:= remaining_length;
	  new_block:= ptr( ord(cur_block) + remaining_length);
	  new_block^.block_length:= desired_length + 1
	end;
      page_info^.vdirty:= true;
    unmask;				(*END critical*)

    (* new_block now points to word containing length of block to allocate *)

    with vp do begin			(*set up virtual pointer to return*)
      vpage:= page_info^.vblock;
      vindex:= ord(new_block) - ord(first_block) + 1	(*point past length word*)
    end
  end
end;					(*firstfit*)
$PAGE VNEW get a record of desired length
public procedure VNEW ( var VP: VIRTUALPOINTER;	(* set to new record *)
			LEN: WORD);	(* desired length *)

var
  WALK: VPAGEPTR;			(* for checking pages in core *)

  begin					(* first a few validity checks--init and valid length *)
  if (VCTL = nil) orif (not VCTL^.VFILEOPEN) then ZAPERR(VNOTINIT);
  if (LEN <= 0) then ZAPERR(VZERONEW);
  if (LEN > MAXINDEX) then ZAPERR(VTOOLONGNEW);
  VP := VNIL;				(* to start off *)

  (* first check all dirty pages in core for sufficient free space,
  then clean pages, and then just grab a new one *)

  with VCTL^ do
    begin
    WALK := VMRUHEAD;			(* start off *)
    while (WALK <> nil)			(* there's pages to check *)
    andif ((not WALK^.VDIRTY)		(* continue if either page is clean *)
    orif (not FIRSTFIT(WALK,VP,LEN))) do    (* orif can't allocate *)
      WALK := WALK^.VNEXT;

    if WALK = nil then			(* got to end without allocating on dirty *)
      begin				(* so try for a clean *)
      WALK := VMRUHEAD;
      while (WALK <> nil)		(* almost same as above *)
      andif ((WALK^.VDIRTY)		(* except continue on dirty *)
      orif (not FIRSTFIT(WALK,VP,LEN))) do  (* or not allocated *)
	WALK := WALK^.VNEXT;

      if WALK = nil then		(* nothing in core, get new one *)
	if not FIRSTFIT(GETPAGE,VP,LEN) then ZAPERR(VINTERR);
      end
    end					(* with *)
  end;					(* PROCEDURE VNEW *)
$PAGE COALESCE free space merging routine
function COALESCE (START, ORIGIN: BLOCK_PTR)
	: boolean;

(* COALESCE tries to combine blocks of free space starting at
   START. It returns FALSE if the entire block is now free space.
   The best calling sequence is to determine the best starting
   place (free block previous to new free one, if any, else new
   one) and use function value to pony up VDIRTY (a totally free
   block should be CLEAN for efficiency. ORIGIN is the hard address
   of the start of the page, START is a hard pointer also.
*)

var
  OFFSET: WORD;				(* offset of START from ORIGIN *)

  begin
  OFFSET := ord(START)-ord(ORIGIN);
  with START^ do
    begin
    while (NEXT_BLOCK <> nil)		(* something after start *)
    andif (BLOCK_LENGTH >= ord(NEXT_BLOCK) - OFFSET) do

    (* andif that next one is right after START *)

      begin				(* let's join them *)
      if BLOCK_LENGTH > ord(NEXT_BLOCK) - OFFSET then
	ZAPERR(VDISPTWICE);
      MASK;				(* we're diddling with the block *)

	(* first get a hard pointer to NEXT_BLOCK so its info
	can be transferred. Then update NEXT_BLOCK to be a
	relative pointer again. *)

	NEXT_BLOCK := ptr(ord(NEXT_BLOCK) + ord(ORIGIN));
	BLOCK_LENGTH := BLOCK_LENGTH + NEXT_BLOCK^.BLOCK_LENGTH;
	NEXT_BLOCK := NEXT_BLOCK^.NEXT_BLOCK;	(* a relative pointer *)
      UNMASK
      end;

    COALESCE := (BLOCK_LENGTH < MAXINDEX)

    (* false iff entire block has been freed *)

    end
  end;					(* FUNCTION COALESCE *)
$PAGE VDISPOSE routine to free up an allocated record
public procedure VDISPOSE (VP: VIRTUALPOINTER);

var
  FPREV,FNEW,FNEXT: BLOCK_PTR;		(* previous, new, and next free blocks *)
  FORIGIN: BLOCK_PTR;			(* hard pointer to start of block *)
  FPAGE: VPAGEPTR;			(* for control info (heh-heh) *)

  begin
  VHOLD (VP, FNEW, true);		(* get dirty, pony later *)
  FNEW := ptr(ord(FNEW) - 1);		(* to get block header word *)
  FPAGE := VCTL^.VMRUHEAD;

  (* sneaky--since we see VCTL, and we just did a VHOLD, we know that
  the control info for VP's block is at VMRUHEAD. *)

  with FPAGE^ do
    begin
    FNEXT := ptr(ord(VSPACE));		(* points to zero-length first block *)
    FORIGIN := FNEXT;			(* hang onto start of block!! *)

    while (FNEXT <> nil)		(* if another free block *)
    andif ( ord(FNEXT) < ord(FNEW) ) do  (* andif still before new one *)
      begin
      FPREV := FNEXT;
      if FNEXT^.NEXT_BLOCK = nil then
	FNEXT := nil
      else FNEXT := ptr(ord(FNEXT^.NEXT_BLOCK) + ord(FORIGIN))
      end;

    (* if FNEXT is not nil, it points to the first free block 'after'
    the FNEW one.  If nil, then FNEW goes after FPREV as the new last
    one, or the only new free one if FPREV = zero-length block. 
    Notice that FPREV^.NEXT_BLOCK can always be assigned. *)

    MASK;
      if FNEXT = nil then
	FNEW^.NEXT_BLOCK := nil
      else FNEW^.NEXT_BLOCK := ptr(ord(FNEXT) - ord(FORIGIN));
      FPREV^.NEXT_BLOCK := ptr(ord(FNEW)-ord(FORIGIN));
    UNMASK;				(* FNEW is now chained on *)

    if ord(FPREV) + FPREV^.BLOCK_LENGTH >= ord(FNEW) then
      FNEW := FPREV;

    (* We can coalesce at FPREV if lengths match up.  Otherwise start
    at FNEW. This is a redundant check, but now we need only call
    COALESCE once, and free blocks both before and after FNEW will
    be merged. *)

    VDIRTY := COALESCE(FNEW,FORIGIN);	(* no need to mask, not critical *)
    VFREE(FNEW);			(* get rid of the page *)

    (* It's safe to free FNEW, because even though we have changed its
    value, it still points to the same page that VP did. *)

    end
  end;					(* PROCEDURE VDISPOSE *)
$PAGE VFETCH returns contents of virtual record
public procedure VFETCH (VP: VIRTUALPOINTER;	(* loc'n of record *)
			 DATA: SPACEPTR);   (* returns data *)

var
  VREC: SPACEPTR;			(* points to record for data transfer *)
  IDX: HALFWORD;			(* transfer loop variable *)
  VBLK: BLOCK_PTR;			(* for HOLD/FREE and length of xfer *)

  begin
  VHOLD(VP, VBLK, false);		(* make it clean *)
  VREC := ptr(ord(VBLK));		(* coerce for array referencing *)
  VBLK := ptr(ord(VBLK)-1);		(* get block header word *)

  (* get length of data transfer as the length of the block, contained
  in the BLOCK_HEADER one word before the actual block *)

  for IDX := 0 to VBLK^.BLOCK_LENGTH - 2 do
    DATA^[IDX] := VREC^[IDX];

  VFREE(VBLK) 

  (* it's safe since header is on same page as record *)
  end;					(* PROCEDURE VFETCH *)
$PAGE VCHANGE sets contents of virtual record
public procedure VCHANGE (VP: VIRTUALPOINTER;	(* what to change *)
			  DATA: SPACEPTR);  (* data source *)

var
  VREC: SPACEPTR;			(* sink pointer for transfer *)
  IDX: HALFWORD;			(* transfer loop variable *)
  VBLK: BLOCK_PTR;			(* for HOLD/FREE and length of xfer *)

  begin
  VHOLD (VP,VBLK,true);			(* this one is dirty *)
  VREC := ptr(ord(VBLK));		(* coerce for array referencing *)
  VBLK := ptr(ord(VBLK) - 1);		(* get block header word *)

  for IDX := 0 to VBLK^.BLOCK_LENGTH - 2 do
    VREC^[IDX] := DATA^[IDX];

  VFREE(VBLK)
  end.					(* PROCEDURE VCHANGE *)
  