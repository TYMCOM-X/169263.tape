$OPTIONS special, notrace, nocheck

module debsy$;

$INCLUDE debtyp.inc  (* needed for half_word and progblkptr *)
$INCLUDE debio.inc
$PAGE

$include dumpio.inc

const
    block_size = 128;	(* # words per disk block *)


type
    disk_block = array[1..block_size] of integer;   (* 128 words per disk block *)
    read_status = (ok,eof,err);	(* return type for read routine *)

    ch_range = integer;	(* should be 0..15  *)


$PAGE file_string

(* FILE STRING extracts a filename string for a fileblock. *)

function file_string (ifile: fileblock): file_name;

const
  null6 = 0;	(* blank in sixbit *)

var
  len, ix: integer;
  str: packed array[1..upperbound (file_string)] of char;

  procedure add_char (ch: char);

  begin
    len := len + 1;
    str[len] := ch;
  end;

  procedure add_char6 (ch6: sixbit);

  begin
    len := len + 1;
    str[len] := chr (ch6 + 40b);
  end;

  procedure add_num (hw: half_word);

  begin
    if hw <> 0 then begin
      add_num (hw div 10b);
      len := len + 1;
      str[len] := chr ((hw mod 10b) + ord ('0'));
    end;
  end;

begin (* file_string *)
  len := 0;
  with ifile do begin
    for ix := 1 to 6 do begin
    exit if device[ix] = null6;
      add_char6 (device[ix]);
    end;
    if len > 0 then
      add_char (':');
    for ix := 1 to 6 do begin
    exit if name[ix] = null6;
      add_char6 (name[ix]);
    end;
    add_char ('.');
    for ix := 1 to 3 do begin
    exit if ext[ix] = null6;
      add_char6 (ext[ix]);
    end;
    add_char ('[');
    add_num (proj);
    add_char (',');
    add_num (prog);
    add_char (']');
    add_char (' ');	(* can't hurt *)
  end;
  file_string := substr (str, 1, len);
end;
$PAGE routine to close a file and release the dev associated with the io channel

 procedure close_release(chan:ch_range);

begin
  clos$file (chan, false);	(* dumpio does the work *)
end;
$PAGE routine to open io channel and lookup a file for input

 function openlookup(ifile: fileblock; var chan:ch_range): boolean;

begin
  opn$file (file_string (ifile), chan, openlookup, true, false);
end;
$PAGE   routine to read a random disk block


 function readblk(block:half_word; var buffer:disk_block; chan:ch_range)
  : read_status;

var
    in_ok: boolean;

begin
  read$page (ord (address (buffer)), 1, block, chan, in_ok);
  if in_ok
    then readblk := ok
    else readblk := err;
end;
$PAGE    type and var definitions for deref and st open and close  routines

const
    maxblocks = 32; (* max # symbol table disk blocks in core at a time *)
    maxfilesopen= 6;	(* max # rel files opened at a time *)
    deb_nset = 2;
    open_look = 3;

type
    st_files_index = 1..maxfilesopen;
    file_num = 0..maxfilesopen;	(* 0 => not a valid file number *)


    st_index = 1..maxblocks;	(* index range for st and map *)
    maptype = packed record
      lru : integer;
      case boolean of
	true : (
	  filenum : file_num;	(* uniquely identify a block *)
	  blocknum : half_word);    (* with block# and file# *)
	false: (
	  file_block: integer)
    end (* maptype *);
    st_type = array[st_index] of disk_block;	(* symboltable st *)
    st_ptr = ^st_type;

var

(* the following  variables are initialized by the routine st$area. *)
(* nxt_blk_free is initially 0, and access fields of map are set to zero. *)
(* the other fields of map do not need to be initialized.  st_area is set up *)
(* to point to the core symboltable area, and st_len is the number of *)
(* disk blocks size entries allocated for st_area *)

    st_area : st_ptr;
    st_len : st_index;
    cur_file : file_num;    (* # of currently opened symboltable file *)
    nxt_blk_free : 0..maxblocks;    (* next virgin block in st_area *)
    map : array[st_index] of maptype;	(* maps disk blocks to core *)
    blk_lru_ctr:integer;    (* updated whenever a blk is accessed *)

    st_files: array[st_files_index] of record
      prog_blk_ptr : progblkptr;    (* ptr to program_block *)
      lru : integer;	(* keeps track of access for this entry *)
      iochan : ch_range;
      opened : boolean	(* file is opened *)
    end;	    (* st_files *)

    file_lru_ctr : integer; (* updated whenever a file is accessed via st$open *)
    nxt_file_free : file_num;	(* next virgin st_files entry *)



 procedure error(errnum:integer);

const
    dsk_rd_err = 1;
    (*   deb_nset = 2; *)
    (*   open_look = 3; *)

begin
  case errnum of
    dsk_rd_err:
      writ$nl('Read error on symbol table file');
    deb_nset :
      writ$nl('Attempt to open module compiled without debug');
    open_look :
      writ$nl('Unable to open symbol table file')
  end;		    (* case *)
  writ$nl('Fatal debugger error');
  stop
end;		    (* error *)
$PAGE   deref$ptr

type
    ptr = integer;  (* really returns a pointer, real kluge *)

public function deref$ptr(i:integer): ptr;

(* this routine converts a symbol table index, i, in a rel file which *)
(* is a word index relative to the base of the symbol table in the *)
(* rel file, to a pointer to either an identifier record(ctp) *)
(* a structure record(stp) or a constant record(csp). it takes *)
(* advantage of the 6 character unique names of the link-10 symbol *)
(* table and is referenced in the debugger via deref$ctp, deref$stp, *)
(* or deref$csp.   *)



const
    maxword = 127;  (* word index relative 0, block-size - 1 *)

    dsk_rd_err = 1; (* error number *)


var
    index : st_index;	(* used to index map and st_area *)
    word : 0..maxword;	(* for word offset into disk blk *)
    block : half_word;	(* relative block from st base *)



$PAGE


  function getblock: st_index;

  (* returns the index into st_area as the buffer for *)
  (* the next disk read. the index is determined by looking at the *)
  (* lru field of the map to determine the least recently *)
  (* accessed block. if there is a block in st_area that has *)
  (* not been used at all (ie nxt_blk_free<=maxblocks), then *)
  (* getblock gets that one *)


  var
      temp : st_index;

  begin
    if nxt_blk_free >= st_len	(* no more unused blocks, so  *)
    then begin	    (* find least recently used *)
      getblock := 1;
      for temp := 2 to st_len do
	if map[getblock].lru > map[temp].lru then
	  getblock := temp;
    end		    (* then *)

    else begin
      nxt_blk_free := nxt_blk_free + 1;
      getblock := nxt_blk_free;
    end (* else *);
  end (* getblock *);

$PAGE


  function searchmap(blk:half_word; var ix:integer): boolean;

  (* returns a true value if the disk block(blk) in the currently *)
  (* opened file is in core. ix will = the index into st_area for *)
  (* the desired block. if the block is not in core, false is returned *)


  var
      maptemp : maptype;    (* used to build file#_block word *)


  begin
    if cur_file <> 0 then begin
      ix := 1;
      searchmap := true;
      with maptemp do begin
	filenum := cur_file;
	blocknum := blk;
	while ix <= st_len do
	  if file_block = map[ix].file_block then
	    return
	  else
	    ix := ix + 1;
      end (* with *);
    end;	    (* if cur_file *)
    searchmap := false;
  end (* searchmap *);

$PAGE

begin		    (* deref *)
  if i = ord(nil) then
    deref$ptr := i
  else begin
    word := i mod block_size;
    block := i div block_size + 1;
    if not searchmap(block,index)   (* returns index if true *)
    then begin	    (* else index unknown *)
      index := getblock;
      with st_files[cur_file] do
	if readblk(block-1,st_area^[index],iochan) = err then begin
	  error(dsk_rd_err);
	  return;
	end
	else
	  with map[index] do begin
	    filenum := cur_file;
	    blocknum := block;
	  end (* else with if *);
    end (* then and if not searchmap *);

    (* calculate value of pointer *)
    (* st_area base + 1st subscript*size of sec subsc + relative word *)

    deref$ptr := (ord(st_area)+(block_size*(index-1))+word);
    blk_lru_ctr := blk_lru_ctr + 1;
    with map[index] do
      lru := blk_lru_ctr;
  end;		    (* else of if i = ord(nil) *)
end;		    (* deref *)
$PAGE   routine to initialize st_area

public procedure st$area(base:st_ptr; len:st_index);

(* this procedure is called be the runtime system whenever the  *)
(* debugger is entered. therefore it is the routine that initilizes *)
(* anything that needs it . particularly st_area and st_len. *)
(* the lru counters are restarted here , and the lru fields are cleared *)

var
    i:integer;	    (* temp used as array index *)


begin		    (* st$area *)
  st_area := base;
  st_len := len;
  blk_lru_ctr := 0;
  file_lru_ctr := 0;
  cur_file := 0;
  nxt_blk_free := 0;
  nxt_file_free := 0;
  for i := 1 to len do
    with map[i] do begin
      lru := 0;
      filenum := 0;
    end;	    (* with map *)
  for i := 1 to maxfilesopen do
    with st_files[i] do begin
      lru := 0;
      prog_blk_ptr := nil;
    end;	    (* with st_files *)
end;		    (* st$area *)

$PAGE   routine to open a symbol table rel file

public procedure st$open(ptr_to_pgb:progblkptr);


  function entry_there(ptr_to_pgb:progblkptr; var i:integer):boolean;

  (* returns a true value if there is an entry in st_files for this file *)
  (* with i equal to the index into st_files for that entry *)
  (* if no entry exists, the func returns a false value with i undefined *)


  begin		    (* entry_there *)
    entry_there := true;
    i := 1;
    while i <= maxfilesopen do
      if st_files[i].prog_blk_ptr = ptr_to_pgb then
	return
      else
	i := i + 1;
    entry_there := false;
  end;		    (* entry_there *)



  function getentry:st_files_index;

  (* since number of files being accessed/opened simultaneously is *)
  (* limited, this routine searches through st_files to determine which *)
  (* files have been accessed least recently, closes that file, invalidates *)
  (* entries in map for that file and returns the index of that entry *)



    procedure st_close(filenum:st_files_index);

    begin
      with st_files[filenum] do begin
	close_release(iochan);
	opened := false;
      end;	    (* with *)
    end;	    (* st_close *)


  var
      temp : st_files_index;
      i : st_index;

  begin		    (* getentry *)
    if nxt_file_free >= maxfilesopen	(* no more unused entrys, so  *)
    then begin	    (* find least recently used *)
      getentry := 1;
      for temp := 2 to maxfilesopen do
	if st_files[getentry].lru > st_files[temp].lru then
	  getentry := temp;
      st_close(getentry);
      for i := 1 to st_len do
	with map[i] do
	  if filenum = getentry then
	    filenum := 0;
    end		    (* then *)

    else begin
      nxt_file_free := nxt_file_free + 1;
      getentry := nxt_file_free;
    end (* else *);
  end (* getentry *);

$PAGE    st$open

begin		    (* st$open *)
  if ord(ptr_to_pgb) = 0    (* not compiled with debug *)
  then begin
    error(deb_nset);
    return;
  end
  else begin
    if not entry_there(ptr_to_pgb,cur_file) then begin
      cur_file := getentry;
      with st_files [cur_file] do begin
	prog_blk_ptr := ptr_to_pgb;
	opened := false;
      end;	    (* with *)
    end;	    (* if *)
    with st_files[cur_file],prog_blk_ptr^ do begin
      if not opened andif not openlookup(symfile_name,iochan) then begin
	error(open_look);   (* openlookup failed *)
	cur_file := 0;
	return;
      end;	    (* with *)
      opened := true;
    end;	    (* with *)
  end;		    (* if ord(ptr_to_pgb *)
end.		    (* st$open *)

   