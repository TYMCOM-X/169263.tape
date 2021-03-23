$TITLE pasifu - Utility routines for manipulating the Intermediate Form
$LENGTH 43

module pasifu;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S I F U                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  PASCAL Compiler
     
     STARTED:  8/22/78
     
     PURPOSE:  This module contains utilities  for  manipulating  the
        intermediate form.  All the procedures in this module operate
        on a "current tuple chain".  This chain  corresponds  to  the
        procedure body currently being compiled.
     
     ENTRY POINTS:
     
        ch_init     should be called at the start of compilation of a
                    module to initialize the module.
     
        ch_end      should be called when compilation of a module  is
                    complete.
     
        t_chain     is  a  pointer  to  the  root node of the current
                    tuple  chain.  This  root  node   is   always   a
                    StartBlock operator.
     
        t_end       is  a  pointer  to  the  end  block  node for the
                    current chain.  This pointer is  undefined  until
                    EndChain (see below) is called.
     
        new_chain   will  create  a new StartBlock tuple node and set
                    T_Chain  to  it.  This  node  thus  becomes   the
                    current  tuple chain.  New tuples may be added to
                    the chain with Emit calls.
     
        end_chain   will create an EndBlock tuple node and append  it
                    to  the  current tuple chain.  A call to EndBlock
                    indicates that the intermediate form chain for  a
                    single block has been completely created.  A call
                    to T_Set must precede any subsequent Emit calls.
     
        emit        is called with a tuple node, which it adds to the
                    current  tuple  chain.  The  tuple is inserted in
                    the chain following the node created by NewChain,
                    the  last  node  added  by Emit, or the last node
                    specified by T_Set, whichever was called last.
     
        t_set       is called with a pointer to a node in the current
                    tuple   chain.  Subsequent  calls  to  Emit  will
                    insert nodes starting after the node specified in
                    this  T_Set  call.  If  the  argument to T_Set is
                    nil, then subsequent calls to  Emit  will  append
                    nodes at the end of the chain.
     
        cursor      is the pointer to the tuple after which Emit will
                    insert nodes.  It should not be set directly, but
                    may be copied to save the emission location.
     
        ap_chain    combines the functions of T_Set and Emit.  A call
                    to ApChain is equivalent to a call to T_Set  with
                    the  first  argument,  followed by a call to Emit
                    with the second argument.
     
        dechain     is called with a pointer to a node in the current
                    tuple chain.  The node is unlinked from the tuple
                    chain and disposed of.
     
        reclaim     scans the current tuple chain.  The  usage  count
                    of  each  expression node is set to the number of
                    distinct references there are to that node in the
                    chain,  and  the  result field of each expression
                    node is set to nil.  Any  expressions  which  are
                    not  elements  of  expressions  in some statement
                    node in the chain are deleted from the chain  and
                    disposed.  The  remaining  nodes of the chain are
                    numbered, starting with one.
     
        clr_rslt    will set the result field of each expression node
                    in the chain to nil.
     
        del_tuples  will  dispose  of  all  the  nodes in the current
                    tuple chain.
     
        wr_tuples   will save the contents of the current tuple chain
                    so  that they can be retrieved later by RdTuples.
                    All of the nodes in the current chain  will  then
                    be disposed of.
     
        rd_tuples   retrieves  the  tuple  chain  associated with the
                    current block  node  in  the  symbol  table.  The
                    chain  to  be  retrieved  must  have  been stored
                    previously with WrTuples.
     
        is_expr     returns a  boolean  value  indicating  whether  a
                    tuple is an expression node.
     
     NOTES:  The intermediate form is represented simultaneously as a
        DAG and as a linear chain.  The  fundamental  rule  which  is
        necessary  to  make  this  dual representation useful is:  An
        expression node which represents an  operand  of  some  other
        tuple  (in the DAG representation) must precede that tuple in
        the linear chain.  This guarantees that, if the  intermediate
        form  is  processed  in  linear  order,  the  operands  of an
        operator will have been processed before the operator  itself
        is processed.
     
     RESPONSIBLE:  Compiler Group
     
     ---------------------------------------------------------------- *)
$PAGE includes

$OPTIONS special, nocheck

$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE pasifu.typ
$INCLUDE corout.inc
$INCLUDE rlb:cmdutl.typ
$INCLUDE tmpnam.inc
$PAGE declarations

(*********   PUBLIC   *********)

public var
    t_chain: tuple; (* The start block of the current chain. *)
    t_end: tuple; (* The end block tuple at the end of the chain. *)
    cursor: tuple; (* Emit adds new tuples following the cursor. *)


(*********   LOCAL   *********)

var
    last_tuple: tuple; (* The last tuple in the chain. *)
$PAGE read/write declarations

(*  The data on this page are only relevant to the RdTuples and WrTuples
    routines.  *)

type
  integer = int_type;


(*  DUMPIO Declarations  *)

type
  fname_str = string [30];

external procedure openfile  ( fname_str; var integer; var boolean;
			       boolean; boolean );
external procedure writepage ( integer; integer; integer; integer; var boolean );
external procedure readpage  ( integer; integer; integer; integer; var boolean );
external procedure closefile ( integer; boolean );


var
  if_in_channel,
  if_out_channel: -1 .. 15;


(*  Coroutine Interface  *)

var
  get_if, put_if: environment;
  if_out_page: int_type;
  if_data_word: integer;
  if_in_page: int_type;
  finish_output: boolean;


(*  Data that is in PASDAT so it is transmitted between passes  *)

external var
  ch_index, ch_last: if_file_index;
$PAGE sequence error

(*  SequenceError is called if NewCall or RdTuples is entered when TChain
    is non-nil.  It prints a message on the terminal, prints a trace, and
    terminates the compilation.  *)


procedure sequence_error;

begin
  writeln (tty, '? Compiler error: Invalid new-chain sequencing.  Please contact');
  writeln (tty, '                  compiler maintenance personel.');
  trace;
  stop;
end (* sequence_error *);
$PAGE ch_init

(*  ChInit will initialize the PASIFU module.  *)

public procedure ch_init;

begin
  t_chain := nil; (* Allow NewChain calls. *)
  ch_index := nil; (* No procedures defined yet. *)
end;
$PAGE ch_open & ch_close

(*  ChOpen opens the intermediate form file at the start of a pass.
    OldFile indicates an existing IF file is to be opened for input; NewFile
    indicates a new IF file is to be created.  *)


public procedure ch_open ( old_file, new_file: boolean );

var flag: boolean;

begin
  if old_file
    then openfile (tempname ('IFM'), if_in_channel, flag, true, false)
    else if_in_channel := -1;
  if new_file
    then openfile (tempname ('IFM'), if_out_channel, flag, false, true)
    else if_out_channel := -1;
end;



(*  ChClose closes the intermediate form file at the end of a pass.  *)


public procedure ch_close;

begin
  if if_in_channel <> -1 then
    closefile (if_in_channel, true); (* Delete old file. *)
  if if_out_channel <> -1 then
    closefile (if_out_channel, not finish); (* Save new file if no errors. *)
end;
$PAGE new_chain

(*  NewChain will create a new StartBlock node, which will become the current
    tuple chain.  *)


public procedure new_chain;

begin
  if t_chain <> nil then
    sequence_error;
  new (t_chain, start_block);
  with t_chain^ do begin
    next := nil;
    prev := nil;
    label_sym := nil;
    block_order_no := 0;
    downward_thread := t_chain;
    upward_thread := t_chain;
    inward_jumps := nil;
    idom := nil;
    dom_son := nil;
    dom_brother := nil;
  end;
  cursor := t_chain;
  last_tuple := t_chain;
end (* new_chain *);
$PAGE end_chain

(*  EndChain will create a new EndBlock node, which it will attach at the end
    of the tuple chain.  *)


public procedure end_chain;

begin
  new (t_end, end_block);
  with t_end^ do begin
    next := nil;
    prev := cursor;
    cursor^.next := t_end;
    label_sym := nil;
    block_order_no := upward_thread^.block_order_no + 1;
    downward_thread := t_chain;
    upward_thread := t_chain^.upward_thread;
    t_chain^.upward_thread := t_end;
    upward_thread^.downward_thread := t_end;
    inward_jumps := nil;
    idom := nil;
    dom_son := nil;
    dom_brother := nil;
  end;
end (* end_chain *);
$PAGE emit

(*  Emit will insert a tuple node into a tuple chain following the node
    indicated by the current cursor.  *)


public procedure emit ( t: tuple );

begin
  with t^ do begin
    next := cursor^.next;
    prev := cursor;
  end;
  with cursor^ do begin
    if next = nil
      then last_tuple := t
      else next^.prev := t;
    next := t;
  end;
  cursor := t;
end (* emit *);
$PAGE t_set

(*  T_Set will reset the chain cursor to a specified node, thus making it
    possible to insert new tuples in the middle of the chain.  *)


public procedure t_set ( t: tuple );

begin
  if t = nil
    then cursor := last_tuple
    else cursor := t;
end (* t_set *);
$PAGE ap_chain

(*  ApChain will reset the chain cursor to its first argument node, and then
    emit its second argument node following the first argument node.  *)


public procedure ap_chain ( after, t: tuple );

begin
  with t^ do begin
    next := after^.next;
    prev := after;
  end;
  with after^ do begin
    if next = nil
      then last_tuple := t
      else next^.prev := t;
    next := t;
  end;
  cursor := t;
end (* ap_chain *);
$PAGE dechain

(*  Dechain is called with a pointer to a node in the tuple chain.  It removes
    the tuple from the chain and disposes it.  *)


public procedure dechain ( t: tuple );

begin
  with t^ do begin
    prev^.next := next;
    if next = nil
      then last_tuple := prev
      else next^.prev := prev;
    if cursor = t then
      cursor := prev;
  end;
  dispose (t);
end (* dechain *);
$PAGE reclaim

(*  Reclaim sets usage counts and node id numbers in the tuple chain nodes, and
    disposes of any expression nodes which cannot be reached from any statement
    node.  The algorithm is as follows:

    (1) Set the usage count of each expression node in the chain to zero.

    (2) Walk the chain backwards.  If an expression node with a zero usage
	count is encountered, it may be discarded, since any references to
	it would already have been processed (an operand node must precede
	any tuple which references it).  Otherwise, increment the usage
	counts of all the operands of this tuple.

    (3) Walk the chain forwards again to set the node id numbers.  *)


public procedure reclaim;

var
    t, next_t: tuple; (* For linear scans of the chain. *)
    i: int_type; (* For indexing expression operands. *)
    number: int_type; (* For numbering the nodes. *)

begin

  (*  Set all the expression usage counts to zero.  *)

  t := t_chain;
  while t <> nil do
    with t^ do begin
      if (opcode >= first_expr) and (opcode <= last_expr) then begin
	usage_count := 0;
	result := nil;
	ref_fre := 0;
      end;
      t := next;
    end;

  (*  Scan backwards, incrementing usage counts (except for EVAL operands).  *)

  t := t_chain^.upward_thread;
  while t <> nil do
    with t^ do begin
      next_t := prev;
      if ((first_expr <= opcode) and (opcode <= last_expr)) andif
	(usage_count = 0) then begin (* Node is unused - delete it. *)
	  prev^.next := next;
	  next^.prev := prev;
	  dispose (t);
	end
      else (* Node is used - mark its operands. *)
	case opcode of

	  start_with,
	  end_with:
	    if with_rec <> nil then
	      with_rec^.usage_count := with_rec^.usage_count + 1;

	  eval_op,
	  assign_op:
	    begin
	      if lhs <> nil then
		lhs^.usage_count := lhs^.usage_count + 1;
	      if rhs <> nil then
		rhs^.usage_count := rhs^.usage_count + 1;
	    end;

	  (* The jump operators appear in groups, which as a group constitute a
	     single operator and therefore a single reference to the condition. *)

	  jump_in_op:		(* count when case_jump encountered *)
	    ;

	  case_jump_op:
	    if cond <> nil then
	      cond^.usage_count := cond^.usage_count + 1;

	  goto_op:
	    if target_frame <> nil
	      then target_frame^.usage_count := target_frame^.usage_count + 1;

	  jump_t_op, jump_f_op:			(* may come in pairs *)
	    begin
	      if cond <> nil then
		cond^.usage_count := cond^.usage_count + 1;
	      if ((prev^.opcode = jump_t_op) or (prev^.opcode = jump_f_op))
	        andif (prev^.cond = cond)	(* if paired, skip the twin *)
		then next_t := prev^.prev;
	    end;

	  dispose_op:
	    if dptrarg <> nil then
	      dptrarg^.usage_count := dptrarg^.usage_count + 1;

	  first_io_stmt..last_io_stmt:
	    if not old_file and (file_arg <> nil) then
	      file_arg^.usage_count := file_arg^.usage_count + 1;

	  read_op, write_op:
	    begin
	      if not rw_old_file and (rw_file <> nil) then
		rw_file^.usage_count := rw_file^.usage_count + 1;
	      if rw_item <> nil then
		rw_item^.usage_count := rw_item^.usage_count + 1;
	      if rw_width <> nil then
		rw_width^.usage_count := rw_width^.usage_count + 1;
	      if rw_precision <> nil then
		rw_precision^.usage_count := rw_precision^.usage_count + 1;
	    end;

	  seek_op:
	    begin
	      if seek_file <> nil then
		seek_file^.usage_count := seek_file^.usage_count + 1;
	      if seek_index <> nil then
		seek_index^.usage_count := seek_index^.usage_count + 1;
	    end;

	  field_ref:
	    if base_rec <> nil then
	      base_rec^.usage_count := base_rec^.usage_count + 1;

	  ptr_ref:
	    if base_ptr <> nil then
	      base_ptr^.usage_count := base_ptr^.usage_count + 1;

	  array_ref:
	    begin
	      if base_array <> nil then
		base_array^.usage_count := base_array^.usage_count + 1;
	      if index_val <> nil then
		index_val^.usage_count := index_val^.usage_count + 1;
	    end;
	  substr_ref:
	    begin
	      if base_string <> nil then
		base_string^.usage_count := base_stringg^.usage_count + 1;
	      if substr_index <> nil then
		substr_index^.usage_count := substr_index^.usage_count + 1;
	      if substr_length <> nil then
		substr_length^.usage_count := substr_length^.usage_count + 1;
	    end;

	  buffer_ref:
	    if base_file <> nil then
	      base_file^.usage_count := base_file^.usage_count + 1;

	  mem_ref, addr_ref, immed_ref:
	    begin
	      if item.index <> nil then
		item.index^.usage_count := item.index^.usage_count + 1;
	      if item.base <> nil then
		item.base^.usage_count := item.base^.usage_count + 1;
	    end;

	  call_op, signal_op, func_call_op, func_signal_op:
	    begin
	      if subr <> nil then
		subr^.usage_count := subr^.usage_count + 1;
	      for i := 1 to nargs do
		if arglist[i] <> nil then
		  arglist[i]^.usage_count := arglist[i]^.usage_count + 1;
	    end;

	  first_nnary_op..last_nnary_op,
	  subr_var_op, (* omit desc_ref, gen_andif_op, gen_orif_op *)
	  first_sunary_op..out_str_op,
	  eoln_op..last_sunary_op,	(* omit io_fix_str_op and io_var_str_op *)
	  random_op, (* omit set_op *)
	  extstatus_op..last_snary_op,
	  first_chk_op..last_chk_op:
	    for i := 1 to nopers do
	      if operand[i] <> nil then
		operand[i]^.usage_count := operand[i]^.usage_count + 1;

	  gen_andif_op, gen_orif_op:	(* operands only place holders, don't count *)
	    ;

	  set_op,			(* These are place-holder operators-- *)
	  desc_ref,			(*   each reference is a reference to *)
	  io_fix_str_op,		(*   to all operands. *)
	  io_var_str_op:
	    for i := 1 to nopers do
	      if operand[i] <> nil then
		operand[i]^.usage_count := operand[i]^.usage_count + usage_count;

	  size_op:
	    if type_parm <> nil then
	      type_parm^.usage_count := type_parm^.usage_count + 1

	end (* case opcode *);
      t := next_t;
    end;

  (*  Number the remaining nodes.  *)

  number := 1;
  t := t_chain;
  while t <> nil do
    with t^ do begin
      nodeid := number;
      number := number + 1;
      t := next;
    end;

end (* reclaim *);
$PAGE clr_rslt

(*  ClrRslt will set the result field of each expression node in the tuple
    chain to nil.  *)

public procedure clr_rslt;

var t: tuple;

begin
  t := t_chain;
  while t <> nil do
    with t^ do begin
      if (opcode >= first_expr) and (opcode <= last_expr) then
	result := nil;
      t := next;
    end;
end;
$PAGE del_tuples

(*  DelTuplestes all the tuple nodes in the current tuple chain.  *)


public procedure del_tuples;

var
    t: tuple; (* Scans the tuple chain. *)
    next: tuple; (* The node after 't'. *)

begin
  t := t_chain;
  while t <> nil do begin
    next := t^.next;
    dispose (t);
    t := next
  end;
  t_chain := nil;
end (* del_tuples *);
$PAGE fix_pointers

(*  FixPointers is used to fix up the links within an intermediate form chain
    when it is written out to disk or read back in.  It applies the parametric
    procedure Fix to each internal chain pointer (except the Next/Prev pointers)
    in the intermediate form.  For writing out, Fix will replace a true pointer
    by the Nodeid of the target node.  For reading in, Fix will replace the node
    id number by the address of the indexed node.  The parameter ToPointers is
    true if id numbers are being replaced by pointers, false if pointers are
    being replaced by id numbers.  *)


type fix_function = function ( tuple ): tuple;

procedure fix_pointers ( fix: fix_function; to_pointers: boolean );

var
  t: tuple;
  i: int_type;

begin
  t := t_chain;
  while t <> nil do begin
    with t^ do begin
      case opcode of

	start_with,
	end_with:
	    with_rec := fix (with_rec);

	eval_op,
	assign_op:
	  begin
	    lhs := fix (lhs);
	    rhs := fix (rhs);
	  end;

	start_block,
	end_block,
	label_node:
	  begin
	    downward_thread := fix (downward_thread);
	    upward_thread := fix (upward_thread);
	    inward_jumps := fix (inward_jumps);
	    idom := fix (idom);
	    dom_son := fix (dom_son);
	    dom_brother := fix (dom_brother);
	  end;

	jump_op,
	jump_t_op,
	jump_f_op,
	jump_in_op,
	gen_jump_op,
	case_jump_op:
	  begin
	    cond := fix (cond);
	    jump_from := fix (jump_from);
	    jump_to := fix (jump_to);
	    next_inward_jump := fix (next_inward_jump);
	  end;

	goto_op:
	    target_frame := fix (target_frame);

	dispose_op:
	    dptrarg := fix (dptrarg);

	first_io_stmt..last_io_stmt:
	  file_arg := fix (file_arg);

	read_op,
	write_op:
	  begin
	    rw_file := fix (rw_file);
	    rw_item := fix (rw_item);
	    rw_width := fix (rw_width);
	    rw_precision := fix (rw_precision);
	  end;

	seek_op:
	  begin
	    seek_file := fix (seek_file);
	    seek_index := fix (seek_index);
	  end;

	field_ref:
	    base_rec := fix (base_rec);

	ptr_ref:
	    base_ptr := fix (base_ptr);

	array_ref:
	  begin
	    base_array := fix (base_array);
	    index_val := fix (index_val);
	  end;

	substr_ref:
	  begin
	    base_string := fix (base_string);
	    substr_index := fix (substr_index);
	    substr_length := fix (substr_length);
	  end;

	buffer_ref:
	  base_file := fix (base_file);

	mem_ref,
	addr_ref,
	immed_ref:
	  with item do begin
	    base := fix (base);
	    index := fix (index);
	  end;

	call_op,
	signal_op,
	func_call_op,
	func_signal_op:
	  begin
	    subr := fix (subr);
	    for i := 1 to nargs do
	      arglist [i] := fix (arglist [i]);
	  end;

	nary_op..last_nary_op,
	first_chk_op..last_chk_op:
	    for i := 1 to nopers do
	      operand [i] := fix (operand [i]);

	size_op:
	    type_parm := fix (type_parm);
	others:
	    (*  no action  *)

      end (* case opcode *);

      t := next;
    end (* with t^ *);
  end (* while t <> nil *);
end (* fix_pointers *);
$PAGE put_if_rtn

(*  PutIfRtn is the coroutine body for PutIf.  It adds IfDataWord to the buffer,
    and writes the buffer to the file if it is full.  *)


procedure put_if_rtn;

var
  buffer: array [0..127] of integer;
  buf_ptr: 0 .. 127;
  io_ok: boolean;

begin
  finish_output := false;
  detach;
  loop
    for buf_ptr := 0 to 127 do begin
      buffer [buf_ptr] := if_data_word;
      detach;
    exit if finish_output;
    end;
    writepage (ord(address(buffer)), 1, if_out_page, if_out_channel, io_ok);
    if_out_page := if_out_page + 1;
  end;
end (* put_if_rtn *);
$PAGE get_if_rtn

(*  GetIfRtn is the coroutine body for GetIf.  *)


procedure get_if_rtn;

var
  buffer: array [0..127] of integer;
  buf_ptr: 0 .. 127;
  io_ok: boolean;

begin
  detach;
  loop
    readpage (ord(address(buffer)), 1, if_in_page, if_in_channel, io_ok);
    if_in_page := if_in_page + 1;
    for buf_ptr := 0 to 127 do begin
      if_data_word := buffer [buf_ptr];
      detach;
    end;
  end;
end (* get_if_rtn *);
$PAGE open_out_tuples

(*  OpenOutTuples will prepare to write out the tuple chain for the current
    block.  If the first index chain entry is not for the current block, then
    this block must not have been opened before, so an entry is created for
    it at the end of the chain.  All writing is done at the end of the file,
    so that previously saved chains will still be valid if a pass is rerun.
    The index chain entry for the current block will be left at the end of the
    chain.  *)


procedure open_out_tuples ( n_nodes: index_range );

begin

  (*  Find a chain index entry for the current block.  Create it if necessary.
      Make it the last entry in the index.  *)

  if ch_index = nil then begin
    new (ch_index);
    ch_last := ch_index;
  end
  else if ch_index^.block <> cur_block then begin
    new (ch_last^.next);
    ch_last := ch_last^.next;
  end
  else if ch_last <> ch_index then begin
    ch_last^.next := ch_index;
    ch_last := ch_index;
    ch_index := ch_index^.next;
  end;

  ch_last^.next := nil;
  ch_last^.block := cur_block;
  ch_last^.page := if_out_page;

  if_data_word := n_nodes;
  call (put_if);
end (* open_out_tuples *);
$PAGE open_in_tuples

(*  OpenInTuples will prepare to read in the tuple chain for the current
    block.  We assume that there is an entry for the current block in the
    index somewhere, and move it to the end when we find it.  *)


procedure open_in_tuples ( var n_nodes: index_range );

var
  scan, follow: if_file_index;
begin
  scan := ch_index;
  follow := nil;
  while scan^.block <> cur_block do begin
    follow := scan;
    scan := scan^.next;
  end;
  if follow <> nil then begin
    follow^.next := scan^.next;
    scan^.next := ch_index;
    ch_index := scan;
    if ch_last = scan then
      ch_last := follow;
  end;

  if_in_page := scan^.page;
  call (get_if);
  n_nodes := if_data_word;
end (* open_in_tuples *);
$PAGE write_tuple

(*  WriteTuple will write a tuple to the file.  The tuple is preceded by a word
    containing its length.  *)


procedure write_tuple ( t: tuple );

type
  data_record = array [index_range] of integer;

var
  dr: ^ data_record;
  i, size: index_range;

begin
  dr := ptr (ord(t) - 1);
  size := - dr^[0] - 1;
  if_data_word := size;
  call (put_if);
  for i := 1 to size do begin
    if_data_word := dr^ [i];
    call (put_if);
  end;
end (* write_tuple *);
$PAGE read_tuple

(*  ReadTuple will return a pointer to a newly created node containing a
    tuple which has been read from the file.  *)


function read_tuple: tuple;

type
  data_record = array [index_range] of integer;

var
  dr: ^ data_record;
  i, size: index_range;

begin
  call (get_if);
  size := if_data_word - 1;
  new (dr: size);
  for i := 0 to size do begin
    call (get_if);
    dr^ [i] := if_data_word;
  end;
  read_tuple := ptr (ord (dr));
end (* read_tuple *);
$PAGE wr_tuples

(*  WrTuples will save the current tuple chain for future reference, and
    release the storage it uses.  *)


public procedure wr_tuples;

var t: tuple;

function
  tuple_number ( t: tuple ): tuple;
  begin
    if t = nil
      then tuple_number := ptr (0)
      else tuple_number := ptr (t^.nodeid);
  end;

begin
  fix_pointers (tuple_number, false);
  put_if := create (put_if_rtn, 200);
  open_out_tuples (ord (t_chain^.upward_thread));
  t := t_chain;
  while t <> nil do begin
    write_tuple (t);
    t := t^.next;
  end;
  finish_output := true;
  call (put_if);
  del_tuples;
end (* wr_tuples *);
$PAGE rd_tuples

(*  RdTuples will restore the tuple chain saved for the current block.  *)


public procedure rd_tuples;

type
    index_array = array [index_range] of tuple;

var
  t: tuple;
  n_nodes, i: int_type;
  heap: integer;
  index: ^ index_array;

function tuple_address ( t: tuple ): tuple;
  begin
    if ord (t) = 0
      then tuple_address := nil
      else tuple_address := index^ [ord(t)];
  end;

begin

  (*  Read the tuple chain.  *)

  get_if := create (get_if_rtn, 200);
  open_in_tuples (n_nodes);
  t_chain := read_tuple ();
  t := t_chain;
  for i := 2 to n_nodes do
    with t^ do begin
      next := read_tuple ();
      next^.prev := t;
      t := next;
    end;

  (*  Build an index to the tuple chain, and fix up the pointers.  *)

  mark (heap);
  new (index: n_nodes);
  t := t_chain;
  for i := 1 to n_nodes do begin
    index^ [i] := t;
    t := t^.next;
  end;
  fix_pointers (tuple_address, true);
  cursor := t_chain;
  last_tuple := t_chain^.downward_thread;
  release (heap);
end (* rd_tuples *);
$PAGE ch_end

(*  ChEnd will discard any saved tuple chains.  *)

public procedure ch_end;

begin
end (* ch_end *);
$PAGE is_expr

(*  IsExpr returns true iff its argument is an expression node.  *)


public function is_expr ( t: tuple ): boolean;

begin
  with t^ do
    is_expr := (opcode >= first_expr) and (opcode <= last_expr);
end (* is_expr *).
 N $Î