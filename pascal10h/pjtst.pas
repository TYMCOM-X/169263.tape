$TITLE PASJMP
$LENGTH (42)

module pasjmp;
$PAGE includes
$include pascal.inc
$include pasist.inc
$include paspt.typ
$include pasif.typ
$include pasifu.inc
$INCLUDE passet.inc
$INCLUDE pasopt.inc
$INCLUDE pa2cgr.inc
$INCLUDE passw.inc
$INCLUDE pa2dmp.inc
$PAGE next_action
(* NEXT ACTION finds the first imperative operator following a node.  Specifically,
   it ignores commentary actions, such as a start_stmt. *)

function next_action (node: tuple): tuple;
 begin
  next_action := node^.next;
  while (next_action <> nil) andif (next_action^.opcode = start_stmt)
    do next_action := next_action^.next;
 end;
$PAGE get_following_label
(* GET FOLLOWING LABEL looks for a label following a "node".  If one is there, then
   it is returned;  otherwise, a label is created and insert following the node. *)

function get_following_label ( node: tuple ): tuple;	(* returns the label node *)
 var lab: tuple;
 begin
  lab := next_action (node);
  if lab^.opcode <> label_node then begin;
    new (lab, label_node);
    ap_chain (node, lab);			(* insert in tuple chain *)
    with lab^ do begin
      label_sym := nil;				(* this is compiler generated label *)
      nodeid := 0;
    end;
  end;
  get_following_label := lab;
 end;
$PAGE append_jump
(* APPEND JUMP is used to add one of the jump operators to the intermediate
   form.  Its adds the jump after a specified node. *)

procedure append_jump
	     (	after_node: tuple;		(* append after this node *)
		opc: tuple_opcodes;		(* jump opcode *)
		test: expr;			(* jump condition, may be nil *)
		target: tuple       );		(* target label of jump *)

 var jump: tuple;
 begin
  new (jump, jump_op);				(* jump_op form is same for all *)
  ap_chain (after_node, jump);		(* chain into tuple chain *)
  with jump^ do begin
    nodeid := 0;
    opcode := opc;				(* select correct jump *)
    cond := test;
    jump_to := target;
    low_cntrl := 0; high_cntrl := 0;
    jump_from := nil;
    next_inward_jump := nil;
  end (* with *) ;
 end;
$PAGE remove_label, remove_jump
(* REMOVE LABEL deletes a label node ("lab") from the label list;  "t_chain" as
   always provides the hook for the start of the list. *)

procedure remove_label (lab: tuple);
 begin
  with lab^ do begin
    if upward_thread = nil
      then t_chain^.first_label := downward_thread
      else upward_thread^.downward_thread := downward_thread;
    if downward_thread <> nil
      then downward_thread^.upward_thread := upward_thread;
  end;
 end;



(* REMOVE JUMP deletes a jump operator ("jmp") from the inward_jump list of its
   target label. *)

procedure remove_jump (jmp: tuple);
 var lab, injmp, last_injmp: tuple;
 begin
  lab := jmp^.jump_to;
  injmp := lab^.inward_jumps;	(* search for jmp, and find preceding jump *)
  last_injmp := nil;
  while injmp <> jmp do begin
    if injmp = nil then return;		(* jmp not on inward jump list *)
    last_injmp := injmp;
    injmp := injmp^.next_inward_jump;
  end;
  if last_injmp = nil	(* remove it from chain *)
    then lab^.inward_jumps := jmp^.next_inward_jump	(* is start of chain *)
    else last_injmp^.next_inward_jump := jmp^.next_inward_jump;
 end;
$PAGE delete_unused_operators, delete_basic_block
(* DELETE UNUSED OPERATORS is called to delete a list of operators which can never
   be reached during execution.  It deletes all nodes from a specified "node" up
   to, but not including, the following label node (which may be the initial 
   node) or end block node. *)

procedure delete_unused_operators ( node: tuple );
 var tnode, next: tuple;
 begin
  tnode := node;
  while (tnode^.opcode <> label_node) and (tnode^.opcode <> end_block) do begin
    next := tnode^.next;
    if not is_expr (tnode) then begin		(* expr's are deleted by garbage collection *)
      if (jump_op <= tnode^.opcode) and (tnode^.opcode <= case_jump_op)
        then remove_jump (tnode);	(* take care to remove jumps from inward jump lists *)
      dechain (tnode);
    end;
    tnode := next;
  end;
 end;



(* DELETE BASIC BLOCK deletes a basic block.  "Node" is the first node of the block
   and is assumed to be a label node. *)

procedure delete_basic_block ( node: tuple );
 begin
  delete_unused_operators (node^.next);
  dechain (node);
 end;
$PAGE complete_graph
(* COMPLETE GRAPH creates the block edge linkages.  When called, it is assumed
   that all label nodes are linked together, that the outward jump lists are
   complete, and that the targets of jumps are filled in. *)

procedure complete_graph;
 var lab, jmp: tuple;

 begin
  (* Walk the list of labels and nil the inward_jump list.  This is to enable
     use of this routine on a previously labeled graph. *)

  lab := t_chain^.first_label;
  repeat				(* there must be at least one label *)
    lab^.inward_jumps := nil;
    lab := lab^.downward_thread;
  until lab = nil;

  (* The steps required to complete the graph are to append each jump to the
     inward jump list of the target label, and to record the basic block
     (i.e. label) from which it exits. *)

  lab := t_chain^.first_label;		(* for each basic block ... *)
  repeat
    jmp := lab^.outward_jumps;		(* for each jump exiting the block *)
    while (jmp <> nil) do begin
      with jmp^ do begin
	jump_from := lab;		(* record block exited *)
	next_inward_jump := jump_to^.inward_jumps;	(* append to chain *)
	jump_to^.inward_jumps := jmp;
	if (next^.opcode < jump_op) or (next^.opcode > case_jump_op)
	  then jmp := nil	(* go on to next jump (if any) *)
	  else jmp := next;
      end;
    end (* while *) ;
    lab := lab^.downward_thread;
  until lab = nil;
 end;
$PAGE transfer_jumps
(* TRANSFER JUMPS transfers jumps targeting on a certain label ("lab") to some 
   other label ("new_lab").  This is used by simplify_jumps to handle jumps to
   jumps. *)

procedure transfer_jumps (lab: tuple; new_lab: tuple);
 var jmp, nextjmp: tuple;
 begin
  jmp := lab^.inward_jumps;		(* walk list of all jumps *)
  lab^.inward_jumps := nil;		(* routine should leave list empty *)
  while jmp <> nil do begin
    with jmp^ do begin
      jump_to := new_lab;			(* point jump at new label *)
      nextjmp := next_inward_jump;			(* do iteration step before updating chain *)
      next_inward_jump := new_lab^.inward_jumps;
      new_lab^.inward_jumps := jmp;
      jmp := nextjmp;
    end;
  end;
 end;
$PAGE simplify_graph
(* SIMPLIFY GRAPH removes jumps to jumps, by setting the initial jump to target
   on the final label, and deleting the superfluous basic block containing the
   label. *)

procedure simplify_graph;
 var lab, next_lab, jmp: tuple;
 begin
  lab := t_chain^.first_label;
  repeat
    next_lab := lab^.downward_thread;		(* record in case node deleted *)

    jmp := next_action (lab);			(* check for lab: jump, stmt marks may intervene *)
    if (jmp <> nil) andif (jmp^.opcode = jump_op) then begin
      transfer_jumps (lab, jmp^.jump_to);	(* make incomming jumps point at outward label *)
      remove_label (lab);	(* slice out of label list *)
      delete_basic_block (lab);	(* delete the block *)
    end;

    lab := next_lab;
  until lab = nil;
 end;
$PAGE order_graph
(* ORDER GRAPH performs a depth-first ordering of the basic blocks in a program.
   The ordering is indicated by the block number and the downward and upward
   threads.  Any blocks which are unreferenced after the ordering is performed
   are deleted. *)

procedure order_graph;


 (* SEARCH recursively walks the spanning tree of labels (i.e. basic blocks)
    and performs the depth-first ordering by building the threads.  The block
    number is used as a marker, and on initial entry all are assumed to be
    zero.  As all labels are initially on the chain of labels, the label is
    removed from the original chain, before being threaded according to the
    ordering.  Thus, any labels left on the original chain are unreferenced. *)

 var last_visited: tuple;		(* label last processed as we back out *)

 procedure search (lab: tuple);
  var jmp: tuple;
  begin
    with lab^ do begin
      block_order_no := 1;		(* flag this node as visited *)
      remove_label (lab);		(* unchain from original thread *)

      jmp := outward_jumps;
      while jmp <> nil do begin	(* process all jmps *)
	if jmp^.jump_to^.block_order_no = 0	(* if unvisited *)
	  then search (jmp^.jump_to);
	with jmp^ do	(* get next outward jump (if any) *)
	  if (next^.opcode < jump_op) or (next^.opcode > case_jump_op)
	    then jmp := nil
	    else jmp := next;
      end;

      if last_visited <> nil		(* thread current node onto the chain *)
	then last_visited^.upward_thread := lab
	else t_chain^.last_label := lab;
      downward_thread := last_visited;
      upward_thread := nil;
      last_visited := lab;
    end;
  end;

 var lab, nextlab: tuple;
     number: 0..4095;

 begin
  (* Process the tree, ordering all nodes which may be reached from the start
     node. *)

  lab := t_chain^.first_label;
  while lab <> nil do begin		(* mark all nodes as unvisited *)
    lab^.block_order_no := 0;
    lab := lab^.downward_thread;
  end;
  last_visited := nil;			(* new chain is initially nil *)
  search (t_chain^.first_label);

  (* Any blocks left on the original chain cannot be reached during execution,
     so they may be deleted. *)

  lab := t_chain^.first_label;
  while lab <> nil do begin
    nextlab := lab^.downward_thread;
    delete_basic_block (lab);
    lab := nextlab;
  end;

  (* Process the new chain, numbering the label nodes on it in order, and attach
     it to the start block node. *)

  t_chain^.first_label := last_visited;
  number := 1;
  lab := last_visited;
  while lab <> nil do begin
    lab^.block_order_no := number;
    number := number + 1;
    lab := lab^.downward_thread;
  end;
 end;
$PAGE order_tuples
(* ORDER TUPLES performs a final scan of the IF, rearranging next/prev links
   so that the order of the basic blocks in the next/prev tuple chain is the
   same as their order in the downward_thread label chain. *)

procedure order_tuples;
 var t, last_t, next_lab: tuple;
 begin
  t := t_chain;
  last_t := nil;
  next_lab := nil;
  loop
    if ( (t^.opcode = label_node) or (t^.opcode = end_block) ) and
      (next_lab <> nil) then begin
	last_t^.next := next_lab;
	next_lab^.prev := last_t;
      end;

  exit if t^.opcode = end_block;

    if t^.opcode = label_node then
      if t^.downward_thread = nil
	then next_lab := t_chain^.final_tuple
	else next_lab := t^.downward_thread;
    last_t := t;
    t := t^.next;
  end;
 end;
$PAGE insert_preheaders
(* INSERT PREHEADERS scans the I/F and inserts preheaders before the headers of
   all loops.  For our purposes, a loop header is any node with an impinging
   edge from a block with follows it in the ordering. *)

procedure insert_preheaders;

 var
   lab, next_lab: tuple;		(* for scanning the input *)
   looplab: tuple;			(* after splitting the header, node which receives
					   the backward edges. *)
   jmp, njmp, ljmp: tuple;			(* for scanning inward jump lists *)
   number: index_range;			(* for renumbering basic blocks *)

 begin
  lab := t_chain^.first_label;		(* examine each label node *)
  number := 1;
  while lab <> nil do begin
    lab^.block_order_no := number;
    number := number + 1;
    next_lab := lab^.downward_thread;

    (* Transfer all back edges entering this basic block, to a newly created
       label node ("looplab").  Forward edges will remain targeted on "lab".
       Insert an uncondition jump between the nodes. *)

    looplab := nil;			(* create when first back edge found *)
    jmp := lab^.inward_jumps;		(* scan inward jumps looking for back edges *)
    ljmp := nil;			(* preceding forward edge *)
    while jmp <> nil do begin
      njmp := jmp^.next_inward_jump;

      if jmp^.jump_from^.block_order_no < lab^.block_order_no
        then ljmp := jmp	(* have a forward edge *)

      else begin		(* have a back edge *)
	if looplab = nil then begin	(* create if no previous back edges *)
	  new (looplab, label_node);
	  ap_chain (lab, looplab);
	  with looplab^ do begin
	    label_sym := nil;
	    nodeid := 0;
	    block_order_no := number;	(* assign it number after lab *)
	    number := number + 1;	(* so next label gets correct number *)
	    downward_thread := lab^.downward_thread;	(* splice into label list *)
	    lab^.downward_thread := looplab;
	    upward_thread := lab;
	    outward_jumps := lab^.outward_jumps;	(* jumps follow new label *)
	    append_jump (lab, jump_op, nil, looplab);	(* insert a jump between lab and looplab *)
	    lab^.outward_jumps := lab^.next;	(* next == jump just created *)
	    inward_jumps := lab^.next;
	  end (* with *) ;
	end;

	if ljmp = nil			(* transfer jmp to looplab *)
	  then lab^.inward_jumps := njmp
	  else ljmp^.next_inward_jump := njmp;
	jmp^.next_inward_jump := looplab^.inward_jumps;
	looplab^.inward_jumps := jmp;
	jmp^.jump_to := looplab;
      end;

      jmp := njmp;
    end;

    lab := next_lab;			(* examine next label, skip the one inserted *)
  end;
 end;
$PAGE make_basic_blocks
(* MAKE BASIC BLOCKS processes the tuple list output from the first pass (that is,
   the labelling is assumed to be incomplete), and builds a complete, simplified,
   and ordered basic block graph. *)

public procedure make_basic_blocks;

 begin
  complete_graph;
  simplify_graph;
  order_graph;
  order_tuples;
  insert_preheaders;
  reclaim;
end.
  