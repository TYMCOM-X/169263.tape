$TITLE PASJMP -- Flow-Graph Analysis

module pasjmp;
$PAGE declarations
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasifu.inc
$SYSTEM pasesu.inc
$SYSTEM passet.inc
$PAGE next_action
(*  NEXT ACTION finds the first imperative operator following a node.
    It ignores commentary actions, such as a StartStmt.  *)

function next_action (node: tuple): tuple;

begin
  next_action := node^.next;
  while next_action^.opcode = start_stmt do
    next_action := next_action^.next;
end (* next_action *);
$PAGE get_following_label
(*  GET FOLLOWING LABEL looks for a label following "node".  If there is one,
    then it is returned; otherwise, a label is created and inserted following
    the node.   *)

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
      inward_jumps := nil;
    end;
  end;
  get_following_label := lab;
end (* get_following_label *);
$PAGE append_jump
(*  APPEND JUMP is used to add one of the jump operators to the intermediate
    form.  Its adds the jump after a specified node.  *)

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
  end (* with jump^ *) ;
end (* append_jump *);
$PAGE remove_jump
(*  REMOVE JUMP deletes a jump operator ("jmp") from the inward_jump list of its
    target label.  *)

procedure remove_jump (jmp: tuple);

var lab, injmp, last_injmp: tuple;

begin
  lab := jmp^.jump_to;
  injmp := lab^.inward_jumps;	(* search for jmp, and find preceding jump *)
  if injmp = jmp then
    lab^.inward_jumps := jmp^.next_inward_jump
  else begin
    repeat
      if injmp = nil then
	return; (* <---- exit if jmp is not on the list *)
      last_injmp := injmp;
      injmp := injmp^.next_inward_jumpt;
    until injmp = jmp;
    last_injmp^.next_inward_jump := jmp^.next_inward_jump;
  end;
end (* remove_jump *);
$PAGE delete_unused_operators
(*  DELETE UNUSED OPERATORS is called to delete a list of operators which can
    never be reached during execution.  It deletes all nodes from a specified
    "node" up to, but not including, the following label node or end block
    node, except for any StartStmt nodes which immediately precede a LabelNode.  *)

procedure delete_unused_operators ( node: tuple );

var snode, tnode, next: tuple;

begin
  tnode := node;
  loop
    snode := tnode;
    while snode^.opcode = start_stmt do
      snode := snode^.next;
  exit if (snode^.opcode = label_node) or (snode^.opcode = end_block);
    while tnode <> snode do begin
      next := tnode^.next;
      dechain (node);
      tnode := next;
    end;
    next := tnode^.next;
    if not is_expr (tnode) then begin		(* expr's are deleted by garbage collection *)
      if (first_jump_op <= tnode^.opcode) and (tnode^.opcode <= last_jump_op)
        then remove_jump (tnode);	(* remove jumps from inward jump lists *)
      dechain (tnode);
    end;
    tnode := next;
  end (* loop *);
end (* delete_unused_operators *);
$PAGE delete_basic_block
(*  DELETE BASIC BLOCK deletes a basic block.  "Node" is the first node of the
    block and is assumed to be a label node.  *)

procedure delete_basic_block ( node: tuple );

begin
  delete_unused_operators (node^.next);
  dechain (node);
end (* delete_basic_block *);
$PAGE number_the_blocks
(*  NUMBER THE BLOCKS sets the BlockOrderNo field of each label tuple.  *)

procedure number_the_blocks;

var number: index_range;
    lab: tuple;

begin
  lab := t_chain;
  number := 0;
  repeat
    lab^.block_order_no := number;
    number := number + 1;
    lab := lab^.downward_thread;
  until lab = t_chain;
end (* number_the_blocks *);
$PAGE boolean_jump
(*  BOOLEAN JUMP transforms a boolean operator into the equivalent jump form.  *)

procedure boolean_jump
      (	test: expr;			(* the boolean operator *)
	tloc: tuple;			(* where to go if the condition is true *)
	floc: tuple;			(* where to go if the condition is false *)
	continue: boolean  );		(* true => falling through leaves control at the
					   tloc location;  false => floc *)

var lab: tuple;			(* inserted label node *)

begin
  with test^ do begin
    case opcode of

      bnot_op:
	begin
	  boolean_jump (operand[1], floc, tloc, not continue);
	  dechain (test);
	end;

      and_op, andif_op:
	begin
	  lab := get_following_label (operand[1]);	(* evaluation of op1 continues at op2 *)
	  boolean_jump (operand[1], lab, floc, true);
	  boolean_jump (operand[2], tloc, floc, continue);
	  dechain (test);
	end;

      or_op, orif_op:
	begin
	  lab := get_following_label (operand[1]);
	  boolean_jump (operand[1], tloc, lab, false);
	  boolean_jump (operand[2], tloc, floc, continue);
	  dechain (test);
	end;

      others:
	begin
	  if continue
	    then append_jump (test, jump_f_op, test, floc)
	    else append_jump (test, jump_t_op, test, tloc);
	end

    end (* case opcode *) ;
  end (* with test^ *) ;
end (* boolean_jump *);
$PAGE explode_booleans
(*  EXPLODE BOOLEANS scans the intermediate form and explodes boolean operators
    into jump form.  There is a strong assumption here, that this processes the
    IF before optimization or shaping has occurred;  this means that operands are
    expected to appear in a regular order.

    A backward scan of the intermediate form is performed into to recognize the
    context in which the operators are used.  In the case where it is used as the
    operand of a jump, it is transformed into condtional jumps to the target and
    continuation of the referencing jump.  

    In the case where the operator yields a boolean result, the action depends on
    the operator.  "And_op" and "or_op" are left as is.  "Andif_op" and "orif_op"
    nodes are expanded into a special form which has the properties of ordering
    the evaluation, and causing generation of a boolean value:

      A andif B    ->        jumpf     A,1
			     jumpf     B,1
			     gen_jump  1
			  1: gen_andif A,B

    The gen_jump operator is interpreted as meaning "load one and jump a location
    following the gen_andif operator";  the gen_andif operator, "load zero and
    continue".  "Orif" is processed in a similar way, but with the jump conditions
    and truth values inverted.  The "gen_" prefix operators differ from the normal
    operators in that they do not constitute a use of their operands; this keeps
    the usage count information accurate.  Their operands are kept for basically
    documentary purposes; and since any boolean operands will be replaced with
    jump logic, they are changed to nil here.  *)

procedure explode_booleans;

var node: tuple;			(* the scanning cursor *)
    lab: tuple;			(* a generated label *)
    anchor: tuple;		(* node following next node on subsequent iterations *)

type
    op_set = set of or_op .. bnot_op;

const
    bool_ops: op_set = [or_op, and_op, orif_op, andif_op, bnot_op];

begin
  node := t_chain^.upward_thread;	(* get end of tuple list *)

  while node <> nil do begin	(* make a backwards scan over the IF *)
    with node^ do begin
      anchor := node;
      case opcode of

	jump_t_op, jump_f_op:
	  begin
	    if cond^.opcode in bool_ops then begin	(* explode and, or, not *)
	     lab := get_following_label (node);		(* get alternative location *)
	     if opcode = jump_t_op
	       then boolean_jump (cond, jump_to, lab, false)
	       else boolean_jump (cond, lab, jump_to, true);
	     anchor := node^.next;	(* to keep our place as node is deleted *)
	     remove_jump (node);	(* delete the original jump *)
	     dechain (node);
	    end;	(* boolean jump deletes extraneous op's *)
	  end;

	andif_op:
	  begin
	    opcode := gen_andif_op;			(* tag with special opcode to merge true/false values *)
	    lab := get_following_label (prev);	(* insert label before gen_andif_op *)
	    append_jump (operand[2], gen_jump_op, nil, lab);	(* these two are in reverse order of appearance in if *)
	    append_jump (operand[2], jump_f_op, operand[2], lab);
	    append_jump (operand[1], jump_f_op, operand[1], lab);	(* short-circuit test *)
	    operand[1] := nil;
	    operand[2] := nil;
	  end;	(* subsequent passes will explode operands *)

	orif_op:
	  begin
	    opcode := gen_orif_op;			(* tag with special opcode to merge true/false values *)
	    lab := get_following_label (prev);	(* insert label before gen_orif_op *)
	    append_jump (operand[2], gen_jump_op, nil, lab);	(* these two are in reverse order of appearance in if *)
	    append_jump (operand[2], jump_t_op, operand[2], lab);
	    append_jump (operand[1], jump_t_op, operand[1], lab);	(* short-circuit test *)
	    operand[1] := nil;
	    operand[2] := nil;
	  end

      end (* case *) ;
    end (* with node^ *) ;
    node := anchor^.prev;
  end (* while node <> nil *) ;
end (* explode_booleans *);
$PAGE complete_labelling
(*  COMPLETE LABELLING insures that the tuple list for a program is completely
    labeled.   Specifically:  (1) Every basic block begins with a label node.
    (2) Every basic block ends with some form of terminating operator.  (3) Unused
    nodes have been removed, although there may be unexecutable basic blocks.
    (4) All labels have been threaded together by their downward threads.  (5) The
    upward thread from the StartBlock tuple points to the EndBlock tuple, and the
    downward thread from the EndBlock tuple points to the StartBlock tuple.
    (6) The pseudo basic block that starts with the StartBlock tuple ends with
    a list of JumpInOps to any LabelNodes in the routine which are the targets
    of nonlocal goto's, followed by a JumpInOp to the first LabelNode in the
    routine.  These JumpInOps will be removed when we have finished with the
    flow graph analysis, but for now they serve to indicate those basic blocks
    which can be reached abnormally -- that is, other than by a normal branch
    from the end of same basic block in the same routine.  *)


procedure complete_labelling;

 var
   cur_bb: tuple;			(* in scan, label node starting basic block;
					   nil'ed when block terminated. *)
   last_lab: tuple;		(* label node starting preceding basic block;
				   lasts past end of that block *)
   node: tuple;			(* current node being scanned *)
   target: tuple;
   jump_opcode: tuple_opcodes;
   icase: int_type;
   jmp: tuple;

 begin

  (* A label is needed at the start of the program.  If there is not one there
     already, create one. *)

  t_chain^.downward_thread := get_following_label (t_chain); (* create if not found *)

  (* Scan the list of operators, completing the labeling as described above. *)

  cur_bb := nil;				(* no label seen yet *)
  last_lab := t_chain;
  node := t_chain^.next;

  while node <> nil do begin
     case node^.opcode of

       (*  Label: this is the start of a basic block.  It is necessary to insure
	  that the label is preceded by a jump (i.e., that the preceding block
	  has been terminated).  In addition, if a label node is preceded by a
	  start statement node, we exchange them, so that the start statement
	  node follows the label.  If the label can be the target of a non-
	  local goto, then a JumpInOp to it is inserted following the Start-
	  Block, to indicate that it can be reached without a branch from any
	  of the other basic blocks in the routine.  *)

      label_node:
	begin
	  while node^.prev^.opcode = start_stmt do begin
	    with node^ do begin
	      prev^.next := next;
	      next^.prev := prev;
	      next := prev;
	      prev := prev^.prev;
	      next^.prev := node;
	      prev^.next := node;
	    end;
	  end;
	  if cur_bb <> nil then	(* last block has no jump *)
	    append_jump (node^.prev, jump_op, nil, node);
	  last_lab^.downward_thread := node;
	  if (not quick) andif
	     ( (last_lab = t_chain) or
	       ( (node^.label_sym <> nil) andif node^.label_sym^.lab_nonlocal_use ) ) then
	    append_jump (t_chain^.downward_thread^.prev, jump_in_op, nil, node);
	  cur_bb := node;			(* remember node starting basic block *)
	  last_lab := node;
	end;

      (*  Unconditional jumps and routine exits terminate their basic blocks.
	  Code between such an operator and the next label is inaccessible,
	  and may be deleted.  Note that local goto's are translated into
	  JumpOp's, and nonlocal goto's are translated into GotoOp's.  *)

      jump_op, gen_jump_op, stop_op, return_op, goto_op:
	begin
	  cur_bb := nil;
	  delete_unused_operators (node^.next);		(* following can never be executed *)
	end;

      (*  A case jump is always followed by a list of JumpInOp's.  However, if
	  the case selector is a constant, then the entire case selection list
	  can be reduced to a single unconditional jump.  *)

      case_jump_op:
	begin
	  cur_bb := nil;
	  if constp (node^.cond) then begin (* can we select a branch now? *)
	    icase := node^.cond^.cst_val.ival;
	    jmp := node^.next; (* find the selected branch *)
	    while (jmp^.opcode = jump_in_op) andif
		  ((icase < jmp^.low_cntrl) or (icase > jmp^.high_cntrl)) do
	      jmp := jmp^.next;
	    if jmp^.opcode = jump_in_op then (* choose selected destination *)
	      node^.jump_to := jmp^.jump_to;
	    node^.opcode := jump_op; (* make the branch unconditional *)
	    node^.cond := nil;
	  end
	  else begin (* find the end of the case list *)
	    while node^.next^.opcode = jump_in_op do
	      node := node^.next;
	  end;
	  delete_unused_operators (node^.next);
	end;

      (*  Conditional jumps must come in pairs to indicate the alternative ways
	  of exiting the basic block (jump condition true of false).  Semantic
	  analysis only creates the first one; the second is added here.  If
	  the jump condition is a constant, the conditional jump pair can be
	  reduced to a single unconditional jump.  *)

      jump_t_op, jump_f_op:
	begin
	  cur_bb := nil;
          if node^.opcode = jump_t_op		(* get reverse operator *)
	    then jump_opcode := jump_f_op
	    else jump_opcode := jump_t_op;
	  if node^.next^.opcode <> jump_opcode then begin	(* not paired, create extra jump *)
	    target := get_following_label (node);	(* must know where to jump to *)
	    append_jump (node, jump_opcode, node^.cond, target);
	  end;
	  if constp (node^.cond) then begin (* can test be resolved now? *)
	    if (node^.opcode = jump_t_op) <> (node^.cond^.cst_val.ival = 1) then
	      node^.jump_to := node^.next^.jump_to; (* take false branch *)
	    node^.opcode := jump_op; (* make the branch unconditional *)
	    node^.cond := nil;
	  end
	  else
	    node := node^.next;			(* skip the paired jump *)
	  delete_unused_operators (node^.next);
	end;

      (*  The EndBlock of the routine must be tied to the StartBlock,
	  and to the last LabelNode.  *)

      end_block:
	begin
	  last_lab^.downward_thread := node;
	  node^.downward_thread := t_chain;
	  t_chain^.upward_thread := node;
	end;

      others:
	(* no action *)

    end (* case node^.opcode *) ;
    node := node^.next;				(* iteration step *)
  end (* while node <> nil *);
end (* complete_labelling *);
$PAGE complete_graph
(*  COMPLETE GRAPH creates the block edge linkages.  When called, it is assumed
    that all label nodes are linked together, that the outward jump lists are
    complete, and that the targets of jumps are filled in.  *)

procedure complete_graph;

var lab, jmp: tuple;

begin

  (*  Walk the list of labels and nil the inward_jump list.  This is to enable
      use of this routine on a previously labeled graph.  *)

  lab := t_chain;
  repeat
    lab^.inward_jumps := nil;
    lab := lab^.downward_thread;
  until lab = t_chain;

  (*  The steps required to complete the graph are to append each jump to the
      inward jump list of the target label, and to record the basic block
      (i.e. label) from which it exits.  *)

  lab := t_chain;		(* for each basic block ... *)
  repeat
    jmp := lab^.downward_thread^.prev;		(* for each jump exiting the block *)
    while (first_jump_op <= jmp^.opcode) and (jmp^.opcode <= last_jump_op) do begin
      with jmp^ do begin
	jump_from := lab;		(* record block exited *)
	next_inward_jump := jump_to^.inward_jumps;	(* append to chain *)
	jump_to^.inward_jumps := jmp;
	jmp := prev;
      end;
    end (* while *) ;
    lab := lab^.downward_thread;
  until lab = t_chain;
end (* complete_graph *);
$PAGE transfer_jumps
(*  TRANSFER JUMPS transfers jumps targeting on a certain label ("lab") to some 
    other label ("new_lab").  This is used by simplify_jumps to handle jumps to
    jumps.  *)

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
    end (* with jmp^ *);
  end (* while jmp <> nil *);
end (* transfer_jumps *);
$PAGE transfer_exit
(*  TRANSFER EXIT finds all unconditional jumps to the sped label "lab",
    and replaces them with copies of a specified exit tuple (stop, return, or
    goto).  *)

procedure transfer_exit (lab: tuple; exit_tuple: tuple);

var jmp, lastjmp, nextjmp, new_exit: tuple;

begin
  jmp := lab^.inward_jumps; (* walk list of all jumps *)
  lastjmp := nil;
  while jmp <> nil do begin
    nextjmp := jmp^.next_inward_jump;
    if jmp^.opcode = jump_op then begin (* replace by exit *)
      if lastjmp = nil
	then lab^.inward_jumps := nextjmp
	else lastjmp^.next_inward_jump := nextjmp;
      if exit_tuple^.opcode = stop_op then (* create new exit tuple *)
	new (new_exit, stop_op)
      else if exit_tuple^.opcode = return_op then
	new (new_exit, return_op)
      else (* exit_tuple^.opcode = goto_op *) begin
	new (new_exit, goto_op);
	new_exit^.target_lab := exit_tuple^.target_lab;
	new_exit^.target_frame := exit_tuple^.target_frame;
      end;
      ap_chain (jmp, new_exit); (* insert exit *)
      dechain (jmp); (* remove jump *)
    end
    else
      lastjmp := jmp;
    jmp := nextjmp;
  end (* while jmp <> nil *);
end (* transfer_exit *);
$PAGE simplify_graph
(*  SIMPLIFY GRAPH removes jumps to jumps by setting the initial jump to target
    on the final label, and replaces unconditional jumps to exit tuples (stop,
    return, and goto ops) by the exit tuples themselves.  Basic blocks which
    become inaccessible as a result of this process will be deleted later by
    OrderGraph.  *)

procedure simplify_graph;

var lab, jmp: tuple;

begin
  lab := t_chain^.downward_thread;
  while lab^.opcode <> end_block do begin
    if (lab^.label_sym = nil) orif (not lab^.label_sym^.lab_nonlocal_use) then begin
      if prog_options.debug_opt
	then jmp := lab^.next (* don't discard stmt marks in debug mode *)
	else jmp := next_action (lab); (* ignore stmt marks normally *)

      if (jmp^.opcode = jump_op) andif (jmp^.jump_to <> lab) then
	transfer_jumps (lab, jmp^.jump_to)	(* make incoming jumps point at outward label *)
      else if (jmp^.opcode = stop_op) or (jmp^.opcode = return_op) or
	      (jmp^.opcode = goto_op) then
	transfer_exit (lab, jmp); (* transform unconditional jumps *)
    end;
    lab := lab^.downward_thread;
  end (* while lab^.opcode <> end_block *);
end (* simplify_graph *);
$PAGE order_graph
(*  ORDER GRAPH rearranges the basic blocks in a program according to a depth-first
    ordering on the flow graph.  At its conclusion, the physical order of the
    basic blocks, as well as the upward and downward threads linking the label
    nodes, will correspond to the DFST ordering of the flow graph.  Any basic
    blocks which are not accessible from the start block will be deleted.  *)

procedure order_graph;


  (*  SEARCH recursively walks the spanning tree of labels (i.e. basic blocks)
      and performs the depth-first ordering by building the threads.  The block
      number is used as a marker, and on initial entry all are assumed to be
      zero.

      Search is called initially with the StartBlock.  This has jumps to the
      first basic block and to each basic block which can be reached by a
      nonlocal goto, so all basic blocks which are de facto accessible will
      automatically be processed at the first level of recursion.  The first
      basic block will be processed first, and thus will become block 1.  *)

 var last_visited: tuple;		(* label last processed as we back out *)

  procedure search (lab: tuple);

  var jmp: tuple;

  begin
    with lab^ do begin
      block_order_no := 1;		(* flag this node as visited *)

      jmp := downward_thread^.prev;
      while (first_jump_op <= jmp^.opcode) and (jmp^.opcode <= last_jump_op) do begin
	if jmp^.jump_to^.block_order_no = 0	(* if unvisited *)
	  then search (jmp^.jump_to);
	jmp := jmp^.prev;
      end;

      last_visited^.upward_thread := lab;
      last_visited := lab;
    end;
  end (* search *);
$PAGE
var lab, nextlab: tuple;

begin
  lab := t_chain;
  repeat (* mark all labels as unvisited *)
    lab^.block_order_no := 0;
    lab := lab^.downward_thread;
  until lab = t_chain;

  (* Process the tree, ordering all nodes which may be reached from the start
      node. *)

  last_visited := t_chain^.upward_thread;	(* end_block is very last node *)
  search (t_chain);

  (*  Any unmarked blocks can not be reached during execution, so they may be
      deleted from the I/F.  Set the DownwardThread of each remaining label
      node to the last tuple in its basic block.  *)

  lab := t_chain;
  while lab^.opcode <> end_block do begin
    with lab^ do begin
      nextlab := downward_thread;
      if block_order_no = 0 then (* never referenced *)
	delete_basic_block (lab)
      else
	downward_thread := downward_thread^.prev; (* last tuple in basic block *)
    end;
    lab := nextlab;
  end;

  (*  Finally, rearrange the basic blocks so that their physical (next/prev)
      order reflects their DFST (upward_thread) order, and make the downward
      threads symmetric to the upward threads.  *)

  while lab <> t_chain do begin
    nextlab := lab^.upward_thread;
    lab^.prev := nextlab^.downward_thread;
    nextlab^.downward_thread^.next := lab;
    nextlab^.downward_thread := lab;
    lab := nextlab;
  end;
end (* order_graph *);
$PAGE insert_preheaders
(*  INSERT PREHEADERS inserts a preheader node before each loop header node in
    the flow graph.  A loop header is a node which is the target of a back edge
    (i.e., an edge from a node which follows it in the ordering).  A preheader
    is a node which immediately precedes a loop header in the ordering, and is
    the only predecessor of the loop header which precedes it in the ordering.

    When this routine is finished, the edge from a preheader to a loop header
    will be the last inward jump to the header.  Therefore, a node will be a
    loop header if and only if its first inward jump is a back edge.  *)

procedure insert_preheaders;

var lab, next_lab: tuple;		(* for scanning the input *)
    looplab: tuple;			(* after splitting the header, node which receives
					   the backward edges. *)
    jmp, njmp, ljmp: tuple;			(* for scanning inward jump lists *)
    ojmp: tuple; (* for scanning outward jump lists *)

begin
  lab := t_chain^.downward_thread;		(* examine each label node *)
  while lab^.opcode <> end_block do begin
    next_lab := lab^.downward_thread;

    (*  Transfer all back edges entering this basic block to a newly created
	label node ("looplab").  Forward edges will remain targeted on "lab".
	Insert an unconditional jump between the nodes.  *)

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
	    label_sym := lab^.label_sym;
	    lab^.label_sym := nil;
	    nodeid := 0;
	    block_order_no := lab^.block_order_no; (* treat like lab, for now *)
	    downward_thread := lab^.downward_thread;	(* splice into label list *)
	    downward_thread^.upward_thread := looplab;
	    lab^.downward_thread := looplab;
	    upward_thread := lab;
	    append_jump (lab, jump_op, nil, looplab);	(* insert a jump between lab and looplab *)
	    lab^.next^.jump_from := lab;
	    inward_jumps := lab^.next;
	    ojmp := downward_thread^.prev; (* change jump_from of outward jumps *)
	    while (first_jump_op <= ojmp^.opcode) and (ojmp^.opcode <= last_jump_op) do begin
	      ojmp^.jump_from := looplab;
	      ojmp := ojmp^.prev;
	    end;
	  end (* with looplab^ *);
	end;

	if ljmp = nil			(* transfer jmp from lab to looplab *)
	  then lab^.inward_jumps := njmp
	  else ljmp^.next_inward_jump := njmp;
	jmp^.next_inward_jump := looplab^.inward_jumps;
	looplab^.inward_jumps := jmp;
	jmp^.jump_to := looplab;
      end (* if back edge *);

      jmp := njmp;
    end;

    lab := next_lab;			(* examine next label, skip the one inserted *)
  end (* while not end_block *);
end (* insert_preheaders *);
$PAGE fuse_blocks
(*  When a basic block is the sole successor of its sole predecessor, FUSE BLOCKS
    will merge the two basic blocks into one by removing the jump from the end of
    the predecessor and the label from the start of the successor.  *)

procedure fuse_blocks;

var lab, nextlab, jmp: tuple;

begin
  lab := t_chain^.downward_thread^.downward_thread;
  while lab^.opcode <> end_block do begin
    with lab^ do begin
      nextlab := downward_thread;
      jmp := prev;
      if (jmp^.opcode = jump_op) andif (jmp^.jump_to = lab) andif
	 (inward_jumps = jmp) andif (jmp^.next_inward_jump = nil) then begin
	downward_thread^.upward_thread := upward_thread;
	upward_thread^.downward_thread := downward_thread;
	dechain (jmp);
	jmp := lab^.downward_thread^.prev;
	while (first_jump_op <= jmp^.opcode) and (jmp^.opcode <= last_jump_op) do begin
	  jmp^.jump_from := nextlab;
	  jmp := jmp^.prev;
	end;
	dechain (lab);
      end;
    end (* with lab^ *);
    lab := nextlab;
  end (* while lab^.opcode <> end_block *);
end (* fuse_blocks *);
$PAGE make_dominator_tree
(*  MAKE DOMINATOR TREE computes the dominator tree of the program flow graph,
    using the algorithm of Aho and Ullman, Principles of Compiler Design,
    pp 445-446.  The function of the working set NEWD in the algorithm is
    filled by set D(0).  All the dominator tree information is returned in
    three label node fields:

	Idom(i)       is the block which immediately dominates block i.
	DomSon(i)     is the first block on the chain of blocks which
		      are dominated by block i.
	DomBrother(i) links together the chain of blocks which are dominated
		      by Idom(i).

    The StartBlock will be the immediate dominator of the first basic block,
    and of each basic block which is the target of a nonlocal goto.  *)


procedure make_dominator_tree;

var
    d: svector; (* The dominator sets. *)
    n_blocks, i: index_range;
    blk_i, blk_j, b_pred: tuple;
    change: boolean;

begin

  (*  Create the dominator sets, according to the Aho-Ullman algorithm.  *)

  n_blocks := t_chain^.upward_thread^.block_order_no - 1;
  d := new_svector (n_blocks, n_blocks);
  for i := 1 to n_blocks do
    unv_set (d, i);

  repeat
    change := false; (* Set if any changes. *)
    blk_i := t_chain^.downward_thread;
    while blk_i^.opcode <> end_block do begin
      b_pred := blk_i^.inward_jumps; (* The first predecessor of block i. *)
      unv_set (d, 0);
      while b_pred <> nil do begin
	with b_pred^ do begin
      exit if jump_from = t_chain  do clr_set (d, 0);
	  intersect (d, jump_from^.block_order_no, 0);
	  b_pred := next_inward_jump;
	end;
      end;
      i := blk_i^.block_order_no;
      add_elem (d, 0, i); (* Every node dominates itself. *)
      add_elem (d, 0, 0); (* Node 0 dominates every node. *)
      if not set_eq (d, 0, i) then begin
	change := true; (* Another iteration is necessary. *)
	cpy_set (d, 0, i);
      end;
      blk_i := blk_i^.downward_thread;
    end (* while not end_block *);
  until not change;

  (*  Now use the dominator set information to build the dominator tree.  The
      key fact is that the immediate dominator of a node is the node in its
      dominator set with the largest block order number, since no node can
      dominate a node which precedes it in the depth-first ordering.  *)

  blk_i := t_chain;
  while blk_i^.opcode <> end_block do begin
    blk_i^.dom_son := nil;
    blk_i := blk_i^.downward_thread;
  end;

  (*  By processing the basic blocks in reverse order in the following loop,
      we guarantee that the children of a node in the dominator tree will
      be ordered by DFST number.  (I.e., for all nodes n, DomBrother(n) > n.)  *)

  blk_i := t_chain^.upward_thread^.upward_thread;
  while blk_i <> t_chain do begin
    i := blk_i^.block_order_no;
    blk_j := blk_i^.upward_thread; (* Find the largest dominator. *)
    while not in_set (d, i, blk_j^.block_order_no) do
      blk_j := blk_j^.upward_thread;
    blk_i^.idom := blk_j;
    blk_i^.dom_brother := blk_j^.dom_son;
    blk_j^.dom_son := blk_i;
    blk_i := blk_i^.upward_thread;
  end;

  del_svector (d); (* We no longer need the set information. *)
end (* make_dominator_tree *);
$PAGE remove_dummy_jumps
(*  REMOVE DUMMY JUMPS will remove the JumpInOps from the StartBlock, since
    these have served their purpose for OrderGraph and MakeDominatorTree.  *)

procedure remove_dummy_jumps;

var lab: tuple;

begin
  lab := t_chain^.downward_thread;
  while lab^.prev^.opcode = jump_in_op do begin
    remove_jump (lab^.prev);
    dechain (lab^.prev);
  end;
end (* remove_dummy_jumps *);
$PAGE make_basic_blocks
(*  MAKE BASIC BLOCKS processes the tuple list output from the first pass (that is,
    the labelling is assumed to be incomplete), and builds a complete, simplified,
    and ordered basic block graph.  *)

public procedure make_basic_blocks;

 begin
  explode_booleans;
  complete_labelling;
  if quick then
    number_the_blocks
  else begin
    complete_graph;
    simplify_graph;
    order_graph;
    number_the_blocks;
    insert_preheaders;
    fuse_blocks;
    number_the_blocks;
    make_dominator_tree;
    remove_dummy_jumps;
  end;
  reclaim;
end (* make_basic_blocks *).
  e F