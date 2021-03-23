$options special, nocheck
module peephole;
$TITLE Peephole optimization pass
$header p10phl.hdr
$PAGE includes
$PAGE declarations

type
  ref = ^reference;	(* links jrsts/jumps to refmarks *)

  reference = packed record
    next_ref, prev_ref: ref;	(* doubly linked list *)
    inst_ref: code;		(* instruction making reference *)
    mark_ref: code		(* "defmark" referenced *)
  end;

const
  popj_return: pdp117bword := (popj, 0, false, 0, 0);
  jrst_return: pdp10word := (jrst, 0, false, 0, 0);
  init_defn: definition := (
	nil, 0, 0, false, true, false, local_def, nil, local_def, 0, 0);
  init_defmark: code_record := (
	nil, defmark, nil, nil);
  minimal_stack := 3;	(* for determination of possible quick block *)
  pass_defmarks := false;	(* in scan over code *)
  stop_on_defmarks := true;

$PAGE peep_hole
(* PEEP HOLE performs various peephole optimizations on the body of a
   routine.  The "codelist" parameter is a chained list of code_records
   for the routine.  Peepholing may be suppressed by specifying a NOPEEPHOLE
   dump switch, and an assembly listing of both before and after peepholing
   may be obtained by specifying the BOTH dump switch.  Temporarily,
   the LISTALL dump switch will produce assembly listings of each pass
   over the code which eliminates instructions.

   The peepholing pass occurs in three stages:

     (1) Initialization of pointers in the code list which are unused
	 elsewhere in the compiler, as well as identification of all
	 "defmark"s or branch targets. Procedure returns (either via
	 POPJ or through PRTRN) are replaced by references to a special
	 return point to simplify cross_jumping.

     (2) Optimization of the code stream. This may involve simplification,
	 combination, rearrangement, and elimination of instructions.
	 This scan is repeated until no code reductions are obtained.

     (3) Cleanup, particularly replacement of return references by
	 POPJ or PRTRN, and clearing fields used in definition records.

*)

public procedure peep_hole (var codelist: code_list);

var
  recycled_defns: def;	(* deleted defmarks which may be reused *)
  finished: boolean;	(* terminates scan *)
  can_be_quick: boolean;	(* true if routine can be made quick (no temps on stack) *)
  quick: boolean;	(* true if this is a quick routine *)
  main_program: boolean;	(* if this is the body of the main routine *)
  stack_size: int_type;	(* from adjsp on entry *)
  detail: boolean;	(* set true if assembly listing of each pass requested *)
  ret_def: code;		(* substitute return def *)
  ret_mark: code_record;	(* initialized as a defmark for returns *)
  ret_defn: definition;	(* phony definition for returns *)


$PAGE peep_init;
(* initializes pointers used by the peephole pass *)
procedure peep_init;
begin
  quick := true;	(* until known otherwise *)
  can_be_quick := true;	(* set false if temps allocated by routine *)
  ret_mark := init_defmark;
  ret_defn := init_defn;
  ret_def := address (ret_defn);
  recycled_defns := nil;
end;
$PAGE delete_code, insert_code
(* DELETE CODE deletes a code record from the code list, disposing the
   node after removal. *)

procedure delete_code (cr: code);
begin
  if codelist.first = cr then (* removing first instuction of body *)
    codelist.first := cr^.next;
  if codelist.last = cr then (* removing last instruction of body *)
    codelist.last := cr^.prev;
  if cr^.prev <> nil then (* just in case *)
    cr^.prev^.next := cr^.next;
  if cr^.next <> nil then
    cr^.next^.prev := cr^.prev;
  dispose (cr);
end (* delete_code *);

procedure insert_code (new_code, next_code: code);
(* INSERT_CODE inserts NEW_CODE in the code chain before NEXT CODE *)

begin
  new_code^.next := next_code;
  new_code^.prev := next_code^.prev;
  if next_code^.prev <> nil then
    next_code^.prev^.next := new_code;
  next_code^.prev := new_code;
  if codelist.first = next_code then	(* insert at head of list *)
    codelist.first := new_code;
end (* insert_code *);
$PAGE free_defn
(* FREE DEFN frees an unused definition record and its associated defmark.
   the definition record is kept on a list for reuse, if necessary. 
   The definition MUST be a local_def, never a label_def. *)

procedure free_defn (dfmark: code);
begin
  dfmark^.defname.rbacklink := ord (recycled_defns);	(* place on free chain *)
  recycled_defns := dfmark^.defname;
  delete_code (dfmark);
end;
$PAGE prev_inst, follow_inst
(* PREV INST and FOLLOW INST scan backward or forward in the code list
   for an adjacent instruction, terminating the scan at the end of the
   list, a location reset, etc.  If the DEF_STOP parameter is false
   then defmark records will be ignored.	*)

function prev_inst (
	cr: code;	(* starting point for the scan *)
	var adjacent_int: code;	(* if found *)
	def_stop: boolean	(* if true, stop at defmarks *)
	  ): boolean;		(* true only if instruction found *)

begin
  adjacent_inst := cr;
  prev_inst := false;
  while adjacent_inst <> code_list.first do begin
    adjacent_inst := adjacent_inst^.prev;
    case adjacent_inst^.kind of
      instruction: begin
	prev_inst := true;	(* found one *)
	return;			(* <--- short circuit *)
      end;
      defmark:
	if def_stop then return;	(* short circut *)
      origion:
	return			(* never back over this *)
    end;
  end;
end (* prev_inst *);
$PAGE remove_ref, dispose_ref
(* REMOVE REF removes a ref from its ref_chain. DISPOSE REF disposes the
   node after removal. *)

procedure remove_ref (r: ref);
begin
  with r^ do begin
    if next_ref = nil
      then ref_mark^.ref_chain := prev_ref
      else next_ref^.prev_ref := prev_ref;
    if prev_ref <> nil then prev_ref^.next_ref := next_ref;
  end;
end (* remove_ref *);

procedure dispose_ref (r: ref);
begin
  remove_ref (r);
  dispose (r);
end;
$PAGE add_ref, create_ref, transfer_ref
(* ADD REF adds an existing ref node to a reference chain.
   CREATE REF generates a new ref node prior to adding it to a ref_chain. 
   TRANSFER REF removes an instruction's present reference from its ref_chain
   prior to adding it to another.	*)

procedure add_ref (
	r: ref;	(* the ref to be added *)
	cr: code;	(* the instruction making the reference *)
	dfmark: code);	(* the defmark referenced *)

begin
  with r^ do begin
    mark_ref := dfmark;	(* link to the defmark node *)
    inst_ref := cr;	(* and to the instruction *)
    next_ref := nil;	(* insert in chain *)
    prev_ref := dfmark^.ref_chain;
  end;
  cr^.reflink := r;	(* complete link to instruction *)
  dfmark^.ref_chain := r;	(* and to defmark *)
end;

procedure create_ref (cr: code; dfmark: code);
var r: ref;
begin
  new (r);
  add_ref (r, cr, dfmark);
end;

procedure transfer_ref (cr: code; target_defmark: code);
begin
  remove_ref (cr^.ref_link);
  add_ref (cr^.ref_link, cr, target_defmark);
end;
$PAGE generate_ref
(* GENERATE REF transfers the reference of an instruction from its
  present point to another. If the target code is preceded
  by a defmark the reference is linked to it, otherwise a defmark is
  inserted before the specified target, recycling an existing 
  definition record if one is available.	*)

procedure generate_ref (from: code; target: code);

var
  dfmark: code;	(* in search back from target *)
  new_defn: def;

begin
  if not prev_defmark (target, dfmark) then begin	(* no prior defmark *)
    if recycled_defs = nil then begin
      new_defn := make_def (local_def);
    end
    else begin	(* reuse a previously freed definition record *)
      new_defn := recycled_defns;
      recycled_defs := ptr (recycled_defs^.rbacklink);	(* remove from free list *)
    end;
    new (dfmark);
    dfmark^ := init_defmark;
    insert_code (dfmark, target);
    new_defn^.rbacklink := ord (dfmark);
  end;
  transfer_ref (from, dfmark);
end (* generate_ref *);
$PAGE merge_refchains (ref1, ref2: code);
(* MERGE REFCHAINS joins to reference chains, placing all ref1 references
   on the ref2 chain.  The ref1 node is deleted. *)

procedure merge_refchains (ref1, ref2: code);
var r: ref;
begin
  r := ref1^.ref_chain;
  if r <> nil then begin	(* nothing to merge otherwise *)
    loop
      r^.mark_ref := ref2;	(* link ref to new markdef *)
      r^.inst_ref^.reloc.reldef := ref2^.defname;	(* link instruction to new relocation *)
    exit if r^.prev_ref = nil;	(* stop on last of list *)
      r := r^.prev_ref;
    end;

    (* now r is last entry on ref1 chain *)

    if ref2^.ref_chain = nil then
      ref2^.ref_chain := ref1^.ref_chain	(* just replace by first chain *)
    else begin	(* merge *)
      r^.prev_ref := ref2^.ref_chain;
      ref2^.ref_chain^.next_ref := r;
      ref2^.ref_chain := r;
    end;
  end;
  delete_code (ref1);
end;
$PAGE init_code;
(* INIT CODE makes two passes over the code list, the first initializing
  required fields and the second constructing reference chains. INIT CODE
  also determines if the routine is or can be quick.	*)


procedure init_code;
var
  cr: code;
  prevcode: code;	(* for initialization of double linked code list *)
  r: ref;
  firstpass: boolean;

begin
  prevcode := nil;
  for firstpass := true downto false do begin
    cr := codelist.first;
    while cr <> nil do with cr^ do begin
      if firstpass then begin
	prev := prevcode;
	prevcode := cr;
	if kind = defmark then begin	(* init the definition record *)
	  ref_chain := nil;	(* no references yet *)
	  if defname^.deftype = label_def then begin
	    new (dfmark: defmark);
	    dfmark^.ref_chain := nil;
	    dfmark^.defname := make_def (local_def);
	    dfmark^.defname^.rbacklink := ord (cr);
	    insert_code (dfmark, cr);	(* insert directly before the label_def *)
	  end
	  else if defname^.deftype = local_def
	    then defname^.rbacklink := ord (cr);	(* link definition to defmark *)
	end;
	reflink := nil;
      end
      else if kind = instruction then begin
	if inst.opcode = popj then begin	(* found a quick return *)
	  inst := jrst_return;	(* replace with jrst to return label *)
	  create_ref (cr, ret_def);	(* link to fake return def *)
	end
	else if inst.opcode = jsp then begin	(* check for INITP or PNTRY call *)
	  if (reloc.kind = runtime_sc) then
	    if reloc.relrtsym = rt_entry then begin
	      quick := false;
	      main_program := true;
	    end
	    else if reloc.relrtsym = rt_entry then quick := false;
	end
	else if (reloc.kind = runtime_sc) and (reloc.relrtsym = rt_return) then begin
	  create_ref (cr, ret_defn);
	end
	else if reloc.kind = def_sc then begin
	  (* a jrst, jump, or case vector *)
	  if reloc.rel_def^.deftype = label_def then	(* replace by ref to preceeding label *)
	    reloc.reldef := prev^.reloc.reldef;
	  if reloc.reldef^.deftype = local_def then
	    create_ref (cr, ptr (reloc.reldef^.rbacklink));
	end
	else if inst.opcode = adjsp then begin
	  if inst.index <> 0 then
	    can_be_quick := false	(* allocates temps *)
	  else begin
	    if reloc.kind = def_sc then
	      stack_size := reloc.reldef^.addr;	(* size of stack frame *)
	  end
	end
      end (* if not firstpass *);
    exit if cr = codelist.last;
      cr := next;
    end (* while, with *);
  end (* two passes *);
end (* init_code *);
$PAGE skips
(* SKIPS returns a Boolean true if the specified opcode skips under
   any conditions. *)

function skips (opc: opc_range): boolean;
var op: int_type;
begin
  skips := false;
  op := opc div 10b;
  if op in [30b (* CAI *), 31b (* CAM *), 33b (* SKIP *), 35b (* AOS *), 37b (* SOS *)] then begin
    if opc mod 10b <> 0 then skips := true
  end
  else if op in [60b..67b] (* tests *) then
    if opc mod 10b >= 2 then skips := true
end;
$PAGE reverse 
(* REVERSE reverses the condition of a skip instruction *)

function reverse (opc: opc_range): opc_range;

begin
  if opc mod 10b < 4
    then reverse := opc + 4
    else reverse := opc - 4
end;
$PAGE peep_hole - body

begin
  if prog_options.debug_opt then return;	(* not in debug compilations *)
  peep_init;	(* initialize pointers, etc. *)
  init_code;	(* set up reference chains *)
  detail := dump_switch ('LISTALL');
  if detail orif dump_switch ('BOTH') then
    mac_list (codelist);
  repeat
    finished := true;	(* set false whenever the codelist changed *)
    scan_code;
    scan_jumps;
    if detail and not finished
      then mac_list (codelist);
  until finished;
  if not main_program and not quick and can_be_quick and (stack_size = minimal_stack) then
    make_quick;
  cleanup;
end.
  