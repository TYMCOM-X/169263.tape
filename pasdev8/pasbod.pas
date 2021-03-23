$TITLE pasbod - semanticate the body of a block
$LENGTH 43

module pasbod;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S B O D                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  PASCAL Compiler
     
     STARTED:  8/22/78
     
     PURPOSE:  This is the module  which  performs  semantication  of
        procedure and program bodies.
     
     ENTRY POINTS:
     
        body        is  the  procedure which semanticates a procedure
                    or program body (that is, the compound  statement
                    which  contains  the  executable statements).  It
                    contains routines  to  process  all  the  various
                    sorts  of  statements which can occur in a Pascal
                    program.  Semantication of  data  references  and
                    values  is  handled  by  the expression semantics
                    modules:  PASESM, PASESU, PASREF, and PASSFC.
     
     RESPONSIBLE:  Compiler Group
     
     ---------------------------------------------------------------- *)
$PAGE includes

$OPTIONS special

$INCLUDE pascal.inc
$INCLUDE ptmcon.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE paserr.inc
$INCLUDE ptmtal.inc
$INCLUDE pasanl.inc
$INCLUDE pasifu.inc
$INCLUDE pa1xrf.inc
$INCLUDE pasglb.inc
$INCLUDE pasref.inc
$INCLUDE pasesm.inc
$INCLUDE pasesu.inc
$INCLUDE pasemu.inc
$INCLUDE pascmp.inc
$INCLUDE pasutl.inc
$INCLUDE pasblk.inc
$INCLUDE pascgr.inc
$PAGE body

public procedure body;
$PAGE stmt_mark

(* STMT MARK emits a start-statement operator for the start of the statement
   indicated by the current parse tree.  "End" and "until" are also marked.
   "Last_stmt_node" is always the last statement mark emitted. *)


var last_stmt_node: tuple;


procedure stmt_mark;
 var t: tuple;
 begin
  new (t, start_stmt);
  with t^ do begin
    stmt_kind := ptree^.sym;
    stmt_source := ptree^.source;
    if (last_stmt_node = nil) orif (last_stmt_node^.stmt_source <> stmt_source)
      then stmt_index := 1
      else stmt_index := last_stmt_node^.stmt_index + 1;
  end;
  emit (t);
  last_stmt_node := t;
 end;
$PAGE make_temp
(* MAKE TEMP creates a compiler temporary of a specified type.  It constructs
   and returns an IF tuple node referencing the temporary. *)

function make_temp ( temp_type: typ ): expr;
 var tsym: sym; texpr: expr;
 begin
  tsym := new_sym (vars);
  with tsym^ do begin
    block := cur_block;
    type_desc := temp_type;
    public_dcl := false;
    dcl_class := local_sc;
    init_value.kind := no_value;
  end;
  chain_sym (cur_block^.id_list, tsym);	(* for allocation purposes *)

  make_temp := new_ident (tsym);	(* return expr node referencing symbol *)
 end;
$PAGE create_label, emit_label
(* CREATE LABEL generates a new label node.  It is not automatically chained
   into the intermediate form, but must be emitted by "emit_label". *)

function create_label: tuple;
 begin
  new (create_label, label_node);
  with create_label^ do begin
    label_sym := nil;
    block_order_no := 1;
    downward_thread := nil;
    upward_thread := nil;
    inward_jumps := nil;
    idom := nil;
    dom_son := nil;
    dom_brother := nil;
  end;
 end;



(* EMIT LABEL outputs a label node operator, and strings it on the label
   list on the block start node. *)

procedure emit_label (lab: tuple);
 begin
  with lab^ do begin
    downward_thread := t_chain;
    upward_thread := t_chain^.upward_thread;
    upward_thread^.downward_thread := lab;
    t_chain^.upward_thread := lab;
    block_order_no := upward_thread^.block_order_no + 1;
  end;
  emit (lab)
 end;
$PAGE get_label
(* GET LABEL obtains a label for a specific named label.  There are three cases:
   (1) the label has already been defined, (2) the label has been forward referenced,
   and (3) the label has been neither forward referenced nor defined.  In the former
   two cases, the existing node is returned;  otherwise (3), a new label node is
   created and returned. *)

var undefined_lab_list: tuple;	(* list of forward referenced labels;
				   inited and terminated in body - main *)


function get_label ( labsym: sym ): tuple;
 begin
  if labsym^.lab_defined then begin	(* label has already been emited *)
    get_label := t_chain^.downward_thread;
    while get_label^.label_sym <> labsym
      do get_label := get_label^.downward_thread;	(* search must succeed *)
  end
  else begin				(* forward reference *)
    get_label := undefined_lab_list;
    while get_label <> nil do begin
      if get_label^.label_sym = labsym then return; (* <---- exit with label found *)
      get_label := get_label^.downward_thread;
    end;
    get_label := create_label;		(* search failed, create it *)
    get_label^.label_sym := labsym;
    get_label^.downward_thread := undefined_lab_list;
    if undefined_lab_list <> nil then
      undefined_lab_list^.upward_thread := get_label;
    undefined_lab_list := get_label;
  end;
 end;
$PAGE define_label
(* DEFINE LABEL places a label node defining a specific named label in the
   intermediate form list.  "Get label" is called to obtain a label node, which
   may have extant forward references.  The node returned is removed from the
   undefined label list, and emitted in the correct location. *)

procedure define_label ( labsym: sym );
 var lab: tuple;
 begin
  lab := get_label (labsym);
  with lab^ do begin			(* slice out of list *)
    if downward_thread <> nil
      then downward_thread^.upward_thread := upward_thread;
    if upward_thread = nil
      then undefined_lab_list := downward_thread
      else upward_thread^.downward_thread := downward_thread;
  end;
  emit_label (lab);	(* emit in correct location *)
  labsym^.lab_defined := true;
 end;
$PAGE jump

(* JUMP emits a jump operator targeting on a given label *)

procedure jump (op: tuple_opcodes; lab: tuple; cond_expr: expr);
 var j: tuple;
 begin
  new (j, jump_op);
  with j^ do begin
    opcode := op;
    cond := cond_expr;
    jump_from := nil;
    jump_to := lab;
    next_inward_jump := nil;
  end;
  emit (j)
 end;

$PAGE assign
(* ASSIGN emits an assignment operator *)

procedure assign (lhs_expr, rhs_expr: tuple);
 var a: tuple;
     r: expr;
 begin
  new (a, assign_op);
  with a^ do begin
    must_store := false;
    lrecursive := false;
    rrecursive := false;
    r := rhs_expr;
    while (r^.opcode = strcvt_op) or (r^.opcode = upc_op) or (r^.opcode = lwc_op) do
      r := r^.operand[1];
    overlaps := (lhs_expr^.desc.kind in [strings, sets, arrays, records]) and
		not ( (r^.opcode = cst_ref) or
		      ( (r^.opcode = ident_ref) andif
			(r^.id_sym^.kind <> vars) ) or
		      ( (r^.opcode in [ident_ref, ptr_ref, buffer_ref, array_ref, substr_ref]) and
			(lhs_expr^.opcode in [ident_ref, ptr_ref, buffer_ref, array_ref]) ) );
    lhs := lhs_expr;
    rhs := rhs_expr;
  end;
  emit (a)
 end;
$PAGE file_parm
(* FILE PARM determines the file parameter for an io call.  It takes the first
   parameter from the call, and a collection of arguments describing the file
   to be obtained.  'Advance' is set true if the parameter is processed as a
   file parameter, and false if a default file parameter is used instead. *)

function file_parm ( param: parse_node; (* the file parameter *)
		     context: xrf_class; (* the context of the file parm *)
		     default: sym; (* the default file symbol--nil if no default *)
		     call_node: parse_node; (* the i/o call statement *)
		     var advance: boolean; (* set true if parameter used *)
		     tty_out: boolean; (* replace TTY by TTYOUTPUT *)
		     text_allowed: boolean; (* file may be text *)
		     typed_allowed: boolean; (* file may be typed *)
		     binary_allowed: boolean (* file may be binary *)
		   ): expr; (* the file expression *)

 var file_kind: file_modes; (* the kind of file, if there is a file *)

 begin
  advance := false;
  if param = nil then begin (* if a file is required, the calling code will *)
    file_parm := new_ident (default); (*   ensure that there is at least one argument *)
    xrf_use (default, call_node^.source, context);
  end

  else begin
    xrf_freeze;
    file_parm := ref_semantics (param, context);

    if file_parm^.desc.kind = files then begin
      if not assignable (file_parm) then begin
	err_node (err_file_variable, param);
	initexpr (file_parm, nil);
      end
      else begin
	file_kind := file_parm^.desc.base^.file_kind;

	if not (typed_allowed or binary_allowed) and (file_kind <> textfile) then
	  err_node (err_fil_not_text, param)

	else if not text_allowed and (file_kind = textfile) then
	  err_node (err_fil_text, param)

	else if not typed_allowed and (file_kind = typedfile) then
	  err_node (err_fil_typed, param)

	else if not binary_allowed and (file_kind = binaryfile) then
	  err_node (err_fil_binary, param)

	else if tty_out andif (file_parm^.opcode = ident_ref) andif
		(file_parm^.id_sym = file_tty) then
	  file_parm := new_ident (filettyoutput);

	advance := true;
      end

    end
    else if file_parm^.desc.kind = unknown_type then
      if default = nil
	then advance := true (* bad file expression--just accept it *)
	else file_parm := new_ident (default) (* bad file expression--ignore it *)

    else (* non-file parameter *) begin
      if default = nil
	then err_op (err_std_call_arg, 'a file', param)
	else file_parm := new_ident (default);
    end;

    if advance then
      xrf_unfreeze (context)
    else begin
      xrf_scratch;
      if default <> nil then
	xrf_use (default, param^.source, context);
    end;
  end (* if param <> nil *);
 end;
$PAGE open_call
(* OPEN CALL processes "open", "reset", "rewrite" and "update" calls.  The
   argument list has the format <file> -> [<name string>] -> [<option set>].
   The <name string> is required if the <file> is not a simple identifier
   reference. *)

procedure open_call ( open_opcode: tuple_opcodes; file_parm: expr; args: parse_node );

 var fname, fopts: expr;
     op: tuple;
     arg2, arg3: parse_node;
     dummy_name: boolean;
     cwidth: bit_range;
     calign: align_range;

 begin
  arg2 := args^.next;
  if arg2 = nil then begin
    fname := nil;
    fopts := nil;
  end
  else begin
    fname := val_semantics (arg2);
    arg3 := arg2^.next;
    if arg3 = nil then begin
      if fname^.desc.kind in [chars, strings] then
	fopts := nil
      else if fname^.desc.kind = sets then begin
	fopts := fname;
	arg3 := arg2;
	fname := nil;
      end
      else if fname^.desc.kind <> unknown_type then begin
	err_op (err_std_call_arg, 'a string or set', arg2);
	fopts := nil;
      end;
    end
    else (* arg3 <> nil *) begin
      fopts := val_semantics (arg3);
      if not (fname^.desc.kind in [chars, strings, unknown_type]) then
	err_op (err_std_call_arg, 'a string', arg2);
      if not (fopts^.desc.kind in [sets, unknown_type]) then
	err_node (err_opt_set_bad, arg3);
    end;
  end (* if arg2 <> nil *);

  if (fopts <> nil) andif (fopts^.desc.kind = sets) andif
     (fopts^.desc.base <> nil) andif
     (fopts^.desc.base^.base_type <> type_options) then
    err_node (err_opt_set_bad, arg3);

  dummy_name := (fname = nil);
  if dummy_name then begin
    if file_parm^.opcode = ident_ref then begin
      if file_parm^.id_sym = file_tty then begin
	dummy_name := false;
	fname := cst_expr (makestring (length (ttyiname)), nil);
	fname^.cst_val.valp^.str_val [1:length (ttyiname)] := ttyiname;
      end
      else if file_parm^.id_sym = filettyoutput then begin
	dummy_name := false;
	fname := cst_expr (makestring (length (ttyoname)), nil);
	fname^.cst_val.valp^.str_val [1:length (ttyoname)] := ttyoname;
      end
      else begin
	with file_parm^.id_sym^.name^ do begin
	  fname := cst_expr (makestring (len), nil);
	  fname^.cst_val.valp^.str_val [1:len] := substr (text, 1, len);
	end;
      end;
    end
    else
      err_node (err_no_file_name, args);
  end;

  if fopts = nil then begin (* create an empty set *)
    new (fopts, gen_set_op, gen_set_op: 0);
    initexpr (fopts, nil);
    with fopts^ do begin
      nopers := 0;
      desc.kind := sets;
      desc.set_lwb := 0;
      desc.set_cst_lwb := true;
      desc.set_length := 0;
      desc.set_cst_len := true;
    end;
    emit (fopts);
  end;

  new (op, open_op, open_op: 3);
  initexpr (op, file_parm^.desc.base);
  with op^ do begin
    opcode := open_opcode;
    nopers := 3;
    if dummy_name then begin
      operand[1] := nil;
      operand[2] := fname;
    end
    else begin
      operand[1] := fname;
      operand[2] := nil;
    end;
    operand[3] := fopts;
  end (* with op^ *);
  emit (op);

  assign (file_parm, op);
 end;
$PAGE simple_io_call
(* SIMPLE IO CALL generates "op (file)", where the "op" is a simple file operator.
   CallNode is the parse node for this call.  It will be nil if SimpleIoCall is
   being used as an inner routine for some other i/o routine. *)

procedure simple_io_call ( op: tuple_opcodes; call_node: parse_node;
			   file_parm: expr; must_check: boolean );

 var operation, chk: tuple;

 begin
  if must_check and (chk_fil_opt in cur_block^.semantic_options) then
    chk := op1 (file_chk, nil, file_parm);
  if call_node <> nil then
    simple_io_call (start_io_op, nil, file_parm, false);
  new (operation, get_op); (* get_op is as good as any other *)
  operation^.opcode := op;
  operation^.old_file := false;
  operation^.file_arg := file_parm;
  emit (operation);
  if call_node <> nil then
    simple_io_call (end_io_op, nil, file_parm, false);
  if (file_parm^.desc.kind = files) and (call_node <> nil) then begin
    xrf_write (fileblk_xrf);
    xrf_use (file_parm^.desc.base^.file_class, call_node^.source, var_parm_ctxt);
  end;
 end;
$PAGE close_call
(* CLOSE CALL is a simple close operation if it has a parameter, or a close_all
   otherwise. *)

procedure close_call ( arg: parse_node );

 var operation: tuple;
     adv: boolean;

 begin
  if arg = nil then begin
    new (operation, close_all_op);
    emit (operation);
  end
  else
    simple_io_call (close_op, arg,
      file_parm (arg, value_ctxt, nil, arg, adv, false, true, true, true), true);
 end;
$PAGE image_io_call
(* IMAGE IO CALL generates "op (file,arg)", where "op" is a read or write
   operator with image formatting. *)

type semantic_function = function ( parse_node ): expr;

procedure image_io_call ( op: tuple_opcodes;
			  arglist: parse_node;
			  sem: semantic_function;
			  default_file: sym;
			  for_output: boolean );

 var f: expr;
     advance: boolean;
     io_arg: parse_node;
     io_item: expr;
     operation, chk: tuple;

 begin
  f := file_parm (arglist, value_ctxt, default_file, arglist, advance,
		  for_output, true, false, false);
  if advance
    then io_arg := arglist^.next
    else io_arg := arglist;
  if io_arg = nil then begin (* no item to read/write *)
    err_node (err_arg_missing, arglist);
    return; (* <---- no call to generate *)
  end;
  io_item := sem (io_arg);
  if not (io_item^.desc.kind in [ints, unknown_type]) then
    err_op (err_std_call_arg, 'an integer', io_arg);
  if chk_fil_opt in cur_block^.semantic_options then
    chk := op1 (file_chk, nil, f);
  simple_io_call (start_io_op, nil, f, false);
  new (operation, read_op);
  with operation^ do begin
    opcode := op;
    rw_mode := imagerw;
    rw_old_file := false;
    rw_file := f;
    rw_item := io_item;
    rw_width := nil;
    rw_precision := nil;
  end;
  emit (operation);
  simple_io_call (end_io_op, nil, f, false);
  if f^.desc.kind = files then begin
    xrf_write (fileblk_xrf);
    xrf_use (f^.desc.base^.file_class, arglist^.source, var_parm_ctxt);
  end;
 end;
$PAGE seek_call
(*  SEEK CALL generates "seek (file, index)".  Arg is the parse node for the
    call.  It will be nil if SeekCall is being used as an inner routine for
    some other i/o routine.  *)

procedure seek_call ( arg: parse_node; file_parm, index: expr );

 var operation, chk: tuple;

 begin
  if chk_fil_opt in cur_block^.semantic_options then
    chk := op1 (file_chk, nil, file_parm);
  if arg <> nil then
    simple_io_call (start_io_op, nil, file_parm, false);
  new (operation, seek_op);
  operation^.seek_file := file_parm;
  operation^.seek_index := index;
  emit (operation);
  if arg <> nil then
    simple_io_call (end_io_op, nil, file_parm, false);
  if (file_parm^.desc.kind = files) and (arg <> nil) then begin
    xrf_write (fileblk_xrf);
    xrf_use (file_parm^.desc.base^.file_class, arg^.source, var_parm_ctxt);
  end;
 end;
$PAGE new_call

(* NEW CALL compiles an invocation of the 'new' procedure.  The argument list is:
   <ptr variable> -> [ <constant tag values> ]* -> [ <flex array bound> ].  *)

procedure new_call ( arglist: parse_node );

 var arg: parse_node;			(* current argument being processed *)
 var ptrvar: expr;			(* reference to pointer being new'ed *)
 var sizeop: expr;			(* size_op giving number of units to allocate *)
 var newop: expr; (* new_op allocating storage *)
 var deref: expr; (* ptr_ref accessing the allocated variable *)
 var size_type: typ;			(* type node whose size is to be allocated *)
 var tag: typ;				(* tag whose variant is being selected *)
 var labval: val;			(* parameter selecting a variant *)
 var dt: typ;				(* need in constant call, discarded *)
 var field: sym; (* scans for the last field in a variant *)
$PAGE deref_field - in new_call

(* DEREF FIELD generates a reference to a field of the allocated record,
   which is referenced by the pointer expression in 'DEREF'. *)

functiref_field ( field: sym ): expr;
begin
  new (deref_field, field_ref, field_ref); (* create the node *)
  initexpr (deref_field, field^.type_desc); (* fill in the type description *)
  emit (deref_field);
  with deref_field^ do begin (* fill in the reference information *)
    base_rec := deref;
    field_sym := field;
  end;
end (* deref_field *);
$PAGE check_flex - in new_call

(* CHECK FLEX looks at a reference.  If it is a flex array, we evaluate the
   last argument, check its type, and allocate the array with the specified
   number of elements. *)

procedure check_flex ( ref: expr; var arg: parse_node );

var
    desc_size: int_type;
    upbop: expr; (* we assign to the upper bound of the reference *)
    parm_type: typ; (* the flex index type *)

begin
  if (ref = nil) orif (ref^.desc.kind = unknown_type) then begin
    arg := nil; (* suppress further error messages *)
    return;
  end;
  with ref^.desc.base^ do begin
    if not ((kind in [arrays,strings]) and flexible) then
      return;
    if arg = nil then begin
      err_node (err_flx_upb_required, arglist);
      return;
    end;
    if kind = arrays
      then parm_type := index_type
      else parm_type := type_non_neg;
    t_set (sizeop^.prev); (* sizeop operand must precede the operator *)

    (*  Go back and set the size op parameter to the argument.  *)

    sizeop^.type_parm := copy_semantics ( arg, parm_type, true );
    arg := arg^.next;

    (*  If the item being allocated is not a record, then allow room for the
	"hidden length word".  (Any hidden length words are already allowed for
	in the size of a record containing flex arrays or strings.)  *)

    if deref^.desc.kind <> records then begin
      if deref^.desc.kind = arrays
	then desc_size := flex_arr_desc_size
	else desc_size := flex_str_desc_size;
      desc_size := (desc_size + byte_size - 1) div byte_size;
      t_set (newop^.prev);
      newop^.operand[1] := op2 (iadd_op, parm_type,
	newop^.operand[1],
	cst_expr (cst_scalar (desc_size), parm_type));
    end;

    t_set (nil); (* back to the end of the tuple chain *)
    if sizeop^.type_parm^.desc.kind = unknown_type then begin
      arg := nil; (* suppress future error messages *)
      return;
    end;

    (*  We set the length field of a flex array by an assignment to its
	upper bound, using the upb_op as a pseudo-function.  *)

    upbop := op1 (upb_op, parm_type, ref);
    assign (upbop, sizeop^.type_parm);
  end (* with ref^.desc.base *);
end (* check_flex *);
$PAGE new_call - main routine

begin
  arg := arglist;			(* get the pointer to new *)
  ptrvar := trgt_semantics (arg);
  if ptrvar^.desc.kind <> pointers then begin	(* cannot continue if not a pointer *)
    if ptrvar^.desc.kind <> unknown_type
      then err_op (err_std_call_arg,'a pointer', arg);
    return;
  end;
  size_type := ptrvar^.desc.base^.target_type;	(* alloc size of what ptr points to *)
  while (size_type <> nil) andif (size_type^.kind = indirect_type)
    do size_type := size_type^.actual_type;	(* get true type node *)
  if size_type = nil then return;		(* bag it *)

  (* Compile new (ptr) into assignment of the following form:
	 ptr := new_op (size_op (target_type)).
     The size_op is used because at this point, storage allocation (a
     machine-specific operation) has not yet been performed. *)

  new (sizeop, size_op, size_op);
  initexpr (sizeop, type_int);
  emit (sizeop);
  with sizeop^ do begin
    type_ref := size_type;
    type_parm := nil;
  end;

  newop := op1 (new_op, ptrvar^.desc.base, sizeop);
  assign (ptrvar, newop);

  new (deref, ptr_ref, ptr_ref);
  initexpr (deref, size_type);
  emit (deref);
  deref^.base_ptr := newop;

  arg := arg^.next;

  (*  If the target of the pointer is a record type then we may have additional
      arguments specifying the particular variant to be allocated. We update the
      size_op type reference for each tag specified, and emit an assignment to
      the tag field to set the tag value.  *)

  if size_type^.kind = records then begin

    while arg <> nil do begin			(* process the tag values *)
      if size_type^.kind = tags			(* get inferior tag, if any *)
	then tag := nil			(* tag node gives size for variants with () fieldlists *)
	else tag := size_type^.variant_tag;
    exit if tag = nil;				(* no tag, no variants *)
    exit if tag^.tag_type = nil do arg := nil;		(* probably safest approach *)
    exit if not constant (arg, tag^.tag_type, labval, dt) do arg := nil;
      size_type := pick_variant (tag, labval);	(* get the variant we need *)
      if tag^.tag_field <> nil then	(* assign labval to the field *)
	assign ( deref_field (tag^.tag_field),
		 cst_expr (labval, tag^.tag_type) );
      sizeop^.type_ref := size_type;	(* update size ref *)
      arg := arg^.next;	(* look for another type parm *)
    end (* while arg <> nil *);

    (*  We now find the last field (if any) for the selected variant of the
	record type, and then check whether it is a flex array or string.  *)

    if (size_type^.kind <> tags) andif
       (size_type^.field_list <> nil) andif
       (size_type^.field_list^.fld_variant = size_type) then begin
      field := size_type^.field_list;
      while (field^.next <> nil) andif (* find the last field in this variant *)
	(field^.next^.fld_variant = size_type) do
	  field := field^.next;
      check_flex ( deref_field (field), arg );
    end;
  end

  (* If the target of the pointer isn't a record type, then maybe it is
     a flex array type.  In that case, we just allocate the flex array
     directly. *)

  else
    check_flex (deref, arg);

  (* If there is still an unused argument in the list, we don't have
     any use for it, so we complain about it. *)

  if arg <> nil then
    err_node (err_flex_type_required, arg)

  (* If we have a possibly-flexible record type, complain. *)

  else if size_type^.flexible andif
	  (size_type^.kind in [records, variants]) andif
	  (size_type^.variant_tag <> nil) then
    err_node (err_flx_record, arglist);
end (* new_call *);
$PAGE allocate_call
(* ALLOCATE CALL assigns a new operator with a specified size to a pointer. *)

procedure allocate_call ( arglist: parse_node );

 var ptrvar: expr; (* the destination pointer *)
     sizeop: expr; (* the size expression *)

 begin
  ptrvar := trgt_semantics (arglist); (* get the pointer *)
  if ptrvar^.desc.kind = pointers then begin
    sizeop := copy_semantics (arglist^.next, type_int, false); (* get the size *)
    assign (ptrvar, sizeop);
  end
  else if ptrvar^.desc.kind <> unknown_type then
    err_op (err_std_call_arg, 'a pointer', arglist);
 end;
$PAGE dispose_call
(* DISPOSE CALL compiles a call to the dispose procedure.  Essentially it takes
   any pointer type value as an argument. *)

procedure dispose_call ( arg: parse_node );
 var op: tuple;
 begin
  new (op, dispose_op);
  op^.dptrarg := copy_semantics (arg, type_ptr, false);	(* PTR is compatible with any type *)
  emit (op);
 end;
$PAGE assert_call
(* ASSERT CALL transforms the call "assert (condition)" into the code
   "if not condition then assert_chk". *)

procedure assert_call ( condition: expr );

var lab, operation: tuple;

 begin
  lab := create_label;
  jump (jump_t_op, lab, condition);
  new (operation, abort_op);
  emit (operation);
  emit_label (lab);
 end;
$PAGE std_call
(* STD CALL semanticates standard procedure invocations.  It performs preliminary
   validity checks, and then dispatches to a procedure specific action routine. *)

procedure std_call ( procid: parse_node; prcode: sym; arglist: parse_node );

 type arg_range = array [std_proc] of packed record min, max: parm_range end;
 const narg_limits: arg_range :=
      (	(* open    *)	( 1, 3),
	(* reset   *)	( 1, 3 ),
	(* rewrite *)	( 1, 3 ),
	(* update  *)	( 1, 3 ),
	(* get     *)	( 1, 1 ),
	(* put     *)	( 1, 1 ),
	(* page    *)	( 0, 1 ),
	(* clear   *)	( 0, 1 ),
	(* break   *)	( 0, 1 ),
	(* empty   *)	( 1, 1 ),
	(* close   *)	( 0, 1 ),
	(* scratch *)	( 1, 1 ),
	(* read8   *)	( 1, 2 ),
	(* write8  *)	( 1, 2 ),
	(* seek    *)	( 2, 2 ),
	(* new     *)	( 1, maximum (parm_range) ),
	(* allocate*)	( 2, 2 ),
	(* dispose *)	( 1, 1 ),
	(* assert  *)   ( 1, 1 )			);

 var adv: boolean;

 begin
  if prog_options.global_opt and in_body then
    with prcode^.name^ do
      writeln (glob_file, '+', substr (text, 1, min (len, 10)));

  with narg_limits [prcode^.std_pf_code] do
    if not ck_arg_count (arglist,min,max,procid) then
      return;

  case prcode^.std_pf_code of

    propen:
      open_call (open_op, 
	file_parm (arglist, mod_ctxt, nil, procid, adv, false, true, false, false), arglist);

    prreset:
      open_call (reset_op, 
	file_parm (arglist, mod_ctxt, nil, procid, adv, false, true, true, true), arglist);

    prrewrite:
      open_call (rewrite_op, 
	file_parm (arglist, mod_ctxt, nil, procid, adv, true, true, true, true), arglist);

    prupdate:
      open_call (update_op, 
	file_parm (arglist, mod_ctxt, nil, procid, adv, false, false, true, true), arglist);

    prget:
      begin
	simple_io_call (get_op, arglist,
	  file_parm (arglist, value_ctxt, file_input, procid, adv, false, true, true, false), true);
	xrf_use (io_opsym, procid^.source, mod_ctxt);
      end;

    prput:
      begin
	simple_io_call (put_op, arglist,
	  file_parm (arglist, value_ctxt, file_output, procid, adv, true, true, true, false), true);
	xrf_use (io_opsym, procid^.source, mod_ctxt);
      end;

    prpage:
      simple_io_call (page_op, procid,
	file_parm (arglist, value_ctxt, file_output, procid, adv, true, true, false, false), true);

    prclear:
      simple_io_call (clear_op, procid,
	file_parm (arglist, value_ctxt, file_tty, procid, adv, false, true, false, false), true);

    prbreak:
      simple_io_call (break_op, procid,
	file_parm (arglist, value_ctxt, filettyoutput, procid, adv, true, true, true, true), true);

    prempty:
      simple_io_call (empty_op, arglist,
	file_parm (arglist, value_ctxt, nil, procid, adv, false, true, true, true), true);

    prclose:
      close_call (arglist);

    prscratch:
      simple_io_call (scratch_op, arglist,
	file_parm (arglist, value_ctxt, nil, procid, adv, false, true, true, true), true);

    prread8:
      image_io_call (read_op, arglist, trgt_semantics, file_tty, false);

    prwrite8:
      image_io_call (write_op, arglist, val_semantics, filettyoutput, true);

    prseek:
      seek_call (arglist,
	file_parm (arglist, value_ctxt, nil, procid, adv, false, false, true, true),
	copy_semantics (arglist^.next, type_int, false));

    prnew:
      new_call (arglist);

    prallocate:
      allocate_call (arglist);

    prdispose:
      dispose_call (arglist);

    prassert:
      assert_call (copy_semantics (arglist, type_bool, false))

 end (* case *) ;
end;
$PAGE proc_call
(* PROC CALL semantics a procedure call (or condition signal).  It accepts either
   a <function qualifier> which specifies a call with arguments, or some expression
   tree, which indicates a call of the procedure value (given by the expression)
   with no arguments.  If a standard procedure is specifed, std_call is invoked to
   generate procedure specifid operators; otherwise a call_op or signal_op is
   generated directly. *)

procedure proc_call ( calltree: parse_node );

 var proc, arglist: parse_node;
 var proc_ref: expr;

 begin
  last_stmt_node^.stmt_kind := func_qualifier;
  if calltree^.sym = func_qualifier
    then begin					(* arglist given *)
      proc := calltree^.defn;
      arglist := proc^.next;
    end
    else begin					(* just have a procedure value *)
      proc := calltree;
      arglist := nil
    end;

  (* Semanticate the procedure value, and verify that it is in fact a procedure
     instead of a function or something else.  If an error occurs, terminate
     processing. *)

  proc_ref := subr_semantics (proc);		(* get proc (or func) value *)
  if not (proc_ref^.desc.kind in [procs, con_procs]) then begin	(* error and abort *)
    if proc_ref^.desc.kind in [funcs, con_funcs] then
      err_node (err_lhs_func, proc)
    else if proc_ref^.desc.kind <> unknown_type then
      err_node (err_not_subr, proc);
    return
  end;

  (* If the procedure value is a standard procedure identifier, enter std_call
     to do per statement semantics. *)

  if (proc_ref^.opcode = ident_ref) andif (proc_ref^.id_sym^.kind = std_procs)
    then
      std_call (proc, proc_ref^.id_sym, arglist)

  (* Otherwise, generate a call to a user-defined procedure or condition. *)

  else
    proc_ref := validate_call (proc_ref, arglist, proc); (* emitted by validate_call *)

end (* proc_call *);
$PAGE statement
(* STATEMENT compiles the statement represented by the current parse tree
   in PTREE.  If the current statement is a structured statement, then PTREE
   is the parse tree for the statement head. *)

procedure statement ( onunit: blk; exit_label: tuple );
$PAGE stop_statement - in statement
(* STOP statement emits a stop operator, there are no semantics involved *)

procedure stop_statement;
 var t: tuple;
 begin
   new (t, stop_op);
   emit (t);
  get_ptree;
 end;
$PAGE return_statement - in statement
(* RETURN STATEMENT emits a return operator.  The only check required is that
   the return appear within a subroutine block -- not the main program. *)

procedure return_statement;
 var t: tuple;
 begin
   if cur_block^.kind <> subr_blk then		(* can only return from a subroutine *)
     err_node (err_return_bad, ptree);

   new (t, return_op);
   emit (t);
  get_ptree;
 end;
$PAGE exit_if - in statement
(* EXIT IF compiles an <exit clause>.  The definition chain is laid out thus:
   <exit clause> (<predicate>) [ || "do" || <statement> ].  A check is made
   that there is a valid exit label, i.e., that the exit clause is within a
   looping construct. *)

procedure exit_if;
 var l1: tuple;
     pred: expr;
 begin
  pred := copy_semantics (ptree^.defn, type_bool, false);

  if exit_label = nil then		(* not within loop construct *)
    err_node (err_exit_bad, ptree);

  (* If there is only a predicate, then jump directly to the exit label
     if the predicate is true. *)

  get_ptree;
  if ptree^.sym <> dosy then
    jump (jump_t_op, exit_label, pred)

  (* If there is an escape action, then we must jump over the action if the
     predicate is false, otherwise execute the action and then jump to the
     exit label. *)

  else begin
    l1 := create_label;
    jump (jump_f_op, l1, pred);
    get_ptree;
    statement (onunit, nil);			(* do the action *)
    jump (jump_op, exit_label, nil);
    emit_label (l1);				(* if pred false, continue *)
  end;
 end;
$PAGE group - in statement
(* GROUP processes the body of a statement group, such as <compound statement>,
   <loop statement> or <repeat statement>.  The definition chain has the form
   <head> || [<statement>]* || <exit code>, where the exit code is "end" or
   <until clause> (<predicate>).  The statements in the group may be normal
   statements or <intconst>s, which are statement labels. *)

procedure group ( exit_label: tuple; exit_code: symbols );
 var lab: sym;
 begin
  get_ptree; (* step past the statement head *)
  while ptree^.sym <> exit_code do begin (* iterate over the statements *)
    if ptree^.sym = intconst then begin			(* have a label *)
      make_label (ptree);			(* transform <intconst> into <identifier> *)
      lab := ptree^.name^.scopechain;		(* get the symbol for this label *)
      if (lab = nil) orif (lab^.block <> cur_block)
	then begin				(* label not declared in this block *)
	  err_node (err_und_label, ptree);
	  lab := declare (ptree,labels,nil); (* dummy up a definition *)
	  define_label (lab);	(* emit and fixup forward ref's *)
	end
      else if lab^.lab_defined then (* already seen in the body of the block *)
	 err_node (err_lab_defined, ptree)
      else begin (* declared but undefined in this block *)
	define_label (lab); (* emit and fix up forward ref's *)
	xrf_use (lab, ptree^.source, decl_xrf);
      end;
      get_ptree; (* get the next statement *)
    end

    else
      statement (onunit, exit_label); (* begin-end is transparent to exit-if *)

    (* "ptree" has been advanced to the next parse tree *)
  end;
  stmt_mark; (* mark the "end" statement *)
 end;
$PAGE compound_statement - in statement
(* COMPOUND STATEMENT compiles a begin-end construct.  It simply calls group
   to compile the body, and then steps past the "end" of the compound. *)

procedure compound_statement;
 begin
  group (exit_label, endsy);
  get_ptree;
 end;
$PAGE for_statement - in statement
(* FOR STATEMENT compiles a for statement by exploding the control construct.
   The definition chain is <for head> || <statement>, where <for head> has the
   definition chain <identifier> -> <expression> -> ( "to" | "downto") ->
    <expression>.  No attempt is made to compile the header if any of the
   required information is missing. *)

procedure for_statement;

 const one: val := (scalar_cst, 1);

 var
   idx, init, term: expr;				(* idx and limit of loop *)
   idn: sym; (* the index symbol *)
   id_kind: sym_kind; (* the index symbol kind *)
   idtype: typ; (* the index type *)
   test: expr;					(* loop termination comparision *)
   step: expr;					(* increment/decrement operator *)
   l1, l2: tuple;
   id, initval, direction, termval: parse_node;
   dir_sym: symbols; (* tosy / downtosy *)

 begin
  (* Determine if the definition of the for statement is correctly formed,
     and extract the components of the statement head. *)

  id := ptree^.defn;
  if id = nil
    then initval := nil
    else initval := id^.next;
 if initval = nil
    then direction := nil
    else direction := initval^.next;
  if direction = nil
    then termval := nil
    else termval := direction^.next;
  dir_sym := direction^.sym;

  (* If the header is in error, just compile the body to check for errors. *)

  if termval = nil then begin
    l2 := create_label;
    get_ptree;
    statement (onunit, l2);
    emit_label (l2);
    return;
  end;

  (* Everything is kosher.  Process the components of the definition, and compile
     the loop by exploding it into jumps and assignments. *)

  l1 := create_label;				(* looping label at start *)
  l2 := create_label;				(* exit label *)

  (* Semanticate the loop index, and set its initial value.  Must check that the
     idx is a scalar, and that it is a simple variable (i.e. not a field, not a
     parameter. *)

  idx := trgt_semantics (id);			(* semanticate the loop index *)
  if not (idx^.desc.kind in [bools, ints, chars, scalars, unknown_type])
    then begin					(* can only iterate over scalars *)
      err_node (err_idx_not_scalar, id);
      idx^.desc.kind := unknown_type;
      idx^.desc.base := nil;
    end
  else if (idx^.opcode <> ident_ref)
    then begin (* not a simple identifier reference *)
      err_node (err_idx_complex, id);
      idx := make_temp (nil);			(* code below expects simple ident_ref *)
    end
  else if (idx^.id_sym^.dcl_class = parameter_sc)
    then err_node (err_idx_parameter, id);
  idn := idx^.id_sym;
  idtype := idn^.type_desc;

  (* The initial and termination values must be of the same type as the
     loop index. *)

  init := copy_semantics (initval, idtype, true); (* get the loop initial value *)
  term := copy_semantics (termval, idtype, false);	(* get the loop termination value *)
  if assignable (term) then
    term^.copy_tuple := true;

  (* If the initial value is less than (greater than) the termination value,
     we can skip the loop entirely. *)

  if direction^.sym = tosy
    then new (test, igt_op, igt_op: 2)
    else new (test, ilt_op, ilt_op: 2);
  initexpr (test, type_bool);
  emit (test);
  with test^ do begin
    nopers := 2;
    operand[1] := init;
    operand[2] := term;
  end;
  jump (jump_t_op, l2, test);

  assign (new_ident (idn), init);

  (* If checking is enabled, we must make sure the loop termination value is
     in the range of the index. *)

  if chk_val_opt in cur_block^.semantic_options then
    chk_scalar (idtype, term, termval);

  (* This is the top of the loop proper. *)

  emit_label (l1);

  (* Insert the body of the for statement *)

  id_kind := idn^.kind;
  idn^.kind := for_inds; (* forbid assignments to the index variable *)
  get_ptree;
  statement (onunit, l2);
  idn^.kind := id_kind; (* restore the index variable *)

  (* End of the loop, emit operators to increment/decrement the loop index, and
     jump back to the test at the start of the loop.  Also emit the exit label. *)

  if dir_sym = tosy			(* create step expression *)
    then new (step, iadd_op, iadd_op: 2)		(* CG doesn't mind an iadd of a non integer *)
    else new (step, isub_op, isub_op: 2);
  initexpr (step, idtype);
  with step^ do begin
    nopers := 2;
    operand[1] := new_ident (idn);
    operand[2] := cst_expr (one, type_int);
    if dir_sym = tosy
      then desc.signed := (idtype^.minval < -1)
      else desc.signed := (idtype^.minval <= 0);
    desc.int_prec := min ( operand[1]^.desc.int_prec + ord (desc.signed) - ord (operand[1]^.desc.signed),
			   maximum (align_range) );
  end;
  emit (step);
  assign (new_ident (idn), step); (* assign stepped idx to itself *)
  cursor^.lrecursive := true;

  (* Now create the test for loop termination.  We do this by introducing a
     jump_t_op, using a constructed compare operator. *)

  if dir_sym = tosy
    then new (test, ile_op, ile_op: 2)			(* jump_t idx <= term *)
    else new (test, ige_op, ige_op: 2);			(* jump_t idx >= term for DOWNTO *)
  initexpr (test, type_bool);
  emit (test);
  with test^ do begin
    nopers := 2;
    operand[1] := step;
    operand[2] := term;
  end;
  jump (jump_t_op, l1, test);

  emit_label (l2);
 end;
$PAGE loop_statement - in statement
(* LOOP STATEMENT compiles the body of a loop statement, arranged as the body
   of a compound statement.  It creates two dummy labels, l1 for the start
   of the loop, and l2 for the end.  After the body is compiled, a jump to
   l1 is emitted; l2 is passed off to "group" to apply to any <exit clause>
   present in the body of the loop. *)

procedure loop_statement;
 var l1, l2: tuple;
 begin
  l1 := create_label;			(* generate the labels *)
  l2 := create_label;

  emit_label (l1);			(* compile the loop *)
  group (l2, endsy);
  jump (jump_op, l1, nil);
  emit_label (l2);
  get_ptree; (* step past the "end" *)
 end;
$PAGE with_statement - in statement
(* WITH STATEMENT compiles a with statement.  The definition chain of the with
   statement is <with statement> ( [<reference>]* ) || <statement>. *)

procedure with_statement;
 var refs: parse_node;
     nrefs, npushed, i: int_type;
     rec, rec1: expr;
     use_temp: boolean;
     wop: tuple;
 begin
  refs := ptree^.defn;
  nrefs := 0;
  npushed := 0;
  while refs <> nil do begin (* process each reference in the with head *)
    rec := base_semantics (refs, ref_ctxt);

    (* Two special concerns arise here.  (1) In case we don't apply optimization
       later, we must check for a reference of the form a[i] or p^, and mark
       "i" or "p" as a copy tuple, to tell the code generator that it must be
       evaluated now, rather than later when the first field of the record is
       loaded.  (2) If the record is a function call or a component of one,
       then the function must be called now, and the result assigned to a new,
       anonymous variable, so that it will be available throughout the scope
       of the with statement. *)

    if rec^.desc.kind <> unknown_type then begin
      rec1 := rec;
      use_temp := false;
      while rec1 <> nil do begin
	with rec1^ do begin
	  case opcode of
	    ident_ref, cst_ref:
	      rec1 := nil;
	    ptr_ref:
	      begin
		base_ptr^.copy_tuple := true;
		rec1 := nil;
	      end;
	    buffer_ref:
	      begin
		base_file^.copy_tuple := true;
		rec1 := nil;
	      end;
	    field_ref:
	      rec1 := base_rec;
	    array_ref:
	      begin
		index_val^.copy_tuple := true;
		rec1 := base_array;
	      end;
	    func_call_op:
	      begin
		use_temp := true;
		rec1 := nil;
	      end;
	    others:
	      begin
		trace;
		stop;
	      end
	  end (* case opcode *);
	end (* with rec1^ *);
      end (* while rec1 <> nil *);

      if use_temp then begin
	rec1 := make_temp (rec^.desc.base);
	assign (rec1, rec);
	cursor^.overlaps := false; (* can't affect an anonymous variable *)
	rec := new_ident (rec1^.id_sym);
      end;

      new (wop, start_with);
      wop^.with_rec := rec;
      emit (wop);

      if rec^.desc.kind = records then begin
	push_with (rec); (* valid with, push it *)
	npushed := npushed + 1;
      end
      else if rec^.desc.kind <> unknown_type then
	err_node (err_with_not_record, refs);

      nrefs := nrefs + 1;
    end (* if rec^.desc.kind <> unknown_type *);
    refs := refs^.next;
  end (* while refs <> nil *);

  get_ptree;
  statement (onunit, exit_label); (* with statements are transparent to exit-if *)

  for i := 1 to nrefs do begin
    new (wop, end_with);
    if i <= npushed then
      pop_with (rec)
    else
      rec := nil;
    wop^.with_rec := rec;
    emit (wop);
  end;
 end;
$PAGE if_statement - in statement
(* IF STATEMENT compiles an <if statement> which has the following definition
   chain: <if statement> (<predicate>) || <statement> [ || "else" || <statement> ]. *)

procedure if_statement;

 var l1, l2: tuple;

 begin
  (* Compile the conditional jump and the <then part>. *)

    l1 := create_label;
    jump (jump_f_op, l1, copy_semantics (ptree^.defn, type_bool, false));
    get_ptree;
    statement (onunit, nil);

  (* Case 1: there is no <else part>.  Compile a jump_f around the <then part>
     of the if statement. *)

  if ptree^.sym <> elsesy then
    emit_label (l1)

  (* Case 2: there is an <else part>.  Add a jump around the <else part> at the
     end of the <then part>. *)

  else begin
    l2 := create_label;				(* end of else *)
    jump (jump_op, l2, nil);
    emit_label (l1);
    get_ptree; (* get the else part *)
    statement (onunit, nil);
    emit_label (l2);
  end
 end;
$PAGE repeat_statement - in statement
(* REPEAT STATEMENT compiles a <repeat statement> whose definition is thus:
   "repeat" || [<statement>]* || <until clause> (<predicate>). *)

procedure repeat_statement;

 var l1, l2: tuple;

 begin
  l1 := create_label;				(* label at start of loop *)
  l2 := create_label;				(* exit label *)

  emit_label (l1);				(* compile the construct *)
  group (l2, until_clause);
  jump (jump_f_op, l1, copy_semantics (ptree^.defn, type_bool, false));
  emit_label (l2);
  get_ptree; (* step past the until clause *)
 end;
$PAGE while_statement - in statement
(* WHILE STATEMENT compiles a <while statement> defined in the following way:
   <while statement> (<predicate) || <statement>. *)

procedure while_statement;

 var l1, l2: tuple;

 begin
  l1 := create_label;				(* start of loop *)
  l2 := create_label;				(* exit of loop *)

  emit_label (l1);				(* compile the loop *)
  jump (jump_f_op, l2, copy_semantics (ptree^.defn, type_bool, false));
  get_ptree;
  statement (onunit, l2);
  jump (jump_op, l1, nil);
  emit_label (l2);
 end;
$PAGE case_statement - in statement

(* CASE STATEMENT compiles an entire case statement.  It generates the initial
   case_jump_op, a sorted case_vector list, and the jumps bracketing each
   contained statement.  In building the case vector list, this is the only
   routine other than "emit" which manipulates the tuple chain.  The definition
   chain is:  <case statement> (<expression>) || [<case member>]* || "end",
   where <case member> ::= (<range list> | "others") || <statement>. *)

procedure case_statement;

 var member_label, range: parse_node;
 var endlab, caselab: tuple;
 var case_jump, case_test, next_test: tuple;
 var case_type: typ;			(* type of selecting expression *)
$PAGE case_label_ok - in case_statement - in statement

(* CASE LABEL OK processes an element of a <range list> and determines that
   the label values are of the selector type and (for a range of values) that
   the upper and lower bounds are in order.  This also generates a jump_in_op
   node for the <range list>. *)

function case_label_ok
	( range: parse_node; sel_type: typ; var vector: tuple): boolean;  (* true if everything okay *)

var
    lowval, highval: val; (* Constant case label values. *)
    dt: typ; (* Type parameter for 'constant' call. *)

begin
  if range^.sym = elipsis then begin		(* defn is lb -> ub *)
    case_label_ok := constant (range^.defn, sel_type, lowval, dt) andif
		     constant (range^.defn^.next, sel_type, highval, dt);
    if case_label_ok andif (lowval.ival > highval.ival) then begin
      err_node (err_low_upb, range^.defn^.next);
      case_label_ok := false;
    end;
  end

  else begin					(* range is single value *)
    case_label_ok := constant (range, sel_type, lowval, dt);
    if case_label_ok then
      highval := lowval;
  end;

  if case_label_ok then begin (* Create the jump vector entry. *)
    new (vector, jump_in_op);
    vector^.low_cntrl := lowval.ival;
    vector^.high_cntrl := highval.ival;
  end;
end (* case_label_ok *);
$PAGE append_case - in case_statement - in statement

(* APPEND CASE adds a jump_in_op to the sorted list of jump nodes for
   this case statement.  This checks that there are no conflicts, and then
   adds the case to the list in order.  If there is a conflict, the jump
   is discarded. *)

var first_jump_test, last_jump_test: tuple;		(* start and end of case_vector list *)

procedure append_case ( range: parse_node; test: tuple );

var t1, t2: tuple;

begin
  with test^ do begin;
    case_jump^.low_cntrl  := min (case_jump^.low_cntrl,  low_cntrl);
    case_jump^.high_cntrl := max (case_jump^.high_cntrl, high_cntrl);

    t1 := last_jump_test;			(* scan backwards, as cases are usually user sorted *)
    t2 := nil;
    while (t1 <> nil) andif (low_cntrl <= t1^.low_cntrl) do begin
      t2 := t1;
      t1 := t1^.prev;
    end;

    (*  Now "test" should be inserted in the list between "t1" and "t2".  *)

    if ( (t1 <> nil) andif (low_cntrl <= t1^.high_cntrl) ) or
       ( (t2 <> nil) andif (high_cntrl >= t2^.low_cntrl) ) then begin
      err_node (err_case_conflict, range);
      dispose (test);
    end

    else begin
      prev := t1;
      next := t2;
      if t1 = nil
	then first_jump_test := test
	else t1^.next := test;
      if t2 = nil
	then last_jump_test := test
	else t2^.prev := test;
    end;
  end (* with test^ *) ;
end (* append_case *);
$PAGE case_statement - main routine - in statement

begin (* case_statement *);
  first_jump_test := nil;				(* init for append_case *)
  last_jump_test := nil;

  (* Generate the initial case_jump operator.  Semanticate the case indexing
     expression, and check that it is a scalar value. *)

  new (case_jump, case_jump_op); (* generate case jump *)
  with case_jump^ do begin
    cond := val_semantics (ptree^.defn); (* get indexing expression *)
    with cond^ do begin
      if not (desc.kind in [bools, ints, chars, scalars, unknown_type]) then begin
	err_node (err_scalar_case_required, ptree^.defn); (* must have scalar value *)
	desc.kind := unknown_type;
	desc.base := nil;
      end;
      case_type := desc.base; (* remember for checking case labels *)
    end;
    low_cntrl := maximum (low_cntrl); (* initialize for min/max checking *)
    high_cntrl := minimum (high_cntrl);
    jump_from := nil;
    jump_to := nil; (* 'others' action not known yet *)
    next_inward_jump := nil;
  end;
  emit (case_jump);

  endlab := create_label;				(* label for exiting loop *)

  (* Iterate over all the <case member>s.  A jump test is emitted for each
     component of the <range list>, except if the case label is "others", in
     which case the case_jump_op node is marked.  If the case has a non-null
     statement, then a label identifying the case is generated and the jump
     nodes marked with that label.  If there is no statement, the jump nodes
     are made to point directly at the end label. *)

  get_ptree;
  while ptree^.sym <> endsy do begin
    member_label := ptree;		(* grab defn *)
    ptree := nil; (* save the label tree *)

    get_ptree; (* get the statement *)
    if ptree^.sym = null_stmt then begin
      caselab := endlab;
      get_ptree; (* step over the statement *)
    end
    else begin
      caselab := create_label;
      emit_label (caselab);
      statement (onunit, nil); (* compile the statement *)
      jump (jump_op, endlab, nil); (* jump to the end-of-case label *)
    end;

    (* If the label is "others", record the label in the case_jump operator,
       checking that there is no duplicate. *)

    if member_label^.sym = otherssy then begin
      if case_jump^.jump_to <> nil
	then err_node (err_case_conflict, member_label)
	else case_jump^.jump_to := caselab
    end

    (* If the label is a <range list> := [ <const> | <const> .. <const> ]*, then
       iterate over each component of the list and create a test jump node for
       the label targeting on the case's label.  Must check that the labels are
       of the same type as the selecting expression, that there are no conflicts
       and (for '..') that the bounds are in ascending order *)

    else begin
      range := member_label^.defn;
      while range <> nil do begin
	if case_label_ok (range, case_type, case_test) then begin
	  with case_test^ do begin
	    cond := nil;
	    jump_from := nil;
	    jump_to := caselab;
	    next_inward_jump := nil;
	  end;
	  append_case (range, case_test)
	end;
	range := range^.next;
      end;
    end;

    del_ptree (member_label);
  end (* while ptree^.sym <> endsy *);

  (* If no 'others' case was specified, then the 'others' case is the exit label
     if case checking is off, and an error call if case checking is on. *)

  if case_jump^.jump_to = nil then begin
    if chk_cas_opt in cur_block^.semantic_options then begin
      caselab := create_label;
      emit_label (caselab);
      case_jump^.jump_to := caselab;
      new (caselab, case_abort_op);
      emit (caselab);
    end
    else
      case_jump^.jump_to := endlab;
  end;

  stmt_mark; (* mark the "end" statement *)

  (* Emit the label designating the end of the case statement. *)

  emit_label (endlab);

  if first_jump_test <> nil then begin

    (* If any two jump tests have adjacent ranges, and refer to the same
       statement, then replace them by a single test with a concatenated
       range. *)

    case_test := first_jump_test;
    next_test := case_test^.next;
    while next_test <> nil do begin
      if (next_test^.low_cntrl = case_test^.high_cntrl + 1) and
	(next_test^.jump_to = case_test^.jump_to) then begin
	  case_test^.high_cntrl := next_test^.high_cntrl;
	  case_test^.next := next_test^.next;
	  if next_test^.next <> nil
	    then next_test^.next^.prev := case_test
	    else last_jump_test := case_test;
	  dispose (next_test);
	  next_test := case_test^.next;
      end
      else begin
	case_test := next_test;
	next_test := case_test^.next;
      end;
    end (* while next_test <> nil *);

    (* Splice the case vector list into the tuple list for the block,
       immediately following the case jump. *)

    last_jump_test^.next := case_jump^.next;
    case_jump^.next^.prev := last_jump_test;	(* case jump must have next as endlab is there *)
    case_jump^.next := first_jump_test;
    first_jump_test^.prev := case_jump;

  end (* if first_jump_test <> nil *);

  get_ptree;
end (* case_statement *);
$PAGE cntrld_statement - in statement
(* CNTRLD STATEMENT compiles a statement prefixed with an <on clause>.  The definition
   chain is: <on clause> (<identifier>) || <handler body> || <statement>. *)

procedure cntrld_statement;

 var
   id: parse_node;
   csym: sym;
   op: tuple;
   handler: blk;
   parm, nparm: sym;

 begin
(*
  (* Verify that the identifier does, in fact, denote a condition.  When done
     "csym" points to the condition symbol or nil, if error. *)

  id := ptree^.defn;
  op := ref_semantics (id,ref_ctxt);		(* lookup with uniform error correction *)
  if op^.desc.kind in [con_procs, con_funcs]
    then csym := id^.name^.scopechain	(* get the symbol *)
    else begin
      csym := nil;			(* condition symbol is not valid *)
      if op^.desc.kind <> unknown_type
	then err_node (err_cond_expected, id);
    end;

  (* Compile the handler.  This involves creating a stand alone block for the
     condition, as it is activated separately from the containing block.  Note
     that the parameters of the condition must be declared in this block.  The
     symbol nodes for the parameters are copied because there may be several
     handlers for the condition, and instantiation of the parameters require
     different parent block pointers.  A return symbol is also created if the
     condition is declared with a return type. *)

  handler := new_blk (handler_blk, cur_block);	(* create it as subordinate of current *)
  with cur_block^ do begin				(* chain to parent *)
    if first_handler = nil then first_handler := handler;
    handler^.peer := children;
    children := handler;				(* order is reversed, fixed up in close_block *)
  end;
  with handler^ do begin
    cond_sym := csym;				(* name of condition enabled *)
    enclosing_handler := onunit;		(* next enclosing condition *)
  end;

  open_block (handler);				(* making it the cur_block *)
  if csym <> nil then begin			(* process parms and return value *)
    with csym^.type_desc^ do begin
      parm := parmlist.first;			(* copy the parameters *)
      while parm <> nil do begin
	nparm := new_sym (parm^.kind);		(* create either a value or var *)
	with nparm^ do begin
	  name := parm^.name;			(* copy info *)
	  type_desc := parm^.type_desc;
	  dcl_class := parameter_sc;		(* these are copies of what must be *)
	  public_dcl := false;
	  init_value.kind := no_value;
	  block := handler;			(* new information *)
	  scopechain := name^.scopechain;	(* put the name in scope *)
	  name^.scopechain := nparm;
	end;
	chain_sym (handler^.parm_list, nparm);		(* chain to block node *)

	parm := parm^.next;
      end (* while *) ;

      if return_type <> nil			(* create return symbol with name of condition *)
	then dcl_rt_sym (handler, id, return_type);
    end (* with csym^.type_desc *) ;
  end;

  get_ptree;
  body; (* compile the text of the block *)
  close_block;						(* reopen containing block of handler *)

  (* Compile the statement subject to control of the handler. It is bracketed
     with tuples denoting the scope of the condition handler. *)

  new (op, start_cond);
  op^.handler := handler;
  emit (op);

  statement (handler, exit_label);

  new (op, end_cond);
  op^.handler := handler;
  emit (op);

*)
 end;
$PAGE check_compatibility
(* CHECK COMPATIBILITY is called with a pair of expressions.  If they are
   (or include) flexible or generic arrays, then it may be necessary to emit
   a runtime check to make sure that their bounds match. *)

procedure check_compatibility ( left, right: expr; node: parse_node );

 var
    left_flex, right_flex: expr;
    left_type, right_type: typ;
    last_field: sym;
    chk_op: tuple_opcodes;
    chk: tuple;


  (* FIELD returns a FIELD REF with a given base record and field symbol. *)

  function field ( e: expr; f: sym ): expr;
   begin
    new (field, field_ref, field_ref);
    initexpr (field, f^.type_desc);
    field^.base_rec := e;
    field^.field_sym := f;
    emit (field);
   end;


 begin
  if (left^.desc.kind = unknown_type) or (right^.desc.kind = unknown_type) or
     (left^.desc.base = nil) or (right^.desc.base = nil) then
    return;

  if not (left^.desc.base^.flexible or right^.desc.base^.flexible) then
    return;

  (* Flex arrays must be checked; strings needn't be.  Records depend on
     the trailing field that makes the record flexible.  However, flex
     variant records may not be assigned at all.  If we do have a record
     with a trailing flex array, then we will need field references to get
     at the actual arrays from the lhs and rhs records, so we can check
     compatibility.  Note that if the lhs and rhs are records, then they must
     have the SAME record type, unlike arrays, which only need to have the
     same element types. *)

  left_flex := left;
  left_type := left^.desc.base;
  right_flex := right;
  right_type := right^.desc.base;
  while left_type^.kind = records do begin
    if left_type^.variant_tag <> nil then begin
      err_node (err_flex_var_assignment, node);
      return;
    end;
    last_field := left_type^.field_list; (* find the trailing, flexible field *)
    while last_field^.next <> nil do
      last_field := last_field^.next;
    left_flex := field (left_flex, last_field);
    right_flex := field (right_flex, last_field);
    left_type := last_field^.type_desc;
    right_type := left_type;
  end;

  if (left_type^.kind <> arrays) or
     not (chk_com_opt in cur_block^.semantic_options) then return;

  if left_type^.generic or right_type^.generic
    then chk_op := dim_op (* compare sizes for generic compare *)
    else chk_op := upb_op; (* compare upper bounds for flex compare *)

  chk := op2 (compat_chk, nil, arr_bound (chk_op, left_flex, left_type^.index_type),
			       arr_bound (chk_op, right_flex, right_type^.index_type) );
 end;
$PAGE simple_statement
(* SIMPLE STATEMENT compiles a <simple statement> which may be either an
   assignment or call depending on the presence or absence of an RHS.  The
   definition chain is:  <lhs> -> [ <rhs> ] *)

procedure simple_statement;
 var lhs, rhs: parse_node;
     left, right: expr;
 begin
  lhs := ptree^.defn;			(* get pieces of statement *)
  rhs := lhs^.next;

  (* If the right-hand side is missing, then this is a call statement. *)

  if rhs = nil then proc_call (lhs)		(* semanticate the call *)

  (* If the right-hand side is present, then this is an assignment statement.
     Check that the left-hand side is an assignable reference, and that the
     right-hand side is of a compatible type.  Convert the right-hand side as
     necessary. *)

  else (* rhs <> nil *) begin
    left := trgt_semantics (lhs); (* makes sure it is assignable *)

    (* Normally, the lhs reference must have a base type, and copy_semantics
       checks for compatibility and generates any necessary conversions.  If
       the lhs reference is a substr ref, it will not have a base type, and
       one must be created to keep copy_semantics happy. *)

    with left^.desc do begin
      if (kind = strings) and (base = nil)
	then right := copy_semantics (rhs, dcl_string (str_length, str_kind, str_flex), true)
	else right := copy_semantics (rhs, base, true);
    end;

    (* If the lhs and rhs are flexible arrays or records, and runtime checking
       is enabled, then generate a bounds compatibility check. *)

    check_compatibility (left, right, lhs);

    assign (left, right);			(* emit the assign_op *)

    (* When a subroutine is assigned to a subroutine variable, it is bound
       to the subr class of its type. *)

    if [left^.desc.kind, right^.desc.kind] <= [procs, funcs] then
      p_calls_q (rep_block (left), rep_block (right));
  end;

  get_ptree; (* advance beyond the statement *)
 end;
$PAGE io_statement
(* IO STATEMENT processes calls on the standard i/o procedures read, readln,
   write, writeln, getstring and putstring.  These procedure calls are parsed
   as statements because of their peculiar parameter syntax.  The general
   parse tree form is:  <io symbol> -> [ <file> ] -> [ <io arg> ]*;
   <io arg> ::= <expr> -> [ <expr> -> [ <expr> -> [ <expr> ] ] ]. The first
   expr is the argument; the next one or two may be the width and precision
   values, and the last one may be a format code. *)

procedure io_statement;

type
    io_direction = ( reading, writing );
$PAGE select_fmt - in io_statement
(* SELECT FMT checks any arguments following the width and precision codes
   in a read or write format item.  (1) If 'fmt_code' is nil, the default
   format code is selected.  (2) If 'fmt_code' is an identifier matching
   'code1' or 'code2', then the corresponding 'mode1' or 'mode2' is selected.
   (3) Otherwise, the default format code is selected, and an error message
   is printed.  (4) If the 'other_arg' is not nil, then it is an unwanted
   argument, and an error message is printed. *)

function select_fmt ( fmt_code: parse_node;
		      code1: char; mode1: rd_wr_mode;
		      code2: char; mode2: rd_wr_mode;
		      default: rd_wr_mode;
		      other_arg: parse_node;
		      bad_fmt_msg: err_codes ): rd_wr_mode;

 var fmt: char;

 begin
  if fmt_code = nil then
    select_fmt := default

  else if (fmt_code^.sym = ident) andif (fmt_code^.name^.len = 1) then begin
    fmt := fmt_code^.name^.text [1];
    if (fmt = code1) or (fmt = code2) then begin
      if fmt = code1
	then select_fmt := mode1
	else select_fmt := mode2;
      if other_arg <> nil then
	err_node (err_fmt_extra, other_arg);
    end
    else begin
      err_node (bad_fmt_msg, fmt_code);
      select_fmt := default;
    end;
  end

  else begin
    err_node (err_fmt_extra, fmt_code);
    select_fmt := default;
  end;
 end;
$PAGE format_arg - in io_statement
(* FORMAT ARG takes an opcode (read_op or write_op) and a parse tree
   representing an argument in the i/o call, and produces a read or
   write tuple. *)

procedure format_arg ( tfile: expr; mode: io_direction; tree: parse_node );

 var item_arg, width_arg, fmt_arg_3, fmt_arg_4: parse_node;
     item_expr: expr;
     operation: tuple;
     ok: boolean;

 begin
  item_arg := tree^.defn;
  width_arg := item_arg^.next;
  if width_arg = nil
    then fmt_arg_3 := nil
    else fmt_arg_3 := width_arg^.next;
  if fmt_arg_3 = nil
    then fmt_arg_4 := nil
    else fmt_arg_4 := fmt_arg_3^.next;

  if mode = reading
    then item_expr := trgt_semantics (item_arg)
    else item_expr := val_semantics (item_arg);

  (*  Simple character read/write on a file can be done with buffer assignment
      and get/put.  *)

  if ((item_expr^.desc.kind = chars) and (width_arg = nil)) andif
     ((tfile^.opcode <> in_str_op) and (tfile^.opcode <> out_str_op)) then begin

    if mode = writing then begin
      assign (fbuffer (tfile), item_expr);
      simple_io_call (put_op, nil, tfile, false);
    end
    else (* mode = reading *) begin
      assign (item_expr,
	      cvt_semantics (fbuffer (tfile), item_expr^.desc.base, item_arg, true, ok));
      simple_io_call (get_op, nil, tfile, false);
    end;
  end

  else (* formatting required *) begin
    if not (item_expr^.desc.kind in [strings, chars]) andif
       (mode = reading) andif
       pcomponent (item_expr) then
      err_node (err_fmt_packed, item_arg);
    if mode = reading then begin
      new (operation, read_op);
      if not (item_expr^.desc.kind in [ints, reals, chars, strings, unknown_types]) then
	err_node (err_text_in_type, item_arg);
    end
    else (* mode = writing *) begin
      new (operation, write_op);
      if not (item_expr^.desc.kind in [ints, reals, chars, strings, bools, unknown_types]) then
	err_node (err_text_out_type, item_arg);
    end;
    with operation^ do begin
      rw_old_file := false;
      rw_file := tfile;
      rw_item := item_expr;
      if width_arg = nil
	then rw_width := nil
	else rw_width := copy_semantics (width_arg, type_int, false);
      rw_precision := nil;
      case rw_item^.desc.kind of

	ints:
	  rw_mode := select_fmt (fmt_arg_3, 'H', hexrw, 'O', octalrw, decimalrw,
				 fmt_arg_4, err_fmt_int);

	reals:
	  begin
	    if fmt_arg_3 = nil then begin
	      if rw_width = nil then begin
		rw_precision := cst_expr (cst_scalar (rw_item^.desc.precision), type_int);
		rw_mode := realrw;
	      end
	      else begin
		rw_precision := nil;
		rw_mode := floatrw;
	      end;
	    end
	    else if mode = writing then begin
	      rw_precision := copy_semantics (fmt_arg_3, type_int, false);
	      rw_mode := select_fmt (fmt_arg_4, 'E', floatrw, ' ', fixedrw, fixedrw,
				     nil, err_fmt_real);
	    end
	    else
	      err_node (err_fmt_extra, fmt_arg_3);
	  end;

	chars,
	strings:
	  if mode = reading then begin
	    rw_mode := leftrw;
	    if fmt_arg_3 <> nil then
	      err_node (err_fmt_extra, fmt_arg_3);
	    if (rw_item^.desc.kind = chars) and (width_arg <> nil) then
	      err_node (err_fmt_char, width_arg);
	  end
	  else
	    rw_mode := select_fmt (fmt_arg_3, 'L', leftrw, ' ', rightrw, rightrw,
				   fmt_arg_4, err_fmt_string);

	bools:
	  begin
	    rw_mode := booleanrw;
	    if fmt_arg_3 <> nil then
	      err_node (err_fmt_extra, fmt_arg_3);
	  end;

	others:
	  (* no formatting *)

      end (* case rw_item^.desc.kind *);
    end (* with operation^ *);
    emit (operation);
  end (* formatting required *);
 end;
$PAGE rd_wr_typed - in io_statement
(* RD WR TYPED takes a file, a transfer direction (reading or writing), and a
   parse tree representing an argument in the read/write call, and produces an
   assignment between the argument and the file buffer, and a get or put opera-
   tion on the file. *)

procedure rd_wr_typed ( tfile: expr; mode: io_direction; arg: parse_node );

 var op: tuple;
     trgt, src: expr;
     ok: boolean;

 begin
  if arg^.defn^.next <> nil then
    err_node (err_fmt_typed, arg^.defn^.next);

  if mode = writing then begin
    assign (fbuffer (tfile), copy_semantics (arg^.defn, tfile^.desc.base^.component_type, true));
    simple_io_call (put_op, nil, tfile, false);
  end

  else (* mode = reading *) begin
    trgt := trgt_semantics (arg^.defn);
    src := cvt_semantics (fbuffer (tfile), trgt^.desc.base, arg^.defn, true, ok);
    if not ok then
      err_node (err_io_arg_type, arg);
    assign (trgt, src);
    simple_io_call (get_op, nil, tfile, false);
  end;
 end;
$PAGE rd_wr_binary - in io_statement
(* RD WR BINARY takes a file, a transfer direction (reading or writing) and a
   parse tree representing an argument in the read/write call, and produces a
   read_op or write_op tuple with the file and the argument.  If the argument
   has a :size parameter, it will be the rw_size of the operator.  Otherwise,
   the size of the operand will be used. *)

procedure rd_wr_binary ( tfile: expr; mode: io_direction; arg: parse_node );

 var io_item, io_size: expr;
     operation: tuple;

 begin
  if mode = reading then
    io_item := trgt_semantics (arg^.defn)
  else begin
    io_item := ref_semantics (arg^.defn, value_ctxt);
    if (io_item^.desc.kind <> unknown_type) andif
       not (io_item^.opcode in [first_data_ref..last_data_ref]) then begin
      err_node (err_bio_ref, arg);
      initexpr (io_item, nil);
    end;
  end;

  if (io_item^.desc.kind <> unknown_type) andif pcomponent (io_item) then begin
    err_node (err_bio_packed, arg);
    initexpr (io_item, nil);
  end;

  if io_item^.desc.kind = unknown_type then return; (* <---- early out if bad argument *)

  if arg^.defn^.next <> nil then
    io_size := copy_semantics (arg^.defn^.next, type_int, false)
  else begin
    new (io_size, size_op, size_op);
    initexpr (io_size, type_int);
    with io_size^ do begin
      type_ref := io_item^.desc.base;
      type_parm := nil;
      if (type_ref <> nil) andif
	 (type_ref^.kind in [arrays, strings]) andif
	 type_ref^.flexible then
	if type_ref^.generic
	  then err_node (err_bio_generic, arg)
	  else err_node (err_bio_flexible, arg);
    end;
    emit (io_size);
  end;

  if (arg^.defn^.next <> nil) andif (arg^.defn^.next^.next <> nil) then
    err_node (err_bio_fmt, arg^.defn^.next^.next);

  if mode = reading
    then new (operation, read_op)
    else new (operation, write_op);
  with operation^ do begin
    rw_mode := binaryrw;
    rw_old_file := false;
    rw_file := tfile;
    rw_item := io_item;
    rw_width := io_size;
    rw_precision := nil;
  end;
  emit (operation);
 end;
$PAGE io_statement - main routine

 var keyword, io_args, file_ptn: parse_node;
     keysym: operators;
     io_file, chk: expr;
     file_given: boolean;
     mode: file_modes;
     direction: io_direction;

 type key_set = set of operators;

 const string_ops: key_set = [getstrsy, putstrsy];
       random_ops: key_set = [readrnsy, writernsy];
       input_ops: key_set = [readsy, readlnsy, readrnsy, getstrsy];
       arg_reqd_ops: key_set = [readsy, writesy, readrnsy, writernsy, getstrsy, putstrsy];

type key_names = array [readsy..putstrsy] of string [10];

const key_name: key_names =
     (  'READ', 'WRITE', 'READLN', 'WRITELN', 'READRN', 'WRITERN',
	'GETSTRING', 'PUTSTRING'  );

 begin
  keyword := ptree^.defn;
  keysym := keyword^.op;
  if prog_options.global_opt and in_body then
    writeln (glob_file, '+', key_name[keysym]);
  io_args := keyword^.next;
  if io_args = nil
    then file_ptn := nil
    else file_ptn := io_args^.defn;
  case keysym of

    readsy:
      io_file := file_parm (file_ptn, value_ctxt, file_input, keyword,
			    file_given, false, true, true, true);

    readlnsy:
      io_file := file_parm (file_ptn, value_ctxt, file_input, keyword,
			    file_given, false, true, false, false);

    readrnsy:
      io_file := file_parm (file_ptn, value_ctxt, file_input, keyword,
			    file_given, false, false, true, true);

    writesy:
      io_file := file_parm (file_ptn, value_ctxt, file_output, keyword,
			    file_given, true, true, true, true);

    writelnsy:
      io_file := file_parm (file_ptn, value_ctxt, file_output, keyword,
			    file_given, true, true, false, false);

    writernsy:
      io_file := file_parm (file_ptn, value_ctxt, file_output, keyword,
			    file_given, true, false, true, true);

    getstrsy, putstrsy:
      begin
	if file_ptn = nil then begin
	  err_node (err_arg_missing, keyword);
	  io_file := bad_expr ();
	  file_given := false;
	end
	else begin
	  io_file := val_semantics (file_ptn);
	  file_given := (io_file^.desc.kind in [strings, unknown_type]);
	  if not file_given then
	    err_op (err_std_call_arg, 'a string', io_args)
	  else
	    if keysym = getstrsy
	      then io_file := op1 (in_str_op, type_text, io_file)
	      else io_file := op1 (out_str_op, type_text, io_file);
	end;
      end

  end (* case keysym *);

  if not (keysym in string_ops) and (chk_fil_opt in cur_block^.semantic_options) then
    chk := op1 (file_chk, nil, io_file);

  simple_io_call (start_io_op, nil, io_file, false);

  if file_given then begin (* was the first parameter used? *)
    if file_ptn^.next <> nil then (* :<format> on the file or string argument *)
      if keysym in string_ops
	then err_node (err_io_str_format, file_ptn^.next)
	else err_node (err_io_file_format, file_ptn^.next);
    io_args := io_args^.next; (* get the first true argument *)
  end;

  if keysym in random_ops then begin
    seek_call (nil, io_file, copy_semantics (io_args^.defn, type_int, false));
    io_args := io_args^.next;
  end;

  if (keysym in arg_reqd_ops) and (io_args = nil) then
    err_node (err_io_no_args, keyword);

  if keysym in string_ops then
    mode := textfile
  else if io_file^.desc.kind = files then
    mode := io_file^.desc.base^.file_kind
  else
    mode := anyfile; (* means "bad file", for the nonce *)

  if io_file^.desc.kind = files then begin
    xrf_write (fileblk_xrf);
    xrf_use (io_file^.desc.base^.file_class, keyword^.source, var_parm_ctxt);
  end;

  if mode = textfile then
    xrf_use (io_opsym, keyword^.source, mod_ctxt);

  if keysym in input_ops
    then direction := reading
    else direction := writing;

  while io_args <> nil do begin
    case mode of
      textfile:	format_arg (io_file, direction, io_args);
      typedfile: rd_wr_typed (io_file, direction, io_args);
      binaryfile: rd_wr_binary (io_file, direction, io_args);
      anyfile: (* bad file, do nothing *)
    end;
    io_args := io_args^.next;
  end;

  case keysym of
    readlnsy:  simple_io_call (readln_op, nil, io_file, false);
    writelnsy: simple_io_call (writeln_op, nil, io_file, false);
    others:
  end;
  simple_io_call (end_io_op, nil, io_file, false);
  get_ptree; (* advance beyond the statement *)
 end;
$PAGE goto_statement
(* GOTO STATEMENT compiles one of those horrible <goto statement>s.  The definition
   contains a single node, the target label.  This may be missing if the statement
   is in error. *)

procedure goto_statement;
 var lab: parse_node;
     labsym: sym;
     ntg: tuple;
 begin
  lab := ptree^.defn;				(* process goto if label present *)
  if lab <> nil then begin
    make_label (lab);				(* transform <intconst> into <identifier> *)
    labsym := lab^.name^.scopechain;
    if labsym = nil
      then begin
	err_node (err_und_label,lab);
	labsym := declare (lab, labels, nil); (* dummy up a declaration *)
	labsym^.lab_declaration := lab^.source;
      end;
    if labsym^.kind <> labels then	(* might happen, if we allow real id's as labels *)
      err_node (err_label_required, lab)
    else begin
      if labsym^.block = cur_block then (* local goto *)
	jump (jump_op, get_label (labsym), nil)
      else begin (* nonlocal goto *)
	new (ntg, goto_op);
	ntg^.target_label := labsym;
	ntg^.target_frame := nil;	(* nil until shaped *)
	emit (ntg);
	labsym^.lab_nonlocal_use := true;
      end;
      xrf_use (labsym, lab^.source, mod_ctxt);
    end;
  end;
  get_ptree; (* advance beyond the statement *)
 end;
$PAGE statement - main routine
(* STATEMENT compiles an arbitrary statement.  It emits a start_stmt operator to
   identify the statement, and dispatches to the appropriate semantic routine for
   the particular statement. *)

 begin
  stmt_mark; (* flag the beginning of the statement *)

  case ptree^.sym of

    simple_stmt:
      simple_statement;

    goto_stmt:
      goto_statement;

    io_stmt:
      io_statement;

    return_stmt:
      return_statement;

    stop_stmt:
      stop_statement;

    beginsy:
      compound_statement;

    if_stmt:
      if_statement;

    for_stmt:
      for_statement;

    repeatsy:
      repeat_statement;

    while_stmt:
      while_statement;

    loopsy:
      loop_statement;

    case_stmt:
      case_statement;

    with_stmt:
      with_statement;

    exit_clause:
      exit_if;

    contrld_stmt:
      cntrld_statement;

    others:
      get_ptree (* step past this statement *)

  end (* case *);
 end;
$PAGE body - main routine
(* BODY compiles the entire body of a main program or subroutine. *)

var
    t: tuple;
    lab: sym;
    stacked_with_list: wl_type;

begin
  if prog_options.global_opt then begin
    if cur_block^.kind = subr_blk then
      with cur_block^.subr_sym^.name^ do
	write (glob_file, '*', substr (text, 1, min (len, 10)), ' ':max (11-len, 1))
    else
      with cur_block^.id^ do
	write (glob_file, '*', substr (text, 1, min (len, 10)), ' ':max (11-len, 1));
    with root_block^.children^.id^ do
      writeln (glob_file, substr (text, 1, min (len, 6)));
    in_body := true;
  end;
  new_chain; (* put out the start of the block *)

  undefined_lab_list := nil;			(* init forward reference list *)

  stacked_with_list := with_list; (* save any old with list *)
  with_list := nil;

  last_stmt_node := nil; (* no statement marks emitted yet *)

  statement (nil, nil); (* compile the body of the block *)

  with_list := stacked_with_list; (* restore the saved with's *)

  while undefined_lab_list <> nil do begin	(* delete undefined label nodes *)
    t := undefined_lab_list;
    undefined_lab_list := undefined_lab_list^.downward_thread;
    with t^.label_sym^ do begin
      err_print (err_lab_not_defined, lab_declaration, name^.text[1:name^.len], 0);
      lab_defined := true;
    end;
    dispose (t);
  end;


  (* Check that all user declared labels have been defined by their appearance
     in the body of the subroutine. *)

  lab := cur_block^.label_list.first;		(* scan list of labels *)
  while lab <> nil do begin
    with lab^ do begin
      if not lab_defined then
	if lab_nonlocal_use
	  then err_print (err_lab_not_defined, lab_declaration, name^.text[1:name^.len], 0)
	  else err_print (err_lab_unrefer, lab_declaration, name^.text[1:name^.len], 0);
      lab := next;
    end;
  end;

  case cur_block^.kind of			(* emit correct operator to exit block *)
    subr_blk, handler_blk:
      new (t, return_op);
    program_blk, module_blk:
      new (t, stop_op)
  end;
  emit (t);

  end_chain; (* terminate this block *)
  reclaim; (* delete unused tuples *)
  wr_tuples; (* save the IF for this block *)
  if prog_options.global_opt then
    in_body := false;
end.
    8«