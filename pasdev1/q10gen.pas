$TITLE Q10GEN - quick pass code generator

module q10gen;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE ptmcon.inc
$INCLUDE pasfil.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE p10cgu.inc
$INCLUDE p10mac.inc
$INCLUDE p10opc.inc
$INCLUDE pasifu.inc
$INCLUDE q10exp.inc
$INCLUDE q10cll.inc
$INCLUDE q10cmp.inc
$INCLUDE p10rel.inc
$INCLUDE pasmap.inc
$INCLUDE p10csp.inc
$INCLUDE p10deb.inc
$INCLUDE pasjmp.inc
$INCLUDE pastal.inc
$INCLUDE passw.inc
$INCLUDE pa2dmp.inc
$INCLUDE pasdmp.inc
$SYSTEM q10set.inc
$system q10dsc.typ
$system q10dsc.inc
$PAGE declarations

public var

  low_base: def; (* relocatable 0 (static area) *)

  high_base: def; (* relocatable #O400000 (code area) *)

  prg_blk_def: def;     (* location of program block in debug *)

  reset_needed: boolean;        (* true while pending dynamic temps *)

const

  prg_blk_size := 6;    (* 5 words data + module word *)

static var

  temp_start: unit_range;       (* start of temps in local stack frame *)

  temp_base: unit_range;        (* top of temps in local stack frame *)

  temp_max: unit_range;         (* max temp location used *)

  save17addr: addr_desc;        (* addr of saved 17 for free of dynamic temps *)

  save17loc: def;               (* for definition of offset *)

  save17inst: code;             (* instruction which does the save *)

  hsb_addr: addr_desc;  (* descriptor for statically allocated temporary area in 
                           block's stack frame where handler state blocks will be located. *)
  overlaid: boolean; (* true if compilation under /OVERLAY *)
$PAGE get_temp
(* GET TEMP allocates static temporaries in the current stack frame.  These temps
   have a duration of one source statement - KILL TEMPS will release the space
   allocated here when the next start_statement tuple is encountered.  *)

public function get_temp (temp_size: unit_range): addr_desc;
begin
  get_temp := temp_reference;
  get_temp.index := sp;
  get_temp.offset := temp_base;
  temp_base := temp_base + temp_size;
  assert (temp_base <= #O777777); (* stack offsets are 18 bits *)
  temp_max := max (temp_max, temp_base);
end;
$PAGE kill_temps
procedure kill_temps;
begin
  temp_base := temp_start;
end;
$PAGE reset_stack, bb_end

(* RESET STACK is called to free dynamic temporaries by restoring the
   saved stack pointer. *)

procedure reset_stack;

begin
  if save17loc = nil then begin (* first use *)
    save17loc := make_def (local_def);
    save17addr.reloc.reldef := save17loc;
    save17inst^.reloc.reldef := save17loc;
  end;
  gen_rm (move, sb, save17addr);
  reset_needed := false;
end;

(* BB END determines if a stack reset is necessary and, if so, calls
   reset_stack to perform it. *)

public procedure bb_end;

begin
  if reset_needed then
    reset_stack;
end;
$PAGE emit_code
(* EMIT CODE takes a code list.  It will write it to the rel file, and to
   the macro listing if assembly_opt is in the options_list parameter. *)

public procedure emit_code ( var area_list: code_list; (* the code area to be written *)
                      var ic: unit_range; (* the address to write the code at *)
                      options_list: set_of_options ); (* to control listing *)

 begin
  if assembly_opt in options_list then
    mac_list (area_list, ic );
  wr_code (area_list, ic, true);
 end;
$PAGE init_static
(* INIT STATIC generates code for the initialized part of the static area, and
   for the declared public constants.  Symbols are processed in the order:
        Symbols within the id list of a block
        Blocks in downward_call_thread order
   This is the order in which they are processed during storage allocation. *)

procedure init_static  options special(coercions);

 var
   block: blk;
   symbols: sym;
   reld: reloc_value;
   cr: code;

 begin
  gen_origin (static_area, loc_static);

  gen_asm_label (static_area, 'STATC.');
  block := root_block;
  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
        with symbols^ do begin
          if (kind = vars) andif (dcl_class = static_sc) andif
             (init_value.kind <> no_value) then begin
            gen_origin (static_area, item_addr);
            gen_val (static_area, init_value);
          end
          else if (kind = consts) andif (init_value.kind <> subr_cst) andif
                  ( public_dcl or
                    ( prog_options.debug_opt and
                      (dcl_class <> external_sc) ) ) then begin
            reld := gen_cval (init_value);
            init_value.kind := alloc_cst;
            init_value.defp := val_ptr (reld.reldef);
          end
          else if (kind = conditions) andif (dcl_class = static_sc) then begin
            gen_origin (static_area, item_addr + size_init + size_uninit);
            new (cr, halfwords);
            with cr^ do begin
              xwd.lh := 0;
              xwd.rh := 0;
              lreloc := none;
              rreloc.kind := static_sc;
              rreloc.relsym := symbols (* word's own address *)
            end;
            gen_emit (static_area, cr)
          end;
          symbols := next;
        end;
      end (* while symbols <> nil *);
      block := downward_call_thread;
    end;
  end (* while block <> nil *);

  gen_origin (static_area, loc_static + size_init); (* start of uninit. static *)
  emit_code (static_area, loc_static, prog_options.semantic_options);
  if (size_uninit > 0) and (assembly_opt in prog_options.semantic_options) then
    mac_pad (size_uninit) (* put BLOCK for uninit. static into macro listing *)
 end;
$PAGE store
(* STORE moves the contents of a register or register pair ("reg") into
   a memory location ("maddr").  The precision "p" denotes whether a single
   register or a pair is to be moved.  "Alignment" gives the data alignment
   requirements of the value in the register;  it is assumed to be compatible
   with that of the target. *)

public procedure store (reg: registers; maddr: addr_desc; p: bit_range; alignment: data_alignment);
 begin
  case maddr.mode of
    fw:     if p > 36
              then gen_rm (dmovem, reg, maddr)
              else gen_rm (movem, reg, maddr);

    lhw:    if alignment = left_aligned
              then gen_rm (hllm, reg, maddr)
              else gen_rm (hrlm, reg, maddr);

    rhw:    if alignment = left_aligned
              then gen_rm (hlrm, reg, maddr)
              else gen_rm (hrrm, reg, maddr);

    byte:   begin
              if alignment = left_aligned then begin
                right_shift (reg, 36, 36 - maddr.slice_size);
              end;
              gen_rm (dpb, reg, maddr);
            end;

    slice:  begin
              if alignment = left_aligned then begin
                right_shift (reg, 36, 36 - maddr.slice_size);
              end;
              gen (dpb, reg, 0, 0, gen_bptr (maddr));
            end
  end;
 end;
$PAGE scalar_assignment
(* SCALAR ASSIGNMENT generates code for assignments of scalar values. *)

procedure scalar_assignment (tpl: tuple);

 var
   r: registers;
   laddr, raddr: addr_desc;
   scalar_val: integer;
   opc: opc_range;

 begin
  with tpl^ do begin
    raddr := fetch (rhs);
    laddr := fetch (lhs);
    if (laddr.mode in [fw, lhw, rhw]) andif aconstp (raddr, scalar_val) andif
      ((scalar_val = 0) orif (laddr.mode = fw) andif (scalar_val = -1)
        orif (laddr.mode <> fw) andif (scalar_val mod #O1000000 = #O777777)) then begin
        case laddr.mode of
          fw:
            if scalar_val = 0
              then opc := setzm
              else opc := setom;
          lhw:
            if scalar_val = 0
              then opc := hrrzs
              else opc := hrros;
          rhw:
            if scalar_val = 0
              then opc := hllzs
              else opc := hllos
        end;
        gen_rm (opc, 0, laddr);
        free (raddr);
    end
    else begin
      r := load_addr (raddr, alignment (rhs), rhs^.desc.int_prec);
      store (r, laddr, 36, signed_value);
      decr_reg_usages (r);
    end;
    free (laddr);
  end (* with *) ;
 end;
$PAGE real_assignment
(* REAL ASSIGNMENT generates code for assignments of real values. *)

procedure real_assignment (tpl: tuple);

 var
   r: registers;
   laddr, raddr: addr_desc;
   regsize: bit_range;

 begin
  with tpl^ do begin
    raddr := fetch (rhs);
    laddr := fetch (lhs);
    if rhs^.desc.precision > srealprec
      then regsize := 72
      else regsize := 36;
    r := load_addr (raddr, right_aligned, regsize);
    store (r, laddr, regsize, right_aligned);
    free (laddr);
    decr_reg_usages (r);
  end (* with *) ;
 end;
$PAGE proc_func_assignment

procedure proc_func_assignment (target, source: tuple);

var
  reg: registers;
  taddr: addr_desc;

begin
  pf_access (values, source, reg, false); (* get address and frame pointer *)
  taddr := fetch (target);
  gen_rm (movem, reg, taddr);
  free (taddr);
  decr_reg_usages (reg);
end;
$PAGE do_blt
(* DO BLT generates the code necessary to move multiple memory words with a BLT instruction.
   It takes addr_descs representing the source and destination addresses for the move, and an
   addr_desc representing the number of words to be moved.  DO BLT will free all three of these
   addr_descs.  It also assumes that that the width addr_desc is either immediate, or the
   value is in a register. *)

(* CST ADDR returns true if the address is known at compile time. *)

public function cst_addr (mem: addr_desc): boolean;
  begin
    with mem do
      cst_addr := (reloc.kind = static_sc) andif (not indirect) andif (index = noreg)
  end;

public procedure do_blt (src_addr, dest_addr, width_addr: addr_desc);

var 
  r, r1: registers;
  temp_addr: addr_desc;

begin
  if width_addr.immediate then begin
    if width_addr.offset = 0 then begin
      free (width_addr);
      free (src_addr);
      free (dest_addr);
      return (* <----  no code for zero-length copy *)
    end;
  end
  else
    assert (is_register (width_addr));

  (*  If both the source and destination addresses are known at compile time, the BLT
      control word may be generated as a constant and loaded.  Otherwise, it must be constructed. *)

  if not (cst_addr (dest_addr) orif (width_addr.immediate and not dest_addr.indirect) ) then
    free (dest_addr);  (* only case 2 below would still need it *)
  r := get_reg (36);
  if cst_addr (dest_addr) andif cst_addr (src_addr) then
    gen (move, r, 0, 0, gen_blt (src_addr.offset, src_addr.reloc,
                                 dest_addr.offset, dest_addr.reloc))
  else begin
    gen_rm (movei, r, dest_addr);
    gen_rm (hrli, r, src_addr)  
  end;
  free (src_addr);

  (*  Once the BLT control word is in register r, several different code
      sequences may be generated:

      (1) If the length and the destination address are both known (at compile time):
            BLT    r, dest+len-1

      (2) If the length is known, and the destination address is "nd(rd)":
            BLT  r, nd+len-1(rd)

      (3) If the length is known, and the destination address is totally unknown:
            HRRZI  r1, 0(r)
            BLT    r, len-1(r1)

      (4) If neither the length nor the destination address is known:
            JUMPLE len_reg, .+3
            ADDI   len_reg, -1(r)
            BLT    r, 0(len_reg)                                                *)

  if cst_addr (dest_addr) orif
           (width_addr.immediate and not dest_addr.indirect) then begin (* cases 1 and 2 *)
    temp_addr := dest_addr;
    temp_addr.offset := temp_addr.offset + width_addr.offset - 1;
    gen_rm (blt, r, temp_addr);
    free (dest_addr)
  end
  else if width_addr.immediate (* and desc_addr.indirect *) then begin (* case 3 *)
    r1 := get_reg (36);
    gen_rx (hrrzi, r1, r);
    decr_reg_usages (r1);
    gen (blt, r, r1, width_addr.offset - 1, none)
  end
  else begin
    r1 := width_addr.offset;
    gen (jump+lec, r1, 0, 3, dot); (* case 4 *)
    gen (addi, r1, r, -1, none);
    gen_rx (blt, r, r1)
  end;

  decr_reg_usages (r);
  free (width_addr)
end;
$PAGE agg_assignment
(* AGG ASSIGNMENT generates code for assignments of array and record values. *)

procedure agg_assignment (tpl: tuple);

var
  raddr, laddr,
  width_addr, temp_addr: addr_desc;
  reg: registers;
  field: sym;
  elem_size: bit_range;
  const_lwb, field_offset: unit_range;

procedure calc_width (agg_type: typ);
  var
    a, b, c, d: unit_range;
    grotesque: boolean;
  begin
    size_of (agg_type, false, a,b,c,d, grotesque);
    assert (not grotesque);
    if a <> 1 then
      gen_ri (imuli, reg, a);
    if b > 0 then
      gen_ri (addi, reg, b)
    else if b < 0 then
      gen_ri (subi, reg, -b);
    if c <> 1 then begin
      reg := coerce_to_double (reg_addr (reg), signed_value);
      gen_ri (idivi, reg, c);
      free_and_disassociate (reg + 1)
    end;
  end;

procedure move_it (laddr, raddr, width_addr: addr_desc);
  begin
    if width_addr.immediate andif (width_addr.offset <= 2) then begin
      free (raddr);
      free (width_addr);
      if width_addr.offset = 1 then begin
        reg := get_reg (36);
        gen_rm (move, reg, raddr);
        gen_rm (movem, reg, laddr);
        decr_reg_usages (reg)
      end
      else if width_addr.offset = 2 then begin
        reg := get_reg (72);
        gen_rm (dmove, reg, raddr);
        gen_rm (dmovem, reg, laddr);
        decr_reg_usages (reg)
      end;
      free (laddr)
    end
    else (* width is greater than 2 words, or isn't known *)
      do_blt (raddr, laddr, width_addr)
  end;

begin
  with tpl^ do
    if rhs^.opcode = func_call_op then
      if not overlaps then
        pas_call (rhs, expr_return_value, lhs, temp_addr)
      else begin
        temp_addr := get_temp ((rhs^.desc.base^.base_size + 35) div 36);
        pas_call (rhs, addr_return_value, nil, temp_addr);
        laddr := fetch (lhs);
        width_addr := int_value ((rhs^.desc.base^.base_size + 35) div 36);
        move_it (laddr, temp_addr, width_addr)
      end

    else (* rhs not a function call *) begin
      raddr := fetch (rhs);
      laddr := fetch (lhs);

      (* determine number of words to be copied, taking advantage of fact that both
         sides must have the same length.  *)

      if not rhs^.desc.base^.flexible then (* rhs has fixed size *)
        width_addr := int_value ((rhs^.desc.base^.base_size + 35) div 36)
      else if not lhs^.desc.base^.flexible then (* lhs has fixed size *)
        width_addr := int_value ((lhs^.desc.base^.base_size + 35) div 36)

      else if rhs^.desc.kind = records then begin (* (flex) record assignment *)
        field := lhs^.desc.base^.field_list;
        while field^.next <> nil do
          field := field^.next;
        field_offset := field^.fld_offset div 36;
        reg := load_addr (increment_addr (duplicate_addr (laddr), field_offset, 0, 36),
                          unsigned_value, 36);
        calc_width (lhs^.desc.base);
        width_addr := reg_addr (reg)
      end 

      else (* rhs^.desc.kind = arrays *) begin (* (flex) array assignment *)
        reg := load_addr ( upper_bound (rhs, raddr), unsigned_value, 36);
        calc_width (rhs^.desc.base);
        width_addr := reg_addr (reg)
      end; 

      if rhs^.desc.kind = arrays then begin
        if dynamic_flex (lhs) then
          laddr := increment_addr (laddr, 1, 0, 36);
        if dynamic_flex (rhs) then
          raddr := increment_addr (raddr, 1, 0, 36)
      end;

      move_it (laddr, raddr, width_addr)
    end

end (* agg_assignment *);
$PAGE expand_aggregates
(*  EXPAND AGGREGATES pre-processes the i/f for a routine, replacing
    operations involving aggregate constructors (AGG_VAL tuples) by
    multiple assignments.  An assignment of a constructor to a variable
    may simply be expanded into multiple component assignments; in all
    other cases, a temporary is allocated, the components of the constructor
    are assigned to components of the temporary, and references to the
    constructor are replaced by referecnes to the temporary.  *)

procedure expand_aggregates;
$PAGE const_match - in expand_aggregates
(*  CONST MATCH takes two VAL nodes, returning true if they represent the same
    constant value.  Record, array and set constants are only checked to see
    if they have the same value pointers.  *)


function const_match ( c1, c2: val ): boolean;

begin
  if c1.kind <> c2.kind then
    const_match := false
  else begin
    case c1.kind of

      scalar_cst:
        const_match := (c1.ival = c2.ival);

      real_cst:
        const_match :=
          (c1.valp^.real_val = c2.valp^.real_val) andif
          (c1.valp^.real_prec = c2.valp^.real_prec);

      string_cst:
        const_match :=
          (length (c1.valp^.str_val) = length (c2.valp^.str_val)) andif
          (c1.valp^.str_varying_ref = c2.valp^.str_varying_ref) andif
          (c1.valp^.str_val = c2.valp^.str_val);

      set_cst,
      array_cst,
      record_cst:
        const_match := (c1.valp = c2.valp);

      ptr_cst:
        const_match := true; (* The only pointer constant is NIL. *)

      subr_cst:
        const_match := (c1.blkp = c2.blkp);

      no_value:
        const_match := true;

    end (* case *);
  end;
end (* const_match *);
$PAGE tpl_match - in expand_aggregates
(*  TPL MATCH compares two reference expressions to determine if they have the
    same opcode and operands.  *)


function tpl_match ( t, t1: expr ): boolean;

begin
  with t^ do
    if t1^.opcode = opcode then begin
      case opcode of

        cst_ref:
          tpl_match := const_match (cst_val, t1^.cst_val);

        ident_ref:
          tpl_match := (t1^.id_sym = id_sym);

        field_ref:
          tpl_match :=
            tpl_match (t1^.base_rec, base_rec) a
            (t1^.field_sym = field_sym);

        ptr_ref:
          tpl_match := tpl_match (t1^.base_ptr, base_ptr);

        buffer_ref:
          tpl_match := tpl_match (t1^.base_file, base_file);

        array_ref:
          tpl_match :=
            tpl_match (t1^.base_array, base_array) andif
            tpl_match (t1^.index_val, index_val);

        substr_ref:
          tpl_match :=
            tpl_match (t1^.base_string, base_string) andif
            tpl_match (t1^.substr_index, substr_index) andif
            tpl_match (t1^.substr_length, substr_length);

        others:
          tpl_match := false;

      end (* case kind *);
    end
    else (* t1^.opcode <> opcode *) begin
      tpl_match := false;
    end;
end (* tpl_match *);
$PAGE circ_fields - in expand_aggregates
(*  CIRC FIELDS is called with a pair of field symbols, Fa and Fb, for fields
    in the same record type.  The question is where (1) Fa and Fb are the same
    field, or (2) Fa is the tag field of a discriminated union which Fb is in
    one of the variants of, or (3) Fa and Fb are in different variants of some
    undiscriminated union.  *)


function circ_fields ( fa, fb: sym ): boolean;

var
    a_tag: typ; (* The variant tag below the rec/var or Fa. *)
    a_var: typ; (* Scans up the Fa variant chain. *)
    b_var: typ; (* Scans up the Fb variant chain. *)

begin

  (*  See if Fa and Fb are the same fields.  *)

  if fa = fb then begin
    circ_fields := true;
    return;
  end;

  (*  See if Fb is a field in a variant of which Fa is the tag field.  *)

  a_tag := fa^.fld_variant^.variant_tag;
  if (a_tag <> nil) andif (a_tag^.tag_field = fa) then begin

    (*  Fa is a tag field.  The question now is whether Fb is in a variant
        controlled by Fa.  The question is actually recursive:  is Fb in a
        variant ... in a variant controlled by Fa?  *)

    b_var := fb^.fld_variant;
    while (b_var^.kind = variants) andif
      (b_var^.tag <> a_tag) do
        b_var := b_var^.tag^.tag_recvar;
    if b_var^.kind = variants then begin
      circ_fields := true;
      return;
    end;
  end;

  (*  See if Fa and Fb are fields in distinct variants controlled by the same
      undiscriminated union.  *)

  a_var := fa^.fld_variant;
  b_var := fb^.fld_variant;
  if (a_var^.kind = variants) and (b_var^.kind = variants) then begin

    (*  Fa and Fb are both fields in variants.  Could they be fields
        of different variants of an undiscriminated union?  *)

    while a_var^.kind = variants do begin
      if a_var^.tag^.tag_field = nil then begin

        (*  We have found an undiscriminated union with a variant containing
            Fa.  Now scan to see if Fb is in a variant of the same undiscri-
            minated union.  *)

        b_var := fb^.fld_variant;
        while (b_var^.kind = variants) andif
          (b_var^.tag <> a_var^.tag) do
            b_var := b_var^.tag^.tag_recvar;

        if b_var^.kind = variants then begin

(*=====>  Common undiscriminated union found for Fa and Fb!!!
          See if Fa and Fb are in different variants.
          Return in any case.  *)

          circ_fields := (a_var <> b_var);
          return;
        end;

      end (* undiscriminated union containing Fa *);
      a_var := a_var^.tag^.tag_recvar;
    end (* while a_var^.kind = variants *);
  end (* if Fa and Fb are both in variants *);

  (*  None of the above.  *)

  circ_fields := false;

end (* circ_fields *);
$PAGE distinct_indices - in expand_aggregates
(*  DISTINCT INDICES returns true if (1) Ia and Ib are distinct constants, or
    (2) Ia = Ib + C (C <> 0), or (3) Ib = Ia + C (C <> 0), or (4) Ia = X + C1
    and Ib = X + C2 (C1 <> C2), where X is any expression.  *)


function distinct_indices ( ia, ib: expr ): boolean;


var
    a_base, b_base: expr; (* The non-constant term from Ia/Ib. *)
    a_cst, b_cst: integer; (* The constant term from Ia/Ib. *)

  procedure sum_test ( i: expr; var base: expr; var cst: integer );
  begin
    with i^ do begin
      if (opcode = iadd_op) andif (operand[1]^.opcode = cst_ref) then begin
        base := operand [2];
        cst := operand[1]^.cst_val.ival;
      end
      else if (opcode = iadd_op) andif (operand[2]^.opcode = cst_ref) then begin
        base := operand [1];
        cst := operand[2]^.cst_val.ival;
      end
      else if (opcode = isub_op) andif (operand[2]^.opcode = cst_ref) then begin
        base := operand [1];
        cst := - operand[2]^.cst_val.ival;
      end
      else if (opcode = cst_ref) andif (cst_val.kind = scalar_cst) then begin
        base := nil;
        cst := cst_val.ival;
      end
      else begin
        base := i;
        cst := 0;
      end;
    end (* with i^ *);
  end (* sum_test *);

begin
  sum_test (ia, a_base, a_cst);
  sum_test (ib, b_base, b_cst);

  if a_base = nil then
    distinct_indices := (b_base = nil) andif (a_cst <> b_cst)
  else
    distinct_indices := (b_base <> nil) andif tpl_match (a_base, b_base) andif
                        (a_cst <> b_cst);
end (* distinct_indices *);
$PAGE circumscribes - in expand_aggregates
(*  CIRCUMSCRIBES tests whether two tuples satisfy the "circumscribes" relation
    as defined in CIN-#5.  *)


function circumscribes ( a, b: tuple ): boolean;

begin
  with a^ do begin
    if opcode <> b^.opcode then
      circumscribes := false
    else begin
      case opcode of

        ident_ref:
          circumscribes :=
            (id_sym = b^.id_sym);

        field_ref:
          circumscribes :=
            circumscribes (base_rec, b^.base_rec) andif
            circ_fields (field_sym, b^.field_sym);

        ptr_ref:
          circumscribes :=
            (base_ptr^.desc.base^.heap_class = b^.base_ptr^.desc.base^.heap_class);

        buffer_ref:
          circumscribes :=
            (base_file^.desc.base^.file_class = b^.base_file^.desc.base^.file_class);

        array_ref:
          circumscribes :=
            circumscribes (base_array, b^.base_array) andif
            not distinct_indices (index_val, b^.index_val);

        substr_ref:
          circumscribes :=
            circumscribes (base_string, b^.base_string) andif not
              ( (desc.kind = chars) andif (b^.desc.kind = chars) andif
                distinct_indices (substr_index, b^.substr_index) )

      end (* case opcode *);
    end (* a^.opcode = b^.opcode *);
  end (* with a^ *);
end (* circumscribes *);
$PAGE chk_overlap - in expand_aggregates
(*  CHK OVERLAP is a function which takes the left and right hand side
    expressions from an assignment, and determines whether a temporary
    location is necessary to perform the assignment safely.  An overlap
    is only indicated for string, set and aggregate assignments.  *)

function chk_overlap (left, right: expr ): boolean;
$PAGE may_be_undiscriminated - in chk_overlap - in expand_aggregates
(*  MAY BE UNDISCRIMINATED takes two non-simple references, and tests whether
    there is a possibility that the two references could be to fields (or
    components) in distinct variants of an undiscriminated union in the same
    record.  *)

function may_be_undiscriminated ( ref1, ref2: expr ): boolean;

const ref_stk_size = 20;

type ref_stack = record
        top: 0 .. ref_stk_size;
        stack: array [1..ref_stk_size] of expr
     end;
$PAGE stack_ref - in may_be_undiscriminated - in chk_overlap - in expand_aggregates
(*  STACK REF creates a stack containing the nested references making up a
    fully qualified reference, down to the first occurrence of a "primitive"
    reference--a constant, ientifier, pointer or buffer reference, or a temporary.  *)

procedure stack_ref ( ref: expr; var stack: ref_stack; var overflow: boolean );

var loop_status: ( stacking, stack_full, finished );
    r: expr;

begin
  r := ref;
  stack.top := 0;
  loop_status := stacking;
  while loop_status = stacking do begin
    if stack.top = ref_stk_size then
      loop_status := stack_full
    else begin
      stack.top := stack.top + 1;
      stack.stack[stack.top] := r;
      with r^ do begin
        case opcode of
          cst_ref, ident_ref, ptr_ref, buffer_ref, alc_temp_op:
            loop_status := finished;
          field_ref:
            r := base_rec;
          array_ref:
            r := base_array
        end;
      end;
    end;
  end (* while loop_status = stacking *);

  overflow := (loop_status = stack_full);
end (* stack_ref *);
$PAGE may_be_undiscriminated - main routine - in chk_overlap - in expand_aggregates
var stk1, stk2: ref_stack;
    r1, r2: expr;
    loop_status: ( comparing, undiscriminated, ok );
    ofl1, ofl2: boolean;
    base_match: boolean;

begin
  stack_ref (ref1, stk1, ofl1);
  stack_ref (ref2, stk2, ofl2);
  if ofl1 or ofl2 then begin (* Expression too complex, assume the worst. *)
    may_be_undiscriminated := true;
    return;
  end;
  
  r1 := stk1.stack[stk1.top];
  stk1.top := stk1.top - 1;
  r2 := stk2.stack[stk2.top];
  stk2.top := stk2.top - 1;

  if r1^.opcode <> r2^.opcode then
    base_match := false
  else begin
    case r1^.opcode of
      ident_ref:
        base_match := (r1^.id_sym = r2^.id_sym);
      ptr_ref:
        base_match := (r1^.base_ptr^.desc.base^.heap_class = r2^.base_ptr^.desc.base^.heap_class);
      buffer_ref:
        base_match := (r1^.base_file^.desc.base^.file_class = r2^.base_file^.desc.base^.file_class);
      alc_temp_op:
        base_match := (r1 = r2);
    end;
  end;

  if base_match
    then loop_status := comparing (* Something in common. *)
    else loop_status := ok; (* Nothing in common. *)

  while loop_status = comparing do begin
    if (stk1.top = 0) or (stk2.top = 0) then
      loop_status := ok (* Match failed. *)
    else begin
      r1 := stk1.stack[stk1.top];
      stk1.top := stk1.top - 1;
      r2 := stk2.stack[stk2.top];
      stk2.top := stk2.top - 1;

      if r1^.opcode <> r2^.opcode then
        loop_status := ok (* Match failed. *)
      else if r1^.opcode = field_ref then begin
        if r1^.field_sym = r2^.field_sym then
          (* matches so far, keep comparing *)
        else if circ_fields (r1^.field_sym, r2^.field_sym) then
          loop_status := undiscriminated
        else
          loop_status := ok; (* Distinct or discriminated fields. *)
      end
      else begin
        assert (r1^.opcode = array_ref);
        if distinct_indices (r1^.index_val, r2^.index_val) then
          loop_status := ok;
      end;
    end (* if neither stack is empty *);
  end (* while loop_status = comparing *);

  may_be_undiscriminated := (loop_status = undiscriminated);
end (* may_be_undiscriminated *);
$PAGE rec_circumscribes - in expand_aggregates
(*  REC CIRCUMSCRIBES tests whether an assignment to L can change the value of R.  *)

function rec_circumscribes ( l, r : expr ) : boolean;

var i : integer;

begin
  if circumscribes (l, r) then
    rec_circumscribes := true
  else begin
    rec_circumscribes := false;
    with r^ do begin
      case opcode of
        first_nnary_op..last_nnary_op :
          for i := 1 to upperbound (operand) do begin
            rec_circumscribes := rec_circumscribes (l, operand [i]);
          exit if rec_circumscribes;
          end;
        first_sbinary_op..last_snary_op :
          (* false *);
        func_call_op :
          rec_circumscribes := true;
        cst_ref,
        ident_ref,
        ptr_ref,
        buffer_ref :
          (* false *);
        array_ref :
          rec_circumscribes := rec_circumscribes (l, base_array) orif
                               rec_circumscribes (l, index_val);
        field_ref :
          rec_circumscribes := rec_circumscribes (l, base_rec);
        substr_ref :
          rec_circumscribes := rec_circumscribes (l, base_string) orif
                               rec_circumscribes (l, substr_index) orif
                               rec_circumscribes (l, substr_length);
      end (* case opcode *);
    end (* with r^ *);
  end (* if not circumscribes (l, r) *);
end (* rec_circumscribes *);
$PAGE str_overlaps - in chk_overlap - in expand_aggregates
(*  STR OVERLAPS is called with the left and right hand side expressions of a
    string assignment, and determines whether the assignment must be performed
    using a temporary.  A temporary is necessary if some part of the lhs
    string appears as part of the rhs string, except at the very beginning,
    or if the rhs string contains a function call which can use or modify the
    lhs string value.  *)

function str_overlaps ( l, r: expr ): boolean;


    function str_ovlay ( l, r: expr ): boolean;
      forward;
$PAGE str_ovl - in str_overlaps - in chk_overlap - in expand_aggregates
(*  STR OVL is the base function for the computation of StrOverlaps.  It also
    provides the base for the helper function StrOvlay.  StrOvl checks whether
    two string expressions "overlap" or are "overlaid" in some respect.  It is
    parameterized with a function OvlPrim which determines its value when none
    of its other rules apply.

    StrOvl (l, r, OvlPrim) ==
        r = substr (r', ...)      => StrOvl (l, r', OvlPrim)
        r = uppercase (r')        => StrOvl (l, r', OvlPrim)
        r = lowercase (r')        => StrOvl (l, r', OvlPrim)
        r = string_convert (r')   => StrOvl (l, r', OvlPrim)
        l = substr (l', 1, ...)   => StrOvl (l', r, OvlPrim)
        r = r1 || ... || rn       => StrOvl (l, r1, OvlPrim) or
                                     StrOvlay (l, r2) ... or StrOvlay (l, rn)
        l = substr (l', ...)      => StrOvlay (l', r)
        else                      => OvlPrim (l, r)                             *)

type ovl_fn = function ( expr; expr ): boolean;


function str_ovl ( l, r: expr; ovl_prim: ovl_fn ): boolean;

var l1, r1: expr;
    i: oper_range;

label 100;

begin
  l1 := l;
  r1 := r;

100:

  with r1^ do begin
    if opcode in [func_call_op, sclcvt_op] then begin
      str_ovl := true;
      return;
    end;

    if opcode = substr_ref then begin
      r1 := base_string;
      goto 100;
    end;

    if opcode in [lwc_op, upc_op, strcvt_op] then begin
      r1 := operand[1];
      goto 100;
    end;
  end;

  with l1^ do begin
    if opcode = substr_ref then begin
      if (substr_index^.opcode = cst_ref) andif
         (substr_index^.desc.kind = ints) andif
         (substr_index^.cst_val.ival = 1) then begin
        l1 := base_string;
        goto 100;
      end;
    end;
  end;

  with r1^ do begin
    if opcode = cat_op then begin
      for i := 2 to upperbound (operand) do begin
        str_ovl := str_ovlay (l1, operand[i]);
        if str_ovl then
          return;
      end;
      r1 := operand[1];
      goto 100;
    end;
  end;

  if l1^.opcode = substr_ref
    then str_ovl := str_ovlay (l1^.base_string, r1)
    else str_ovl := ovl_prim (l1, r1);
end (* str_ovl *);
$PAGE str_ovlay - in str_overlaps - in chk_overlap - in expand_aggregates
(*  STR OVLAY is a helper function for StrOverlaps.  It is essentially the same
    as StrOverlaps, except that two strings are said to be "overlaid" if they
    can have anything in common, while they do not "overlap" unless the common
    portion comes at the head of the lhs string.  *)

function str_ovlay (* l, r: expr ): boolean *);

begin
  str_ovlay := str_ovl (l, r, rec_circumscribes);
end;
$PAGE str_overlaps - main routine - in chk_overlap - in expand_aggregates
begin
  str_overlaps := str_ovl (l, r, may_be_undiscriminated);
end;
$PAGE set_agg_overlaps - in chk_overlap - in expand_aggregates
(*  SET AGG OVERLAPS is called with the left and right hand side expressions of
    a set or aggregate assignment, and determines whether the assignment must
    be performed using a temporary.  A temporary is necessary if the lhs
    expression, or some component of it, occurs in the rhs expression, or if
    the rhs expression contains a function call which can use or modify the
    lhs expression value.

    SetAggOverlaps (l, r) ==
        r = func_call (...)  => true
        r = gen_set (...)    => false
        r = set_cvt (r')     => SetOverlaps (l, r')
        r = r1 + ... + rn    => Circumscribes (r, l) ... or
                                Circumscribes (rn, l)
        r = r1 - r2          => Circumscribes (r, l) or Circumscribes (r2, l)
        r = r1 * r2          => Circumscribes (r, l) or Circumscribes (r2, l)
        r = (r1, ..., rn)    => true
        else                 => MayBeUndiscriminated (l, r)                     *)

function set_agg_overlaps ( l, r: expr ): boolean;

var i: oper_range;

begin
  if r^.opcode in [first_data_ref..last_data_ref] then
    set_agg_overlaps := may_be_undiscriminated (l, r)
  else
    set_agg_overlaps := rec_circumscribes (l, r);
end (* set_agg_overlaps *);
$PAGE chk_overlap - main routine - in expand_aggregates
begin
  case left^.desc.kind of

    strings:
      chk_overlap := str_overlaps (left, right);

    sets, arrays, records:
      chk_overlap := set_agg_overlaps (left, right);

    others:
      chk_overlap := false
  end;
end (* chk_overlap *);
$PAGE initexpr - in expand_aggregates
(*  INIT EXPR is called with an expression tree node and a type record, and
    sets up the type information in the node in accordance with the record.  *)

procedure initexpr ( node : expr; node_type : typ );

var nt : typ;

begin
  nt := node_type; (* If this is an indirect type node, *)
  while (nt <> nil) andif (*   find the actual type. *)
    (nt^.kind = indirect_type) do
      nt := nt^.actual_type;
  assert (nt <> nil);
  with node^ do begin
    result := nil;
    ref_fre := 0;
    context := valx;
    blk_input_tuple := false;
    copy_tuple := false;
    killed_tuple := false;
    desc.base := nt;
    with nt^ do begin
      desc.kind := kind;
      case kind of
        bools, ints, chars, scalars:
          begin
            desc.signed := minval < 0;
            desc.int_prec := base_size;
          end;
        files, pointers:
          begin
            desc.signed := false;
            desc.int_prec := base_size;
          end;
        reals:
          desc.precision := precision;
        strings:
          begin
            desc.str_kind := str_kind;
            desc.str_length := str_length;
            desc.str_flex := flexible;
          end;
        sets:
          begin
            if nt^.set_element_type = nil then
              desc.base := nil;
            desc.set_cst_lwb := true;
            desc.set_lwb := set_element_type^.minval;
            desc.set_cst_len := true;
            desc.set_length := set_element_type^.maxval - set_element_type^.minval + 1;
          end;
        arrays, records:
          (* no special descriptor *);
      end (* case kind *);
    end (* nt <> nil *);
  end (* with node^ *);
end (* initexpr *);
$PAGE cst_expr - in expand_aggregates
(*  CST EXPR returns a CST_REF tuple with a specified scalar value and type.  *)

function cst_expr ( n : integer; t : typ ) : expr;

begin
  new (cst_expr, cst_ref, cst_ref);
  initexpr (cst_expr, t);
  cst_expr^.cst_val := (scalar_cst, n);
  emit (cst_expr);
end (* cst_expr *);
$PAGE field_expr - in expand_aggregates
(*  FIELD EXPR returns a FIELD_REF tuple referring to a specified field in
    a specified record expression.  *)

function field_expr ( fld : sym; rec : expr ) : expr;

begin
  new (field_expr, field_ref, field_ref);
  initexpr (field_expr, fld^.type_desc);
  field_expr^.base_rec := rec;
  field_expr^.field_sym := fld;
  emit (field_expr);
end (* field_expr *);
$PAGE upb_expr - in expand_aggregates
(*  UPB EXPR returns an UPB_OP tuple referring to the upper bound of a
    specified expression which is, presumably, a trailing flex array or
    string field in some record.  *)

function upb_expr ( ref : expr ) : expr;

begin
  new (upb_expr, upb_op, upb_op, 1);
  if ref^.desc.kind = arrays
    then initexpr (upb_expr, ref^.desc.base^.index_type)
    else initexpr (upb_expr, type_non_neg);
  upb_expr^.operand[1] := ref;
  emit (upb_expr);
end (* upb_expr *);
$PAGE agg_temp - in expand_aggregates
(*  AGG TEMP allocates a temporary to hold a specified aggregate constructor.
    It creates an ALC_TEMP tuple whose type descriptor is the same as that
    for the constructor.  In most cases, the size required for the temporary
    is completely determined by the type descriptor, and the operand of the
    ALC_TEMP tuple is nil.  The only exception occurs when the aggregate is
    being assigned to a record with a trailing flex string field.   In this
    case, the DEST argument must be the variable the aggregate is being
    assigned to, and the operand of the ALC_TEMP tuple will be a reference
    to the upper bound of the last field of this record variable.  *)

function agg_temp ( agg, dest : expr ) : expr;

var fld : sym;

begin
  new (agg_temp, alc_temp_op, alc_temp_op, 1);
  initexpr (agg_temp, agg^.desc.base);

  if agg^.desc.base^.flexible then begin
    assert (agg^.desc.kind = records);
    assert (dest <> nil);
    fld := dest^.desc.base^.field_list;
    assert (fld <> nil);
    while fld^.next <> nil do
      fld := fld^.next;
    assert (fld^.fld_variant = dest^.desc.base);
    agg_temp^.operand[1] := upb_expr (field_expr (fld, dest));
  end
  else
    agg_temp^.operand[1] := nil;

  emit (agg_temp);
end (* agg_temp *);
$PAGE assign - in expand_aggregates
(*  ASSIGN generates an assignment tuple to assign RHS_EXPR to LHS_EXPR.  *)

procedure assign ( lhs_expr, rhs_expr : expr );

var tpl : tuple;

begin
  new (tpl, assign_op);
  with tpl^ do begin
    must_store := true;
    lrecursive := false;
    rrecursive := false;
    overlaps := true;
    lhs := lhs_expr;
    rhs := rhs_expr;
  end;
  emit (tpl);
end (* assign *);
$PAGE assign_agg_val - in expand_aggregates
(*  ASSIGN AGG VAL generates code to assign each component of the aggregate
    constructor RHS to the corresponding component of the aggregate reference
    LHS.  DEST is the ultimate destination of the aggregate, while LHS is
    the immediate destination; LHS may be the same as DEST, or it may be an
    intermediate temporary.  *)

procedure assign_agg_val ( lhs, rhs, dest : expr );

var index_type, comp_type : typ;
    i : oper_range;
    component, upb : expr;
    cur_var, var_list : typ;
    cur_tag, fields : sym;
    tag_val : integer;

begin

  (*  For an array constructor, simply generate assignments LHS[i] :=
      RHS[i], for each i from 1 to the number of components.  *)

  if rhs^.desc.kind = arrays then begin
    index_type := rhs^.desc.base^.index_type;
    comp_type := rhs^.desc.base^.element_type;
    for i := 1 to upperbound (rhs^.operand) do begin
      new (component, array_ref, array_ref);
      initexpr (component, comp_type);
      component^.base_array := lhs;
      component^.index_val := cst_expr (index_type^.minval + i - 1, index_type);
      emit (component);
      assign (component, rhs^.operand[i]);
    end;
  end

  (*  For a record constructor, generate assignments LHS.Fi := RHS[i] for
      each selected field Fi.  *)

  else begin
    assert (rhs^.desc.kind = records);
    cur_var := rhs^.desc.base;  (* the record type *)
    if cur_var^.variant_tag = nil then
      cur_tag := nil    (* no variants *)
    else
      cur_tag := cur_var^.variant_tag^.tag_field;       (* the variant tag field *)
    fields := cur_var^.field_list;
    for i := 1 to upperbound (rhs^.operand) do begin

      (*  If the field list is exhausted, then any remaining operands must be
          selectors for undiscriminated unions, so they can be discarded.  *)

      exit if fields = nil;

      (*  If the field belongs to the current variant, then assign the
          current rhs component to it.  *)

      if fields^.fld_variant = cur_var then begin
        component := field_expr (fields, lhs);

        (*  An assignment to a trailing flex field of a temporary must also
            set the hidden upper bound word for the field, so that when the
            temporary is copied to its ultimate destination, the upper bound
            word will have the correct value.  *)

        if fields^.type_desc^.flexible and (lhs <> dest) then begin
          assert (fields^.type_desc^.kind in [arrays, strings]);
          assert (dest <> nil);
          assign (upb_expr (component), upb_expr (field_expr (fields, dest)));
        end;
        assign (component, rhs^.operand[i]);
      end;

      (*  If the field is the variant tag, then the current operand must
          be used to select a discriminated union variant.  If the field
          doesn't belong to the current variant, then the current operand
          must be used to select an undiscriminated union variant.  The
          code is the same in either case.  *)

      if (fields = cur_tag) or (fields^.fld_variant <> cur_var) then begin
        tag_val := rhs^.operand[i]^.cst_val.ival;
        var_list := cur_var^.variant_tag^.first_variant;
        while (var_list <> nil) andif not ( var_list^.others_var orif
              ( (var_list^.minlab <= tag_val) and (tag_val <= var_list^.maxlab) ) ) do
          var_list := var_list^.next_variant;
        if var_list = nil then  (* no variant selected - no more fields *)
          fields := nil
        else begin      (* select the new variant *)
          fields := var_list^.field_list;
          if fields <> nil then begin
            cur_var := fields^.fld_variant;     (* if fields = nil, it doesn't matter *)
            if cur_var^.variant_tag = nil then
              cur_tag := nil    (* no sub-variants *)
            else
              cur_tag := cur_var^.variant_tag^.tag_field;       (* the sub-variant tag field *)
          end;
        end;
      end
      else
        fields := fields^.next;
    end (* for i *);
  end;

end (* assign_agg_val *);
$PAGE exp_agg_val - in expand_aggregates
(*  EXP AGG VAL is called with an AGG_VAL tuple, and returns an ALC_TEMP
    tuple for an aggregate of the correct size, to whose components the
    components of the aggregate constructor have been assigned.  DEST, if
    it is not nil, is the reference expression to which the aggregate will
    ultimately be assigned.  *)

function exp_agg_val ( agg, dest : expr ) : expr;

begin
  t_set (agg);
  exp_agg_val := agg_temp (agg, dest);
  assign_agg_val (exp_agg_val, agg, dest);
end (* exp_agg_val *);
$PAGE expand_aggregates - main routine
var t, t1 : tuple;
    i : oper_range;

begin
  t := t_chain^.final_tuple;
  while t <> nil do begin
    with t^ do begin
      t1 := prev;

      (*  An aggregate constructor can occur (1) on the rhs of an assignment,
          (2) as an actual value parameter, or (3) as the base record whose
          last field is being referenced for an upper bound check.  In cases
          (1) and (2), the constructor will be expanded into assignments; in
          case (3), the last field will simply be selected.  *)

      if (opcode = assign_op) andif (rhs^.opcode = agg_val) then begin
        if chk_overlap (lhs, rhs) then begin
          rhs := exp_agg_val (rhs, lhs);
          t1 := prev;
        end
        else begin
          t_set (t);
          assign_agg_val (lhs, rhs, lhs);
          dechain (t);
          t1 := if_cursor;
        end;
      end

      else if (opcode = call_op) or (opcode = func_call_op) then begin
        for i := 1 to upperbound (arglist) do begin
          if arglist[i]^.opcode = agg_val then
            arglist[i] := exp_agg_val (arglist[i], nil);
        end;
        t1 := prev;
      end

      else if (opcode = upb_op) andif
              (operand[1]^.opcode = field_ref) andif
              (operand[1]^.base_rec^.opcode = agg_val) then begin
        assert (operand[1]^.field_sym^.next = nil);
        operand[1] := operand[1]^.base_rec^.operand[upperbound(operand[1]^.base_rec^.operand)];
      end;
    end;
    t := t1;
  end (* while t <> nil *);
  reclaim;
end (* expand_aggregates *);
$PAGE prologue
(* PROLOGUE saves parameters on entry to a routine *)

procedure prologue;

var
  parm: sym;
  reg, rv_reg: registers;
  instr_count: 0..6;
  index: 1..6;
  stack_offset: array [1..6] of integer;
  next_link: call_link;

begin
  with cur_block^ do begin
    if (kind = subr_blk) andif (subr_sym^.type_desc^.parmlist_size > 6) then begin
      gen (movem, 2, sp, parm_list_base, none);
      rv_reg := 3;
    end
    else begin  (* save individual parameters *)

      (* First, determine where on the stack Pass 1 has decided each successive
         parameter register should be stored.  Actually, the regs will go into
         successive locations, but we explicitly do not make that assumption here. *)

      instr_count := 0;
      parm := parm_list.first;
      while parm <> nil do begin
        if parm^.type_desc^.flexible then begin
          stack_offset [instr_count + 1] := parm^.item_addr - 1;
          stack_offset [instr_count + 2] := parm^.item_addr;
          instr_count := instr_count + 2
        end
        else begin
          stack_offset [instr_count + 1] := parm^.item_addr;
          instr_count := instr_count + 1;
          if not passed_by_address (parm) andif (parm^.type_desc^.base_size > 36) then begin
            stack_offset [instr_count + 1] := parm^.item_addr + 1;
            instr_count := instr_count + 1
          end
        end;
        parm := parm^.next;
      end;

      (* Traverse the list of offsets emitting the movem instructions.  Where
         possible use dmovem's for adjacent pairs.  Actually its always possible,
         but again we explicitly do not make that assumption here.  *)

      reg := 2;
      index := 1;
      while index < instr_count do
        if stack_offset [index] = stack_offset [index + 1] - 1 then begin
          gen (dmovem, reg, sp, stack_offset [index], none);
          reg := reg + 2;
          index := index + 2
        end
        else begin
          gen (movem, reg, sp, stack_offset [index], none);
          reg := reg + 1;
          index := index + 1
        end;
      if index = instr_count then begin (* odd one left? *)
        gen (movem, reg, sp, stack_offset [index], none);
        reg := reg + 1
      end;
      rv_reg := reg;
    end;
    if (return_sym <> nil) andif passed_by_address (return_sym) then
      gen (movem, rv_reg, sp, return_sym^.item_addr, none);
  end;

  reg_init;     (* mark all registers as free *)

  (* temp_start  records where we start allocating temps for this block - takes into account
                 quick-blocking
     temp_base   is advanced as temps are needed within a statement, and is
                 reset to temp_start at start of each statement
     temp_max    keeps track of highest value rached by temp_base  i.e.
                 requirements of "worst" statement.  *)

  temp_start := cur_block^.pos_stk_end;
  next_link := cur_block^.calls;
  while next_link <> nil do begin
    if cur_block^.owner = next_link^.called_subr^.owner then
      temp_start := max (temp_start, next_link^.called_subr^.pos_stk_end);
    next_link := next_link^.rlink
  end;

  temp_base := temp_start;
  temp_max := temp_start;

  (* Emit instruction to save 17 so that is can be restored after statements
     that have allocated dynamic temps by changin it.  If there never are any
     such statements, this instruction will be deleted at the end of
     COMPILE_BODY.  The location in which to store 17 is left undefined at
     this point.  If its actually required, RESET_STACK will obtain a
     symbol, and COMPILE_BODY will ultimately give the symbol a value.  *)

  reset_needed := false;
  save17loc := nil;
  save17addr := absolute_reference;
  save17addr.index := sp;
  save17addr.reloc.kind := def_sc;
  gen_rm (movem, sb, save17addr);
  save17inst := code_area.last;

  (* Initialize descriptor for stack locations where handler state blocks will reside. *)

  hsb_addr := absolute_reference;
  with hsb_addr do begin
    index := sp;
    reloc.kind := def_sc;
    reloc.reldef := make_def (local_def)
  end;
end;
$PAGE case_jump
(* CASE JUMP generates code for a case jump operator and following jump in operators.
   The parameter "tpl" points to the case operator on entry; on return, it is
   set to the last jump in operator. *)

procedure case_jump (var tpl: tuple);

 var
   reg: registers;
   caddr: addr_desc;
   jmp: tuple;
   i: integer;

 begin
  with tpl^ do begin
    caddr := fetch (cond);
    if reset_needed then
      reset_stack;
    if next^.opcode <> jump_in_op then begin    (* no (non "others") cases *)
      free (caddr);
    end
    else begin
      reg := load_addr (caddr, alignment (cond), 36);
      gen_rm (cam+ltc, reg, int_value (low_cntrl));
      gen_rm (cam+lec, reg, int_value (high_cntrl));
      gen_rl (jrst, 0, jump_to);

      genind (jrst, 0, reg, 1 - low_cntrl, dot);

      jmp := next;
      loop              (* over jump_in_op's, assume at least one *)
        for i := jmp^.low_cntrl to jmp^.high_cntrl do
          gen_rl (arg, 0, jmp^.jump_to);
      exit if jmp^.next^.opcode <> jump_in_op;
        jmp := jmp^.next;
        for i := jmp^.prev^.high_cntrl + 1 to jmp^.low_cntrl - 1 do
          gen_rl (arg, 0, tpl^.jump_to);        (* goto 'others' for unspecified cases *)
      end;
      tpl := jmp;                       (* advance to last jump in op *)
      decr_reg_usages (reg);
    end;
  end (* with *) ;
 end;
$PAGE cond_handler_stmts
(* COND HNDLR STMTS generates code for the statement tuples associated with
   condition handling.  *)

procedure cond_handler_stmts (var tpl: tuple);

var
  cr: code;
  cond_addr: addr_desc;
  jmp: tuple;
  area: code_list;

procedure cond_cell_arg (rtsym: rt_symbol);
  begin
    cond_addr := fetch (tpl^.cond_parm);
    gen_rt (pushj, sb, rtsym);
    gen_rm (arg, 0, cond_addr);
    free (cond_addr)
  end;

begin
  with tpl^ do
    case opcode of

      set_handler_op, rst_handler_op:
        begin
          if opcode = set_handler_op
            then gen_rt (pushj, sb, rt_set_handler)
            else gen_rt (pushj, sb, rt_rst_handler);
          if hndlr_tuple = nil then
            gen_ri (arg, 0, 0)
          else
            gen (arg, 0, 0, 0, reldef (get_def (hnd_tab_def, hndlr_tuple^.block_order_no)))
        end;

      signal_op:
        cond_cell_arg (rt_signal);

      mask_op:
        cond_cell_arg (rt_mask);

      unmask_op:
        cond_cell_arg (rt_unmask);

      resignal_op:
        gen_rt (pushj, sb, rt_resignal);

      hndlr_jump_op: begin
  
        (* handler common entry code *)
  
        with_restore;
        reset_stack;
        genind (jrst, 0, 0, 0, none);  (* jrst @0 *)
        if overlaid
          then area := hbt_area
          else area := code_area;
         
        (* handler branch table *)
 
        mark_def (area, get_def (hnd_tab_def, jump_from^.block_order_no)); (* mark with H.nn label *)

        new (cr, halfwords); (* XWD L.m,H.n where L.m is common entry code, and
                                                  H.n is encompassing hbt  *)
        with cr^ do begin
          xwd.lh := 0;
          xwd.rh := 0;
          lreloc := reldef (get_def (label_def, jump_from^.block_order_no));
          if jump_from^.in_handler = nil then
            rreloc := none
          else
            rreloc := reldef (get_def (hnd_tab_def, jump_from^.in_handler^.block_order_no))
        end;
        gen_emit (area, cr);

        new (cr, halfwords);    (* XWD 0,n  where n is hsb offset in stack frame *)
        with cr^ do begin
          xwd.lh := 0;
          lreloc := none;
          xwd.rh := (high_cntrl - 1) * 2;
          rreloc := hsb_addr.reloc
        end;
        gen_emit (area, cr);

        (* body of table: one word per condition *)
 
        jmp := tpl;
        while jmp^.next^.opcode = jump_cond_op do begin
          jmp := jmp^.next;
          new (cr, halfwords); (* XWD  <condition>,L.n  where L.n is label of handler *)
          cond_addr := fetch (jmp^.cond);
          free (cond_addr);
          with cr^ do begin
            xwd.lh := 0;
            xwd.rh := 0;
            lreloc := cond_addr.reloc;
            rreloc := reldef (get_def (label_def, jmp^.jump_to^.block_order_no))
          end;
          gen_emit (area, cr)
        end;

        new (cr, halfwords);    (* last word of handler branch table *)
        with cr^ do begin
          lreloc := none;
          if jump_to^.next^.opcode = resignal_op then begin
            xwd.lh := #O777777; (* others = -1 *)
            xwd.rh := 0;
            rreloc := none
          end
          else begin
            if low_cntrl = 0 then
              xwd.lh := #O777777 (* others = -1 *)
            else
              xwd.lh := #O777776; (* allconditions = -2 *)
            assert (jump_to^.opcode = label_node);
            rreloc := reldef (get_def (label_def, jump_to^.block_order_no))
          end
        end;
        gen_emit (area, cr);
 
        tpl := jmp;  (* advance to last jump_cond_op *)
        if overlaid
          then hbt_area := area
          else code_area := area;
      end;

      others:
        assert (false)

    end (* case opcode *);
end (* cond_handler_stmts *);
$PAGE rt_io_call
(* RT IO CALL generates a runtime routine call for one of the simple i/o
   routines, given "op", the runtime statement tuple, and "rts", the runtime
   routine code.  If there are both explicit and implicit argument forms of
   a call, it is assumed that the code for the implicit call is the successor
   of the code for the explicit call. *)

procedure rt_io_call ( op: tuple; rts: rt_symbol );

 var
    f: expr;
    mem: addr_desc;

 begin
  with op^ do begin
    if old_file then
      gen_rt (pushj, sb, succ (rts))

    else begin
      f := file_arg;
      if (f^.opcode = io_var_str_op) or (f^.opcode = io_fix_str_op) then
        f := f^.operand[1];
      mem := argument (f);
      gen_rt (pushj, sb, rts);
      gen_rm (arg, 0, arg_addr (mem));
      free (mem);
    end;
  end;
 end;
$PAGE rt_seek_call
(* RT SEEK CALL generates a call to the runtime file random access routine,
   using the specified file and index arguments. *)

procedure rt_seek_call ( fil, ind: expr );

var
  fil_mem, ind_mem: addr_desc;
  cval: integer;

begin
  fil_mem := argument (fil);
  ind_mem := argument (ind);
  if aconstp (ind_mem, cval) then
    ind_mem := gen_cst (cval);
  gen_rt (pushj, sb, rt_seek);
  gen_rm (arg, 0, arg_addr (fil_mem));
  gen_rm (arg, 0, ind_mem);
  free (fil_mem);
  free (ind_mem);
end;
$PAGE value_check

(* VALUE CHECK generates code for value and subscript range check tuples.  *)

procedure value_check (value_chk_op: tuple; rts: rt_symbol);

var
  reg:  registers;
  low_addr, high_addr: addr_desc;

begin
  with value_chk_op^ do begin
    (* Fetch addresses of bounds. *)

    if operand[2] <> nil then
      low_addr := fetch_fullword (operand[2]);
    if operand[3] <> nil then
      high_addr := fetch_fullword (operand[3]);

    (* might as well load up the value now - the check tuple wouldn't exist if it was detectable
       as useless at compile time (but note that it might be guaranteed to fail!)  *)

    reg := nond_load (operand[1], 36);

    if operand[2] <> nil then
      if operand[3] <> nil then
        gen_rm (cam+ltc, reg, low_addr)
      else
        gen_rm (cam+gec, reg, low_addr);
    if operand[3] <> nil then
      gen_rm (cam+lec, reg, high_addr);
    gen_rt (jsp, 1, rts);

    decr_reg_usages (reg);
    if operand[2] <> nil then
      free (low_addr);
    if operand[3] <> nil then
      free (high_addr)
  end (* with value_chk_op^ *);
end;
$PAGE compatibility_check

(* COMPATIBILITY CHECK generates code for compatibility check tuples.  *)

procedure compatibility_check (compat_chk_tpl: tuple);

var
  reg: registers;

begin
  reg := do_binary_op (cam + eqc, compat_chk_tpl^.operand[1], compat_chk_tpl^.operand[2], [commutative]);
  gen_rt (jsp, 1, rt_cmp_chk);
  decr_reg_usages (reg)
end;
$PAGE pointer_check

(* POINTER CHECK generates code for a pointer or file check tuple.  Tests
   for NIL and zero are generated.  *)

procedure pointer_check (ptr_check_tpl: tuple; rts: rt_symbol);

var 
  reg: registers;

begin
  reg := nond_load (ptr_check_tpl^.operand[1], 36);
  gen (jump + eqc, reg, 0, 2, dot);
  gen_ri (cai + nec, reg, #O377777);
  gen_rt (jsp, 1, rts);
  decr_reg_usages (reg)
end;
$PAGE substring_check

(* SUBSTRING CHECK generates code for string range check tuples.  It compiles
   str_range_chk (sidx, slen, rlen) to check that:

            1 <= sidx <= sidx + slen <= rlen + 1

    The basic instruction sequence is:

        1) move sidx_reg, <sidx>
        2) move slen_reg, <slen>
        3) jumpl        slen_reg, .+5
        4) jumple       sidx_reg, .+4
        5) movei        1, -1(sidx_reg)
        6) add  1, slen_reg
        7) camle        1, <rlen>
        8) jsp  1, strer.

    If sidx and/or slen are known at compile-time, from two to five of the above eight
    instructions will be eliminated.  No attempt has been made yet to take advantage of
    rlen being constant, not that that would be a bad thing to do.  Note that the check
    tuple should not have been generated if it is known to be useless at compile_time,
    but it will be generated if its known at compile time that the check will fail.  *)

procedure substring_check (str_chk_tpl: tuple);

var
  sidx_reg,
  slen_reg: registers;
  rlen_addr: addr_desc;
  sidx_const, slen_const: boolean;
  sidx_value, slen_value: integer;

begin
  sidx_const := iconstp (str_chk_tpl^.operand[1], sidx_value) andif (sidx_value > 0);
  slen_const := iconstp (str_chk_tpl^.operand[2], slen_value) andif (slen_value >= 0);

  if sidx_const then
    free (fetch (str_chk_tpl^.operand[1]))
  else
    sidx_reg := nond_load (str_chk_tpl^.operand[1], 36);
  if slen_const then
    free (fetch (str_chk_tpl^.operand[2]))
  else
    slen_reg := nond_load (str_chk_tpl^.operand[2], 36);
  rlen_addr := fetch (str_chk_tpl^.operand[3]);
  free (rlen_addr);  (* any register allocation is already done *)

  if not slen_const then
    if sidx_const then
      gen (jump + ltc, slen_reg, 0, 4, dot)
    else
      gen (jump + ltc, slen_reg, 0, 5, dot);
  if not sidx_const then
    gen (jump + lec, sidx_reg, 0, 4, dot);

  if not sidx_const then
    gen (movei, 1, sidx_reg, -1, none)
  else if not slen_const then
    gen_ri (movei, 1, sidx_value - 1)
  else (* if both constant *)
    gen_ri (movei, 1, sidx_value + slen_value - 1);

  if not slen_const then
    gen_rr (add, 1, slen_reg)
  else if not sidx_const then
    gen_ri (addi, 1, slen_value);
  (* else both constant and the movei above did it all *)

  gen_rm (cam + lec, 1, rlen_addr);
  gen_rt (jsp, 1, rt_str_chk);

  if not sidx_const then
    decr_reg_usages (sidx_reg);
  if not slen_const then
    decr_reg_usages (slen_reg)
end;
$PAGE perform_check
(* PERFORM CHECK merely dispatches a check tuple to the appropriate routine. *)

procedure perform_check (check_tuple: tuple);

begin
  case check_tuple^.opcode of

    val_range_chk:
      value_check (check_tuple, rt_val_chk);

    file_chk:
      pointer_check (check_tuple, rt_fil_chk);

    ptr_chk:
      pointer_check (check_tuple, rt_ptr_chk);

    sub_range_chk:
      value_check (check_tuple, rt_sub_chk);

    str_range_chk:
      substring_check (check_tuple);

    compat_chk:
      compatibility_check (check_tuple);

    others:
      assert (false)

  end;
end;
$PAGE do_check
(* DO CHECK forces evaluation of a delayed check tuple, given one of its
   operands. The result field of the operand indicates its check tuple. *)

public procedure do_check (exp: expr);

var
  tpl: tuple;

begin
  tpl := exp^.result;
  exp^.result := nil;   (* to allow evaluation *)
  exp^.ref_fre := 0;
  perform_check (tpl);
end;
$PAGE attach_check_op
(* ATTACH CHECK OP links a check tuple and one of its operands for
   evaluation when the operand is to be used. If the operand has already
   been evaluated, the check is performed now. *)

procedure attach_check_op (
        check_tpl: tuple;       (* the check tuple *)
        operand: expr);         (* the operand to be tagged *)

begin
  if operand^.result <> nil then        (* already evaluated, do the check *)
    perform_check (check_tpl)
  else begin
    operand^.result := check_tpl;
    operand^.ref_fre := 1;              (* mark as linked *)
    assert (operand^.usage_count > 1)  (* verify that we're setting our hook on
                                          a tuple that has a use other than the
                                          check, so the check WILL be invoked *)
  end;
end;
$PAGE get_next_action
(* GET NEXT ACTION scans the IF from a given tuple and returns the first tuple
   which is not a "start_stmt" operator. *)

function get_next_action (tpl: tuple): tuple;
 begin
   get_next_action := tpl^.next;
   while get_next_action^.opcode = start_stmt do begin
     get_next_action := get_next_action^.next;
   end;
 end;
$PAGE compile_body
(* COMPILE BODY compiles the body of the current block.  "Stack_frame" is the
   definition node (if any) which must be defined in alc_temps as the stack
   frame size of the block. *)

procedure compile_body ( stack_frame: def )  options special(coercions);

 var
   tpl,                          (* scanning cursor for compilation *)
   temp_cursor, (* for disposing tuples *)
   label_chain, label_end, (* for preserving label_nodes until end of block *)
   lab: tuple;
   d: def;
   reg: registers;
   i: integer;
   mem: addr_desc;
   addr_ptr: ^ addr_desc;
   dump_final: boolean;

 begin
  rd_tuples;                            (* fetch the intermediate form *)

  (* If original IF dump requested, do it. *)

  if switch (cur_block^.dump_switches, 'IFM0') then
    dmptuples ('ORIGINAL INTERMEDIATE FORM FOR BLOCK $');

  make_basic_blocks;    (* explode booleans and supply labels *)
  expand_aggregates;    (* expand AGG_VALs into assignments *)

  (* Dump IF after make_basic_blocks, if requested. *)

  if switch (cur_block^.dump_switches, 'IFM') then
    dmptuples ('INTERMEDIATE FORM AFTER MAKE_BASIC_BLOCKS FOR BLOCK $');

  (* Note whether we will be dumping the final intermediate form.  If so, we'll
     have to suppress the business of deleting I/F after every statement, while
     preserving the labels on another chain.  *)

  dump_final := switch (cur_block^.dump_switches, 'FINAL'); (* going to be dumping final form? *)

  prologue;             (* fetch parameters *)

  tpl := t_chain;
  label_chain := nil; (* we'll hold onto labels until end of block *)
  while tpl <> nil do begin
    with tpl^ do begin
      case opcode of

        start_block,
        end_block:  ; (* no action required *)

        assign_op:
          begin
            case lhs^.desc.kind of
              bools, ints, chars, scalars, pointers, files:
                scalar_assignment (tpl);
              reals:
                real_assignment (tpl);
              strings:
                str_assignment (tpl);
              sets:
                set_assignment (tpl);
              procs, funcs:
                proc_func_assignment (lhs, rhs);
              arrays, records:
                agg_assignment (tpl)
            end (* case *) ;
            if reset_needed then
              reset_stack;
          end;

        start_with: begin
          with_start (with_rec);
          if reset_needed then
            reset_stack;
        end;

        end_with:
          with_end (with_rec);

        call_op: begin
          procedure_call (tpl);
          if reset_needed then
            reset_stack;
        end;

        label_node:
          begin
            d := get_def (label_def, block_order_no);
            mark_def (code_area, d);
            if (label_sym <> nil) andif (label_sym^.lab_nonlocal_use) then begin
              d := get_def (sym_def, label_sym^.id_number);
              mark_def (code_area, d);
              reset_stack; (* may be dynamic temps to free *)
            end;
	    (* Move this tuple from normal chain to special label chain. *)
	    if not dump_final then begin
	      if prev = nil then
		t_chain := next
	      else
		prev^.next := next;
	      if next <> nil then
		next^.prev := prev;
	      if label_chain = nil then
		label_chain := tpl
	      else
		label_end^.next := tpl;
	      label_end := tpl
	    end
          end;

        jump_op:
          begin
            if jump_to <> get_next_action (tpl) (* generate only if useful *)
              then gen_rl (jrst, 0, jump_to);
          end;

        jump_t_op, jump_f_op:
          begin
            tpl := tpl^.next;           (* skip to alternate jump op *)
            lab := get_next_action (tpl);       (* this may be label node *)
            if opcode = jump_t_op
              then test_and_jump (cond, jump_to, tpl^.jump_to, lab)
              else test_and_jump (cond, tpl^.jump_to, jump_to, lab);
          end;

        case_jump_op:
          case_jump (tpl);

        goto_op:
          begin
            d := get_def (sym_def, target_lab^.id_number);      (* get label definition *)
            if target_lab^.block^.owner = cur_block^.owner then
              gen (jrst, 0, 0, 0, reldef (d))
            else if target_lab^.block^.kind = program_blk then begin (* label is in main program *)
              gen_rt (pushj, sb, rt_uw_prg);
              gen (jrst, 0, 0, 0, reldef (d));
            end
            else begin (* label is in a containing routine *)
              reg := get_reg (36);
              gen (hlrz, reg, sp, 1, none);     (* get owners stack frame pointer *)
              for i := 2 to (cur_block^.apparent_level - target_lab^.block^.apparent_level) do
                gen (hlrz, reg, reg, 1, none);
              gen_rt (pushj, sb, rt_uw_rtn);    (* unwind and jump *)
              gen (jump+awc, reg, 0, 0, reldef (d));    (* reg is arg to unwind *)
              decr_reg_usages (reg);
            end;
          end;

        gen_jump_op:
          begin
            bb_end;     (* clean up temps, etc. *)
            tpl := tpl^.next;   (* presumably the jump_to label *)
            lab := tpl;         (* remember the label node *)
            tpl := tpl^.next;   (* this should be the gen_xxxif operator *)
            reg := get_reg (36);        (* get a register in which to load the value *)
            case tpl^.opcode of
              gen_andif_op:  gen (skip+awc, reg, 0, 0, gen_cint (1));
              gen_orif_op:   gen_ri (tdza, reg, reg);
              others:        assert (false)
            end;
            d := get_def (label_def, lab^.block_order_no);
            mark_def (code_area, d);
            case tpl^.opcode of
              gen_andif_op:  gen_ri (setz, reg, 0);
              gen_orif_op:   gen_ri (movei, reg, 1)
            end;
            new (addr_ptr);
            addr_ptr^ := get_temp (1);
            gen_rm (movem, reg, addr_ptr^);
            tpl^.result := ptr (ord (addr_ptr));        (* chain to orif/andif node *)
            decr_reg_usages (reg)
          end;

        dispose_op:
          begin
            mem := argument (dptrarg);
            gen_rt (pushj, sb, rt_dispose);
            gen_rm (arg, 0, arg_addr (mem));
            free (mem);
            if reset_needed then
              reset_stack;
          end;

        start_stmt:
          begin
            kill_temps;                 (* reset stack if required *)
            cur_source := stmt_source;  (* for debugging *)
            gen_source (stmt_source, stmt_index);   (* comment for assembly listing *)
            if (stmt_index = 1) andif prog_options.debug_opt then begin
              stmt_block (stmt_source, stmt_kind);
            end;
	    if not dump_final then
	      repeat
		temp_cursor := tpl^.prev;
		tpl^.next^.prev := tpl^.prev;
		tpl^.prev^.next := tpl^.next;
		dispose (tpl);
		tpl := temp_cursor
	      until (tpl^.opcode = start_block) orif
		    ( (tpl^.opcode in [first_expr..last_expr, first_chk_op..last_chk_op]) andif
		      (tpl^.usage_count <> 0) )
          end;

        stop_op:
          gen_rt (jrst, 0, rt_stop);

        return_op:
          begin
            if cur_block <> cur_block^.owner    (* if quick *)
              then gen_ri (popj, sb, 0)
              else gen_rt (jrst, 0, rt_return);
          end;

        abort_op:
          gen_rt (jsp, 1, rt_ass_chk);

        case_abort_op:
          gen_rt (jsp, 1, rt_cas_chk);

        set_handler_op,
        rst_handler_op,
        signal_op,
        mask_op,
        unmask_op,
        resignal_op,
        hndlr_jump_op,
        jump_cond_op:
          cond_handler_stmts (tpl);

        start_io_op:
          io_begins (tpl);

        end_io_op: begin
          io_ends (tpl);
          if reset_needed then
            reset_stack;
        end;

        get_op:
          if file_arg^.desc.base^.file_kind = textfile
            then rt_io_call (tpl, rt_get_char)
            else rt_io_call (tpl, rt_get);

        put_op:
          if file_arg^.desc.base^.file_kind = textfile
            then rt_io_call (tpl, rt_put_char)
            else rt_io_call (tpl, rt_put);

        readln_op:
          rt_io_call (tpl, rt_rd_lnn);

        writeln_op:
          rt_io_call (tpl, rt_wr_lnn);

        page_op:
          rt_io_call (tpl, rt_page);

        clear_op:
          rt_io_call (tpl, rt_clear);

        break_op:
          rt_io_call (tpl, rt_break);

        empty_op:
          if file_arg^.desc.base^.file_kind = textfile
            then rt_io_call (tpl, rt_empty_text)
            else rt_io_call (tpl, rt_empty);

        close_op:
          rt_io_call (tpl, rt_close);

        scratch_op:
          rt_io_call (tpl, rt_scratch);

        read_op,
        write_op:
          read_write_call (tpl);

        seek_op:
          rt_seek_call (seek_file, seek_index);

        close_all_op:
          gen_rt (pushj, sb, rt_close_all);

        val_range_chk,
        file_chk,
        ptr_chk,
        sub_range_chk,
        str_range_chk:
          attach_check_op (tpl, operand[1]);

        compat_chk: begin
          lab := operand[1]^.operand[1]; (* back over dim_op, lwb_op, or upb_op *)
          if not (lab^.usage_count > 1) then begin
            assert (lab^.opcode = field_ref); (* must be for flex record *)
            lab := lab^.base_rec
          end;
          attach_check_op (tpl, lab)
        end;

        first_expr..last_expr:
          (* no action is needed *);

        others:
          assert (false)

      end (* case *) ;
    end (* with *) ;
    tpl := tpl^.next;   (* ready for next time around while loop *)
  end;

  (* Clean up debug stuff. *)

  if prog_options.debug_opt then
    blk_end;

  (* If any dynamic temps allocated, supply temp location, otherwise
     delete the save of the stack pointer. *)

  if save17loc <> nil then (* reset_stack obtained the symbol *) begin
    def_value (save17loc, temp_max, false);
    temp_max := temp_max + 1;
  end
  else
    save17inst^.kind := nullcode; (* we finally know where to locate the temp *)

  (* If saving of with-registers was required, allocate the temp locations and
     aim the symbol definition at the high end (the block of temps is used from
     the high end down, just as with-regs are allocated from #O14 down).  *)

  if maxwith_regs > 0 then begin
    def_value (savewithaddr.reloc.reldef, temp_max + maxwith_regs - 1, false);
    temp_max := temp_max + maxwith_regs
  end;

  (* If condition handlers in procedure, allocate space for handler state blocks. *)

  if cur_block^.hndlr_depth > 0 then begin
    def_value (hsb_addr.reloc.reldef, temp_max, false);
    temp_max := temp_max + 2*cur_block^.hndlr_depth
  end;

  cur_block^.pos_stk_end := temp_max;
  if stack_frame <> nil then      (* not a quick routine *)
    def_value (stack_frame, temp_max, false); (* fill in ADJSP's value *)

  (* Verify that all registers were freed. *)

  for reg := 2 to #O14 do
    assert ( regdesc[reg].uses_remaining = 0);
  assert (with_base = #O15);

  if hbt_area.first <> nil then begin
    set_origin (hbt_area, loc_hbt);
    set_origin (code_area, loc_code);
    wr_code (hbt_area, loc_hbt, false); (* define the handler addresses *)
  end;
  emit_code (code_area, loc_code, cur_block^.semantic_options);
  if hbt_area.first <> nil then
    emit_code (hbt_area, loc_hbt, cur_block^.semantic_options);

  if dump_final then begin
    reclaim;
    dmptuples ('FINAL INTERMEDIATE FORM FOR BLOCK $')
  end;
  del_def_list (label_def);
  del_def_list (local_def);

(*
  if not dump_final then
    assert ((t_chain^.opcode = start_block) and
	    (t_chain^.next^.opcode in [return_op, stop_op, goto_op]) and
	    (t_chain^.next^.next^.opcode = end_block));
*)
  del_tuples; (* get rid of IF for this block *)
  if not dump_final then begin
    t_chain := label_chain;
    label_end^.next := nil;
    del_tuples
  end
 end (* compile_body *);
$PAGE compile_subr
(* COMPILE SUBR generates code for the body of a procedure or function.  It is
   assumed that cur_block points to the block node for the subroutine on entry. *)

procedure compile_subr;

 var
   tb, entryaddr: def;
   lev: level_index;
   quick: boolean;
   stack_frame: def;

 begin
  gen_cmt (code_area, 'Begin subroutine ' || cur_block^.subr_sym^.name^.text);

  quick := (cur_block^.owner <> cur_block);     (* entry sequence differs for quick/nonquick *)

  if not quick then begin
    if prog_options.debug_opt or (trace_opt in cur_block^.semantic_options)
      then tb := trace_block (cur_block)        (* emit the trace control block *)
      else tb := nil;

    if (not quick) and (cur_block^.level > 2) then (* may be called by parent *)
      gen (hrli, sp, sp, 0, none); (* set static = dynamic link *)
  end;

  with cur_block^.subr_sym^ do
    if public_dcl then
      gen_asm_label (code_area, name^.text);
  entryaddr := get_def (subr_def, cur_block^.number);   (* define the entry point *)
  mark_def (code_area, entryaddr);

  if not quick then begin               (* generate full entry sequence *)
    gen_rt (jsp, 1, rt_entry);
    if tb <> nil        (* trace or debug in effect *)
      then gen_xwd (0, none, 0, reldef (tb))
      else gen_xwd (0, none, int_nil, none);
    stack_frame := get_def (temp_size_def, cur_block^.number);
    gen (adjsp, sb, 0, 0, reldef (stack_frame));
  end
  else
    stack_frame := nil;

  if cur_block^.return_sym <> nil       (* if function, mark return symbol *)
    then cur_block^.return_sym^.allocated := true;      (* to prevent deletion of ref'ing code records *)


  compile_body (stack_frame);                           (* compile the body of the subroutine *)
 end;
$PAGE compile_main
(* COMPILE MAIN generates code for the body of a main program.  It is assumed that
   cur_block points to the program's block node at entry. *)

procedure compile_main (var startaddr: def);

  var
   tb: def;
   stack_frame: def;

begin
  gen_cmt (code_area, 'Begin program ' || cur_block^.id^.text);

  if prog_options.debug_opt or (trace_opt in cur_block^.semantic_options)
    then tb := trace_block (cur_block)
    else tb := nil;

  startaddr := get_def (subr_def, cur_block^.number);
  mark_def (code_area, startaddr);

  gen_ri (jfcl, 0, 0);
  gen_ri (movei, sp, prog_options.storage);
  gen_rt (jsp, 7, rt_init);
  if tb <> nil          (* trace or debug *)
    then gen_xwd (0, none, 0, reldef (tb))
    else gen_xwd (0, none, int_nil, none);
  stack_frame := get_def (temp_size_def, cur_block^.number);
  gen (adjsp, sb, 0, 0, reldef (stack_frame));

  compile_body (stack_frame);   (* generate code for body of block *)
end;
$PAGE gen_code
(* GEN CODE is the driving program for the code generator.  It directs initialization,
   generation, and termination. *)

public procedure gen_code ( var code_size, const_size, static_size: unit_range );

var loc_fp: code_address;
    lowseg_break: unit_range;
    startaddr: def; (* start address of main program, nil otherwise *)
    temp_file: text;

begin
  cur_block := root_block^.children;    (* set immediately in case of assert failure *)
  cur_source := (0, 0, 0);              (*  "       "      "   "   "   "        "    *)

  lowseg_break := size_init + size_uninit + size_cond;
  overlaid := prog_options.overlay_opt or prog_options.mainseg_opt;
  startaddr := nil;                     (* init globals in this module *)
  gen_init;     (* and others *)
  low_base := make_def (code_def);
  def_value (low_base, loc_static, true);
  high_base := make_def (code_def);
  def_value (high_base, loc_code, true);
  loc_hbt := lowseg_break;
  if assembly_opt in all_opts then
    mac_header ('DEC 10 Checkout Code Generator');
  if map_opt in all_opts then
    map_init;
  rel_init;
  init_static;

  (* First word is the module word for the debugger.  Emit it here even if this
     is a data module and doesn't contain any procedures. *)

  gen_origin (code_area, loc_code);
  deb_init; (* reserves program block *)
  emit_code (code_area, loc_code, prog_options.semantic_options);

  (* Compile the constituent blocks in reverse depth first order. *)

  cur_block := root_block;      (* look for end of call chain *)
  while cur_block^.downward_call_thread <> nil do cur_block := cur_block^.downward_call_thread;

  while cur_block <> root_block do begin        (* compile in reverse order *)
    case cur_block^.kind of 
      program_blk:    compile_main (startaddr);
      subr_blk:       compile_subr
    end;
    cur_block := cur_block^.upward_call_thread;
  end;
  cur_block := root_block^.children; (* in case of assertion failure *)

  (* Final cleanup *)

  pool_constants;
  if overlaid then begin
    loc_cst := loc_hbt; (* emit consts in lowseg for overlay compilation *)
    set_origin (cst_area, loc_cst);
  end
  else
    loc_cst := loc_code; (* normally, consts go at end of code area *)
  emit_code (cst_area, loc_cst, prog_options.semantic_options);
  emit_code (bptr_area, loc_cst, prog_options.semantic_options);
  emit_code (blt_area, loc_cst, prog_options.semantic_options);
  if prog_options.debug_opt then begin
    if overlaid
      then loc_fp := loc_code
      else loc_fp := loc_cst;
    gen_origin (code_area, loc_fp);
    fp_blocks;  (* dump page and file blocks in code area *)
    emit_code (code_area, loc_fp, prog_options.semantic_options);
    deb_stable; (* dump symbol table *)
    prog_block;
  end
  else if rel_file <> '' then begin
    reset (temp_file, rel_file || '.DEB[,]');
    scratch (temp_file)
  end;

  if prog_options.debug_opt then begin
    if overlaid
      then code_size := loc_fp - #O400000
      else code_size := (loc_fp - loc_cst) + (loc_code - #O400000);
  end
  else
    code_size := loc_code - #O400000;
  if overlaid
    then const_size := loc_cst - lowseg_break
    else const_size := loc_cst - loc_code;
  static_size := lowseg_break;

  if assembly_opt in all_opts then
    mac_end (startaddr, code_size, const_size, static_size);    (* terminate assembly listing *)
  if overlaid and prog_options.debug_opt then
    rel_end (startaddr, loc_cst, loc_fp)
  else if overlaid and not prog_options.debug_opt then
    rel_end (startaddr, loc_cst, loc_code)
  else if not overlaid and prog_options.debug_opt then
    rel_end (startaddr, lowseg_break, loc_fp)
  else
    rel_end (startaddr, lowseg_break, loc_code);
  gen_term;
  dmp_close;
  if map_opt in all_opts then begin
    map_close;
    map_print;
  end;
end.
 @9é