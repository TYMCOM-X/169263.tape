$TITLE VAXGEN - VAX quick pass code generator

module vaxgen;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM vaxcgu.inc
$SYSTEM vaxmac.inc
$SYSTEM vaxopc.inc
$SYSTEM pasifu.inc
$SYSTEM vaxexp.inc
$SYSTEM vaxcll.inc
$SYSTEM vaxcmp.inc
$SYSTEM vaxstr.inc
$system vaxset.inc
$SYSTEM vaxrel.inc
$SYSTEM vaxcsp.inc
$SYSTEM vaxdeb.inc
$SYSTEM pasjmp.inc
$SYSTEM pastal.inc
$SYSTEM passw.inc
$SYSTEM pa2dmp.inc
$SYSTEM pasdmp.inc
$system vaxutl.inc
$system vaxio.inc
$system pasmth.inc
$system pascv.inc
$SYSTEM pasmap.inc
$system ptmcon.inc
$PAGE global variables

public var

  p4_error_level: error_levels; (* max severity of any pass4 error *)

static var

  temp_base: unit_range;        (* top of temps in local stack frame *)

  temp_max: unit_range;         (* offset of max allocated temps *)

  stack_fixup: code;            (* for resolution of stack frame size *)


  ANY_DYNAMICS : Boolean;       (* Any dynamic temporaries for this statement ? *)

  DYN_DEF : DEF;                (* Definition record used to clean up the
                                   stack because of dynamic temps *)

  have_code_origin : boolean;   (* Origin emitted in code psect *)

type
  attr_array = array[psect_type] of set of psect_attributes;

public const
  psectattrs: attr_array := (
        (* static_psect *)
        [longword_aligned,concatenate,position_independent,readable,writeable,relocatable],
        (* code psect *)
        [longword_aligned,concatenate,executable,position_independent,shareable,readable,relocatable]);

public var
  psect_id: array[psect_type] of byte;
$PAGE fatal_error

(* FATAL_ERROR is called in the event of an unrecoverable error in
   PASS4.  (Currently this means either register overflow or a single
   procedure so large it requires branch displacements larger than
   16 bits).  *)

public procedure fatal_error ( message: string );

begin
  (* Write an error message to the terminal *)

  writeln ( tty, '?Fatal error at ', cv_source_id ( cur_source ) );
  writeln ( tty, '   - ', message );
  break ( tty );

  (* If an object file is being written then close and delete it
     (leaving any previous version intact).  *)

  p4_error_level := fatals;
  rel_end ( nil, 0, 0 );

  stop;
end  (* proc fatal_error *) ;
$PAGE get_temp
public function get_temp (desc: addr_desc; vax_alignment: vax_type): addr_desc;

(* Allocate a temporary of the specified size and alignment in the local stack frame. *)

type
  size_array = array[vax_type] of bit_range;

const
  vax_size: size_array := (1, 2, 4, 4, 4, 4);

Var TEMP_DESC : ADDR_DESC;      (* Must have SP into a longword register *)
    LEN : integer;             (* For return from ACONSTP, size of static *)

begin

  (* IF the address descriptor is IMMEDIATE then allocate a static temp, else
     allocate a dynamic temp *)

  If ACONSTP ( DESC , LEN )
    Then Begin
      get_temp := temp_reference;
      temp_base := ngm (abs(cur_block^.neg_stk_end) + temp_base + LEN ,
                   vax_size[vax_alignment]) + cur_block^.neg_stk_end;
      temp_max := max (temp_max, temp_base);
      get_temp.offset := cur_block^.neg_stk_end - temp_base;
    End
  Else Begin
    ANY_DYNAMICS := True;       (* Set the static flag so the stack may be reset *)
    GET_TEMP := GET_TYP_REG_ADDR ( VAX_LONG );  (* REG fr the temp *)

    (* Now fix the stack pointer using address descriptor passed to 
       this routine. It contains the size of the temp. SUBL2 <size>,SP *)

    TEMP_DESC := CVT_LONG ( DUPLICATE_ADDR ( DESC ) , UNSIGNED_VALUE );
    GEN2 ( SUBL2 , TEMP_DESC , SP_ADDR );
    Free ( TEMP_DESC );

    (* Now generate address of temp into a register, MOVAB (SP),Rn *)

    GEN2 ( MOVAB , STACK_TOP_REFERENCE , GET_TEMP );

    (* Change return value so it is (Rn) and not Rn *)

    GET_TEMP.OFFSET := 0;               (* Insurance *)
    GET_TEMP.RELOC.KIND := ABSOLUTE_SC
  End
end;
$PAGE kill_temps

(* KILL_TEMPS resets the staic temporary offset, thus effectively
   deallocating any active static temps.  *)

procedure kill_temps;

begin
  temp_base := 0;       (* no temps allocated *)
end;
$PAGE ovly_emit_indirect_words
(* This routine generates the code records for the indirect words used to
   access public consts and vars in overlay compilations. This routine is called
   from gen_code in vaxgen, after constant pooling has beeen done. *)

procedure ovly_emit_indirect_words
  options special(coercions);

var symbol : sym;

begin

  (* generate a comment code record to indicate that this is the beginning
     of the indirect words for overlay compilations *)

  gen_cmt ( cst_area , 'Indirect words for an overlay compilation.' );

  (* Walk the symbol nodes looking for candidates for the indirect words *)

  symbol := root_block^.children^.id_list.first;

  while symbol <> nil do
    with symbol^ do begin
      if public_dcl andif
      ((kind = vars ) or ((kind=consts) andif ( init_value.kind = alloc_cst)))
        then genindirect_word ( cst_area, symbol );

      symbol := next
    end;                (* with symbol^ do *)

end;            (* ovly_emit_indirect_words *)
$PAGE bb_start, reset_stack

(* BB_START performs processing which must be done on entry to
   each basic block (i.e., at each LABLEL_NODE tuple).  In
   particular it generates an instruction on entry to each basic
   block which deallocates any active dynamic temps.  If the label
   is the target of a non_local goto or the label is already
   known to be the target of a branch out of a block which
   allocated dynamic temps, then the code record is simply
   generated normally.  Otherwise, the deallocation may in fact be
   unnecessary so the KIND of the code record is set to NULLCODE.
   If the deallocation turns out to be necessary, then at the end
   of some other basic block which this one succeeds, the kind of the
   code record will be reset to INSTRUCTION.  The flag indicating whether
   dynamic temps were allocated in the current basic block is 
   always reset by this routine.  *)
  
  
procedure reset_stack;
  var
    restore_addr : addr_desc;
  begin
    restore_addr := fp_addr;
    restore_addr.reloc.kind := def_sc;
    if dyn_def = nil                    (* static cell for defn ptr *)
      then dyn_def := make_def ( temp_size_def );
    restore_addr.reloc.reldef := dyn_def;
    gen2 ( moval, restore_addr, sp_addr )
  end;
  
  
procedure bb_start ( label_node_tuple: tuple )  options special(coercions);

begin

  (* Reset the flag indicating whether dynamic temps have been allocated
     in the current basic block.  *)

  any_dynamics := false;

  (* Generate an instruction to deallocate dynamic temps.  Note
     that the stack offset in the restore address is a forward
     reference.  The value is represented here by a definition 
     record and resolved at the end of the compilation of the
     current routine. *)

  reset_stack;

  (* If the basic block is either the target of a non-local goto
     or is already known to be the successor of a block which uses
     dynamic temps then leave the code record alone; otherwise mark
     it as KIND NULLCODE.  Note that we overload the DOM_SON field
     of the LABEL_NODE and use it as a boolean flag indicating
     whether or not the block is the target of a branch from a block
     using dynamic temps.  This field is only used when the jump
     tuple is processed before the LABEL_NODE tuple.  The IDOM
     field of the LABEL_NODE tuple is also overloaded; it is
     coerced to point to the code record allocated.  *)

  with label_node_tuple^ do begin
    idom := tuple ( code_area.last );
    if ( (label_sym = NIL) orif (not label_sym^.lab_nonlocal_use) ) and
       ( dom_son = nil ) then
      code_area.last^.kind := nullcode;
  end  (* with *) ;

end  (* proc bb_start *) ;
$PAGE bb_end

(* BB_END does processing necessary at the end of a basic block.
   In particular, if dynamic temps were used in the basic block
   terminated by the jump, then the label node for the basic
   block being branched to by the jump is flagged as requiring
   dynamic temp deallocation.  This 'flagging' is done in one of
   two ways.  If the label node for the target has already been
   processed then the IDOM field of the LABEL_NODE has been 
   coerced to point to the code record for the deallocation
   instruction;  in this case we set the KIND of that code record
   to INSTRUCTION (it was NULLCODE).  If the LABEL_NODE for the
   target has not yet been processed, then we flag the LABEL_NODE
   by setting its DOM_SON field to a non-nil value.  *)

procedure bb_end ( jump_tuple: tuple )  options special(coercions);

begin
  if any_dynamics then begin
    with jump_tuple^.jump_to^ do begin
      assert ( opcode = label_node );
      if idom = nil then begin
        dom_son := jump_tuple           (* set to any non-nil value *)
      end
      else begin
        code (idom)^.kind := instruction;
      end;
    end  (* with *);
  end  (* if *) ;
end  (* proc bb_end *) ;
$PAGE emit_code
(* EMIT CODE takes a code list.  It will write it to the rel file, and to
   the macro listing if assembly_opt is in the options_list parameter. *)

procedure emit_code ( var area_list: code_list; (* the code area to be written *)
                      var ic: unit_range; (* the address to write the code at *)
                      psect: byte;      (* psect id of the code area *)
                      options_list: set_of_options ); (* to control listing *)

 begin
  if assembly_opt in options_list then
    mac_list (area_list, ic );
  wr_code (area_list, ic, psect);
 end;
$PAGE init_static
(* INIT STATIC generates code for the initialized part of the static area, and
   for the declared public constants.  Symbols are processed in the order:
        Symbols within the id list of a block
        Blocks in downward_call_thread order
   This is the order in which they are processed during storage allocation. *)

procedure init_static (static_size: unit_range)  options special(coercions);

 var
   block: blk;
   symbols: sym;
   reld: reloc_value;
   id: byte;
   static_counter, actual_size, tsize: bit_range;
   talign: align_range;

 begin

  if static_size > 0 then       (* init psect *)
    gen_origin (static_area, static_psect);
  static_counter := 0;  (* Track size of emitted constants *)
  block := root_block;

  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
        with symbols^ do begin
          if (kind = vars) andif (dcl_class = static_sc) andif
             (init_value.kind <> no_value) then begin

            (* Force alignment *)

            alc_data (type_desc, tsize, talign);
            skip_bytes (static_area, static_counter, ngm (static_counter, talign));
            assert ((item_addr * bits_per_byte) = static_counter); (* verify assumption *)
            gen_val (static_area, init_value, type_desc);
            if (init_value.kind = string_cst) andif (init_value.valp^.str_varying_ref) then begin

              (* Pad out to full declared length *)

              actual_size := str_lw_width + length (init_value.valp^.str_val) * char_size;
              skip_bytes ( static_area, actual_size , type_desc^.base_size );
            end;
            static_counter := static_counter + tsize;
          end
          else if (kind = consts) andif (init_value.kind <> subr_cst) andif
                  ( public_dcl or
                    ( prog_options.debug_opt and
                      (dcl_class <> external_sc) ) ) then begin
            if not have_code_origin then begin
              gen_origin (cst_area, code_psect);
              have_code_origin := true;
            end;
            reld := gen_cval (init_value, type_desc);
            init_value.kind := alloc_cst;
            init_value.defp := val_ptr (reld.reldef);
          end;
          symbols := next;
        end;
      end (* while symbols <> nil *);
      block := downward_call_thread;
    end;
  end (* while block <> nil *);

  skip_bytes (static_area, static_counter, ngm (static_counter, bits_per_unit));
  block := root_block;
  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
        with symbols^ do begin
          if (kind = conditions) andif (dcl_class = static_sc) then begin
            alc_data (type_desc, tsize, talign);
            skip_bytes (static_area, static_counter, ngm (static_counter, talign));
            assert (((item_addr + size_init) * bits_per_byte) = static_counter);
            genindirect_word (static_area, symbols);
            static_counter := static_counter + bits_per_address
          end;
          symbols := next;
        end;
      end (* while symbols <> nil *);
      block := downward_call_thread;
    end;
  end (* while block <> nil *);
  emit_code (static_area, loc_static, psect_id [static_psect], prog_options.semantic_options);
  assert (ngm (loc_static, bytes_per_unit) = (size_init + size_cond));
  if (static_size > loc_static) and (assembly_opt in prog_options.semantic_options) then
    mac_pad (static_size - loc_static);  (* put .BLKB for unitialized static area into macro listing *)
 end;
$PAGE store
(* STORE moves a value to a destination, with truncation or
   expansion if necessary. Both source and destination must be
   no larger than a quadword. "Alignment" parameter determines
   if sign extension should be performed.       *)

public procedure store (source, dest: addr_desc; alignment: data_alignment);
var
  op: opc_range;
  cval: integer;
  new_source: addr_desc;

type
  ops_array = packed array[vax_type,vax_type,data_alignment] of opc_range;

const
  convert_ops: ops_array := (

        (* byte destination *)  (

        (movb,          movb,           movb),  (* byte source *)
        (movb,          movb,           movb),  (* word source *)
        (movb,          movb,           movb),  (* long source *)
        (halt,          halt,           halt),  (* quad source *)
        (cvtfb,         cvtfb,          cvtfb), (* real source *)
        (cvtdb,         cvtdb,          cvtdb)),(* dblr source *)

        (* word destination *)  (

        (movzbw,        cvtbw,          movzbw),(* byte source *)
        (movw,          movw,           movw),  (* word source *)
        (movw,          movw,           movw),  (* long source *)
        (halt,          halt,           halt),  (* quad source *)
        (cvtfw,         cvtfw,          cvtfw), (* real source *)
        (cvtdw,         cvtdw,          cvtdw)),(* dblr source *)

        (* long destination *)  (

        (movzbl,        cvtbl,          movzbl),(* byte source *)
        (movzwl,        cvtwl,          movzwl),(* word source *)
        (movl,          movl,           movl),  (* long source *)
        (halt,          halt,           halt),  (* quad source *)
        (cvtfl,         cvtfl,          cvtfl), (* real source *)
        (cvtdl,         cvtdl,          cvtdl)),(* dblr source *)

        (* quad destination *)  (

        (halt,          halt,           halt),  (* byte source *)
        (halt,          halt,           halt),  (* word source *)
        (halt,          halt,           halt),  (* long source *)
        (movq,          movq,           movq),  (* quad source *)
        (halt,          halt,           halt),  (* real source *)
        (halt,          halt,           halt)), (* dblr source *)

        (* single real destination *)   (

        (cvtbf,         cvtbf,          cvtbf), (* byte source *)
        (cvtwf,         cvtwf,          cvtwf), (* word source *)
        (cvtlf,         cvtlf,          cvtlf), (* long source *)
        (halt,          halt,           halt),  (* quad source *)
        (movf,          movf,           movf),  (* real source *)
        (cvtdf,         cvtdf,          cvtdf)),(* dblr source *)

        (* double real destination *)   (

        (cvtbd,         cvtbd,          cvtbd), (* byte source *)
        (cvtwd,         cvtwd,          cvtwd), (* word source *)
        (cvtld,         cvtld,          cvtld), (* long source *)
        (halt,          halt,           halt),  (* quad source *)
        (cvtfd,         cvtfd,          cvtfd), (* real source *)
        (movd,          movd,           movd)));(* dblr source *)

begin
  if adr_equal (source, dest) then
    return;
  if aconstp (source, cval) then
    move_immediate (cval, dest)
  else begin
    
    (* if the source is unsigned, indexed and larger than the
       destination then we first move the source to a volatile 
       register so that the index calculation is correct.  *)

    if (source.index <> noreg) and
       (alignment <> signed_value) and
       (vax_type_size(source.byte_size) > vax_type_size(dest.byte_size)) then begin
      gen2(typ_opc(movl,source.byte_size), source, r0_addr);
      new_source := r0_addr;
      new_source.byte_size := source.byte_size;
    end
    else new_source := source;

    op := convert_ops [dest.byte_size, new_source.byte_size, alignment];
    assert (op <> halt);
    gen2 (op, new_source, dest);
  end;
end;
$PAGE scalar_assignment
(* SCALAR ASSIGNMENT generates code for assignments of scalar values. *)

procedure scalar_assignment (tpl: tuple);

 var
   laddr, raddr: addr_desc;

 begin
  with tpl^ do begin
    laddr := fetch ( lhs , no_preference );
    raddr := fetch ( rhs , laddr );

    (* check to see if the fetch was able to fulfill the target request.
       If so, just free the address descriptor. If not, then move the
       result into the left hand side. *)

    if not adr_equal ( raddr , laddr )
      then begin
        store ( raddr , laddr , alignment ( rhs ) );
        free ( laddr );
      end;
    free ( raddr );nd;
end (* scalar_assignment *);
$PAGE proc_func_assignment
(* PROC FUNC ASSIGNMENT generates assignment to procedure and function variables. *)

procedure proc_func_assignment (tpl: tuple);

var
  target, source, addr: addr_desc;
  i, levels_dif: integer;
  no_parent: boolean;
  reg: registers;

begin
  with tpl^ do begin
    source := fetch (rhs,no_preference);

    (* If the source is another procedure/function variable it can simply be
       moved to the destination. *)

    if (source.reloc.kind in [local_sc,parameter_sc,static_sc]) orif
      (source.reloc.kind = external_sc) andif (source.reloc.relsym^.kind <> consts) then begin
        target := fetch (lhs,no_preference);
        gen2 (movq, source, target);
    end
    else begin
      target := argument (lhs); (* must not be indirect *)

      (* Determine if the routine has a parent. *)

      no_parent := (source.reloc.kind = external_sc) orif (rhs^.cst_val.blkp^.apparent_level <= 2);

      if target.index <> noreg then begin       (* load address *)
        free (target);
        reg := get_reg (bits_per_unit);
        gen_mr (movaq, target, reg);
        target := absolute_reference;
        target.register := reg;
      end;

      (* Move the routine's address. *)

      gen2 (moval, source, off_addr (target, bytes_per_unit));

      if no_parent then
        gen1 (clrl, target)
      else begin
        levels_dif := cur_block^.apparent_level - rhs^.cst_val.blkp^.apparent_level;
        if levels_dif = -1 then
          gen2 (movl, fp_addr, target)
        else begin
          addr := absolute_reference;
          addr.register := fp;
          addr.offset := -bytes_per_unit;
          if levels_dif = 0 then
            gen2 (movl, addr, target)
          else begin
            gen2 (movl, addr, r0_addr);
            addr := push_reference;
            addr.register := r0;
            for i := 1 to levels_dif - 1 do
              gen2 (movl, addr, r0_addr);
            gen2 (movl, addr, target);
          end;
        end;
      end;
    end;
    free (target);
    free (source);
  end (* with *);
end (* proc_func_assignment *);
$PAGE block_move

(* BLOCK_MOVE moves a specified number of bytes of uninterpreted
   data from one location to another.  Paramerer LEN_ADDR is an 
   address descriptor for the length in bytes of the block;
   SOURCE_ADDR is the base address of the source and DEST_ADDR
   is the base address of the destination.  

   Note that:
     1. This routine does not FREE any address descriptors,
     2. Unless the length is constant and in the set [0,1,2,4,8]
        a MOVC3 instruction is generated.  This instruction
        assumes a data type of BYTE for it source and destination
        addresses, i.e., do not pass in an immediate or indexed
        source or destination address unless its BYTE_SIZE field
        is VAX_BYTE.
     3. Similarly, the BYTE_SIZE field of the address descriptor
        for the length of the block must be VAX_WORD if the operand
        is an immediate or indexed.  *)

procedure block_move ( len_addr, source_addr, dest_addr: addr_desc );

var
  length_value: unit_range;
  regs_saved: set_of_registers;

begin

  (* If possible a two operand move instruction is used.  *)

  if aconstp ( len_addr, length_value ) andif
     ( length_value in [0, 1, 2, 4, 8] ) then begin
    
    case length_value of
      0:        ;
      1:        gen2 ( movb, source_addr, dest_addr );
      2:        gen2 ( movw, source_addr, dest_addr );
      4:        gen2 ( movl, source_addr, dest_addr );
      8:        gen2 ( movq, source_addr, dest_addr )
    end  (* case *) ;

  end

  (* Otherwise use the MOVC3 *)

  else begin
    regs_saved := save_regs ( [R0..R5] );
    gen3 ( movc3, len_addr, source_addr, dest_addr );
    restore_regs ( regs_saved );
    mark_regs_used ( [R0..R5] );
  end;
end  (* proc block_move *) ;
$PAGE agg_assignment
(* AGG ASSIGNMENT generates code to assign one record or array to another. *)

procedure agg_assignment (tpl: tuple);

var
  raddr, laddr, upb: addr_desc;
  i: integer;
  byte_offset, elem_size: unit_range;
  field: sym;
  field_offset: unit_range;

begin
  with tpl^ do begin
    raddr := fetch (rhs,no_preference);
    laddr := fetch (lhs,no_preference);

    (* Record assignment.  *)

    if rhs^.desc.kind = records then begin
      if rhs^.desc.base^.flexible then begin    (* ends with flex structure *)
        field := rhs^.desc.base^.field_list;
        while field^.next <> nil do     (* find last (flex) field *)
          field := field^.next;
        field_offset := field^.fld_offset div bits_per_byte;
        upb := increment_addr (duplicate_addr (raddr), field_offset);
        if field^.type_desc^.kind = strings then begin
          if field^.type_desc^.str_kind = varying then
            field_offset := field_offset + (str_lw_width div bits_per_byte);
          gen3 (addw3, int_value (field_offset + flex_str_desc_size div bits_per_byte),
                        upb, r1_addr);
        end
        else begin
          byte_offset := field^.type_desc^.index_type^.minval - 1;
          elem_size := field^.type_desc^.element_size div bits_per_byte;
          if byte_offset <> 0 then begin
            if byte_offset > 0
              then gen3 (subl3, int_value (byte_offset), upb, r1_addr)
              else gen3 (addl3, int_value (-byte_offset), upb, r1_addr);
            if elem_size > 1 then
              gen2 (mull2, int_value (elem_size), r1_addr)
          end
          else if elem_size > 1 then
            gen3 (mull3, int_value (elem_size), upb, r1_addr)
          else gen2 (movl, upb, r1_addr);
          byte_offset := field_offset + flex_arr_desc_size div bits_per_byte;
          gen2 (addl2, int_value (byte_offset), r1_addr);
        end;
        block_move ( r1_addr, raddr, laddr);
        free (upb);
      end
      else block_move ( typ_int_value (ngm (rhs^.desc.base^.base_size, bits_per_byte)
                 div bits_per_byte, vax_word), raddr, laddr)
    end

    (* Array assignment.  *)

    else begin  (* array assignment *)
      if dynamic_flex (lhs) then begin  (* target on heap *)
        laddr := increment_addr ( laddr, flex_arr_desc_size div byte_size );    (* skip upb word *)
        upb := upper_bound (rhs, raddr);        (* upb of rhs *)
        if dynamic_flex (rhs) orif (rhs^.opcode = ident_ref) andif
         (rhs^.desc.base^.flexible) then begin  (* on heap or parameter *)
          if dynamic_flex ( rhs ) then
            raddr := increment_addr ( raddr, flex_arr_desc_size div byte_size );
          byte_offset := rhs^.desc.base^.index_type^.minval - 1;
          elem_size := rhs^.desc.base^.element_size div bits_per_byte;
          if byte_offset <> 0 then begin        (* correct by lowerbound *)
            if byte_offset > 0
              then gen3 (subl3, int_value (byte_offset), upb, r1_addr)
              else gen3 (addl3, int_value (-byte_offset), upb, r1_addr);
            if elem_size > 1 then
              gen2 (mull2, int_value (elem_size), r1_addr);
          end
          else if elem_size > 1 then
            gen3 (mull3, int_value (elem_size), upb, r1_addr)
          else gen2 (movl, upb, r1_addr);
          block_move ( r1_addr, raddr, laddr);
        end
        else begin      (* fixed upb array *)
          block_move ( typ_int_value (rhs^.desc.base^.base_size div bits_per_byte, vax_word), raddr, laddr);
        end;
        free (upb);
      end (* target flexible *)
      else begin        (* array with non-flex lhs *)
        if dynamic_flex (rhs)
          then raddr := increment_addr ( raddr, flex_arr_desc_size div byte_size );
        block_move ( typ_int_value (lhs^.desc.base^.base_size div bits_per_byte, vax_word), raddr, laddr);
      end;
    end (* array assignment *);
    free (raddr);
    free (laddr);
  end;
end;
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
            tpl_match (t1^.base_rec, base_rec) andif
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
        r = r1 - r2          => Circumscribes (r, l) or Circumscribes (r0 l)
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
$PAGE save_ap

(* SAVE_AP returns a boolean value indicating whether or not
   the argument pointer will be saved upon entry to the routine
   corresponding to parameter BLOCK. *)

function save_ap ( block: blk ): boolean;

begin
  with block^ do begin
    save_ap := (kind = subr_blk) and
               (children <> nil) and
               ( (parm_list.first <> nil) or
                 ( (return_sym <> nil) andif passed_by_address(return_sym) ) );
  end;
end  (* func save_ap *) ;
$PAGE prologue
(* PROLOGUE emits the entry sequence of a procedure or function. *)

procedure prologue (var addr: def);

var
  tb: def;

begin
  with cur_block^ do begin

    if prog_options.debug_opt or (trace_opt in cur_block^.semantic_options)
      then tb := trace_block    (* mark its address *)
      else tb := nil;

    (* mark the start address *)

    addr := get_def (subr_def, number);
    mark_def (code_area, addr);

    (* Emit the register save mask *)

    gen_mask (cur_block);

    (* Set the static link. *)

    if apparent_level <= 2
      then gen1 (clrl, push_reference)  (* no parent *)
      else gen1 (pushl, r0_addr);
    
    (* Set the traceblock pointer. *)

    if tb = nil
      then gen1 ( clrl, push_reference )
      else gen1 (pushal, def_addr (tb));

    (* if this block has children AND parameters, then save the
       parameter list address for use by inner routines. *)

    if save_ap ( cur_block ) then gen1 ( pushl, ap_addr );

    (* Allocate stack frame (size resolved later). *)

    gen2 (subl2, int_value (0), sp_addr);

    (* mark for fixup of allocated stack size *)

    stack_fixup := code_area.last;
    if kind = program_blk then
      gen1 (jsb, rt_addr (rt_main_frame));
      
    (* allocate handler state blocks *)
  
    neg_stk_end := neg_stk_end - 8 * hndlr_depth;
  end;
  reg_init;     (* mark all regs as free *)
  temp_max := 0;        (* no temps allocated yet *)
  DYN_DEF := Nil;       (* Pointer to save SP fixup value for dynamic temps *)
  ANY_DYNAMICS := False;        (* True if dynamic temps were used *)
end;
$PAGE case_jump
(* CASE JUMP generates code for a case jump operator and following jump in operators.
   The parameter "tpl" points to the case operator on entry; on return, it is
   set to the last jump in operator. *)

procedure case_jump (var tpl: tuple);

 var
   jmp: tuple;
   i: integer;
   addr: addr_desc;
   d: def;

 begin
  with tpl^ do begin
    addr := fetch_fullword (cond);
    bb_end ( tpl );

    (* If next^.opcode <> jump_in_op, then there are no  (non "others") cases
        so we do nothing. *)

    if next^.opcode = jump_in_op then begin     (* 1 case at least *)

      (* Generate the CASE instruction. *)

      gen3 (casel, addr, int_value (low_cntrl), int_value (high_cntrl-low_cntrl));

      (* offset of from start of displacement list *)

      d := make_def (local_def);
      mark_def (code_area, d);

      jmp := next;

      (* For each iteration of the following loop, we generate the displacements
         for a label range of the form 'n..m:' and any displacements from
         'm' to the start of the next explicitly given range.  *)

      loop              (* over jump_in_op's, assume at least one *)

        (* Generate displacements for a label of the form 'i..j'. *)

        bb_end ( jmp );
        for i := jmp^.low_cntrl to jmp^.high_cntrl do
          gen_displacement (d, get_def (label_def, jmp^.jump_to^.block_order_no), 2 * byte_size);
      exit if jmp^.next^.opcode <> jump_in_op;
        jmp := jmp^.next;

        (* If there is a gap between the last label range and the next one,
           then generate displacements to the 'others' case till the gap
           is filled. *)

        for i := jmp^.prev^.high_cntrl + 1 to jmp^.low_cntrl - 1 do
          gen_displacement (d, get_def (label_def, tpl^.jump_to^.block_order_no), 2 * byte_size);
      end;

      if jump_to <> nil (* at end of table gen branch to 'others' case, if present *)
        then gen_branch ( brb, get_def ( label_def, jump_to^.block_order_no ) );

      tpl := jmp;                       (* advance to last jump in op *)
    end;                (* If ... = jump_in_op then begin *)
  end (* with *) ;
  free (addr);
 end;
$PAGE cond_handler_stmts
(* COND HNDLR STMTS generates code for the statement tuples associated with
   condition handling.  *)
  
procedure cond_handler_stmts (var tpl: tuple);
 
var
  addr: addr_desc;
  jump: tuple;
  defn: def;
  
begin
  with tpl^ do
    case opcode of
 
      set_handler_op, rst_handler_op: begin
        if hndlr_tuple <> nil then
          push_address (def_addr (get_def (hnd_tab_def, hndlr_tuple^.block_order_no)))
        else
          push_value (int_value (0), signed_value);
        if opcode = set_handler_op then
          gen_rt (1, rt_set_handler)
        else
          gen_rt (1, rt_rst_handler)
      end;
 
      signal_op: begin
        addr := fetch_fullword (cond_parm);
        push_address (addr);
        gen_rt (1, rt_signal)
      end;
 
      mask_op: begin
        addr := fetch_fullword (cond_parm);
        push_address (addr);
        gen_rt (1, rt_mask)
      end;
 
      unmask_op: begin
        addr := fetch_fullword (cond_parm);
        push_address (addr);
        gen_rt (1, rt_unmask)
      end;
 
      resignal_op:
        gen_rt (0, rt_resignal);
 
      hndlr_jump_op: begin
  
        (* handler common entry code *)
  
        reset_stack; 
        addr := r0_addr;
        addr.reloc := (absolute_sc);
        gen1 (jmp, addr); (* jmp  0(r0) *)
  
        (* handler branch table *)
  
        mark_def (code_area, get_def (hnd_tab_def, jump_from^.block_order_no)); (* satisfy set_handler *)
        gen_mask (nil);
        gen1 (jsb, rt_addr (rt_exc_vaxcond));
  
        defn := make_def (local_def);
        mark_def (code_area, defn); (* mark loc of pointer to encompassing hbt *)
        if jump_from^.in_handler = nil then
          gen_word (code_area, 0) (* no outer handler *)
        else
          gen_displacement (defn, get_def (hnd_tab_def, jump_from^.in_handler^.block_order_no),
                            2 * byte_size);
 
        gen_word (code_area, cur_block^.neg_stk_end + (high_cntrl - 1) * 8); (* hsb's offset *)
  
        defn := make_def (local_def);
        mark_def (code_area, defn); (* mark loc of pointer to common code *)
        gen_displacement (defn, get_def (label_def, jump_from^.block_order_no), 2 * byte_size);
  
        jump := tpl;
        while jump^.next^.opcode = jump_cond_op do begin
          jump := jump^.next;
          addr := fetch_fullword (jump^.cond);
          assert (addr.reloc.kind in [static_sc, external_sc]);
          defn := make_def (local_def);
          mark_def (code_area, defn); (* mark loc of condition *)
          if addr.reloc.kind = static_sc then
            gen_displacement (defn, get_ext_or_stat (static_def, addr.reloc.relsym), 4 * byte_size)
          else if (* addr.reloc.kind = external_sc and *) addr.reloc.relsym^.standard then 
            genindirect_word (code_area, addr.reloc.relsym)
          else (* .kind = external_sc and not relsym^.standard *)
            gen_displacement (defn, get_ext_or_stat (extern_def, addr.reloc.relsym), 4 * byte_size);
          defn := make_def (local_def);
          mark_def (code_area, defn); (* mark loc of pointer to handler *)
          gen_displacement (defn, get_def (label_def, jump^.jump_to^.block_order_no),
                            2 * byte_size);
        end;
  
        if jump_to^.next^.opcode = resignal_op then begin
          gen_longword (code_area, -1);
          gen_word (code_area, 0)
        end
        else begin
          if low_cntrl = 0 then
            gen_longword (code_area, -1)
          else
            gen_longword (code_area, -2);
          assert (jump_to^.opcode = label_node);
          defn := make_def (local_def);
          mark_def (code_area, defn); (* mark loc of pointer to others or allcond. handler *)
          gen_displacement (defn, get_def (label_def, jump_to^.block_order_no), 2 * byte_size)
        end;
        tpl := jump; (* advance to last jump_cond_op *)
      end;
  
      others:
        assert (false)
 
    end (* case opcode *);
end (* cond_handler_stmts *);
$PAGE rt_seek_call
(* RT SEEK CALL generates a call to the runtime file random access routine. *)

procedure rt_seek_call ( skfil: expr; ind: expr );

 begin
  push_value ( fetch ( skfil , no_preference ) , alignment ( skfil ) );
  push_value (fetch (ind,no_preference), alignment (ind));
  gen_rt (1, rt_seek);
 end;
$PAGE value_check
 (* VALUE_CHECK generates code for value (subrange) check tuples.
    Value check tuples have 3 operands: the value, the low limit
    and the high limit.  The low and high limits are constants,
    though this code does not depend on it.  Currently no value
    check ops are generated for real subranges, though this
    code does not make that assumption either.  *)

procedure value_check ( value_chk_op: tuple );

var
  value_addr, low_addr, high_addr: addr_desc;
  vtype: vax_type;
  err_call_label, low_ok_label, high_ok_label: def;
  low_value, high_value: integer;

begin
  with value_chk_op^ do begin

    assert ( upperbound (operand) = 3 );
    value_addr := fetch_fullword ( operand[1] );        (* fetch operands *)
    vtype := value_addr.byte_size;      (* we assume all operands are *)
    if operand[2] <> nil then begin
      low_addr := fetch_fullword ( operand[2] );
      assert ( vtype = low_addr.byte_size )     (* of the same VAX_TYPE *)
    end;
    if operand[3] <> nil then begin
      high_addr := fetch_fullword ( operand[3] );
      assert ( vtype = high_addr.byte_size )
    end;

    if operand[2] <> nil then begin
      if aconstp (low_addr, low_value) andif (low_value = 0)
        then gen1 ( tstl, value_addr )
        else gen2 ( typ_opc (cmpl, vtype), value_addr, low_addr );      (* cmp value to low limit *)
      low_ok_label := make_def ( local_def );
      gen_branch ( bgeq, low_ok_label );        (* if value >= low limit, branch *)
      if operand[3] <> nil then begin
        err_call_label := make_def ( local_def );
        mark_def ( code_area, err_call_label )
      end;
      gen1 ( jsb, make_rt_addr ( rt_val_chk ) );        (* gen call to error routine *)
      mark_def (code_area, low_ok_label)
    end;
    if operand[3] <> nil then begin
      if aconstp (high_addr, high_value) andif (high_value = 0)
        then gen1 ( tstl, value_addr )
        else gen2 ( typ_opc (cmpl, vtype), value_addr, high_addr );     (* cmp value to high limit *)
      if operand[2] <> nil then
        gen_branch ( bgtr, err_call_label )     (* if value > high limit, branch *)
      else begin
        high_ok_label := make_def (local_def);
        gen_branch (bleq, high_ok_label);
        gen1 (jsb, make_rt_addr (rt_val_chk));
        mark_def (code_area, high_ok_label)
      end;
    end;
    
    free ( value_addr );
    if operand[2] <> nil then
      free ( low_addr );
    if operand[3] <> nil then
      free ( high_addr );

  end (* with *) ;
end  (* proc value_check *) ;
$PAGE pointer_check

(* POINTER_CHECK generates code for a pointer or file check tuple.
   Tests for NIL and for zero are generated.  *)

procedure pointer_check ( ptr_check_tpl: tuple );

var
  ptr_addr: addr_desc;
  nil_cmp_label: def;
  error_call_label: def;
  error_routine_addr: addr_desc;
  nil_addr: addr_desc;

begin
  if ptr_check_tpl^.opcode = ptr_chk then begin (* pointer check *)
    error_routine_addr := make_rt_addr ( rt_ptr_chk );
    nil_addr := int_value ( int_nil );
  end
  else begin    (* file check *)
    error_routine_addr := make_rt_addr ( rt_fil_chk );
    nil_addr := int_value ( int_nilf );
  end;

  ptr_addr := fetch ( ptr_check_tpl^.operand[ 1 ] , no_preference );
  gen1 ( tstl, ptr_addr );      (* test pointer *)
  nil_cmp_label := make_def ( local_def );
  gen_branch ( bnequ, nil_cmp_label );  (* if not 0, branch to NIL test *)
  error_call_label := make_def ( local_def );
  mark_def ( code_area, error_call_label );
  gen1 ( jsb, error_routine_addr );     (* gen call to error routine *)
  mark_def ( code_area, nil_cmp_label );
  gen2 ( cmpl, nil_addr, ptr_addr );    (* compare ptr to NIL *)
  gen_branch ( beqlu, error_call_label );       (* if equal, branch to error call *)
  free ( ptr_addr );
end (* proc pointer_check *) ;
$PAGE subscript_check

(* SUBSCRIPT_CHECK generates a subscript range check.  The INDEX
   instruction is used for subscript calculations unless the 
   subscript value is a constant.  Since the INDEX instruction
   automatically does a range check, an explicit check is generated
   by this routine only if the index value is constant.  In addition,
   PASS1 does a range check if the bounds are known at compile time.
   However the compiler will only generate a warning message when it
   finds a range error.  Thus this routine generates code only if the 
   index is constant and one of the following is true: the upperbound
   of the range is not known at compile time, or, the (constant) index
   is outside the limits of a constant bound.  *)

procedure subscript_check ( sub_check_op: tuple );

var
  index_value: integer;
  lwb_is_constant: boolean;
  lwb_value: integer;
  upb_is_constant: boolean;
  upb_value: integer;
  upb_addr: addr_desc;
  upb_vtype: vax_type;
  index_error: boolean;
  index_addr: addr_desc;
  ok_label: def;
  lwb_addr: addr_desc;

begin
  with sub_check_op^ do begin

    (* Generate the check only if the index is constant.  *)

    if iconstp ( operand[1], index_value ) then begin

      (* Get the value of the (necessarily constant) lower bound and
         determine if the upperbound is constant.  If it is, then
         get its value also.  *)

      if operand[2] <> nil then begin
        lwb_is_constant := iconstp ( operand[2], lwb_value );
        assert ( lwb_is_constant )      (* no generic arrays !! *)
      end;
      if operand[3] <> nil then begin
        upb_is_constant := iconstp ( operand[3], upb_value );
        upb_addr := fetch ( operand[3] , no_preference );
        upb_vtype := upb_addr.byte_size
      end;

      (* If we detect a range error at compile time, then simply generate a call
         to the error routine.  We'll check for the index below the lower bound,
         or above the upper bound, or if the index is into a string as is outside
         the hardware imposed limits.  *)

      index_error :=
        ((operand[2] <> nil) andif (index_value < lwb_value))   
                or
        ((operand[3] <> nil) andif
                     ((upb_is_constant) andif (index_value > upb_value)) or
                     ((upb_vtype = vax_word) and
                         ((index_value < 1) or (index_value > max_str_length))));

      if index_error then begin
        gen1 ( jsb, make_rt_addr ( rt_sub_chk ) );
      end

      (* The upperbound is not known at compile time.  Emit code to
         check it at runtime.  *)

      else if operand[3] <> nil then begin
        index_addr := typ_int_value ( index_value, upb_vtype );
        gen2 ( typ_opc ( cmpl, upb_vtype ), index_addr, upb_addr );
                                                  (* compare the index to the upperbound *)
        ok_label := make_def ( local_def );
        if upb_vtype = vax_word
          then gen_branch ( blequ, ok_label )   (* string lengths are unsigned *)
          else gen_branch ( bleq, ok_label );   (* array bounds are signed *)
        gen1 ( jsb, make_rt_addr ( rt_sub_chk ) );      (* generate the error call *)
        mark_def ( code_area, ok_label );
      end  (* else *) ;

      if operand[3] <> nil then
        free ( upb_addr );

    end

    (* If we do not generate an explicit check, we still fetch the
       operands and free them so that the register usages are
       correct.  This simply means that some code is generated earlier
       than it otherwise would be.  *)

    else begin
      index_addr := fetch ( operand[1] , no_preference );
      free ( index_addr );
      if operand[2] <> nil then begin
        lwb_addr := fetch ( operand[2] , no_preference );
        free ( lwb_addr )
      end;
      if operand[3] <> nil then begin
        upb_addr := fetch ( operand[3] , no_preference );
        free ( upb_addr )
      end
    end;
  end  (* with *) ;
end  (* proc subscript_check *) ;
$PAGE compatability_check

(* COMPATABILITY_CHECK generates code for a compatability check
   (COMPAT_CHK) tuple.  Compatability checks are generated for
   array assignments when one of the arrays is flexible, and, when
   a flex array or string is passed as a VAR parameter and the type
   of the formal is not flex.  A COMPAT_CHK tuple has two parameters:
   the upperbound or dimension of each array.  *)

procedure compatability_check ( tpl: tuple );

var
  bound1: addr_desc;
  bound2: addr_desc;
  skip_label: def;

begin
  with tpl^ do begin

    bound1 := fetch_fullword ( operand[ 1 ] );
    bound2 := fetch_fullword ( operand[ 2 ] );

    gen2 ( cmpl, bound1, bound2 );
    skip_label := make_def ( local_def );
    gen_branch ( beql, skip_label );
    gen1 ( jsb, make_rt_addr ( rt_cmp_chk ) );
    mark_def ( code_area, skip_label );

    free ( bound1 );
    free ( bound2 );
  end  (* with *) ;
end  (* proc compatability_check *) ;
$PAGE substring_check

(* SUBSTRING_CHECK generates code for substring check tuples.  Three
   tests are generated:

        1. length (substring) >= 0,
        2. start_index > 0,
        3. start_index + length (substring) - 1 <= length (base_string).

   The substring check tuple has three operands: the starting index of the
   substring, the length of the substring and the length of the base
   string.  *)

procedure substring_check (check_tuple: tuple);

var
  out_label,
  error_call_label: def;
  substr_addr,
  index_addr,
  base_addr,
  calc_end_pos: addr_desc;
  substr_const,
  index_const,
  base_const: boolean;
  substr_value,
  index_value,
  base_value: char_range;

begin

  (* Determine if any of the three tests can be done at compile time.  *)

  substr_const := iconstp (check_tuple^.operand[2], substr_value);
  index_const := iconstp (check_tuple^.operand[1], index_value);
  base_const := iconstp (check_tuple^.operand[3], base_value);

  error_call_label := make_def (local_def);

  (* Emit the test for length (substring) >= 0.  *)

  if (not substr_const) orif (substr_value < 0) then begin
    substr_addr := fetch_fullword (check_tuple^.operand[2]);
    gen1 (tstl, substr_addr);
    gen_branch (blss, error_call_label);
  end;

  (* Emit the test for start_index > 0.  *)

  if (not index_const) orif (index_value <= 0) then begin
    index_addr := fetch_fullword (check_tuple^.operand[1]);
    gen1 (tstl, index_addr);
    gen_branch (bleq, error_call_label);
  end;

  (* Calculate (start_index + length (substring) - 1) for use in the
     third and final check.  Constant components of the expression are
     accumulated at compile time.  *)

  if substr_const and index_const then
    calc_end_pos := int_value (substr_value + index_value - 1)
  else if substr_const then
    calc_end_pos := add_constant (index_addr, substr_value - 1, no_preference)
  else if index_const then
    calc_end_pos := add_constant (substr_addr, index_value - 1, no_preference)
  else begin
    free (substr_addr);
    free (index_addr);
    calc_end_pos := reg_addr (get_reg (bits_per_integer));
    gen3 (addl3, substr_addr, index_addr, calc_end_pos);
    add2_constant (calc_end_pos, -1);
  end;

  (* Generate code to compare the substring and position calculated above
     to the length of the base string.  *)

  base_addr := fetch_fullword (check_tuple^.operand[3]);
  gen2 (cmpl, calc_end_pos, base_addr);
  out_label := make_def (local_def);
  gen_branch (bleq, out_label);
  mark_def (code_area, error_call_label);
  gen1 (jsb, make_rt_addr (rt_str_chk));
  mark_def (code_area, out_label);

  free (base_addr);
  free (calc_end_pos);
end  (* proc substring_check *);
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
(* COMPILE BODY compiles the body of the current block. *)

procedure compile_body ( var begin_addr: def )  options special(coercions);

 var
   tpl,                          (* scanning cursor for compilation *)
   temp_cursor, (* for disposing tuples *)
   label_chain, label_end, (* for preserving label_nodes until end of block *)
   lab: tuple;
   d, d1: def;
   op: opc_range;
   reg: registers;
   i: integer;
   mem, addr: addr_desc;
   addrp: addr_ptr;
   ret_type: typ;
   ret_size: bit_range;
   align: align_range;
   first_jump: tuple;
   dump_final: boolean;

 begin
  if not have_code_origin then begin
    gen_origin (code_area, code_psect);
    have_code_origin := true;
  end;
  rd_tuples;                            (* fetch the intermediate form *)

  (* If original IF dump requested, do it. *)

  if switch (cur_block^.dump_switches, 'IFM0') then
    dmptuples ('ORIGINAL INTERMEDIATE FORM FOR BLOCK $');

  make_basic_blocks;    (* explode booleans and supply labels *)
  expand_aggregates; (* expand AGG_VAL's into assignments *)

  (* Dump IF after make_basic_blocks, if requested. *)

  if switch (cur_block^.dump_switches, 'IFM') then
    dmptuples ('INTERMEDIATE FORM AFTER MAKE_BASIC_BLOCKS FOR BLOCK $');

  (* Note whether we will be dumping the final intermediate form.  If so, we'll
     have to suppress the business of deleting I/F after every statement, while
     preserving the labels on another chain.  *)

  dump_final := switch (cur_block^.dump_switches, 'FINAL'); (* going to be dumping final form? *)


  prologue (begin_addr);                (* entry sequence *)

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
              bools, ints, chars, scalars, pointers, files, reals:
                scalar_assignment (tpl);
              strings:
                str_assignment (tpl);
              sets:
                set_assignment (tpl);
              procs, funcs:
                proc_func_assignment (tpl);
              arrays, records:
                agg_assignment (tpl) 
            end (* case *) ;
          end;

        start_with:
          with_start (with_rec);
        
        end_with:
          free (fetch (with_rec, no_preference)); (* last usage of the with_rec expr *)

        call_op:
          procedure_call (tpl);

        label_node:
          begin
            d := get_def (label_def, block_order_no);
            mark_def (code_area, d);
            if (label_sym <> nil) andif (label_sym^.lab_nonlocal_use) then begin
              d := get_def (sym_def, label_sym^.id_number);
              mark_def (code_area, d);
            end;
            bb_start ( tpl );
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
            bb_end ( tpl );
            if jump_to <> get_next_action (tpl) (* generate only if useful *)
              then gen_branch (brb, get_def (label_def, jump_to^.block_order_no));
          end;

        jump_t_op, jump_f_op:
          begin
            first_jump := tpl;
            tpl := tpl^.next;           (* skip to alternate jump op *)
            lab := get_next_action (tpl);       (* this may be label node *)
            if opcode = jump_t_op
              then test_and_jump (cond, jump_to, tpl^.jump_to, lab)
              else test_and_jump (cond, tpl^.jump_to, jump_to, lab);
            bb_end ( first_jump );
            bb_end ( tpl );
          end;

        case_jump_op:
          case_jump (tpl);

        goto_op:
          begin
            d := get_def (sym_def, target_lab^.id_number);      (* get label definition *)
            if target_lab^.block^.owner = cur_block^.owner then
              gen_branch (brb, d)
            else if target_lab^.block^.kind = program_blk then begin (* label is in main program *)
              gen2 (moval, def_addr (d), r0_addr);
              gen1 ( jmp, make_rt_addr ( rt_uw_prg ) );
            end
            else begin (* label is in a containing routine *)
              gen2 (moval, def_addr (d), r0_addr);
              move_immediate (cur_block^.apparent_level - target_lab^.block^.apparent_level, r1_addr);
              gen1 ( jmp, make_rt_addr ( rt_uw_rtn ) );
            end;
          end;

        gen_jump_op:
          begin
            bb_end ( tpl );
            tpl := tpl^.next;   (* presumably the jump_to label *)
            lab := tpl;         (* remember the label node *)
            tpl := tpl^.next;   (* this should be the gen_xxxif operator *)
            reg := get_reg (bits_per_unit);     (* get a register in which to load the value *)
            case tpl^.opcode of
              gen_andif_op: move_immediate (1, reg_addr (reg));
              gen_orif_op: gen1 (clrl, reg_addr (reg));
              others:      assert (false)
            end;
            d1 := make_def (local_def);
            gen_branch (brb, d1);
            d := get_def (label_def, lab^.block_order_no);
            mark_def (code_area, d);
            bb_start ( lab );
            case tpl^.opcode of
              gen_andif_op: gen1 (clrl, reg_addr (reg));
              gen_orif_op: move_immediate (1, reg_addr (reg))
            end;
            mark_def (code_area, d1);
            new (addrp);
            addrp^ := reg_addr (reg);
            tpl^.result := ptr (ord (addrp));   (* chain to orif/andif node *)
          end;

        dispose_op:
          begin
            push_value (fetch (dptrarg,no_preference), unsigned_value);
            gen_rt (1, rt_dispose);
          end;

        start_stmt:
          begin
            kill_temps;                 (* reset stack if required *)
            cur_source := stmt_source;  (* for debugging *)
            gen_source (stmt_source);   (* comment for assembly listing *)
            if (* first_stmt_on_line andif *) prog_options.debug_opt then begin
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
          gen1 (jmp, rt_addr (rt_stop));

        return_op:
          begin
            if (cur_block^.return_sym <> nil) (* is a function *) andif
              not passed_by_address (cur_block^.return_sym) then begin
                mem := temp_reference;
                mem.reloc.kind := local_sc;
                mem.reloc.relsym := cur_block^.return_sym;
                ret_type := cur_block^.return_sym^.type_desc;
                mem.byte_size := unpacked_vax_type ( ret_type );
                if ret_type^.kind = sets then begin
                  alc_data ( ret_type, ret_size, align );
                  if ret_size > bits_per_unit
                    then mem.byte_size := vax_quad;
                end;
                addr := r0_addr;
                if not (mem.byte_size in [vax_byte, vax_word])
                  then addr.byte_size := mem.byte_size;
                store ( mem, addr, unsigned_value );
            end;
            gen_opcode (ret);
          end;

        abort_op:
          gen1 ( jsb, make_rt_addr(rt_ass_chk) );

        case_abort_op:
          gen1 ( jsb, make_rt_addr(rt_case_chk) );
 
        set_handler_op,
        rst_handler_op,
        signal_op,
        mask_op,
        unmask_op,
        resignal_op,
        hndlr_jump_op,
        jump_cond_op:
          cond_handler_stmts (tpl);

        get_op:
          begin
            (*A fetch can be used since PUT/GET are not IN_STR_OP or OUT_STR_OP *)
            push_value( fetch( file_arg, no_preference), unsigned_value );
            if file_arg^.desc.base^.file_kind = textfile
              then gen_rt (1, rt_get_char)
              else gen_rt (1, rt_get);
          end;

        put_op:
          begin
            (* Fetch can be used cause PUT is not IN_STR_OP or OUT_STR_OP *)
            push_value( fetch( file_arg, no_preference), unsigned_value );
            if file_arg^.desc.base^.file_kind = textfile
              then gen_rt (1, rt_put_char)
              else gen_rt (1, rt_put);
          end;

        readln_op:
          begin
            rt_io_call (rt_readln);
            free ( fetch ( file_arg , no_preference ) );        (* fetch and free file variable so *)
                                                (* reg usave counts are correct *)
          end;

        writeln_op:
          begin
            rt_io_call (rt_writeln);
            free ( fetch ( file_arg , no_preference ) );        (* fetch and free file variable so *)
                                                (* reg usage counts are correct *)
          end;

        page_op:
          begin
            rt_io_call (rt_page);
            free ( fetch ( file_arg , no_preference ) );
          end;

        clear_op:
          begin
            rt_io_call (rt_clear);
            free ( fetch ( file_arg , no_preference ) );
          end;

        break_op:
          begin
            rt_io_call (rt_break);
            free ( fetch ( file_arg , no_preference ) );
          end;

        empty_op:
          begin
            rt_io_call (rt_empty_text);
            free ( fetch ( file_arg , no_preference ) );
          end;

        close_op:
          begin
            rt_io_call (rt_close);
            free ( fetch ( file_arg , no_preference ) );
          end;

        scratch_op:
          begin
            rt_io_call (rt_scratch);
            free ( fetch ( file_arg , no_preference ) );
          end;

        start_io_op:
          io_begins (tpl);

        end_io_op:
          io_ends ( tpl );

        read_op:
          read_write_call (tpl);

        write_op:
          read_write_call (tpl);

        seek_op:
          rt_seek_call ( seek_file , seek_index);

        close_all_op:
          begin
            gen_rt (0, rt_close_all);
          end;

        val_range_chk:
          value_check ( tpl );

        file_chk,
        ptr_chk:
          pointer_check ( tpl );

        sub_range_chk:
          subscript_check ( tpl );

        str_range_chk:
          substring_check ( tpl );

        compat_chk:
          compatability_check ( tpl );

        first_expr..last_expr:
          (* will be fetched later *)

      end (* case *) ;
    end (* with *) ;
    tpl := tpl^.next;   (* this must be outside the with *)
  end;

  (* Clean up debug stuff. *)

  if prog_options.debug_opt
    then blk_end;

  (* Resolve allocated stack size and suppress allocation if zero. *)

  i := ngm (cur_block^.neg_stk_begin + temp_max - cur_block^.neg_stk_end, bytes_per_unit);
  if dyn_def <> nil then begin          (* If dynamics must fix up definition record for SP *)
    DYN_DEF^.DEFINED := True;
    DYN_DEF^.ADDR := -i + cur_block^.neg_stk_begin
  end;
  (* 1st pass always allocates 4 bytes for save of ap - if we're really saving
     it, pushl from prologue took care of it.  If we're not, and no locals or
     temps follow it (whose offsets we can't screw up) then dispense with the space. *)
  if (cur_block^.kind = subr_blk) and (save_ap (cur_block) or (i = 4))
    then i := i - 4;
  if i > 0
    then stack_fixup^.operands[1].offset := i
    else stack_fixup^.kind := nullcode; (* To ignore operation *)

  (* Verify that all registers were freed.  *)

  for i := 2 to max_reg do
    assert ( regdesc[ i ].uses_remaining = 0 );

  fix_branches (code_area, loc_code);

  emit_code (code_area, loc_code, psect_id [code_psect], cur_block^.semantic_options);


  (* Dump the final IF , if requested *)

  if switch (cur_block^.dump_switches, 'FINAL') then begin
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
   entryaddr: def;

 begin
  gen_cmt (code_area, 'Begin subroutine ' || cur_block^.subr_sym^.name^.text);

  compile_body (entryaddr)                              (* compile the body of the subroutine *)
 end;
$PAGE compile_main
(* COMPILE MAIN generates code for the body of a main program.  It is assumed that
   cur_block points to the program's block node at entry. *)

procedure compile_main (var startaddr: def);

begin
  gen_cmt (code_area, 'Begin program ' || cur_block^.id^.text);

  compile_body (startaddr);     (* generate code for body of block *)
end;
$PAGE gen_code
(* GEN CODE is the driving program for the code generator.  It directs initialization,
   generation, and termination. *)

public procedure gen_code ( var code_size, const_size, static_size: unit_range );

var
  last_cst: code;
  startaddr: def; (* start address of main program, nil otherwise *)
  debfile: text;

begin
  cur_block := root_block^.children;    (* set immediately in case of assertion failure *)
  cur_source := (0, 0, 0);              (*  "       "      "   "    "     "        "    *)
 
  startaddr := nil;                     (* init globals in this module *)
  p4_error_level := no_errors;  (* no pass4 errors so far *)
  gen_init;     (* and others *)
  exp_init;
  if assembly_opt in all_opts
    then mac_header;
  if map_opt in all_opts
    then map_init;
  rel_init;
  if prog_options.debug_opt then
    deb_init;

  (* ASsign psect id's. *)

  static_size := size_init + size_cond + size_uninit;
  if static_size <> 0 then begin
    psect_id[static_psect] := 0;
    psect_id[code_psect] := 1;
  end
  else (* no static storage *)
    psect_id[code_psect] := 0;
  have_code_origin := false;
  init_static (static_size);

  (* Compile the constituent blocks in reverse depth first order. *)

  cur_block := root_block;              (* look for end of call chain *)
  while cur_block^.downward_call_thread <> nil do cur_block := cur_block^.downward_call_thread;

  while cur_block <> root_block do begin        (* compile in reverse order *)
    case cur_block^.kind of 
      program_blk:    compile_main (startaddr);
      subr_blk:       compile_subr
    end;
    cur_block := cur_block^.upward_call_thread;
  end;
  cur_block := root_block^.children; (* in case of later assertion failure *)

  (* Final cleanup *)

  loc_cst := loc_code; (* normally, consts go at end of code area *)
  pool_constants (loc_cst);
  emit_code (cst_area, loc_cst, psect_id [code_psect], prog_options.semantic_options);
  if prog_options.debug_opt then begin
    fp_blocks;  (* dumps page and file blocks in code area *)
    emit_code (code_area, loc_cst, psect_id [code_psect], prog_options.semantic_options);
  end;

  (* If this is an overlay compilation generate the indirect word used to access
     public vars and constants *)

  if prog_options.overlay_opt then begin
    loc_cst := ngm ( loc_cst , bytes_per_unit );        (* must longword align *)
    ovly_emit_indirect_words;
    emit_code ( cst_area, loc_cst, psect_id[code_psect], prog_options.semantic_options )
  end;

  code_size := loc_code;
  const_size := loc_cst - loc_code;
  if assembly_opt in all_opts then
    mac_end (startaddr, code_size, const_size, static_size);    (* terminate assembly listing *)
  if map_opt in all_opts then begin
    map_close;
    map_print;
  end;
  rel_end (startaddr, code_size + const_size, static_size);

  (* Close the dump file, if necessary. *)

  dmp_close;
end.
     zd