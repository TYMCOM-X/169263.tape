module vaxcll;


(* Includes *)

$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM vaxcgu.inc
$SYSTEM vaxopc.inc
$SYSTEM vaxexp.inc
$SYSTEM pastal.inc
$SYSTEM vaxgen.inc
$SYSTEM vaxutl.inc
$SYSTEM vaxstr.inc
$SYSTEM pasmth.inc
$SYSTEM vaxset.inc

(* Types for the temporary storing of procedure and function parameters
   while dynamic temporaries are created *)

Type PARAM_REC = Record
        PSH_VAL : Boolean;              (* Push value, vs. address *)
        ALIGN   : DATA_ALIGNMENT;       (* data alignment for push value *)
        DESC    : ADDR_DESC             (* The address descriptor *)
     End;

     PARAM_ARY = Array [ 1..* ] of PARAM_REC;
     param_ptr = ^param_ary;

$PAGE ADD_TO_PARAM_ARY
(* Add a PUSH (value or address) onto a parameter array. This array will
   be walked and all the arguments pushed once all the arguments have been
   fetched. This routine was necessitated by the addition of dynamic temps. *)

Public Procedure ADD_TO_PARAM_ARY ( DESC : ADDR_DESC;
                             ALIGN: DATA_ALIGNMENT;
                             VAL  : Boolean;
                        var proc_params : param_ptr;
                        var param_ary_idx : parm_index );

(* Fill in the fields of the parameter array record, then increment
   the array index. *)

Begin
  proc_params^[ PARAM_ARY_IDX ].DESC := DESC;
  proc_params^[ PARAM_ARY_IDX ].PSH_VAL := VAL;
  proc_params^[ PARAM_ARY_IDX ].ALIGN := ALIGN;
 
  (* Now increment the array pointer to the next slot *)

  PARAM_ARY_IDX := PARAM_ARY_IDX + 1
End;
$PAGE pas_call
(* PAS CALL generates code to call a PASCAL procedure or function. The parameter
   "node" is the call_op or func_call_op node for which code is to be generated;
   and "rv_addr" is a pointer to a location in which an aggregate valued function
   is to place its result if the return value is "structured". *)

procedure pas_call ( node: expr; structured: boolean; rv_addr: addr_desc );

var
  saddr, addr, rv: addr_desc;
  i, parmlist_size: integer;
  levels_dif: integer;

  idx: parm_index;
  param_ary_idx : parm_index;
  proc_params : param_ptr;
$PAGE push_parameters - in pas_call

  procedure push_parameters ( selector: parm_range );

  (* PUSH PARAMETERS evaluates and pushes the same in reverse order. *)

  var addr: addr_desc;
      temp_addr : addr_desc;
      value_signed: boolean;
      op: opc_range;
      parm_typ_kind: type_kind;
      parm_sym_kind: sym_kind;
      parm_type: typ;
      parm_size: bit_range;
      upb_addr: addr_desc;
      base_addr: addr_desc;
      i, levels_dif: integer;
      set_value: integer;
      align: align_range;
      NON_ZERO_REG_ADDR : ADDR_DESC;            (* For value procedure proc_params *)

  begin
    with node^ do begin
      if selector < upperbound (arglist) then   (* find end of parameter chain *)
        push_parameters ( selector + 1 );
      parm_sym_kind := subr^.desc.base^.params[selector].parm_kind;
      parm_type := subr^.desc.base^.params[selector].parm_type;
      parm_typ_kind := parm_type^.kind;
      alc_data ( parm_type, parm_size, align ); (* get param size *)
      if parm_typ_kind = strings then begin
        str_parameter ( arglist[selector], parm_sym_kind, parm_type,
                proc_params , param_ary_idx );
        if parm_type^.flexible
          then parmlist_size := parmlist_size + 2
          else parmlist_size := parmlist_size + 1;
      end
      else if parm_typ_kind = sets then begin
        with parm_type^.set_element_type^ do
          add_to_param_ary (set_parameter (arglist [selector], parm_sym_kind, minval, maxval),
                unsigned_value , false , proc_params , param_ary_idx );
        parmlist_size := parmlist_size + 1
      end
      else if parm_typ_kind in [procs, funcs] then begin
        base_addr := fetch (arglist[selector] , no_preference );

        (* A procedure or function variable or parameter can merely be
           pushed into the parameter list.      *)

        if (base_addr.reloc.kind in [local_sc,parameter_sc,static_sc]) orif
          (base_addr.reloc.kind = external_sc) andif (base_addr.reloc.relsym^.kind <> consts) then
            add_to_param_ary ( base_addr , unsigned_value ,
                parm_sym_kind <> vars , proc_params , param_ary_idx )
        else begin      (* must generate address and stack frame base *)
          add_to_param_ary ( base_addr , unsigned_value , false , proc_params , param_ary_idx );
          if (base_addr.reloc.kind = external_sc) orif (arglist[selector]^.cst_val.blkp^.apparent_level <= 2) then
            add_to_param_ary ( int_value(0) , unsigned_value , true , proc_params , param_ary_idx )
          else begin    (* find its parent stack pointer *)
            levels_dif := cur_block^.apparent_level - arglist[selector]^.cst_val.blkp^.apparent_level;
            if levels_dif = -1 then
              add_to_param_ary ( fp_addr , unsigned_value , true , proc_params , param_ary_idx )
            else begin
              addr := absolute_reference;
              addr.register := fp;
              addr.offset := -4;
              if levels_dif = 0 then
                add_to_param_ary ( addr , unsigned_value , true , proc_params , param_ary_idx )
              else begin
                NON_ZERO_REG_ADDR := GET_TYP_REG_ADDR ( VAX_LONG );
                GEN2 ( MOVL , ADDR , NON_ZERO_REG_ADDR );
                addr := push_reference;
                ADDR.REGISTER := NON_ZERO_REG_ADDR.REGISTER;
                for i := 1 to levels_dif - 1 do
                  gen2 (movl, addr, NON_ZERO_REG_ADDR);
                add_to_param_ary ( addr , unsigned_value , true , proc_params , param_ary_idx )
              end;
            end;
          end;
        end;
        if parm_sym_kind = vars then
          parmlist_size := parmlist_size + 1
        else
          parmlist_size := parmlist_size + 2
      end

      else begin


        (* If there is only one parameter then use targetting !!! *)
        (* BUT ONLY USE TARGETTING FOR VALUE, NON-ADDRESS parameters *)
        (* Also, if the expression type must be long, float, or double *)

        if ( upperbound (arglist) = 1 )
        and ( parm_sym_kind <> vars )
        and not p_b_address ( parm_type )
          then begin
            temp_addr := push_reference;
            temp_addr.byte_size := expr_vax_type ( arglist [ selector ] );
            if temp_addr.byte_size in [ vax_byte , vax_word ]
              then temp_addr.byte_size := vax_long;
            base_addr := fetch ( arglist [ selector ] , temp_addr );
          end
        else base_addr := fetch ( arglist [ selector ] , no_preference );

        (* if actual is a dynamic flex, then skip the bounds word
           preceeding the array.  *)

        if (parm_type^.kind = arrays) andif
           (dynamic_flex ( arglist[ selector ] )) then begin
          base_addr := duplicate_addr ( base_addr );
          addr := increment_addr ( base_addr, flex_arr_desc_size div bits_per_byte );
          addr.byte_size := vax_byte;
        end
        else addr := duplicate_addr ( base_addr );

        if (parm_sym_kind = vars) or p_b_address ( parm_type ) then begin
          add_to_param_ary ( duplicate_addr ( addr ) , unsigned_value ,
                false , proc_params , param_ary_idx );
          parmlist_size := parmlist_size + 1;
        end
        else begin
          add_to_param_ary ( duplicate_addr ( addr ) , alignment ( arglist[selector] ) ,
                true , proc_params , param_ary_idx );
          if parm_size > bits_per_unit
            then parmlist_size := parmlist_size + 2
            else parmlist_size := parmlist_size + 1;
        end;
        if parm_type^.flexible then begin       (* flex - push upperbound also *)
          upb_addr := upper_bound ( arglist[ selector ], base_addr );
          add_to_param_ary ( upb_addr , signed_value , true , proc_params , param_ary_idx );
          parmlist_size := parmlist_size + 1;
        end;
        free ( addr );
        free ( base_addr );
      end;
    end (* with node^ *)
  end (* push_parameters *);
$PAGE pas_call - body
begin (* pas_call *)

  (* Fetch the routine to be called - we do it before processing the parameters so that
     if dynamic temps are allocated in the process, it will be done with before we
     begin pushing parameters.  *)


  saddr := argument (node^.subr);       (* if a variable, no indirection *)

  parmlist_size := 0;
  if upperbound (node^.arglist) > 0
    Then Begin
      (* New array to hold the parameters until dymnamic temps are allocated *)
      new ( proc_params, upperbound (node^.arglist) * 2 );      (* Will always be <= 2*num of args *)
      PARAM_ARY_IDX := 1;               (* lets start at the very beginning *)

      (* Call push_parameters to fill array and make dyn temps *)
      push_parameters ( 1 );

      (* Now that the array is full dump the push addr and push vals *)
      for IDX := 1 to PARAM_ARY_IDX-1 Do
        if ( proc_params^[idx].desc.addr_mode = auto_dec )
        and ( proc_params^[idx].desc.register = sp )
          then free ( proc_params^[idx].desc )
        else if proc_params^[ IDX ].PSH_VAL
          then PUSH_VALUE ( proc_params^[ IDX ].DESC, proc_params^[ IDX ].ALIGN )
          else PUSH_ADDRESS ( proc_params^[ IDX ].DESC );

      Dispose ( proc_params )           (* Get back storage used *)
    End;
  if structured then begin
    gen1 (pushab, rv_addr);
    parmlist_size := parmlist_size + 1;
  end;

  (* check for subroutine variable *)

  if ( node^.subr^.opcode <> cst_ref ) and
     ( (node^.subr^.opcode <> ident_ref) orif
       (node^.subr^.id_sym^.kind <> consts) ) then begin (* must be subr var *)
    gen2 (movl, saddr, r0_addr);        (* get routine's parent frame pointer *)
    
    (* Increment the address so it addresses the longword containing the
       routine's start address.  The call to INCREMENT_ADDR also has the
       necessary side effect of eliminating index mode addresses.  This
       is necessary because the code generated treats subroutine variables
       as quadwords but the call instruction expects a byte mode address. *)

    saddr := increment_addr ( saddr, 4 );
    saddr.byte_size := vax_byte;
    saddr.indirect := true;     (* indirect through variable *)
    free ( saddr );
  end
  else if (node^.subr^.opcode = cst_ref) andif (node^.subr^.cst_val.blkp^.level > 2) then begin
    levels_dif := cur_block^.apparent_level - node^.subr^.cst_val.blkp^.apparent_level;
    if levels_dif = -1 then     (* caller is parent of callee *)
      gen2 (movl, fp_addr, r0_addr)
    else begin
      addr := absolute_reference;
      addr.register := fp;
      addr.offset := -4;        (* recursing, pick up own parent pointer *)
      gen2 (movl, addr, r0_addr);
      if levels_dif > 0 then begin      (* chase static chain *)
        addr := push_reference; (* use -(R0) mode for rest of chain *)
        addr.register := r0;
        for i := 1 to levels_dif do
          gen2 (movl, addr, r0_addr);
      end;
    end;
  end;
  gen2 (calls, int_value (parmlist_size), saddr);
end;
$PAGE for_call
(* FOR CALL generates a call to a fortran procedure or function. The parameter
   "node" is the call_op or func_call_op node. *)

procedure for_call (node: expr);

type
  len_locs_rec = record
    next: ^len_locs_rec;
    len_in_descriptor,
    len_in_string: addr_desc
  end;
var
  vax_str_desc: str_desc;
  mem, addr: addr_desc;
  i: parm_range;
  len_list, temp_ptr: ^len_locs_rec;

begin
  with node^ do begin

    (* Evaluate parameters in reverse order. All parameters except strings
       are passed by referenced. Strings are passed by reference to a
       descriptor record of the form:
                .word <string length>
                .byte 14                ; char type code
                .byte 1                 ; scalar class code
                .long <addr of text>
                                                                *)

    len_list := nil;
    for i := upperbound (arglist) downto 1 do begin
      if arglist [I]^.desc.kind <> strings then
	mem := mem_fetch (arglist [I])
      else begin
	vax_str_desc := fetchtranslated (arglist [I], actual_length);
	mem := get_temp (int_value (8), vax_long); (* 8-byte temp for descriptor *)

	(* Copy the length word to the first field of the descriptor.
	   For varying-string var-parameters, remember the addresses for copying
	   the length back again after the call.  *)

	gen2 (movw, vax_str_desc.len_addr, mem);
	if (subr^.desc.base^.params[i].parm_kind = vars) and
	   (arglist[i]^.desc.str_kind = varying) then begin
	  temp_ptr := len_list;
	  new (len_list);
	  len_list^ := (temp_ptr, duplicate_addr (mem), duplicate_addr (vax_str_desc.len_addr))
	end;

	(* 14, 1 values created as immediate values *)

	addr := typ_int_value (#O416, vax_word);    (* Integers are  backwards *)
	mem.offset := mem.offset + 2; (* 2nd word for the 14 and 1 *)
	gen2 (movw, addr, mem);

	(* Free the string so that the registers are re-usable, because
	   this is the last reference. Find the beginning of the string,
	   not the length word and write its address into the temp *)

	addr := skip_len_words (vax_str_desc);
	free_string (vax_str_desc);
	mem.offset := mem.offset + 2;  (* beginning of third word for str *)
	gen2 (movab, addr, mem);
	free (addr);
	mem.offset := mem.offset - 4; (* Restore the original value *)
      end;

      push_address (mem);
    end;

    (* Call fortran routine *)

    gen2 (calls, int_value (upperbound (arglist)), fetch (subr,no_preference));

    (* For any varying-string var-parameters, copy the length back from the descriptor
       back to the string length word. *)

    while len_list <> nil do begin
      gen2 (movw, len_list^.len_in_descriptor, len_list^.len_in_string);
      free (len_list^.len_in_descriptor);
      free (len_list^.len_in_string);
      temp_ptr := len_list;
      len_list := len_list^.next;
      dispose (temp_ptr)
    end
  end;
end;
$PAGE procedure_call
(* PROCEDURE CALL compiles code for a call operator "node". *)

public procedure procedure_call (node: tuple);

var taddr: addr_desc;

begin
  if node^.subr^.desc.base^.fortran_call
    then for_call (node)
    else pas_call (node, false, taddr);
end;
$PAGE scl_function_call
(* SCL FUNCTION CALL compiles code for a func_call_op "node" whose result
   may be of any size (scalar or otherwise. *)

public function scl_function_call (node: expr; targ_addr:addr_desc ): addr_desc;

var
  addr: addr_desc;
  t: typ;
  result_size: bit_range;

begin

  (* Check for structured function call and pass address for result if so. *)

  t := node^.desc.base;
  result_size := expr_size ( node );
  if (node^.desc.kind = sets) orif
     (t^.kind in [arrays,records,strings]) orif (result_size > 2*bits_per_unit) then begin
    SCL_FUNCTION_CALL := GET_TEMP (
        INT_VALUE( NGM ( RESULT_SIZE,BITS_PER_BYTE) Div BITS_PER_BYTE ),
        EXPR_VAX_TYPE ( NODE ) );
    if node^.desc.kind = sets then
      scl_function_call.byte_size := vax_byte;
    pas_call (node, true, scl_function_call);
  end
  else begin
    procedure_call (node);

    (* Result is left in R0 or R0/R1 and must be copied immediately to
       the target, if possible, or into a more permanent register *)

    if adr_equal(targ_addr,no_preference)
    or ( targ_addr.byte_size <> expr_vax_type ( node ) )
      then begin
        scl_function_call := reg_addr ( get_reg ( result_size ) );
        addr := r0_addr;
        addr.byte_size := expr_vax_type (node);
        scl_function_call.byte_size := addr.byte_size;
      end
    else begin
      addr := r0_addr;
      addr.byte_size := targ_addr.byte_size;
      scl_function_call := targ_addr
    end;
    store (addr, scl_function_call, alignment (node));
  end;
end.
 