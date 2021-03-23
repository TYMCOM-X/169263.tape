$TITLE Environment load/save routines for resident VAX compiler

module vax_environment options special (word, ptr, coercions), check (assertions, cases);
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM pasmth.inc
$SYSTEM dtime.inc
$PAGE declarations
const
  env_file_format_version = 1;	(* Insure consistancy *)
  fake_nil: val_ptr := ptr ( 0 );
  fixed_pointer_data_length = 23 * 4;

  (* Byte offsets within symbol table records for all relevant pointer fields. *)

  parent_offset :=
$IFNOT field_offset	 0;
$IF    field_offset	 field_offset ( block_node, parent ) div storage_unit_size );
  peer_offset :=
$IFNOT field_offset	 4;
$IF    field_offset	 field_offset ( block_node, peer ) div storage_unit_size );
  children_offset :=
$IFNOT field_offset	 8;
$IF    field_offset	 field_offset ( block_node, children ) div storage_unit_size );
  return_sym_offset :=
$IFNOT field_offset	 25;
$IF    field_offset	 field_offset ( block_node, return_sym ) div storage_unit_size );
  parm_list_offset :=
$IFNOT field_offset	 29;
$IF    field_offset	 field_offset ( block_node, parm_list ) div storage_unit_size );
  label_list_offset :=
$IFNOT field_offset	 37;
$IF    field_offset	 field_offset ( block_node, label_list ) div storage_unit_size );
  type_list_offset :=
$IFNOT field_offset	 45;
$IF    field_offset	 field_offset ( block_node, type_list ) div storage_unit_size );
  id_list_offset :=
$IFNOT field_offset	 53;
$IF    field_offset	 field_offset ( block_node, id_list ) div storage_unit_size );
  owner_offset :=
$IFNOT field_offset	 68;
$IF    field_offset	 field_offset ( block_node, owner ) div storage_unit_size );
  downward_call_thread_offset :=
$IFNOT field_offset	 76;
$IF    field_offset	 field_offset ( block_node, downward_call_thread ) div storage_unit_size );
  upward_call_thread_offset :=
$IFNOT field_offset	 80;
$IF    field_offset	 field_offset ( block_node, upward_call_thread ) div storage_unit_size );
  lex_thread_offset :=
$IFNOT field_offset	 84;
$IF    field_offset	 field_offset ( block_node, lex_thread ) div storage_unit_size );
  id_offset :=
$IFNOT field_offset	 117;
$IF    field_offset	 field_offset ( block_node, id ) div storage_unit_size );
  subr_sym_offset :=
$IFNOT field_offset	 117;
$IF    field_offset	 field_offset ( block_node, subr_sym ) div storage_unit_size );
  class_type_offset :=
$IFNOT field_offset	 117;
$IF    field_offset	 field_offset ( block_node, class_type ) div storage_unit_size );
  type_id_offset :=
$IFNOT field_offset	 4;
$IF    field_offset	 field_offset ( type_node, type_id ) div storage_unit_size );
  base_type_offset :=
$IFNOT field_offset	 16;
$IF    field_offset	 field_offset ( type_node, base_type ) div storage_unit_size );
  cst_list_offset :=
$IFNOT field_offset	 28;
$IF    field_offset	 field_offset ( type_node, cst_list ) div storage_unit_size );
  set_element_type_offset :=
$IFNOT field_offset	 16;
$IF    field_offset	 field_offset ( type_node, set_element_type ) div storage_unit_size );
  target_type_offset :=
$IFNOT field_offset	 16;
$IF    field_offset	 field_offset ( type_node, target_type ) div storage_unit_size );
  heap_class_offset :=
$IFNOT field_offset	 20;
$IF    field_offset	 field_offset ( type_node, heap_class ) div storage_unit_size );
  element_type_offset :=
$IFNOT field_offset	 17;
$IF    field_offset	 field_offset ( type_node, element_type ) div storage_unit_size );
  index_type_offset :=
$IFNOT field_offset	 21;
$IF    field_offset	 field_offset ( type_node, index_type ) div storage_unit_size );
  component_type_offset :=
$IFNOT field_offset	 17;
$IF    field_offset	 field_offset ( type_node, component_type ) div storage_unit_size );
  file_class_offset :=
$IFNOT field_offset	 21;
$IF    field_offset	 field_offset ( type_node, file_class ) div storage_unit_size );
  field_list_offset :=
$IFNOT field_offset	 16;
$IF    field_offset	 field_offset ( type_node, field_list ) div storage_unit_size );
  variant_tag_offset :=
$IFNOT field_offset	 20;
$IF    field_offset	 field_offset ( type_node, variant_tag ) div storage_unit_size );
  tag_offset :=
$IFNOT field_offset	 24;
$IF    field_offset	 field_offset ( type_node, tag ) div storage_unit_size );
  next_variant_offset :=
$IFNOT field_offset	 28;
$IF    field_offset	 field_offset ( type_node, next_variant ) div storage_unit_size );
  tag_field_offset :=
$IFNOT field_offset	 16;
$IF    field_offset	 field_offset ( type_node, tag_field ) div storage_unit_size );
  tag_type_offset :=
$IFNOT field_offset	 20;
$IF    field_offset	 field_offset ( type_node, tag_type ) div storage_unit_size );
  tag_recvar_offset :=
$IFNOT field_offset	 24;
$IF    field_offset	 field_offset ( type_node, tag_recvar ) div storage_unit_size );
  first_variant_offset :=
$IFNOT field_offset	 28;
$IF    field_offset	 field_offset ( type_node, first_variant ) div storage_unit_size );
  class_block_offset :=
$IFNOT field_offset	 17;
$IF    field_offset	 field_offset ( type_node, class_block ) div storage_unit_size );
  return_type_offset :=
$IFNOT field_offset	 21;
$IF    field_offset	 field_offset ( type_node, return_type ) div storage_unit_size );
  params_offset :=
$IFNOT field_offset	 33;
$IF    field_offset	 field_offset ( type_node, params ) div storage_unit_size );
  actual_type_offset :=
$IFNOT field_offset	 16;
$IF    field_offset	 field_offset ( type_node, actual_type ) div storage_unit_size );
  parm_type_offset :=
$IFNOT field_offset	 0;
$IF    field_offset	 field_offset ( param_desc, parm_type ) div storage_unit_size );
  name_offset :=
$IFNOT field_offset	 0;
$IF    field_offset	 field_offset ( symbol_node, name ) div storage_unit_size );
  block_offset :=
$IFNOT field_offset	 4;
$IF    field_offset	 field_offset ( symbol_node, block ) div storage_unit_size );
  next_offset :=
$IFNOT field_offset	 8;
$IF    field_offset	 field_offset ( symbol_node, next ) div storage_unit_size );
  type_desc_offset :=
$IFNOT field_offset	 16;
$IF    field_offset	 field_offset ( symbol_node, type_desc ) div storage_unit_size );
  fld_record_offset :=
$IFNOT field_offset	 32;
$IF    field_offset	 field_offset ( symbol_node, fld_record ) div storage_unit_size );
  fld_variant_offset :=
$IFNOT field_offset	 36;
$IF    field_offset	 field_offset ( symbol_node, fld_variant ) div storage_unit_size );
  init_value_offset :=
$IFNOT field_offset	 43;
$IF    field_offset	 field_offset ( symbol_node, init_value ) div storage_unit_size );
  first_offset :=
$IFNOT field_offset	 0;
$IF    field_offset	 field_offset ( sym_list, first ) div storage_unit_size );
  last_offset :=
$IFNOT field_offset	 4;
$IF    field_offset	 field_offset ( sym_list, last ) div storage_unit_size );
  valp_offset :=
$IFNOT field_offset	 1;
$IF    field_offset	 field_offset ( val, valp ) div storage_unit_size );
  blkp_offset :=
$IFNOT field_offset	 1;
$IF    field_offset	 field_offset ( val, blkp ) div storage_unit_size );
  struc_type_offset :=
$IFNOT field_offset	 5;
$IF    field_offset	 field_offset ( value_node, struc_type ) div storage_unit_size );
  elem_vals_offset :=
$IFNOT field_offset	 13;
$IF    field_offset	 field_offset ( value_node, elem_vals ) div storage_unit_size );
  alink_offset :=
$IFNOT field_offset	 0;
$IF    field_offset	 field_offset ( name_node, alink ) div storage_unit_size;
  zlink_offset :=
$IFNOT field_offset	 4;
$IF    field_offset	 field_offset ( name_node, zlink ) div storage_unit_size;
  scopechain_offset :=
$IFNOT field_offset	 8;
$IF    field_offset	 field_offset ( name_node, scopechain ) div storage_unit_size;

$PAGE vax_save_environment
(* VAX SAVE ENVIRONMENT is called by the resident VAX compiler to save
   the current environment in a file.  It returns a Boolean value indicating
   whether the save was successful. *)

public function vax_save_environment: boolean;

const
  bit_map_upb = 16383;		(* upperbound of bit map set *)

var
  env_file: file of *;		(* the environment file itself *)
  file_index: integer;		(* cursor within the environment file *)
  bit_index: integer;		(* index to last entry made in bit map *)
  bit_map: set of 0 .. bit_map_upb;
  map_area: integer;		(* cursor into environment file where the
				   next bit map is to be written *)
  node_base: integer;		(* offset in env file of start of variable
				   length node area *)
  node_base_address: integer;	(* used to emit bit map relocation *)
  last_loc: integer;		(* offset in env file of byte following
				   last relocated address (initially equal
				   to NODE_BASE *)
  relocating: boolean;		(* TRUE => relocation pass, FALSE => emission pass *)
$PAGE initialize_environment, terminate_environment
procedure initialize_environment;

begin
  rewrite ( env_file, '.ENV ' || rel_file, [seekok] );
  vax_save_environment := false;	(* Until all done *)
end (* initialize_environment *);

procedure terminate_environment;

begin
  close ( env_file );
  if iostatus = io_ok then
    vax_save_environment := true;	(* all done *)
end (* terminate_environment *);
$PAGE environment_header
(* ENVIRONMENT HEADER emits the environment name and version number. *)

procedure environment_header;

var
  len: integer;

begin
  len := upperbound ( env_name^.text );
  write ( env_file, len: 1 );
  write ( env_file, env_name^.text: len );;
  file_index := ngm ( len + 1, 4 );
  seek ( env_file, file_index );	(* keep longword aligned *)
  write ( env_file, env_dtime );
  write ( env_file, env_file_format_version: 4 );
end (* environment_header *);
$PAGE initial_global_data
(* INITIAL GLOBAL DATA saves some initial global variables in the environment *)

procedure initial_global_data;

begin
  write ( env_file, sym_vl_number: 4 );
  write ( env_file, vl_base: 4 );
  write ( env_file, sym_nvl_number: 4 );
  write ( env_file, nvl_base: 4 );
end (* initial_global_data *);
$PAGE prepare_for_relocation
(* PREPARE FOR RELOACTION  initializes everything for the relocation pass
   over the symbol table. *)

procedure prepare_for_relocation;

begin

  (* Mark the base of the variable node area, reserving a longword for its length. *)

  node_base := cursor ( env_file ) + 4 - 1;
  file_index := node_base;

  (* Sanity check to insure all nodes are longword aligned in the file. *)

  assert ( file_index mod 4 = 0 );

  (* Identify relocation pass active. *)

  relocating := true;

end (* prepare_for_relocation *);
$PAGE prepare_for_emission
(* PREPARE FOR EMISSION closes down the relocating pass and prepares for
   the emission of the variable node area. *)

procedure prepare_for_emission;

var
  len: integer;

begin

  (* Set up bit map structure. *)

  bit_map := [];
  bit_index := -1;
  map_area := file_index + fixed_pointer_data_length + 1;
  last_loc := node_base;

  (* Emit the length of the variable node area. *)

  len := file_index - node_base;
  write ( env_file, len: 4 );

  (* Identify emission pass active. *)

  relocating := false;

end (* prepare_for_emission *);
$PAGE fixed_pointer_data
(* FIXED POINTER DATA emits the relative offsets for various important
   pointers. *)

procedure fixed_pointer_data;

var
  loc: integer;
  file_loc: integer;

begin
  file_loc := cursor ( env_file );
  loc := root_name^.file_loc;
  write ( env_file, loc: 4 );
  loc := root_block^.file_loc;
  write ( env_file, loc: 4 );
  loc := file_chain^.file_loc;
  write ( env_file, loc: 4 );
  loc := io_opsym^.file_loc;
  write ( env_file, loc: 4 );
  loc := cdatesym^.file_loc;
  write ( env_file, loc: 4 );
  loc := ctimesym^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_int^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_fullword^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_non_neg^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_bool^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_char^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_real^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_ptr^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_text^.file_loc;
  write ( env_file, loc: 4 );
  loc := type_options^.file_loc;
  write ( env_file, loc: 4 );
  loc := stat_io^.file_loc;
  write ( env_file, loc: 4 );
  loc := stat_program^.file_loc;
  write ( env_file, loc: 4 );
  loc := stat_math^.file_loc;
  write ( env_file, loc: 4 );
  loc := stat_special^.file_loc;
  write ( env_file, loc: 4 );
  loc := file_input^.file_loc;
  write ( env_file, loc: 4 );
  loc := file_output^.file_loc;
  write ( env_file, loc: 4 );
  loc := file_tty^.file_loc;
  write ( env_file, loc: 4 );
  loc := filettyoutput^.file_loc;
  write ( env_file, loc: 4 );

  (* Double check that we have written precisely the number of bytes earlier
     reserved for this table. *)

  assert ( cursor ( env_file ) - file_loc = fixed_pointer_data_length );
end (* fixed_pointer_data *);
$PAGE clear_map
(* CLEAR MAP writes the current segment of the relocation bit map at the
   end of the environment file and resets the map to null.  It is left
   to the caller to reset BIT_INDEX. *)

procedure clear_map;

var
  save_position: integer;

begin
  save_position := cursor ( env_file );	(* Remember where we were *)
  seek ( env_file, map_area );
  write ( env_file, bit_map );
  map_area := map_area + size ( bit_map );
  seek ( env_file, save_position );
  bit_map := [];
end (* clear_map *);
$PAGE emit_relocation_map
(* EMIT RELOCATION MAP dumps the last of the bit map, if any. *)

procedure emit_relocation_map;

begin
  if bit_index >= 0 then
    clear_map;
end (* emit_relocation_map *);
$PAGE relocate
(* RELOCATE is passed the offset of a non-NIL pointer within the node
   currently being written to the environment file.  It emits a "1" bit
   in the appropriate place in the relocation bit map. *)

procedure relocate ( offset: integer );

var
  zero_bits: integer;

begin
  zero_bits := node_base_address + offset - last_loc;
  while bit_index + zero_bits >= bit_map_upb do begin
    clear_map;
    bit_index := bit_index - bit_map_upb - 1;
  end;
  bit_index := bit_index + zero_bits + 1;
  bit_map := bit_map + [ bit_index ];
  last_loc := node_base_address + offset + 4;
end (* relocate *);
$PAGE set_context
(* SET CONTEXT establishes the base address, offset relative to its location
   within the environment file, of the next node record to be written.
   This information is used by RELOCATE to make bit map entries for pointers
   within this node which must be relocated. *)

procedure set_context ( file_loc: integer );	(* offset within file *)

begin
  node_base_address := file_loc;
end (* set_context *);
$PAGE file_location
(* FILE LOCATION returns a relative offset within the environment file
   variable node area at which a record is to be emitted.  This offset
   is always longword aligned. *)

function file_location ( size: integer ): integer;

begin
  file_location := file_index;
  file_index := ngm ( file_index + size, 4 );	(* Longword align always. *)
end (* file_location *);
$PAGE relocate pointer routines
(* Each of these routines is passed a pointer field (or a record field
   containing pointers) within the record currently being emitted to the 
   environment file, and the offset of that field from the start of the record.
   If the pointers are NIL then NIL is returned, otherwise they are converted
   to their relative offsets within the variable node area and tagged as
   requiring relocation when reloaded. *)

function relocate_nam ( n: nam; offset: integer ): nam;

begin
  if n = nil then
    relocate_nam := nil
  else begin
    relocate_nam := nam ( n^.file_loc );
    relocate ( offset );
  end;
end (* relocate_nam *);

function relocate_blk ( b: blk; offset: integer ): blk;

begin
  if b = nil then
    relocate_blk := nil
  else begin
    relocate_blk := blk ( b^.file_loc );
    relocate ( offset );
  end;
end (* relocate_blk *);

function relocate_sym ( s: sym; offset: integer ): sym;

begin
  if s = nil then
    relocate_sym := nil
  else begin
    relocate_sym := sym ( s^.file_loc );
    relocate ( offset );
  end;
end (* relocate_sym *);

function relocate_sym_list ( list: sym_list; offset: integer ): sym_list;

begin
  relocate_sym_list.first := relocate_sym ( list.first, offset + first_offset );
  relocate_sym_list.last := relocate_sym ( list.last, offset + last_offset );
end (* relocate_sym_list *);

function relocate_typ ( t: typ; offset: integer ): typ;

begin
  if t = nil then
    relocate_typ := nil
  else begin
    relocate_typ := typ ( t^.file_loc );
    relocate ( offset );
  end;
end (* relocate_typ *);

function relocate_val_ptr ( v: val_ptr; offset: integer ): val_ptr;

begin
  if v = nil then
    relocate_val_ptr := nil
  else begin
    relocate_val_ptr := v^.def_addr;
    relocate ( offset );
  end;
end (* relocate_val_ptr *);

function relocate_val ( v: val; offset: integer ): val;

begin
  relocate_val := v;
  with v do
    case kind of

      real_cst,
      string_cst,
      set_cst,
      array_cst,
      record_cst:
	relocate_val.valp := relocate_val_ptr ( valp, offset + valp_offset );

      subr_cst:
	relocate_val.blkp := relocate_blk ( blkp, offset + blkp_offset );

      others:

    end (* case *);
end (* relocate_val *);
$PAGE relocate_val_node

procedure relocate_value_node ( v: val_ptr ); forward;

procedure relocate_type_node ( t: typ ); forward;

procedure relocate_block_node ( b: blk ); forward;

procedure relocate_val_node ( v: val );

begin
  with v do begin
    case kind of

      subr_cst:
	;	(* block node itself relocated elsewhere *)

      real_cst, string_cst, set_cst, array_cst, record_cst: begin
	relocate_value_node ( valp );
      end;

      others:

    end (* case *);
  end (* with *);
end (* relocate_val_node *);
$PAGE relocate_value_node
(* RELOCATE VALUE NODE is similar to the rest of the relocation routines
   except for one point: there are no FILE_LOC and VISITED f in the
   VALUE_NODE.  To overcome this lack we overload the DEF_ADDR pointer
   field (since it is only required during code generation) and use it
   for both FILE_LOC and VISITED.

   In order to do this some minor atrocities are performed.  First we
   assume that DEF_ADDR is initially NIL, just as VISITED is initially
   FALSE in the other nodes.  During the relocation pass, whenever a
   VALUE_NODE with a NIL DEF_ADDR field is encountered it is immediately
   processed just as if its non-existant VISITED flag were FALSE. 
   The DEF_ADDR field is then set to FAKE_NIL while its other fields are 
   relocated (just as VISITED would be set to TRUE), after which it is
   used to store the value normally saved in FILE_LOC, incremented by 1.
   Since nodes are always longword aligned their relative offset in the
   environment file is even, and incrementing this offset allows the
   second or emission pass to check ODD ( ORD ( DEF_ADDR ) ) just as it
   checks VISITED in other nodes.  DEF_ADDR is then decremented to yield
   the proper file offset as well as to prevent undesired recusion,
   and the rest follows.	*)

procedure relocate_value_node (* v: val_ptr *);	(* forward declared *)

var
  node_size, ix: integer;
  temp: val_ptr;

  function value_node_size: integer;

  begin
    case v^.kind of

      scalar_cst:
	value_node_size := size ( value_node, scalar_cst );

      real_cst:
	value_node_size := size ( value_node, real_cst );

      string_cst:
	value_node_size := size ( value_node, string_cst, upperbound ( v^.str_val ) );

      set_cst:
	value_node_size := size ( value_node, set_cst, upperbound ( v^.set_val ) );

      array_cst:
	value_node_size := size ( value_node, array_cst, upperbound ( v^.elem_vals ) );

      record_cst:
	value_node_size := size ( value_node, record_cst, upperbound ( v^.elem_vals ) )

    end (* case *);
    value_node_size := ngm ( value_node_size, 4 );
  end (* value_node_size *);

begin
  if v <> nil then with v^ do begin
    if ( relocating and ( def_addr = nil ) ) orif
       ( not relocating andif odd ( ord ( def_addr ) ) ) then begin
      node_size := value_node_size;
      if relocating
	then def_addr := fake_nil
	else def_addr := val_ptr ( pred ( ord ( def_addr ) ) );
      case kind of

	array_cst, record_cst: begin
	  relocate_type_node ( struc_type );
	  for ix := 1 to upperbound ( elem_vals ) do
	    relocate_val_node ( elem_vals[ix] );
	  if not relocating then begin
	    set_context ( ord ( def_addr ) );
	    struc_type := relocate_typ ( struc_type, struc_type_offset );
	    for ix := 1 to upperbound ( elem_vals ) do
	      elem_vals[ix] := relocate_val ( elem_vals[ix], elem_vals_offset + ( ix - 1 ) * size ( val ) );
	  end;
	end;

	others:

      end (* case *);
      if relocating then
	def_addr := val_ptr ( succ ( file_location ( node_size ) ) )
      else begin
	temp := def_addr;
	def_addr := nil;
	write ( env_file, v^: node_size );
	def_addr := temp;
      end;
    end;
  end (* with *);
end (* relocate_value_node *);
$PAGE relocate_name_node

procedure relocate_name_node ( n: nam );

var
  node_size: integer;

  function name_node_size: integer;

  begin
    name_node_size := ngm ( size ( n^, upperbound ( n^.text ) ), 4 );
  end (* name_node_size *);

begin
  if n <> nil then with n^ do begin
    if visited <> relocating then begin
      node_size := name_node_size;
      visited := relocating;

      (* Assume that all name nodes will be referenced from processed
	 symbol nodes so we need not recurse over the name tree during
	 the relocation pass.  For the same reason SCOPE_CHAIN can be
	 skipped the first time around. *)

      if relocating then
	file_loc := file_location ( node_size )
      else begin
	set_context ( file_loc );
	alink := relocate_nam ( alink, alink_offset );
	zlink := relocate_nam ( zlink, zlink_offset );
	scopechain := relocate_sym ( scopechain, scopechain_offset );
	visited := false;
	write ( env_file, n^: node_size );
	visited := true;
      end;
    end;
  end (* with *);
end (* relocate_name_node *);
$PAGE relocate_symbol_nodes

procedure relocate_symbol_nodes ( first, last: sym );

var
  node_size: integer;
  current_sym, next_sym: sym;

  function symbol_node_size: integer;

  const
    sizes: array[sym_kind] of integer := (
		  size ( symbol_node, labels ),
		  size ( symbol_node, fields ),
		  size ( symbol_node, types ),
		  size ( symbol_node, consts ),
		  size ( symbol_node, vars ),
		  size ( symbol_node, values ),
		  size ( symbol_node, for_inds ),
		  size ( symbol_node, std_procs ),
		  size ( symbol_node, std_funcs ),
		  size ( symbol_node, conditions ),
		  size ( symbol_node, blocks ) );

  begin
    symbol_node_size := ngm ( sizes[current_sym^.kind], 4 );
  end (* symbol_node_size *);

begin
  current_sym := first;
  while current_sym <> nil do with current_sym^ do begin
    next_sym := current_sym^.next;
    if visited <> relocating then begin
      node_size := symbol_node_size;
      visited := relocating;
      relocate_name_node ( name );
      relocate_block_node ( block );
      assert ( scopechain = nil );
      relocate_type_node ( type_desc );
      case kind of

	fields: begin
	  relocate_type_node ( fld_record );
	  relocate_type_node ( fld_variant );
	end;

	consts, vars, values, for_inds, conditions:
	  relocate_val_node ( init_value );

	others:

      end (* case *);
      if relocating then
	file_loc := file_location ( node_size )
      else begin
	set_context ( file_loc );
	name := relocate_nam ( name, name_offset );
	block := relocate_blk ( block, block_offset );
	next := relocate_sym ( next, next_offset );
	type_desc := relocate_typ ( type_desc, type_desc_offset );
	case kind of

	  fields: begin
	    fld_record := relocate_typ ( fld_record, fld_record_offset );
	    fld_variant := relocate_typ ( fld_variant, fld_variant_offset );
	  end;

	  consts, vars, values, for_inds, conditions:
	    init_value := relocate_val ( init_value, init_value_offset );

	  others:

	end (* case *);
	visited := false;
	write ( env_file, current_sym^: node_size );
	visited := true;
	next := next_sym;
      end (* if writing *);
    end;
  exit if current_sym = last;
    current_sym := next_sym;
  end;
end (* relocate_symbol_nodes *);
$PAGE relocate_type_node

procedure relocate_type_node (* t: typ *);	(* forward declared *)

var
  node_size, ix: integer;

  function type_node_size: integer;

  const
    sizes: array[type_kind] of integer := (
		  size ( type_node, scalars ),
		  size ( type_node, bools ),
		  size ( type_node, chars ),
		  size ( type_node, ints ),
		  size ( type_node, reals ),
		  size ( type_node, sets ),
		  size ( type_node, pointers ),
		  size ( type_node, files ),
		  size ( type_node, strings ),
		  size ( type_node, arrays ),
		  size ( type_node, records, records ),
		  size ( type_node, variants, variants ),
		  size ( type_node, tags ),
		  0,
		  0,
		  size ( type_node, unknown_type ),
		  size ( type_node, indirect_type ) );

  begin
    case t^.kind of

      procs:
	type_node_size := size ( t^, procs, upperbound ( t^.params ) );

      funcs:
	type_node_size := size ( t^, funcs, upperbound ( t^.params ) );

      others:
	type_node_size := sizes[t^.kind];

    end (* case *);
    type_node_size := ngm ( type_node_size, 4 );
  end (* type_node_size *);

begin
  if ( t <> nil ) andif ( t^.visited <> relocating ) then with t^ do begin
    node_size := type_node_size;
    visited := relocating;
    relocate_symbol_nodes ( type_id, type_id );
    case kind of

      bools, ints, chars, scalars: begin
	relocate_type_node ( base_type );
	case kind of

	  bools, scalars:
	    relocate_symbol_nodes ( cst_list.first, cst_list.last );

	  others:

	end;
      end;

      sets:
	relocate_type_node ( set_element_type );

      pointers: begin
	relocate_type_node ( target_type );
	relocate_symbol_nodes ( heap_class, heap_class );
      end;

      arrays: begin
	relocate_type_node ( element_type );
	relocate_type_node ( index_type );
      end;

      files: begin
	relocate_type_node ( component_type );
	relocate_symbol_nodes ( file_class, file_class );
      end;

      records, variants: begin
	relocate_symbol_nodes ( field_list, field_list );
	relocate_type_node ( variant_tag );
	if kind = variants then begin
	  relocate_type_node ( tag );
	  relocate_type_node ( next_variant );
	end;
      end;

      tags: begin
	relocate_symbol_nodes ( tag_field, tag_field );
	relocate_type_node ( tag_type );
	relocate_type_node ( tag_recvar );
	relocate_type_node ( first_variant );
      end;

      procs, funcs: begin
	relocate_block_node ( class_block );
	relocate_type_node ( return_type );
	for ix := 1 to upperbound ( params ) do
	  relocate_type_node ( params[ix].parm_type );
      end;

      unknown_type, indirect_type:
	relocate_type_node ( actual_type );

      others:

    end (* case *);
    if relocating then
      file_loc := file_location ( node_size )
    else begin
      set_context ( file_loc );
      type_id := relocate_sym ( type_id, type_id_offset );
      case kind of

	bools, ints, chars, scalars: begin
	  base_type := relocate_typ ( base_type, base_type_offset );
	  case kind of

	    bools, scalars:
	      cst_list := relocate_sym_list ( cst_list, cst_list_offset );

	    others:

	  end;
	end;

	sets:
	  set_element_type := relocate_typ ( set_element_type, set_element_type_offset );

	pointers: begin
	  target_type := relocate_typ ( target_type, target_type_offset );
	  heap_class := relocate_sym ( heap_class, heap_class_offset );
	end;

	arrays: begin
	  element_type := relocate_typ ( element_type, element_type_offset );
	  index_type := relocate_typ ( index_type, index_type_offset );
	end;

	files: begin
	  component_type := relocate_typ ( component_type, component_type_offset );
	  file_class := relocate_sym ( file_class, file_class_offset );
	end;

	records, variants: begin
	  field_list := relocate_sym ( field_list, field_list_offset );
	  variant_tag := relocate_typ ( variant_tag, variant_tag_offset );
	  if kind = variants then begin
	    tag := relocate_typ ( tag, tag_offset );
	    next_variant := relocate_typ ( next_variant, next_variant_offset );
	  end;
	end;

	tags: begin
	  tag_field := relocate_sym ( tag_field, tag_field_offset );
	  tag_type := relocate_typ ( tag_type, tag_type_offset );
	  tag_recvar := relocate_typ ( tag_recvar, tag_recvar_offset );
	  first_variant := relocate_typ ( first_variant, first_variant_offset );
	end;

	procs, funcs: begin
	  class_block := relocate_blk ( class_block, class_block_offset );
	  return_type := relocate_typ ( return_type, return_type_offset );
	  for ix := 1 to upperbound ( params ) do
	    params[ix].parm_type := relocate_typ ( params[ix].parm_type, params_offset + parm_type_offset + ( ix - 1 ) * size ( param_desc ) );
	end;

	unknown_type, indirect_type:
	  actual_type := relocate_typ ( actual_type, actual_type_offset );

	others:

      end (* case *);
      visited := false;
      write ( env_file, t^: node_size );
      visited := true;
    end (* if writing *);
  end (* with *);
end (* relocate_type_node *);
$PAGE relocate_block_node

procedure relocate_block_node (* b: blk *);	(* forward declared *)

var
  node_size: integer;

  function block_node_size: integer;

  const
    sizes: array[block_kind] of integer := (
		  size ( block_node, root_blk ),
		  size ( block_node, program_blk ),
		  size ( block_node, module_blk ),
		  size ( block_node, subr_blk ),
		  size ( block_node, class_blk ),
		  size ( block_node, extern_blk ) );

  begin
    block_node_size := ngm ( sizes[b^.kind], 4 );
  end (* block_node_size *);

begin
  if ( b <> nil ) andif ( b^.visited <> relocating ) then with b^ do begin
    visited := relocating;
    node_size := block_node_size;
    relocate_block_node ( peer );
    relocate_block_node ( children );
    relocate_symbol_nodes ( return_sym, return_sym );
    relocate_symbol_nodes ( parm_list.first, parm_list.last );
    relocate_symbol_nodes ( label_list.first, label_list.last );
    relocate_symbol_nodes ( type_list.first, type_list.last );
    relocate_symbol_nodes ( id_list.first, id_list.last );
    assert ( calls = nil );
    case kind of

      program_blk, module_blk:
	relocate_name_node ( id );

      subr_blk:
	relocate_symbol_nodes ( subr_sym, subr_sym );

      class_blk:
	relocate_type_node ( class_type );

      others:

    end (* case *);
    if relocating then
      file_loc := file_location ( node_size )
    else begin	(* write record to environment file *)
      set_context ( file_loc );
      parent := relocate_blk ( parent, parent_offset );
      peer := relocate_blk ( peer, peer_offset );
      children := relocate_blk ( children, children_offset );
      return_sym := relocate_sym ( return_sym, return_sym_offset );
      parm_list := relocate_sym_list ( parm_list, parm_list_offset );
      label_list := relocate_sym_list ( label_list, label_list_offset );
      type_list := relocate_sym_list ( type_list, type_list_offset );
      id_list := relocate_sym_list ( id_list, id_list_offset );

      (* ??? *)

      dump_switches := nil;

      owner := relocate_blk ( owner, owner_offset );
      downward_call_thread := relocate_blk ( downward_call_thread, downward_call_thread_offset );
      upward_call_thread := relocate_blk ( upward_call_thread, upward_call_thread_offset );
      lex_thread := relocate_blk ( lex_thread, lex_thread_offset );
      case kind of

	program_blk, module_blk:
	  id := relocate_nam ( id, id_offset );

	subr_blk:
	  subr_sym := relocate_sym ( subr_sym, subr_sym_offset );

	class_blk:
	  class_type := relocate_typ ( class_type, class_type_offset );

	others:

      end (* case *);
      visited := false;
      write ( env_file, b^: node_size );
      visited := true;
    end (* if writing *);
  end (* with *);
end (* relocate_block_node *);
$PAGE vax_save_environment - body
begin

  (* Open the environment file and initialize data structures. *)

  initialize_environment;

  (* Emit header information. *)

  environment_header;

  (* Emit global data. *)

  initial_global_data;

  (* Set up for relocation walk of symbol table. *)

  prepare_for_relocation;

  (* Perform relocation pass over symbol table. *)

  relocate_block_node ( root_block );

  (* Set up for emission walk of symbol table. *)

  prepare_for_emission;

  (* Emit it. *)

  relocate_block_node ( root_block );

  (* Dump fixed length pointer data. *)

  fixed_pointer_data;

  (* Emit relocation bit map. *)

  emit_relocation_map;

  (* Clean up, close environment file, etc. *)

  terminate_environment;

  exception

    io_error: begin

      (* Some problem in writing the environment file.  Just return
	 error indication. *)

      return;
    end;
end (* vax_save_environment *).
  wb!E