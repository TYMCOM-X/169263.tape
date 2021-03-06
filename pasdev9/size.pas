$PAGE size routines
(* These routines return the size in bytes of various symbol table nodes.
   Note that the EXTENT function CANNOT be used since the nodes may have
   been loaded from an environment and thus not allocated via calls to NEW. *)

function name_node_size ( n: nam ): integer;

begin
  name_node_size := size ( n^, upperbound ( n^.text ) );
end (* name_node_size *);

function block_node_size ( b: blk ): integer;

const
  sizes: array[block_kind] of integer := (
		size ( block_node, root_blk ),
		size ( block_node, program_blk ),
		size ( block_node, module_blk ),
		size ( block_node, subr_blk ),
		size ( block_node, class_blk ),
		size ( block_node, extern_blk ) );

begin
  block_node_size := sizes[b^.kind];
end (* block_node_size *);

function type_node_size ( t: typ ): integer;

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
		size ( type_node, unknown_type ) );

begin
  case b^.kind of

    procs:
      type_node_size := size ( b^, procs, upperbound ( b^.params ) );

    funcs:
      type_node_size := size ( b^, funcs, upperbound ( b^.params ) );

    others:
      type_node_size := sizes[b^.kind]

  end (* case *);
end (* type_node_size *);

function symbol_node_size ( s: sym ): integer;

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
  symbol_node_size := sizes[s^.kind];
end (* symbol_node_size *);

function value_node_size ( v: val_ptr );

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
      value_node_size := size ( value_node, record_cst, upperbound ( v^.elem_vals ) );

    others:
      assert ( false )

  end (* case *);
end (* value_node_size *);
 