(* X4MOVE.TYP - specification of types required to communicate with the move module. *)

const
  bits_per_coefficient = 9;
  coef_excess = 2 ** (bits_per_coefficient - 1);
 
type
  coef_type = 0..2 ** bits_per_coefficient - 1;	(* for internal use *)
  ext_coef_type = -coef_excess..(coef_excess - 1);
  coef_index_type = 1..18;
  packed_coef_array = packed array [coef_index_type] of coef_type;
  structure_fitness_type = 4..126;		(* 4..64 for loss; 65 tie; 66..126 for win *)

  (* Parameters specific to determining the fitness of a given structure. *)

  fitness_parameter_record = record
    opponents_primary_limit,
    opponents_critical_limit: 1..maximum (integer);
    opponents_strategy: packed_coef_array;
    primary_limit,
    critical_limit: 1..maximum (integer)
  end;
