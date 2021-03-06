$INCLUDE passet.typ

external function new_svector ( n_sets: set_number; n_elems: elem_number ): svector;
external procedure clr_set ( v: svector; i: set_number );
external procedure unv_set ( v: svector; i: set_number );
external procedure add_elem ( v: svector; i: set_number; e: elem_number );
external procedure del_elem ( v: svector; i: set_number; e: elem_number );
external procedure mov_set ( v: svector; i: set_number; w: svector; j: set_number );
external procedure cpy_set ( v: svector; i, j: set_number );
external procedure union ( v: svector; i, j: set_number );
external procedure intersect ( v: svector; i, j: set_number );
external procedure subtract ( v: svector; i, j: set_number );
external function set_eq ( v: svector; i, j: set_number ): boolean;
external function in_set ( v: svector; i: set_number; e: elem_number ): boolean;
external function is_empty ( v: svector; i: set_number ): boolean;
external procedure del_svector ( v: svector );
    