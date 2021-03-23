external function makestring ( len: char_range ): val;
external function makeset ( min_range, max_range: bit_range ): val;
external function makearray ( n_comps: integer ): val;
external function makerecord ( n_fields: integer ): val;
external function cst_scalar ( svalue: integer ): val;
external function mkreal ( rval: real_type; rprec: prec_type ): val;
