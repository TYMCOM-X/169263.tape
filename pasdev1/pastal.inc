external procedure tal_init;
external procedure alloc_type ( typ );
external procedure alc_subr ( typ; sym; sym );
external procedure alc_data (typ; var bit_range; var align_range );
external procedure size_of ( data_type: typ; alloc_size: boolean;
			     var a, b, c, d: integer; var gruesome: boolean );
external function  p_b_address ( typ ): boolean;
external function  passed_by_address ( sym ): boolean;
 