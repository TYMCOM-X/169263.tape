external var
  p4_error_level: error_levels;
external procedure fatal_error ( string );
external procedure gen_code ( var code_size, const_size, static_size: unit_range );
external procedure store (source, dest: addr_desc; align: data_alignment);
external function get_temp (addr_desc; vax_type): addr_desc;

external var
  psect_id: array[psect_type] of byte;
  psectattrs: array[psect_type] of set of psect_attributes;
    