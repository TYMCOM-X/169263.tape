external function parmsym ( var_parm: boolean; parm_type: typ ): sym;
external function prtyp0 ( return_type: typ ): typ;
external function prtyp1 ( sym_kind; typ; typ ): typ;
external function prtyp2 ( sym_kind; sym_kind; typ; typ; typ ): typ;
external procedure predef ( int_name, ext_name: line_string; kind: sym_kind; sym_type: typ );
external procedure stt_constant ( line_string; typ; integer );
  