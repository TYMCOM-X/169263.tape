type
  d_real = minimum ( real ) .. maximum ( real ) prec 16;
  h_real = packed array[1..8] of 0 .. #HFFFF;
  real_format = (
      s_format,       (* Single precision *)
        d_format,       (* Double precision *)
        g_format,       (* VAX only, extended exponent precision *)
   h_format );     (* VAX only, ultra-extended precision *)

external function realhex (
       real_address: ptr;      (* Call with ADDRESS (real #) *)
      format: real_format )   (* Indicates whether s, d, g, or h format *)
    : string;             (* Returns # in ASCII E-format and in hex *)

external procedure hexreal (
  real_address: ptr;      (* Address of real number for result *)
       format: real_format;    (* Format of result *)
        str: string );          (* String from which to extract it *)

external function realhd ( h_real ): d_real;   (* H to D conversion *)

external function realdh ( d_real ): h_real; (* D to H conversion *)
 