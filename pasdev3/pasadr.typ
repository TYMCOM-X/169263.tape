(* ADDRESSING INFORMATION for the VAX-11/780 *)

const
  byte_size = 8;			(* same as above *)
  max_set_length = 128;

const
  int_std_min = -20000000000b;			(* standard minimum(integer) *)
  int_std_max =  17777777777b;			(* standard maximum(integer) *)

type
  unit_range = 0..37777777777B;			(* addressing units in memory *)
  bit_range =  0..37777777777b;			(* bit offsets specifiable *)
  char_range = 0..177777b;			(* string length range *)
  align_range = 0..64;				(* possible bit alignments *)

  prec_type = 1..16;				(* possible precisions *)

  set_element_range = 0..32767;
  set_len_range = 0..32768;

  id_range = 0 .. 777777b; (* type for identifier numbers in nodes *)
  index_range = 0 .. 4095; (* type for index numbers (for basic and subroutine blocks) *)

  parm_range = 0 .. 1023;			(* for counting subroutine parameters *)
  parm_index = 1 .. maximum (parm_range);

const
  set_alignment = 32;			(* intrinsic alignments of structured types *)
  array_alignment = 32;
  flex_arr_alignment = 32;
  flex_arr_desc_size = 32;
  flex_str_desc_size = 16;		(* NOT the same as for flex arrays *)
  record_alignment = 32;
  string_alignment = 16;

   