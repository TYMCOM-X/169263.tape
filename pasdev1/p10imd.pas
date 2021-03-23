(*********     Initialization -- Machine-dependent Declarations

**********     PDP-10

*********)


module p10imd;

$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM pasdat.inc
$SYSTEM pasini.inc
$SYSTEM ptmimd.typ

public const
    mword_size: integer = 36;
    max_real: real_type = 1.0e+38;
    max_char: integer = #O177;
    fnamsize: integer = 80;
    fnamextern: extname = 'FNAME.';
    trnamextern: extname = 'TRACE';
    exnamextern: extname = 'EX.STR';
    condnamextern: condnames = ( 'EX.MTH', 'EX.IO', 'EX.USR', 'EX.ATN',
                                 'EX.STO', 'EX.STK', 'EX.SPC' );
$PAGE target machine constants
$INCLUDE ptmcon.inc
$PAGE sys_ppf
(*  SYS PPF will enter the system-dependent predefined procedures and functions
    into the initial symbol table.  *)

public procedure sys_ppf;

begin
  predef ('GETCHANNEL', 'GTCHN.', consts, prtyp0 (type_int));

  predef ('FREECHANNEL', 'FRCHN.', consts, prtyp1 (values, type_int, nil));
end;
$PAGE init_tal_tables
(*  INIT TAL TABLES initializes the type allocation tables used by PASTAL.  *)

external var tal_tables: save_pointer;

procedure init_tal_tables  options special(coercions);

$INCLUDE pastal.prm
$PAGE
type
    x_packing_table = array [packing_contexts] of integer;


static var
    r_36_36: rule_node := ( nil, maximum (bit_range), 36, 36 );
    r_18_18: rule_node := ( nil, 18, 18, 18 );
    r_1_1  : rule_node := ( nil, 36, 1, 1 );

    rules: array [1..3] of rule;

procedure make_rules;
begin
  new (rules [3]);      (* Rule 3 - *)
  rules [3]^ := r_36_36;        (*    0..*:  36, 36 *)

  new (rules [1]);      (* Rule 1 - *)
  rules [1]^ := r_1_1;  (*   0..36:  1, 1 *)
  rules [1]^.next := rules [3]; (*   37..*:  36, 36 *)

  new (rules [2]);      (* Rule 2 - *)
  rules [2]^ := r_18_18;        (*   0..18:  18, 18 *)
  rules [2]^.next := rules [3]; (*   19..*:  36, 36 *)
end (* make_rules *);
$PAGE
static var
    x_efw: bit_range := 36;
    x_efa: bit_range := 36;

    x_str_lw_width: bit_range := 36;
    x_str_char_size: bit_range := 7;

    x_real_base_size: array [prec_type] of bit_range :=
      ( 36, 36, 36, 36, 36, 36, 36, 72, 72, 72, 72, 72, 72, 72, 72, 72 );
    x_pointer_base_size: bit_range := 18;
    x_file_base_size: bit_range := 18;
    x_subr_base_size: bit_range := 36;

    x_allocation_tables: array [type_kind] of x_packing_table :=
      ( ( 1, 2, 3, 3, 3 ),      (* scalars *)
        ( 1, 2, 3, 3, 3 ),      (* bools *)
        ( 1, 2, 3, 3, 3 ),      (* chars *)
        ( 1, 2, 3, 3, 3 ),      (* unsigned integers *)
        ( 3, 3, 3, 3, 3 ),      (* reals *)
        ( 1, 2, 3, 3, 3 ),      (* sets *)
        ( 1, 2, 3, 3, 3 ),      (* pointers *)
        ( 1, 2, 3, 3, 3 ),      (* files *)
        ( 3, 3, 3, 3, 3 ),      (* non-varying strings *)
        ( 3, 3, 3, 3, 3 ),      (* arrays *)
        ( 3, 3, 3, 3, 3 ),      (* records *)
        ( 3, 3, 3, 3, 3 ),      (* variants *)
        ( 3, 3, 3, 3, 3 ),      (* tags *)
        ( 3, 3, 3, 3, 3 ),      (* procs *)
        ( 3, 3, 3, 3, 3 ),      (* funcs *)
        ( 3, 3, 3, 3, 3 ),      (* unknown_type *)
        ( 3, 3, 3, 3, 3 ) );    (* indirect_type *)
    x_packed_scalar_rules: array [scalars..chars] of x_packing_table :=
      ( ( 1, 2, 3, 3, 3 ),      (* scalars *)
        ( 1, 2, 3, 3, 3 ),      (* bools *)
        ( 1, 2, 3, 3, 3 ) );    (* chars *)
    x_integer_rules: array [boolean (* signed *), boolean (* packed *)] of x_packing_table :=
    ( ( ( 1, 2, 3, 3, 3 ),      (* unsigned / unpacked *)
        ( 1, 2, 3, 3, 3 ) ),    (* unsigned /   packed *)
      ( ( 2, 2, 3, 3, 3 ),      (*   signed / unpacked *)
        ( 2, 2, 3, 3, 3 ) ) );  (*   signed /   packed *)
    x_var_string_rules: x_packing_table :=
        ( 3, 3, 3, 3, 3 );      (* varying strings *)
    x_arr_desc_rules: x_packing_table :=
        ( 3, 3, 3, 3, 3 );      (* array descriptors *)
    x_str_desc_rules: x_packing_table :=
        ( 3, 3, 3, 3, 3 );      (* string descriptors *)

    x_pl_base: bit_range := 0;

    x_rv_addr_loc: rv_loc_type := rv_nowhere;
    x_rv_value_loc: rv_loc_type := rv_nowhere;

    x_a_prm_size: bit_range := 36;
    x_a_prm_alignment: align_range := 36;

    x_pba_types: set of type_kind := [arrays, strings, records];
    x_pbv_limit: bit_range := 72;
    x_pba_retsym: boolean := false;
$PAGE
$INCLUDE pastal.ini
$PAGE init_alc_tables
(*  INIT ALC TABLES initializes the storage allocation tables used by PASALC.  *)

external var alc_tables: save_pointer;

procedure init_alc_tables  options special(coercions);
$PAGE
$INCLUDE pasalc.prm
$PAGE
static var
    x_alc_area_descs: array [area_class] of area_desc :=
      ( ( 0, true ), (* initialized static *)
        ( 0, true ), (* uninitialized static *)
        ( 0, true ), (* condition cells *)
        ( 4, true ), (* stack local variable area *)
        ( 0, false ) ); (* unused *)

    x_loc_area: local_areas := pos_stack_area;
    x_retsym_loc: array [boolean (* passed by address *)] of rv_loc_type :=
      ( ( false, true, true ), ( false, true, true ) );
    x_parmlist_loc: array [boolean (* size <= limit *)] of pl_loc_type :=
      ( ( pos_stack_area, true ), ( pos_stack_area, true ) );
    x_pl_size_limit: unit_range := 6;
    x_parm_ptr_size: unit_range := 1;
    x_basic_alignment: align_range := 1;
    x_cond_size: unit_range := 1;
    x_cond_alignment: align_range := 1;
$PAGE
$INCLUDE pasalc.ini
$PAGE tm_constants
(*  TM_CONSTANTS will initialize the PTMCON variables to values which
    characterize the target machine.  *)

public procedure tm_constants;

begin
  tmprefix := 'P10';
  ttyiname := 'TTY:';
  ttyoname := 'TTY:';
  rel_extension := 'REL';
  have_checkout := true;
  have_optimizer := true;
  radix := octal_radix;
  adr_width := 6;
  srealprec := 7;
  set_lwb_limit := 0;
  set_upb_limit := 32767;
  set_size_limit := 32768;
  set_lquantum := 1;
  set_uquantum := 1;
  set_lbase := maximum (integer);
  set_ubase := minimum (integer);
  byte_size := 36;
  int_prec_limit := 36;
  qbl_allowed := true;

  init_tal_tables;
  init_alc_tables;
end (* tm_constants *);
$PAGE dcl_stt_constants
(* DCL SCONSTS will declare the elements of the scalar status types.  *)

public procedure dcl_sconsts;

begin
  stt_constant ('IO_OK'  , stat_io, 0);
  stt_constant ('IO_NOVF', stat_io, 1);
  stt_constant ('IO_POVF', stat_io, 2);
  stt_constant ('IO_DGIT', stat_io, 3);
  stt_constant ('IO_GOVF', stat_io, 4);
  stt_constant ('IO_INTR', stat_io, 5);
  stt_constant ('IO_REWR', stat_io, 6);
  stt_constant ('IO_EOF' , stat_io, 7);
  stt_constant ('IO_OUTF', stat_io, 8);
  stt_constant ('IO_INPF', stat_io, 9);
  stt_constant ('IO_SEEK', stat_io, 10);
  stt_constant ('IO_ILLC', stat_io, 11);
  stt_constant ('IO_NEMP', stat_io, 12);
  stt_constant ('IO_OPNF', stat_io, 13);

  stt_constant ('MATH_OK',              stat_math, 0);
  stt_constant ('MATH_FLT_UND',         stat_math, 1);
  stt_constant ('MATH_FLT_OVF',         stat_math, 2);
  stt_constant ('MATH_INT_OVF',         stat_math, 3);
  stt_constant ('MATH_ZERO_DIVIDE',     stat_math, 4);
  stt_constant ('MATH_ARG_ARCSIN',      stat_math, 5);
  stt_constant ('MATH_ARG_ARCCOS',      stat_math, 6);

  stt_constant ('PROGRAM_OK',           stat_program, 0);
  stt_constant ('PROGRAM_ASSERTION',    stat_program, 1);
  stt_constant ('PROGRAM_CASE',         stat_program, 2);
  stt_constant ('PROGRAM_COMPATIBILITY',stat_program, 3);
  stt_constant ('PROGRAM_FILE',         stat_program, 4);
  stt_constant ('PROGRAM_POINTER',      stat_program, 5);
  stt_constant ('PROGRAM_SUBSTRING',    stat_program, 6);
  stt_constant ('PROGRAM_SUBSCRIPT',    stat_program, 7);
  stt_constant ('PROGRAM_VALUE',        stat_program, 8);

  stt_constant ('SPECIAL_OK',           stat_special, 0);
  stt_constant ('SPECIAL_ILL_MEM_REF',  stat_special, 1);
  stt_constant ('SPECIAL_ILL_INST',     stat_special, 2);
  stt_constant ('SPECIAL_NEW_NEG',      stat_special, 3);
  stt_constant ('SPECIAL_DISP_PTR',     stat_special, 4);
  stt_constant ('SPECIAL_DISP_TWICE',   stat_special, 5);
end.
    