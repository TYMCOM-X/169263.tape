module realhex options special (ptr, coercions), nocheck;

type
  d_real = minimum ( real ) .. maximum ( real ) prec 16;
  h_real = packed array[1..8] of 0 .. #HFFFF;
  real_format = (
 s_format,       (* Single precision *)
        d_format,       (* Double precision *)
        g_format,       (* VAX only, extended exponent precision *)
   h_format );     (* VAX only, ultra-extended precision *)
$PAGE declarations
type
  word = 0 .. #HFFFF;
  byte = 0 .. #HFF;
  format_parameters = array[real_format] of integer;
  conversion_ptr = ^ conversion_record;
  conversion_record = packed record
    case boolean of
      true: (
        case real_format of
     s_format: ( reals: real );
    d_format: ( reald: d_real );
          g_format: ( realg: packed array[1..4] of 0..#HFFFF );
         h_format: ( realh: h_real ) );
      false: (
$IF P10
     xreal: packed array[1..4] of 0 .. #O777777 )
$ENDIF

$IF VAX
       xreal: packed array[1..8] of word )
$ENDIF

$IF M68
        xreal: packed array[1..8] of byte )
$ENDIF
    end;

const
  sfw = 15;
  dfw = 24;
  blank = ' ';
  exp_field_width: format_parameters := ( 2, 2, 3, 4 );
  sign: array[boolean] of char := ( '+', '-' );
  max_exponent: format_parameters := (
$IF P10                     #O377, #O377, 0, 0 );
$IF VAX                  #HFF, #HFF, 0, #H7FFF );
$IF M68                       #H7F, #HFF, 0, 0 );
  minimal_length: format_parameters := (
$IF P10                  8, 16, 0, 0 );
$IF VAX                 6, 14, 0, 29 );
$IF M68                        6, 14, 0, 0 );

  mantissa_length: format_parameters := (
$IF P10                    0, 0, 0, 0 );
$IF VAX                  2, 4, 0, 0 );
$IF M68                  3, 7, 0, 0 );

  exp_offset: format_parameters := ( 
$IF P10                 #O200, #O200, 0, 0 );
$IF M68                  #H40, #H80, 0, 0 );
$IF VAX                    #H80, #H80, #H400, #H4000 );

$IF VAX
  vaxupb: format_parameters := ( 2, 4, 4, 8 );
  vax_sign_bit = #H8000;
  left_7_bits = #H200;
  right_7_bits = #H80;
$ENDIF
$PAGE sign_extend

function sign_extend (value: integer): integer;

begin
  assert ( value >= 0 );
  if value < #H8000
    then sign_extend := value
    else sign_extend := value - #H10000;
end (* sign_extend *);
$PAGE realdh - d to h conversion
$IF VAX
public function realdh ( r: d_real ): h_real;

var
  cvt: conversion_record;
  i, loop_index, carry: integer;

begin
  if r = 0.0 then begin        (* Short circuit return *)
    for loop_index := 1 to 8 do
      realdh[loop_index] := 0;
    return;
  end;
  with cvt do begin
    reald := abs ( r );
    realdh[1] := sign_extend (  realh[1] div right_7_bits
                  - exp_offset[d_format]
                + exp_offset[h_format]
                + vax_sign_bit * ord ( r < 0.0 ) );
    carry := realh[1] mod right_7_bits;
    for loop_index := 2 to 4 do begin
      i := carry * #H10000 + realh[loop_index];
      realdh[loop_index] := sign_extend ( i div right_7_bits );
      carry := i mod right_7_bits;
    end;
    realdh[5] := sign_extend ( carry * left_7_bits );
    for loop_index := 6 to 8 do
      realdh[loop_index] := 0;
  end;
end (* realdh *);
$ENDIF
$PAGE realhd - h to d format conversion
$IF VAX
public function realhd ( h: h_real ): d_real;

var
  cvt1, cvt2: conversion_record;
  i, loop_index, carry, exponent: integer;

begin
  with cvt1 do begin
    if h[1] = 0 then begin
      realhd := 0.0;
      return;
    end;
    realh := h;
    exponent := realh[1] mod vax_sign_bit - exp_offset[h_format] + exp_offset[d_format];

    (* First round up if necessary *)

    (* Later *)

    assert ( exponent <= max_exponent[d_format] );
    carry := realh[5] div left_7_bits;
    for loop_index := 4 downto 2 do begin
      cvt2.realh[loop_index] := sign_extend ( ( realh[loop_index] mod left_7_bits ) * right_7_bits + carry );
      carry := realh[loop_index] div left_7_bits;
    end;
    cvt2.realh[1] := sign_extend ( carry + exponent * right_7_bits + vax_sign_bit * ord ( realh[1] >= vax_sign_bit ) );
    realhd := cvt2.reald;
  end;
end (* realhd *);
$ENDIF
$PAGE hexreal
public procedure hexreal (
     real_address: ptr;      (* Address of real number for result *)
       format: real_format;    (* Format of result *)
        str: string );          (* String from which to extract it *)

var
  i, j, k, loop_index, carry, exponent: integer;
  negative, negative_exponent: boolean;
  str1: string;
  cvt: conversion_record;
  cvt_ptr: conversion_ptr;

begin
$IF VAX
  assert ( format <> g_format );
$ENDIF

$IFNOT VAX
  assert ( format in [s_format, d_format] );
$ENDIF
  cvt_ptr := real_address;
  str1 := substr ( str, index ( str, '[' ) + 1 );
  i := search ( str1, [blank] ) - 1;
  getstring ( substr ( str1, 2 ), exponent:i:H );
  if str1[1] = '-' then
    exponent := - exponent;
  exponent := exponent + exp_offset[format];
  assert ( ( exponent >= 0 ) andif ( exponent <= max_exponent[format] ) );
  i := search ( substr ( str1, i ), [ '-', '+' ] ) + i - 1;
  str1 := substr ( str1, i );
  negative := str1[1] = '-';

  (* If the first mantissa bit is zero then the number is zero. *)

  if str1[2] = '0' then with cvt_ptr^ do begin
    case format of
      s_format:
        reals := 0.0;
      d_format:
        reald := 0.0;
$IF VAX
      h_format: begin
 for loop_index := 1 to 8 do
     realh[loop_index] := 0;
      end
$ENDIF
    end;
    return;
  end;
  str1 := substr ( str1, 2, length ( str1 ) - 2 );
  assert ( length ( str1 ) >= minimal_length[format] );
  with cvt do begin

$IF M68
  for loop_index := 1 to mantissa_length[format] do begin
    getstring ( substr ( str1, loop_index * 2 - 1, 2 ), i:2:H );
    xreal[loop_index] := i;
  end;
  if format = s_format then
    xreal[4] := ord ( negative ) * #H80 + exponent
  else begin
    xreal[7] := ( xreal[7] div 2 ) * 2 + ord ( negative );
    xreal[8] := exponent;
  end;
$ENDIF

$IF VAX
    case format of
      s_format, d_format: begin
      getstring ( substr ( str1, 1, 2 ), i:2:H );
   xreal[1] := sign_extend ( vax_sign_bit * ord ( negative ) + exponent * #H80 + ( i - #H80 ) );
 for loop_index := 2 to mantissa_length[format] do begin
         getstring ( substr ( str1, loop_index * 4 - 5, 4 ), i:4:H );
          xreal[loop_index] := sign_extend ( i );
     end;
      end;

      h_format: begin
     realh[1] := sign_extend ( vax_sign_bit * ord ( negative ) + exponent );
       if str1[30] in ['8', '9', 'A' .. 'F']
   then carry := 1
       else carry := 0;
    for loop_index := 8 downto 2 do begin
   getstring ( substr ( str1, 4 * loop_index - 7, 4 ), i:4:H );
          i := i + carry;
       realh[loop_index] := sign_extend ( i mod #H10000 );
   carry := i div #H10000;
     end;
      end;
    end;
$ENDIF

$IF P10
  getstring ( substr ( str1, 1, 7 ), i:7:H );
  carry := i mod 2;
  i := i div 2;
  xreal[1] := exponent * #O1000 + i div #O1000000;
  xreal[2] := i mod #H1000000;
  if format = d_format then begin
    getstring ( substr ( str1, 8, 4 ), i:4:H );
    xreal[3] := carry * #O200000 + i;
    getstring ( substr ( str1, 12, 5 ), i:5:H );
    xreal[4] := i div 4;
  end;
  if negative then
    if format = s_format
      then reals := - reals
      else reald := - reald;
$ENDIF

  case format of

    s_format:
      cvt_ptr^.reals := reals;

    d_format:
      cvt_ptr^.reald := reald;

    h_format:
      cvt_ptr^.realh := realh;
  end;
  end (* with *);
end (* hexreal *);
$PAGE realhex
public function realhex (
        real_address: ptr;      (* Call with ADDRESS (real #) *)
      format: real_format )   (* Indicates whether s, d, g, or h format *)
    : string;

const
  shift_0 = 1;
  shift_1 = 2;
  carry_0 = 0;
  carry_1 = 1;

var
  cvt_ptr: conversion_ptr;
  cvt: conversion_record;
  i, shift, loop_index, carry, exponent: integer;
  field: string;
  negative: boolean;
$IF VAX
  vaxdreal: d_real;
$ENDIF

  procedure add_hex_field ( value, field_width: integer );

  begin
    putstring ( field, value:field_width:H );
    realhex := realhex || field;
  end;

begin

$IF VAX
  assert ( format <> g_format );
$ENDIF

$IFNOT VAX
  assert ( format in [s_format, d_format] );
$ENDIF

  cvt_ptr := real_address;
  with cvt do begin

    (* Save sign and get absolute value and E-format representation of # *)

    case format of

      s_format: begin
      reals := abs ( cvt_ptr^.reals );
      negative := cvt_ptr^.reals < 0.0;
     putstring ( realhex, cvt_ptr^.reals:sfw );
      end;

      d_format: begin
       reald := abs ( cvt_ptr^.reald );
      negative := cvt_ptr^.reald < 0.0;
     putstring ( realhex, cvt_ptr^.reald:dfw );
      end;

$IF VAX
      h_format: begin
      realh := cvt_ptr^.realh;
      negative := realh[1] >= vax_sign_bit;
 if negative then
        realh[1] := realh[1] - vax_sign_bit;
        vaxdreal := realhd ( cvt_ptr^.realh );
        putstring ( realhex, vaxdreal:dfw );
      end;
$ENDIF
    end (* case *);

    realhex := realhex || ' [';

$IF VAX
    case format of

      s_format, d_format:
  exponent := realh[1] div #H80;

      h_format:
     exponent := realh[1]
    end;
$ENDIF

$IF P10
    exponent := xreal[1] div #O1000;
$ENDIF

$IF M68
    exponent := xreal[mantissa_length[format]+1];
$ENDIF

    if exponent <> 0 then
      exponent := exponent - exp_offset[format];    (* Normalize *)
    putstring ( field, sign[exponent < 0.0], abs ( exponent ): exp_field_width[format]:H, blank );
    realhex := realhex || field || blank || sign[negative];

$IF P10
    i := ( ( xreal[1] mod #O1000 ) * #O1000000 + xreal[2] ) * 2;
    if format = d_format then
      i := i + xreal[3] div #O200000;
    add_hex_field ( i, 7 );
    if format = d_format then begin
      i := xreal[3] mod #O200000;
      add_hex_field ( i, 4 );
      i := xreal[4] * 4;
      add_hex_field ( i, 5 );
    end;
$ENDIF

$IF M68
    for loop_index := 1 to mantissa_length[format] do begin
      add_hex_field ( xreal[loop_index], 2 );
    end;
$ENDIF

$IF VAX
    if format in [s_format, d_format] then begin
      if reals <> 0.0
       then i := xreal[1] mod right_7_bits + right_7_bits
    else i := xreal[1];
      add_hex_field ( i, 2 );
      carry := carry_0;
      shift := shift_0;
    end
    else begin
      if realh[1] = 0
  then carry := carry_0
 else carry := carry_1;
      shift := shift_1;
    end;
    for loop_index := 2 to vaxupb[format] do begin
      i := carry * #H10000 + realh[loop_index];
      carry := i mod shift;
      i := i div shift;
      add_hex_field ( i, 4 );
    end;
    if format = h_format then begin
      add_hex_field ( carry * right_7_bits, 1 );
    end;
$ENDIF

    realhex := realhex || ']';
  end;
end (* realhex *).
     