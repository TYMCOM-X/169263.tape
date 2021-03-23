(* TST008 - size, lower/upper bound and dimension operations. *)

program tst008;

type
  aset = set of 0..39;
  notflx = packed array [1..17] of 0..255;
  flx_fstr = packed array [1..*] of char;
  frec = packed record
    f1: boolean;
    case boolean of
      true: (f2: string[20]);
      false: (f3: set of 0..35;
              f4: flx_fstr )
  end;
  irec = packed record
    f1: boolean;
    f2: char;
    f3: packed array [-5..*] of integer
  end;
  flx_vstr = string[ * ];
  byte_array = packed array [0..*] of boolean;
  word_array = packed array [-7..*] of 0..256;
  int_array = packed array [1..*] of integer;
  fstr_array = packed array [1..10] of string[ 6 ];
  double = minimum(real)..maximum(real) prec 16;
  too_flex = packed array [-1..*] of array [1..20] of double;

var
  i,j: integer;
  nonflx: notflx;
  f: ^frec;
  irec_ptr: ^irec;
  fstr: ^flx_fstr;
  vstr: ^flx_vstr;
  barr: ^byte_array;
  warr: ^word_array;
  iarr: ^int_array;
  tarr: ^too_flex;
  ch: char;
  small_int: 0..3;
  b: boolean;
  vs: string[13];

begin
  new(f, false, j); 
  new ( fstr, j );
  new ( vstr, i + j );
  new ( barr, i - j );
  new ( warr, i + 1 );
  new ( iarr, 7 );
  new ( irec_ptr, i );
  i := size( f^, false, upperbound(f^.f4) );
  i := upperbound ( nonflx );
  i := upperbound ( aset );
  i := upperbound ( 'abcdefg' );

  i := dimension ( vstr^ );
  i := dimension ( barr^ );
  i := dimension ( fstr^ );
  i := dimension ( warr^ );
  i := dimension ( iarr^ );
  i := dimension ( f^.f2 );
  i := dimension ( f^.f4 );
  i := dimension ( fstr_array, 1 );
(*
  i := dimension ( fstr_array, 2 );
*)
  i := dimension ( tarr^, 1 );
  i := dimension ( tarr^, 2 );

  i := size ( f^, true  );
  i := size ( fstr^, upperbound ( fstr^ ) );
  i := size ( vstr^, upperbound (  vstr^ ) );
  i := size ( barr^, upperbound ( barr^ ) );
  i := size ( warr^, upperbound ( warr^ ) );
  i := size ( iarr^, upperbound ( iarr^ ) );
  i := size ( irec_ptr^, upperbound ( irec_ptr^.f3 ) );
  i := size ( notflx );
  i := size ( aset ) ;
  i := size ( ch );
  i := size ( small_int );
  i := size ( b );
  i := size ( vs );
end.
