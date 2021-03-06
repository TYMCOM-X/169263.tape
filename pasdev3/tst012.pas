(* TST012 - value, subscript, pointer and file check ops. *)

program tst012 options check;

type
  color = (red, green, blue);
  a_rec = packed record
    f1: integer
  end;

var
  p1,p2: ^integer;
  c1,c2: color;
  i,j,k: -128..127;
  l: 0..255;
  b1,b2: boolean;
  fs: packed array [1..10] of char;
  vs: string[12];
  ch: char;
  flx_fs: ^packed array [1..*] of char;
  flx_vs: ^string[*];
  flx_arr: ^packed array [1..*] of integer;
  byte_arr: packed array [-2..10] of 0..255;
  arr: packed array [1..10] of integer;
  arec_ptr: ^a_rec;
  f: text;
  x: real;
  subx: 0.0..maximum(real);

begin
  c1 := succ ( c2 );
  b1 := pred ( b2 );
  i := p1^;
  ch := fs[ i ];
  ch := vs[ j ];
  ch := flx_fs^[ i ];
  ch := flx_vs^[ i ];
  ch := flx_fs^[ 3 ];
  ch := flx_vs^[ 5 ];
  ch := flx_fs^[ -2 ];
  ch := flx_vs^[ 100000 ];
  i := flx_arr^[ i ];
  i := arr[ i ];
  i := flx_arr^[ 7 ];
  i := flx_arr^[ -3 ];
  i := arec_ptr^.f1;
  i := 1;
  ch := f^;
  subx := x;
  i := 128;
  ch := vs[ 15 ];
  i := byte_arr[ j ];
  l := i;
  p1^ := i + j;
end.
 