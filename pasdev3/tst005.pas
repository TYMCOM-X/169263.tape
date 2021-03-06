(* TST005 - MIN, MAX, scalar conversions and standard functions.  *)

program tst005 options special( ptr, coercions );

var
  i,j,k: integer;
  x,y,z: real;
  dx,dy,dz: minimum(real)..maximum(real) prec 8;
  ch: char;
  b: boolean;
  p: ^integer;

begin
  ch := chr ( i );
  ch := chr ( 011b );
  i := ord ( b );
  i := ord ( p );
  i := ord ( ch );
  p := ptr ( 123456b );
  p := ptr ( ord ( ch ) );
  i := min(i,j,7);
  x := max(x, 13.0, y);
  dx := min(dx,-13.0, dy,dz);
  x := max(i,j,z,k);
  dx := min(x,i,y,dz);
  i := j ** k;
  x := y ** i;
  dx := dy ** i;
  x := i ** y;
  dx := i ** dy;
  x := y ** z;
  x := dx ** dy;
  i := sqr ( i );
  dx := sqr ( dy );
  dx := sqrt ( y );
  y := sqrt ( dx );
  x := sin ( y + z );
  dx := cos ( i + z );
  dz := arctan ( dy );
  x := arctan ( x, y );
  y := arctan ( x + dx, dy );
  dz := arctan ( dy, j );
  dz := arctan ( x, dy * i );
  x := arcsin ( 1 );
end.
   