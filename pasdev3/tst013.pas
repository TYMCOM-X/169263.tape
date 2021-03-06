(* TST013 - miscellaneous - address function, file check ops, extent function *)

program tst013 options special( coercions, ptr ), check;
var
  flx_arr: ^array [1..*] of boolean;
  p: ptr;
  f: text;
  f2: file of array [1..512] of 0..255;
  ch: char;
  i,j: 0..255;
  flx_vstr: ^string[ * ];
  flx_rec: ^packed record
	     f1: boolean;
	     f2: string[ * ];
	   end;
begin
  p := address ( flx_arr^ );
  p := address ( f2 );
  p := address ( flx_vstr^ );
  p := address ( flx_rec^.f2 );
  ch := f^;
  i := f2^[ 3 ];
  i := extent ( flx_arr );
  i := extent ( flx_vstr );
  i := extent ( flx_rec );
  i := extent ( f2 );
end.
    