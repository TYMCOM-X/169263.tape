(* TST002 - string parameter passing *)

program tst002;
var
  fs6: packed array [1..6] of char;
  vs12: string[12];
  fs3: packed array [1..3] of char;
  fs8: packed array [1..8] of char;
  fs20: packed array [1..20] of char;
  vs3: string[3];
  vs20: string[20];
  ch: char;
  flx_fs: ^packed array [1..*] of char;
  flx_vs: ^string[ * ];
  i: integer;

procedure foo_son( s: packed array [1..6] of char; str: string[12] );
  begin
  end;

procedure flex_son ( fs: packed array [1..*] of char; vs: string[ * ] );
  begin
    foo_son ( fs, vs );
    foo_son ( vs, fs );
    flex_son ( fs, vs );
    flex_son ( vs, fs );
    flex_son ( uppercase ( fs ), uppercase ( vs ) );
    flex_son ( uppercase ( vs ), uppercase ( fs ) );
  end;

procedure var_son ( var fs6: packed array [1..6] of char;
		    var vs: string[ 12 ];
		    var ffs: packed array [1..*] of char;
		    var fvs: string[ * ] );
  begin
  end;

begin
  foo_son ( vs12, fs6 );
  foo_son ( fs3, vs12 );
  foo_son ( fs8, vs12 );
  foo_son ( fs6, vs3 );
  foo_son ( fs6, vs20 );
  foo_son ( fs6, fs3 );
  foo_son ( fs6, fs20 );
  foo_son ( ch, vs12 );
  foo_son ( flx_fs^, flx_vs^ );
  foo_son ( flx_vs^, flx_fs^ );
  flex_son ( fs6, vs12 );
  flex_son ( flx_fs^, flx_vs^ );
  flex_son ( uppercase ( flx_fs^ ), uppercase ( flx_vs^ ) );
  flex_son ( vs12, fs6 );
  flex_son ( flx_vs^, flx_fs^ );
  foo_son ( '', '' );
  flex_son ( '', '' );
  foo_son ( 'a', 'b' );
  flex_son ( 'c', 'D' );
  foo_son ( fs6 || ch, substr ( vs20, i ) );
  foo_son ( uppercase ( flx_vs^ ), lowercase ( flx_fs^ ) );
  flex_son ( substr ( flx_fs^, i ), ch || vs3 );
  var_son ( fs6, vs12, fs20, vs3 );
  var_son ( flx_fs^,flx_vs^, flx_fs^, flx_vs^ );
end.
  