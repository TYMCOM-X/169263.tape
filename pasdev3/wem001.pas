program bug;

external var int : dir_int;

var i : integer;

const j : integer := 1;

type dir_int = ^integer;

external procedure foo ( abc : dir_int );

begin
  foo ( int )
end.
