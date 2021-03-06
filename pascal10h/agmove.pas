program agmove options dump (final);

type
    x10 = packed array [1..5] of 0..3;
    x18 = packed array [1..9] of 0..3;
    x30 = packed array [1..15] of 0..3;
    x36 = packed array [1..18] of 0..3;
    x48 = packed array [1..24] of 0..3;

var
    a: packed record
	a10: x10;
	a18: x18;
	a30: x30;
	a36: x36;
	a48: x48
    end;

    b: record
	a10: x10;
	a18: x18;
	a30: x30;
	a36: x36;
	a48: x48
    end;

    x: packed array [1..20] of 0..127;
    y: ^ packed array [1..20] of 0..127;
    z: ^ packed array [1..*] of 0..127;

begin
  a.a10 := b.a10;  b.a10 := a.a10;
  a.a18 := b.a18;  b.a18 := a.a18;
  a.a30 := b.a30;  b.a30 := a.a30;
  a.a36 := b.a36;  b.a36 := a.a36;
  a.a48 := b.a48;  b.a48 := a.a48;

  x := x;
  x := y^;
  x := z^;
  y^ := x;
  y^ := y^;
  y^ := z^;
  z^ := x;
  z^ := y^;
  z^ := z^;
end.
    