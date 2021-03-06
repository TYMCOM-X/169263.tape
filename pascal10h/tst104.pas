program tst104;

type
  color = (red, blue, white, black, yellow, orange, green, purple, mustard);

type
  smallset = set of 0..12;
  smallset2 = set of 10..24;
  mediumset = set of 1..71;
  bigset = set of 0..255;
  colorset = set of color;

var
  ss: smallset;
  ss2: smallset2;
  ms: mediumset;
  bs: bigset;
  cs: colorset;

  i,j: lowerbound (bs)..upperbound (bs);
  flag: boolean;

begin
  ss := ss;
  ss := ss2;
  ss := ms;
  ss := bs;

  ss2 := ss;
  ss2 := ss2;
  ss2 := ms;
  ss2 := bs;

  ms := ss;
  ms := ss2;
  ms := ms;
  ms := bs;

  bs := ss;
  bs := ss2;
  bs := ms;
  bs := bs;

  ss := ss + ss2;
  ss := ss * ss2;
  ss := ss - ss2;

  ss := [];
  ss := [1, 3..5];
  ss := [i, j];
  ss := [2..i];
  ss := [j..16];
  ms := (ss2 * (ms - (bs + ss2)));

  flag := ss2 <> ms;
  flag := [red, green, yellow] <= cs;
  flag := ms = (ss * [1..12]);
  flag := bs = [i];
end.
  