program tst107;

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
  k: lowerbound (ss)..upperbound (ss);
  bint: integer;
  flag: boolean;

begin
  ss := ss;
  ss := ss2;
  ss := ms;

  ss2 := ss;
  ss2 := ss2;
  ss2 := ms;

  ms := ss;
  ms := ss2;
  ms := ms;

  flag := (i in [j..k]);
  flag := (j in [3..12]);
  flag := (chr (j) in ['A'..'Z']);
  flag := (blue in cs);
  flag := (k in ss);
  flag := (bint in ms);

  if i in [j..k] then
  if j in [3..12] then
  if blue in cs then
  if k in ss then
  if bint in ms then ;

  flag := ss <= ss;
  flag := ms <= ms;
  flag := ss <= ss2;
  flag := ss <= ms;
  flag := ss >= ss2;
  flag := ss >= ms;
  flag := ms >= ss2;

  flag := ss = ss;
  flag := ss = [];
  flag := ss2 <> ss;
  flag := ss2 <> [];
  flag := ms = ss;
  flag := ms <> [];

  if ss <= ss2 then
  if ss = [] then 
  if ms <> ss then
  if [] <> ss then
  if ss2 = ss then ;

  ss := ss + ss;
  ss := ss * ss2;
  ss := ss - ms;

  ms := ms + ss;
  ms := ms * ss2;
  ms := ms - ms;

  ss := [];
  ss := [i];
  ss := [3..j];
  ss := [i..72];
  ss := [k];
  ss := [1..k];
  ss := [k..40];
  ss := [i..j];

  ss2 := [];
  ss2 := [i];
  ss2 := [3..j];
  ss2 := [i..72];
  ss2 := [k];
  ss2 := [1..k];
  ss2 := [k..40];
  ss2 := [i..j];

  ms := [];
  ms := [i];
  ms := [3..j];
  ms := [i..72];
  ms := [k];
  ms := [1..k];
  ms := [k..40];
  ms := [i..j];
end.
    