program rnf001;

type
  atype = set of 0..7;
  btype = set of 10..20;

public const
  a: atype = [];
  a_1: atype = [1];
  a_0_7: atype = [0..7];
  b: btype = [];
  b_15: btype = [15];
  b_10_20: btype = [10..20];

const
  c = [];
  c_3 = [3];
  c_1_20 = [1..20];
  c_1__20 = [1,20];

var
  s: set of 0..31;
begin
  s := c;
  s := c_3;
  s := c_1_20;
  s := c_1__20;
end.
