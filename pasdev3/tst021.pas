(* TST021 - constant generation test program. *)

datamodule tst021;

type
  neg_bound = -3..7;
  color = (red, blue, green);
  color_arr = packed array [1..20] of color;
  str3 = packed array [1..3] of char;
  str7 = packed array [1..7] of char;
  str = string[16];
  set8 = set of 1..8;
  set40 = set of 1..40;
  set8array = array [1..6] of set8;
  pset8array = packed array [1..6] of set8;
  set40array = array [1..3] of set40;
  pset40array = packed array [1..3] of set40;
  rec8 = record
    f1: char;
    f2: boolean
  end;
  prec8 = packed record
    pf1: char;
    pf2: boolean
  end;
  int9 = 0..777b;
  bigrec = packed record
    f1: str3;
    f2: int9;
    f3: set8;
    f4: boolean
  end;
  intarr = packed array [1..5] of int9;
  sarr = array [1..5] of prec8;
  psarr = packed array [1..5] of prec8;
  barr = array [1..3] of bigrec;
  pbarr = packed array [1..3] of bigrec;
  arrarr = array [1..3] of intarr;
  parrarr = packed array [1..3] of intarr;
  bigbigrec = packed record
    f1: boolean;
    f2: bigrec;
    f3: prec8;
    f4: intarr
  end;
  therec = packed record
    f1: boolean;
    case blah: int9 of
      1,3..5: (f2: int9);
      2,6..7: (case boolean of
        true: (case int9 of
          1..3,6: (f3: prec8);
          4..5,7: (f4: set8));
        false: (f5: intarr))
  end;
  str3arr = packed array [1..3] of str3;
  srec = packed record
    f1: intarr;
    f2: boolean
  end;
  another_rec = packed record
    f1: integer;
    case f2: boolean of
      true: ();
      false: (f3: 0..777777b)
  end;
  yet_another_rec = packed record
    case char of
      'a'..'c': (f1: set8);
      'w'..'z': (f2: str3);
      others:   (f3: color)
  end;
  neg_arr = packed array [-2..3] of neg_bound;
  neg_rec = packed record
    f1: int9;
    f2: boolean;
    f3: neg_bound;
    f4: color
  end;

public var
  acolor: color := blue;
  an_int: integer := -3;
  ch: char := 'A';
  bool: boolean := true;
  x:real := 2.0;
  abc: str3 := 'ABC';
  efg: str7 := 'ABCDEFG';
  efgstr: str := '890123456';
  set81: set8 := [2,4,6,8];
  set401: set40 := [5,10,15,20,25,30,35,40];
  set8arr: set8array := ([1],[1,2],[1,2,3],[8],[7,8],[6,7,8]);
  pset8arr: pset8array := ([1],[1,2],[1,2,3],[8],[7,8],[6,7,8]);
  set40arr: set40array := ([40,39,38,37],[1,2,3],[10,20,30,40]);
  pset40arr: pset40array := ([40,39,38,37],[1,2,3],[10,20,30,40]);
  colors: color_arr := (red, blue, green, red, red, blue, blue, green, green,
      green, blue, red, green, green, blue, blue, red, red, blue, green);
  rec81: rec8 := ('Z',true);
  prec81: prec8 := ('A',true);
  recarr: sarr := (('A',true),('B',false),('C',true),('D',false),('E',false));
  precarr: psarr := (('A',true),('B',false),('C',true),('D',false),('E',false));
  brecarr: barr := (('ABC',777b,[1,2,3],true),('xyz',0,[],false),('abc',1,[2,4,6],true));
  pbrecarr: pbarr := (('ABC',777b,[1,2,3],true),('xyz',0,[],false),('abc',1,[2,4,6],true));
  a2d: arrarr := ((1,2,3,4,5),(5,4,3,2,1),(507,508,509,510,511));
  pa2d: parrarr := ((1,2,3,4,5),(5,4,3,2,1),(507,508,509,510,511));
  bbrec: bigbigrec := (true, ('lmn',3,[6,7,8],false),('z',true),(2,4,6,8,10));
  arec: therec := (true,7,true,5,[1,3,5,7]);
  other1: another_rec := (-64,true);
  other2: another_rec := (-32,false,400000b);
  shortarr: parrarr := ((1,2,3,4,5),(2,4,6));
  shortrec: bigbigrec := (false,('abc',3),('z',true));
  srec1: srec := ((1),true);
  str3arr1: str3arr := ('ABC', 'DEF', 'GHI');
  arec2: therec := (false, 2, false, (8, 16, 24, 32, 40));
  yet1: yet_another_rec := ('a', [8,3..5]);
  yet2: yet_another_rec := ('y','oof');
  yet3: yet_another_rec := (chr(2),green);
  neg_arr1: neg_arr := (-3,7,-2,-1,6,0);
  neg_rec1: neg_rec := (511, false, -2, blue);
end.
  