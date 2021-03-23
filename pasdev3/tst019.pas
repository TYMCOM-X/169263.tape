$SOURCE
program djwv02 options ass,nocheck;
  
const null = [];
  c1 = [0,1,6];
  c2 = [0,1,6,8,9,14];
  c3 = [0,1,6,8,9,14,16,17,22];
  c4 = [0,1,6,8,9,14,16,17,22,24,25,30];
  c5 = [0,1,39];
  c6 = [0,1,47];
  c7 = [0,1,55];
  c8 = [0,1,63];
  c16 = [0,1,127]; copy16 = [0,1,127];
  copt: set of io_options := [ascii, retry];
  
var
  bool: boolean;
  i, j, k, l: integer; ipos, jpos, kpos: 0..maximum (integer);
  i_0_to_7: 0..7; i_0_to_20: 0..20; i_min10_to_7: -10..7;
  i_48_to_100: 48..100;  i_0_to_55: 0..55;  i_18_to_10000: 18..10000;
  set1: set of 0..7;
  set2: set of 0..15;
  set3: set of 0..23;
  set4: set of 0..31;
  set5: set of 0..39;
  set6: set of 0..47;
  set7: set of 0..55;
  set8: set of 0..63;
  set16: set of 0..127;
  
  set1_l2: set of 16..23;
  set2_l1: set of 8..23;
  set3_l5: set of 40..63;
  set5_l6: set of 48..87;
  set7_l2: set of 16..71;
  
  set_3_to_4: set of 3..4;
  set_20_to_39: set of 20..39;
  set_32_to_56: set of 32..56;
  set_31_to_56: set of 31..56;
  
  a_array_1: array [0..20] of set of 1..6;
  b_array_1: array [0..20] of set of 0..7;
  a_array_16: array [0..20] of set of 0..126;
  b_array_16: array [0..20] of set of 0..127;
  iopt, jopt: io_options;
  optset: set of io_options;
  str: string;
  ix: integer;
  achar,bchar,cchar: char;
  dchar,echar,fchar: ' '..'z';
  charset1: set of chr(8)..chr(127);
  charset2: set of chr(0)..chr(127);
  charset3: set of chr(31)..chr(56);
  
function f_0_to_7: set of 0..7;
  begin
    f_0_to_7 := set1
  end;
  
function f_0_to_126: set of 0..126;
  begin
    f_0_to_126 := set16;
    f_0_to_126 := set3_l5 - (f_0_to_126 - (f_0_to_126 - f_0_to_126));
    f_0_to_126 := [126];
    f_0_to_126 := set_31_to_56 + [126];
    f_0_to_126 := set_31_to_56 + f_0_to_126 + [126];
  end;
  
function f_31_to_56 (seta: set of 16..71; var setb: set of 31..56): set of 31..56;
  var
    arr_40_to_63: array [1..10] of set of 40..63;
    loc_i: integer;
  begin
    f_31_to_56 := set_20_to_39;
    f_31_to_56 := set8;
    f_31_to_56 := arr_40_to_63 [i] - set3_l5;
    f_31_to_56 := [31];
    f_31_to_56 := f_31_to_56 + [31];
    f_31_to_56 := [56];
    f_31_to_56 := f_31_to_56 + [56];
    f_31_to_56 := [loc_i];
    f_31_to_56 := f_31_to_56 + [loc_i];
    f_31_to_56 := set1 + setb;
  end;
  
function f_1_to_62: set of 1..62;
  begin
    f_1_to_62 := f_1_to_62 - ((f_1_to_62 * set5_l6) * set5_l6);
    f_1_to_62 := (set16 * set8) - set_3_to_4;
  end;
  
procedure proc1 (seta: set of 8..23; setb: set of 31..56; var setc: set of 3..4; setd: set of 3..4);
  begin
    set3 := seta;
    set_3_to_4 := setc;
    setc := set_3_to_4;
    set_32_to_56 := setb
  end;
  
begin
  set1 := set1;
  set1 := c1;
  set1 := null;
  
  set2 := set2;
  set2 := c2;
  set2 := [];
  
  set3 := c3;
  set3 := [];
  set4 := c4;
  set4 := [];
  set5 := c5;
  set5 := [];
  set7 := c7;
  set7 := [];
  set8 := c8;
  set8 := [];
  set16 := c16;
  set16 := [];
  
  set1 := set1_l2;
  set3 := set1_l2;
  set1_l2 := set3;
  set4 := set2_l1;
  set3_l5 := set16;
  
  set16 := set_3_to_4;
  set_3_to_4 := set1;
  set_20_to_39 := set5;
  set5 := set_20_to_39;
  set6 := set_20_to_39;
  set_32_to_56 := set5;
  set_32_to_56 := set8;
  set8 := set_32_to_56;
  set7 := set_32_to_56;
  set16 := set_32_to_56;
  set_31_to_56 := set16;
  
  set1 := set1 - set1;
  set1 := set1 - set4;
  set1 := set4 - set2_l1;
  
  set2 := set3 - set2;
  set2 := set2 - set1;
  set2 := set3 - set5;
  
  set3 := set4 - set1;
  set3 := set3 - set2_l1;
  set3 := set2_l1 - set3;
  
  set_3_to_4 := set1 - set_3_to_4;
  set_3_to_4 := set_3_to_4 - set2;
  
  set4 := set4 - set16;
  set16 := set16 - set8;
  set16 := set3_l5 - set8;
  
  set_20_to_39 := set_32_to_56 - set_31_to_56;
  set_32_to_56 := set_31_to_56 - set16;
  set7 := set8 - (set16 - set_31_to_56);
  set7 := (set16 - set_31_to_56) - set8;
  set7 := set5 - (set16 - set_31_to_56);
  set7 := (set5 - set_31_to_56) - set8;
  set7 := (set5 - set_31_to_56) - set4;
  set_31_to_56 := (set_20_to_39 - set_3_to_4) - (set_32_to_56 - set_31_to_56);
  
  set1 := f_0_to_7();
  set16 := f_0_to_126();
  set8 := f_0_to_126() - f_31_to_56 (set7_l2, set_31_to_56);
  set16 := f_0_to_126() - (f_0_to_126() - f_31_to_56 (set7_l2, set_31_to_56));
  set8 := set8 - (f_1_to_62() - f_0_to_126());
  set7 := (set5 - f_31_to_56 (set7_l2, set_31_to_56)) - f_0_to_126();
  set7 := (set5 - f_31_to_56 (set7_l2, set_31_to_56)) - set4;
  
  a_array_16 [i] := b_array_16 [i] - a_array_16 [i];
  a_array_1 [i] := b_array_1 [i] - a_array_1 [i];
  
  set1 := set1 * set1;
  set2 := set1 * set3;
  set2 := set2 * set2;
  set3 := set5 * set2;
  set3 := set3 * set3;
  set5 := set3 * set4;
  set4 := set4 * set4;
  set5 := set5 * set5;
  set16 := set16 * copy16;
  set16 := set7 * set5;
  set16 := set4 * set5;
  
  set4 := set4 * (set4 * set4);
  set5 := set5 * (set5 * set5);
  set16 := set4 * (set4 * set4);
  set16 := set5 * (set5 * set6);
  set4 := (set4 * set5) * (set5 * set4);
  set5 := (set5 * set6) * (set6 * set5);
  set16 := (set4 * set5) * (set5 * set4);
  set16 := (set5 * set6) * (set6 * set5);
  set1 := (set1 * set1) * set2;
  set2 := (set2 * set3) * set2;
  set5 := (set1 * set1) * set2;
  set5 := (set2 * set3) * set2;
  
  set8 := set2_l1 * set1_l2;
  set8 := set2_l1 - (set2_l1 * set1_l2);
  set8 := set8 - (set4 * set4);
  set8 := set7 - (set3_l5 * set3_l5);
  set16 := set4 - (set2_l1 * set1_l2);
  set16 := set16 - (set5_l6 * set7_l2);
  set8 := set8 - (set5_l6 * set7_l2);
  set8 := set7_l2 - (set5_l6 * set7_l2);
  set8 := set7_l2 - ((set7_l2 * set7_l2) * set5_l6);
  set8 := set8 - ((set8 * set5_l6) * set5_l6);
  set8 := set8 - ((set8 * set7_l2) * set5_l6);
  set16 := set16 - ((set8 * set5_l6) * set5_l6);
  set16 := set16 - ((set8 * set7_l2) * set5_l6);
  
  set1 := [1,2] + [7];
  set1 := [0,1,6];
  set1 := [-1,2,4,6,8];
  set1 := [3..4];
  set_3_to_4 := [3..4];
  set1 := [-2..6];
  set1 := [i_min10_to_7..i_0_to_7];
  set_3_to_4 := [-2..80];
  set_3_to_4 := [i_0_to_7..i_0_to_7];
  set3 := [5..22];
  set3 := [5..60];
  set4 := [-2..80];
  set4 := [25..30];
  set5 := [-2..80];
  set_31_to_56 := [31..56];
  set_31_to_56 := [30..57];
  set16 := [-2..150];
  set16 := [64..127];
  set2 := [0,5,10,15];
  set2 := [1..14];
  set5_l6 := [0..63,72..100];
  
  set1 := set1 + set1;
  set1 := set2 + set3;
  set2 := set2 + set3;
  set2 := set3 + set4;
  set3 := set4 + set5;
  set4 := set6 + set4;
  set5 := set6 + set7;
  set16 := set16 + f_0_to_126();
  set1 := set2 + set_3_to_4;
  set1 := set_3_to_4 + set2;
  set16 := set16 - (set8 + set7);
  set16 := set16 - (set3_l5 + set5_l6);
  set16 := set16 - (set5_l6 + set3_l5);
  set4 := (set5 + set6) + (set8 + set4);
  set4 := (set5 + set6) + (set8 + set4) + (set4 + set8);
  set4 := (set5 + set6) + set8;
  set4 := (set5 + set6) + (set8 + set4) + set8;
  
  set_31_to_56 := (set4 + set7_l2) + (set3_l5 * set5_l6) - (set_3_to_4 + set_20_to_39);
  set_31_to_56 := (set4 + set7_l2) + (set3_l5 * set5_l6);
  set_31_to_56 := (set4 + [3,6,9,12..15] + set7_l2) + (set3_l5 * set5_l6) + [0..30];
  set_31_to_56 := (set4 + [3,6,9,12..15] + set7_l2) + (set3_l5 * set5_l6) + [0..31];
  set_31_to_56 := (set4 + [3,6,56..100,12..15] + set7_l2) + (set3_l5 * set5_l6) + [0..31];
  
  set1 := set1 + [0,3,5];
  set1 := set2 + [0,3,5];
  set2 := set2 + [0,3,5];
  set2 := set3 + [0,3,5];
  set2 := set3 + [8,10,12];
  set2 := set3 + [3,5,7,9];
  set16 := set16 + [52,54,56];
  set16 := f_0_to_126() + [52,54,56];
  set16 := f_0_to_126() + [8..127];
  set1 := [0];
  set1 := [7];
  set1 := [8];
  set1 := [-1];
  set1 := [i];
  set1 := [i_0_to_7];
  set1 := [i_0_to_20];
  set1 := [i_min10_to_7];
  
  set_3_to_4 := [i_0_to_7];
  set2 := [8];
  set2 := [9];
  set3 := [23];
  set4 := [31];
  set16 := [127];
  set16 := [127] + [127];
  set5_l6 := [48];
  set5_l6 := [i];
  set5_l6 := [i_48_to_100];
  set5_l6 := [i_0_to_55];
  set_31_to_56 := [32];
  set_31_to_56 := [i];
  set_31_to_56 := [i_48_to_100];
  set_31_to_56 := [i_0_to_55];
  set_31_to_56 := set_31_to_56 - [i];
  set_31_to_56 := set_31_to_56 - [i_48_to_100];
  set_31_to_56 := set_31_to_56 - [i_0_to_55];
  set_31_to_56 := set7_l2 - [i];
  set_31_to_56 := set7_l2 - [i_48_to_100];
  set_31_to_56 := set7_l2 - [i_0_to_55];
  set_31_to_56 := (set7_l2 + set7_l2) - [i];
  set7_l2 := set7_l2 - ((set_31_to_56 + set_31_to_56) - [i]);
  set7_l2 := set7_l2 - (set7_l2 - [i]);
  set7_l2 := set7_l2 - (set_31_to_56 - [i]);
  
  set1 := [2,4,5] + set2 + set3;
  set1 := [2,4,5] + set2 + set1;
  set_31_to_56 := set_31_to_56 + [i];
  set_31_to_56 := set_31_to_56 + [i_48_to_100];
  set_31_to_56 := set_31_to_56 + [i_0_to_55];
  set_31_to_56 := set7_l2 + [i];
  set_31_to_56 := set7_l2 + [i_48_to_100];
  set_31_to_56 := set7_l2 + [i_0_to_55];
  set_31_to_56 := (set7_l2 + set7_l2) + [i];
  set7_l2 := set7_l2 - ((set_31_to_56 + set_31_to_56) + [i]);
  set7_l2 := set7_l2 - (set7_l2 + [i]);
  set7_l2 := set7_l2 - (set_31_to_56 + [i]);
  
  set_31_to_56 := set_31_to_56 - [31];
  set_31_to_56 := set_31_to_56 - [56];
  set7_l2 := set7_l2 - [31];
  set7_l2 := set_31_to_56 - [31];
  set7_l2 := set7_l2 + [31];
  set7_l2 := set_31_to_56 + [31];
  set7_l2 := set7_l2 + [i];
  set7_l2 := set_31_to_56 + [i];
  set7_l2 := [i] + set7_l2;
  set7_l2 := [i] + set_31_to_56;
  set7_l2 := [100] + set_31_to_56 + [71];
  
  proc1 (set2_l1, set_31_to_56, set_3_to_4, set_3_to_4);
  proc1 (set3, set_32_to_56, set_3_to_4, set1);
  proc1 (set6, set_20_to_39, set_3_to_4, set2);
  proc1 (set1_l2, set5_l6, set_3_to_4, [i]);
  proc1 ([0..31, 31], set7_l2, set_3_to_4, [i, i_0_to_7]);
  
  rewrite (output);
  rewrite (output, [seekok]);
  rewrite (output, [ascii..retry]);
  rewrite (output, [preserve, seekok, retry]);
  rewrite (output, [iopt]);
  rewrite (output, [iopt..jopt]);
  rewrite (output, optset);
  rewrite (output, optset * [iopt..jopt]);
  
  ix := search (str, charset1);
  ix := verify (str, charset2);
  ix := search (str, charset3);
  ix := verify (str, [achar]);
  ix := search (str, [achar,bchar,cchar]);
  ix := search (str, [' '..'z']);
  ix := search (str, [dchar, echar, fchar]);
  ix := search (str, ['d','e','f']);
  ix := search (str, charset1 * charset3);
  ix := search (str, []);
  ix := verify (str, charset3 * ['a'..'z']);
  
  if not (i_0_to_7 in set1_l2) then
    set1 := c1;
  if not (i_0_to_7 in [0]) then
    set1 := c1;
  if i_0_to_7 in [i_48_to_100] then
    set1 := c1;
  if i_0_to_7 in [i_0_to_20] then
    set1 := c1;
  if i_0_to_7 in [10..20] then
    set1 := c1;
  if i_0_to_7 in [7..20] then 
    set1 := c1;
  if i_0_to_7 in [4..20] then
    set1 := c1;
  if i_48_to_100 in [i_0_to_55..100] then
    set1 := c1;
  if i_48_to_100 in [i_0_to_7..i] then
    set1 := c1;
  if i_0_to_7 in set1 then
    set1 := c1;
  if i_0_to_7 in set_3_to_4 then
    set1 := c1;
  if i_0_to_20 in set1_l2 then
    set1 := c1;
  if i_48_to_100 in set5_l6 then
    set1 := c1;
  if i_min10_to_7 in set1 then
    set1 := c1;
  if i_0_to_7 in [i,j] then
    set1 := c1;
  if k in [i,j] then
    set1 := c1;
  if i_18_to_10000 in [i,j] then
    set1 := c1;
  if i in [i,j] * [k] then
    set1 := c1;
  
  bool := not (i_0_to_7 in set1_l2);
  bool := not (i_0_to_7 in [0]);
  bool := i_0_to_7 in [i_48_to_100];
  bool := i_0_to_7 in [i_0_to_20];
  bool := i_0_to_7 in [10..20];
  bool := i_0_to_7 in [7..20]; 
  bool := i_0_to_7 in [4..20];
  bool := not (i_0_to_7 in [4..20]);
  bool := i_48_to_100 in [i_0_to_55..100];
  bool := i_48_to_100 in [i_0_to_7..i];
  bool := i_0_to_7 in set1;
  bool := i_0_to_7 in set_3_to_4;
  bool := i_0_to_20 in set1_l2;
  bool := i_48_to_100 in set5_l6;
  bool := not (i_48_to_100 in set5_l6);
  bool := i_min10_to_7 in set1;
  bool := i_0_to_7 in [i,j];
  bool := k in [i,j];
  bool := not (k in [i,j]);
  bool := i_18_to_10000 in [i,j];
  bool := i in [i,j] * [k];
  bool := k in [i..j];
  bool := not (k in [ipos..jpos]);
   
  if [] = [] then
    set1 := c1;
  if [] <> [] then
    set1 := c1;
  if not ([] = []) then
    set1 := c1;
  if not ([] <> []) then
    set1 := c1;
  if [] = [] then
    set1 := c1
  else
    set2 := c2;
  bool := [] = [];
  bool := [] <> [];
  bool := not ([] = []);
  bool := not ([] <> []);
  if [3] = [] then
    set1 := c1
  else
    set2 := c2;
  bool := [] = [i];
  bool := not ([] = [j..j]);
  if [i..j] = [] then
    set1 := c1;
  if not ([i_48_to_100..i_0_to_20] = []) then
    set1 := c1;
  bool := [] <> [10..20];
  bool := [] = set1;
  bool := [] <> set2;
  bool := not (set3 = []);
  bool := not (set4 <> []);
  bool := set5 = [];
  if not (set16 <> []) then
    set1 := c1
  else
    set2 := c2;
  bool := [] = [i,j,k];
  bool := [] = [i_0_to_55, i_48_to_100] * [ipos];
  bool := [i..j] = [k..l];
  bool := [i..i] = [j..j];
  bool := [i..i] <> [j..k];
  bool := not ([i..j] = [k..k]);
  bool := not ([i..j] = [k..l]);
  bool := set1 = [i];
  bool := [i] = set16;
  bool := set2 <> [15];
  bool := not (set3 <> [i..15]);
  bool := not ([i..23] <> set3);
  bool := not ([i..23] <> [i,j,k]);
  bool := not ([i..-5] <> set3);
  bool := set_31_to_56 = [i_48_to_100];
  bool := set_31_to_56 = [i];
  bool := [i..j] = set_31_to_56;
  bool := not ([i_0_to_55..i_48_to_100] <> set_32_to_56);
  bool := [i_0_to_55..i_48_to_100] <> set_32_to_56;
  if [i_48_to_100..i_0_to_55] <> set_32_to_56 then
    set1 := c1;
  bool := set4 <> [0..31];
  bool := set7_l2 = [16..i];
  bool := [16..ipos] <> set7_l2;
  bool := [i_18_to_10000..ipos] <> set7_l2;
  bool := [i_min10_to_7..32767] = set7_l2;
  bool := set1 = set1;
  bool := set2 <> set2;
  bool := set3 = set3;
  bool := set4 = set4;
  bool := set5 <> set5;
  bool := set1 = set2;
  bool := set7_l2 <> set5_l6;
  bool := not ([i,j] <> [k,l]);
  bool := not ([i,j] <> [i_18_to_10000, i_48_to_100]);
  if not (set16 = [1,5,10]) then
    set1 := c1
  else
    set2 := c2;
  
  bool := [] <= [];
  bool := null >= [];
  bool := [i] >= null;
  bool := [i..j] >= null;
  bool := [] <= set7_l2;
  bool := set7_l2 >= (set7_l2 - set5_l6) * set2;
  if set7_l2 >= (set7_l2 - set5_l6) * set2 then
    set1 := c1
  else
    set2 := c2;
  if not (set7_l2 >= (set7_l2 - set5_l6) * set2) then
    set1 := c1
  else
    set2 := c2;
  bool := [i] <= [];
  bool := not ([] >= [i]);
  bool := [i] <= [i];
  bool := [j] >= [i];
  bool := not ([ipos] <= [i]);
  bool := [i] <= [j..k];
  bool := [j..27] >= [i_0_to_7];
  bool := not ([10..27] >= [6]);
  bool := [i] <= set1;
  bool := [4] <= set3;
  bool := not ([100] <= set4);
  bool := set16 >= [i];
  bool := [i,j] >= [i];
  bool := [0,5,10] >= [5];
  bool := [1..5] <= [];
  bool := [1..i] <= [];
  bool := not ([] >= [i..j]);
  bool := [i..j] <= [k];
  bool := not ([ipos..jpos] <= [kpos]);
  if [kpos] >= [5..10] then
    set1 := c1
  else
    set2 := c2;
  bool := [i..j] <= [k..l];
  bool := not ([5..10] >= [6..9]);
  bool := [i..j] <= set2;
  bool := [i..15] <= set2;
  bool := set5 >= [-50..100];
  bool := not (set_31_to_56 >= [i..j]);
  bool := [ipos..jpos] <= [i,j];
  bool := set1 <= [];
  bool := not ([i,j] <= []);
  bool := not ([] >= [5,10,15]);
  bool := set1 <= [1];
  bool := not (set3 <= [i_0_to_7]);
  bool := [i,j] <= [k];
  bool := set1 <= [1..5];
  bool := not (set2 <= [i..15]);
  bool := [i..i_48_to_100] >= set5_l6;
  bool := [i,j] <= [k..l];
  bool := set1 <= c1;
  bool := set1 <= set2;
  bool := set2 <= set1;
  bool := set16 <= set1;
  bool := set16 <= set5_l6;
  bool := set16 >= set5_l6;
  bool := [i,j] <= set5_l6;
  bool := [i,j] >= set5_l6;
  bool := (set1 + set2 + set4) * set16 <= ((set16 - set_31_to_56) * set_20_to_39) + set4;
end.
  