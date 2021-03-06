(* EXE004 - program to test code generation for sets. *)

program EXE004;

type
  color = (orange, purple, red, green, blue, yellow);
  alphanums = '0'..'Z';
  uppers = 'A'..'Z';
  numbers = '0'..'9';
  color_set = set of color;
  rgb_set = set of red..blue;
  zlen1set = set of 0..35;
  len1set = set of 1..36;
  s10to30 = set of 10..30;
  s50to100 = set of 50..100;
  s35to127 = set of 35..127;
  s50to75 = set of 50..75;
  s75to110 = set of 75..110;
  double_set = set of 0..63;
  single_set = set of 0..31;
  len2set = set of chr(0)..chr(71);
  len1_char_set = set of chr(0)..chr(31);
  len4_char_set = set of chr(0)..'z';
  len4_int_set  = set of 0..127;
  alphanums_set = set of alphanums;
  uppers_set = set of uppers;
  numbers_set = set of numbers;
  char_set = set of char;
  setrec1 = packed record
    f1: uppers_set;
    f2: numbers_set;
    f3: len1set
  end;
  setrec2 = packed record
    s1: rgb_set;
    s2: len2set;
    s3: alphanums_set
  end;
  setarr1 = packed array [1..3] of color_set;
  setarr2 = packed array [1..3] of alphanums_set;
  boolset = set of false..true;
  double_array = packed array [1..5] of double_set;
  single_array = packed array [1..5] of single_set;

const
  empty_set = [];
  empty_char_set: set of char = [];
  len1_ch_cons: len1_char_set = [chr(7)..chr(17), chr(30)];
  len1_ch_cons2: len1_char_set = [chr(0)..chr(31)];
  len4_ch_cons: len4_char_set = [chr(7)];
  color_cons: color_set := [red..yellow];
  rgb_cons: rgb_set := [red, green, blue];
  zlen1_cons: zlen1set := [0..8, 18..26];
  len2_cons: len2set := ['0'..'G'];
  uppers_cons: uppers_set := ['A','B'..'M'];
  numbers_cons: numbers_set := ['0'..'5'];
  rec1_cons: setrec1 := (['W'..'Z'],['0','2','4','6'],[1..36]);
  rec2_cons: setrec2 := ([red..green],[chr(0)..chr(35)],['3'..'C']);
  arr1_cons: setarr1 := ([red],[],[green,blue]);
  arr2_cons: setarr2 := (['0'..'9'],['A'..'Z'],[':'..'@']);
  boolset_cons: boolset := [true];
  nul: char := chr(0);
  slash: char := '/';
  double_cons: double_array := ([0..63], [], [32..63], [1..3,34..36],
				[30..40]);
  single_cons: single_array := ([0..31], [2,4,6,8], [0..8,18..26], [7], []);


public var
  rgb_var: rgb_set;
  len1_var: len1set;
  len2_var: len2set;
  ln2_varb: len2set;
  len4_int_var: len4_int_set;
  alphanum_var: alphanums_set;
  uppers_var: uppers_set;
  numbers_var: numbers_set;
  rec1_var: setrec1;
  rec2_var: setrec2;
  arr1_var: setarr1;
  non_alpha_nums: char_set;
  double_arr: double_array;
  sa10to30, sb10to30: s10to30;
  s50to100a: s50to100;
  s35to127a: s35to127;
  s50to75a: s50to75;
  s75to110a: s75to110;
  char1: char;
  char2: char;
  char3: char;
  color1: color;
  color2: color;
  i, j, k: integer;
  b: boolean;

public procedure error (err_num: integer);
begin
  writeln (ttyoutput,'Error ',err_num);
  break (ttyoutput);
end;

begin
  rewrite (ttyoutput);
  writeln( ttyoutput, 'Begin EXE004' );
  char1 := 'C';
  char2 := 'Q';
  char3 := 'A';
  color1 := red;
  color2 := blue;
  i := 3;
  j := 0;
  k := 127;
  len4_int_var := [0..29, 41..63];
  ln2_varb := [ chr(7)..chr(17), chr(30) ];
  sa10to30 := [10..20];
  s35to127a := [35..50, 100..127];
  rgb_var := [blue, red];
  uppers_var := ['Q', 'S', 'V'];
  alphanum_var := len2_cons;
  len2_var := [char3..char1, ' ', chr(0)..chr(10),'0', '7'];
  non_alpha_nums := [nul..slash, ':'..'@', '['..chr(127)];
  rec2_var := ( rgb_var, len2_cons, ['A', ':'..'?', '7'] );
  arr1_var := ( [color2..yellow], [orange..color1], [] );
  rec1_var := ( ['W'..'Z'], ['0', chr(i + 48)], [1..3, 34..36] );
  numbers_var := rec1_var.f2;
  len1_var := zlen1_cons - [0];
  double_arr := double_cons;

  if color_cons * rgb_cons <> [red..blue] then error (1);
  if not (non_alpha_nums * arr2_cons[3] = [':',';'..'?','@']) then error (2);
  if not ([] >= (boolset_cons * [false]) ) then error (3);
  if not (rec1_cons.f2 <= arr2_cons[1]) then error (4);
  if not (rec2_cons.s1 + arr1_cons[3] >= rgb_cons) then error (5);
  if not (uppers_cons * numbers_cons <= []) then error (6);
  if rec1_cons.f3 * len1_var <> len1_var then error (7);
  if not (char2 in uppers_var) then error (8);
  if color2 in arr1_cons[1] then error (9);
  if not ( nul in non_alpha_nums ) then error (10);
  if i in rec1_cons.f3 - len1_var then error (11);
  if not ( '3' in rec2_var.s2 - len2_var ) then error (12);
  if not ( arr1_var[1] * color_cons - [red,green] = arr1_var[1])
    then error (13);
  if not ( ( rec1_var.f2 + ['1', '2'] = ['0'..'3'] ) in [true] ) then error (14);
  if numbers_var <> ['0', '3'] then error (15);
  if color1 in [purple,green] then error ( 16 );
  if double_arr[ i + 2 ] <> [ 30..40 ] then error ( 17 );
  if not ( 34 in (double_cons[ 4 ] + double_arr[ 5 ]) ) then error ( 18 );
  if not (7 in single_cons[ i + 1 ]) then error ( 19 );
  if single_cons[ 3 ] <> zlen1_cons then error ( 20 );
  if double_arr[ 1 ] = empty_set then error ( 21 );
  if non_alpha_nums = empty_char_set then error ( 22 );
  if empty_set <> empty_char_set then error ( 23 );
  if empty_set <> [] then error ( 24 );
  if not ( [] = single_cons[ 5 ] ) then error ( 25 );
  if not ( double_arr[ 2 ] = [] ) then error ( 26 );
  if non_alpha_nums * uppers_var <> empty_set then error ( 27 );
  if single_cons[ 1 ] * single_cons[ 2 ] <> [2,4,6,8] then error ( 28 );
  if zlen1_cons - [30..35] <> single_cons[ 3 ] then error ( 29 );
  if rec1_var.f3 + [4..8] <> len1_var + [34..36] - [18..26] then error ( 30 );
  if single_cons[ 3 ] <> [0..8, 18..26] then error ( 31 );
  if rec1_var.f3 <> double_arr[ 4 ] then error ( 32 );
  if rec2_cons.s2 - [chr(32)..chr(35)] <> len1_ch_cons2 then error ( 33 );
  if ln2_varb <> len1_ch_cons then error ( 34 );
  if ln2_varb + [chr(0)..chr(6), chr(18)..chr(31)] <> len1_ch_cons2 
	then error ( 35 );
  if (len1_ch_cons - [chr(8)..chr(17),chr(30)]) <> len4_ch_cons then error ( 36 );
  if (double_arr[ 1 ] - double_cons[ 5 ] <> len4_int_var) then error ( 37 );
  if len2_cons * uppers_cons <> arr2_cons[ 2 ] - ['H'..'Z'] then error ( 38 );
  if not (green in [green]) then error ( 39 );
  b := 4 in single_cons[ 2 ];
  if not b then error ( 40 );
  b := color1 in [orange..red];
  if not b then error ( 41 );
  b := 40 in double_arr[ 5 ] * double_cons[ 1 ];
  if not b then error ( 42 );
  sb10to30 := single_cons[ 1 ] + sa10to30;
  if sb10to30 <> [10..30] then error ( 43 );
  s50to100a := s35to127a * [j..k];
  if s50to100a <> [50, 100] then error ( 44 );
  s50to100a := [j..k];
  if s50to100a <> [50..100] then error ( 45 );
  sb10to30 := [0..50];
  if sb10to30 <> [10..30] then error ( 46 );
  s50to75a := [j..k];
  if 76 in s50to75a then error ( 47 );
  s35to127a := [0..20];
  if s35to127a <> [] then error ( 48 );
  s35to127a := [40..60] * [j..k];
  if s35to127a <> [40..60] then error ( 49 );
  sb10to30 := [0..31];
  if sb10to30 <> [10..30] then error ( 50 );
  s50to100a := [30..80] + double_arr[ 2 ];
  if s50to100a <> [50..80] then error ( 51 );
  s50to100a := [j..k] * double_arr[ 1 ];
  if s50to100a <> [50..63] then error ( 52 );
  s75to110a := [10..115];
  if s75to110a <> [75..110] then error ( 53 );
  s50to75a := [20..80];
  if s50to75a <> [50..75] then error ( 54 );
  s75to110a := [80..109];
  if s75to110a <> [80..109] then error ( 55 );
  writeln( ttyoutput, 'End EXE004' );
end.
   