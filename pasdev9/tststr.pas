module test options special, ass;

public function f (
	var first: integer;
	str1: string[*];
	var str2: string[*];
	pstr1: packed array[1..*] of char;
	var pstr2: packed array[1..*] of char;
	str3: string;
	var str4: string;
	pstr3: packed array[1..100] of char;
	var pstr4: packed array[1..100] of char;
	var last: integer ): boolean;

var
  p: ^ integer;
  i: integer;
  str5: string;
  pstr5: packed array[1..100] of char;

begin
  first := last;
  p := address (str1);
  i := upperbound (str1);
  p := address (str2);
  i := upperbound (pstr1);
(*  p := address (pstr1); *)
  i := upperbound (pstr1);
  p := address (pstr2);
  i := upperbound (pstr2);
  p := address (str3);
  i := upperbound (str3);
  p := address (str4);
  i := upperbound (str4);
  p := address (pstr3);
  i := upperbound (pstr3);
  p := address (pstr4);
  i := upperbound (pstr4);
  str5 := str1;
  str5 := str2;
  str5 := str3;
  str5 := str4;
  str5 := pstr1;
  str5 := pstr2;
  str5 := pstr3;
  str5 := pstr4;
  pstr5 := str1;
  pstr5 := str2;
  pstr5 := str3;
  pstr5 := str4;
  pstr5 := pstr1;
  pstr5 := pstr2;
  pstr5 := pstr3;
  pstr5 := pstr4;
  str2 := str1;
  str2 := str2;
  str2 := str3;
  str2 := str4;
  str2 := str5;
  str2 := pstr1;
  str2 := pstr2;
  str2 := pstr3;
  str2 := pstr4;
  str2 := pstr5;
  pstr2 := str1;
  pstr2 := str2;
  pstr2 := str3;
  pstr2 := str4;
  pstr2 := str5;
  pstr2 := pstr1;
  pstr2 := pstr2;
  pstr2 := pstr3;
  pstr2 := pstr4;
  pstr2 := pstr5;
  str4 := str1;
  str4 := str2;
  str4 := str3;
  str4 := str4;
  str4 := str5;
  str4 := pstr1;
  str4 := pstr2;
  str4 := pstr3;
  str4 := pstr4;
  str4 := pstr5;
  pstr4 := str1;
  pstr4 := str2;
  pstr4 := str3;
  pstr4 := str4;
  pstr4 := str5;
  pstr4 := pstr1;
  pstr4 := pstr2;
  pstr4 := pstr3;
  pstr4 := pstr4;
  pstr4 := pstr5;
end.
 