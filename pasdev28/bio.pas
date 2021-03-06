program bio options special, nocheck;
label 1;
PUBLIC VAR

PP: ARRAY[0..45] OF INTEGER;
PROCEDURE INIT1;
BEGIN
  PP[ 0] :=   0;
  PP[ 1] :=   4;
  PP[ 2] :=   9;
  PP[ 3] :=  13;
  PP[ 4] :=  17;
  PP[ 5] :=  21;
  PP[ 6] :=  24;
  PP[ 7] :=  27;
  PP[ 8] :=  29;
  PP[ 9] :=  31;
  PP[10] :=  32;
  PP[11] :=  33;
  PP[12] :=  33;
  PP[13] :=  32;
  PP[14] :=  31;
  PP[15] :=  29;
  PP[16] :=  27;
  PP[17] :=  24;
  PP[18] :=  21;
  PP[19] :=  17;
  PP[20] :=  13;
  PP[21] :=   9;
  PP[22] :=   4;
  PP[23] :=   0;
  PP[24] :=  -4;
  PP[25] :=  -9;
  PP[26] := -13;
  PP[27] := -17;
  PP[28] := -21;
  PP[29] := -24;
  PP[30] := -27;
  PP[31] := -29;
  PP[32] := -31;
  PP[33] := -32;
  PP[34] := -33;
  PP[35] := -33;
  PP[36] := -32;
  PP[37] := -31;
  PP[38] := -29;
  PP[39] := -27;
  PP[40] := -24;
  PP[41] := -21;
  PP[42] := -17;
  PP[43] := -13;
  PP[44] :=  -9;
  PP[45] :=  -4
END;

PUBLIC VAR
EE: ARRAY[0..55] OF INTEGER;
PROCEDURE INIT2;
BEGIN
  EE[ 0] :=   0;
  EE[ 1] :=   4;
  EE[ 2] :=   7;
  EE[ 3] :=  11;
  EE[ 4] :=  14;
  EE[ 5] :=  18;
  EE[ 6] :=  21;
  EE[ 7] :=  23;
  EE[ 8] :=  26;
  EE[ 9] :=  28;
  EE[10] :=  30;
  EE[11] :=  31;
  EE[12] :=  32;
  EE[13] :=  33;
  EE[14] :=  33;
  EE[15] :=  33;
  EE[16] :=  32;
  EE[17] :=  31;
  EE[18] :=  30;
  EE[19] :=  28;
  EE[20] :=  26;
  EE[21] :=  23;
  EE[22] :=  21;
  EE[23] :=  18;
  EE[24] :=  14;
  EE[25] :=  11;
  EE[26] :=   7;
  EE[27] :=   4;
  EE[28] :=   0;
  EE[29] :=  -4;
  EE[30] :=  -7;
  EE[31] := -11;
  EE[32] := -14;
  EE[33] := -18;
  EE[34] := -21;
  EE[35] := -23;
  EE[36] := -26;
  EE[37] := -28;
  EE[38] := -30;
  EE[39] := -31;
  EE[40] := -32;
  EE[41] := -33;
  EE[42] := -33;
  EE[43] := -33;
  EE[44] := -32;
  EE[45] := -31;
  EE[46] := -30;
  EE[47] := -28;
  EE[48] := -26;
  EE[49] := -23;
  EE[50] := -21;
  EE[51] := -18;
  EE[52] := -14;
  EE[53] := -11;
  EE[54] :=  -7;
  EE[55] :=  -4
END;


PUBLIC VAR
II: ARRAY[0..65] OF INTEGER;
PROCEDURE INIT3;
BEGIN
  II[ 0] :=   0;
  II[ 1] :=   3;
  II[ 2] :=   6;
  II[ 3] :=   9;
  II[ 4] :=  12;
  II[ 5] :=  15;
  II[ 6] :=  18;
  II[ 7] :=  20;
  II[ 8] :=  23;
  II[ 9] :=  25;
  II[10] :=  27;
  II[11] :=  29;
  II[12] :=  30;
  II[13] :=  31;
  II[14] :=  32;
  II[15] :=  33;
  II[16] :=  33;
  II[17] :=  33;
  II[18] :=  33;
  II[19] :=  32;
  II[20] :=  31;
  II[21] :=  30;
  II[22] :=  29;
  II[23] :=  27;
  II[24] :=  25;
  II[25] :=  23;
  II[26] :=  20;
  II[27] :=  18;
  II[28] :=  15;
  II[29] :=  12;
  II[30] :=   9;
  II[31] :=   6;
  II[32] :=   3;
  II[33] :=   0;
  II[34] :=  -3;
  II[35] :=  -6;
  II[36] :=  -9;
  II[37] := -12;
  II[38] := -15;
  II[39] := -18;
  II[40] := -20;
  II[41] := -23;
  II[42] := -25;
  II[43] := -27;
  II[44] := -29;
  II[45] := -30;
  II[46] := -31;
  II[47] := -32;
  II[48] := -33;
  II[49] := -33;
  II[50] := -33;
  II[51] := -33;
  II[52] := -32;
  II[53] := -31;
  II[54] := -30;
  II[55] := -29;
  II[56] := -27;
  II[57] := -25;
  II[58] := -23;
  II[59] := -20;
  II[60] := -18;
  II[61] := -15;
  II[62] := -12;
  II[63] :=  -9;
  II[64] :=  -6;
  II[65] :=  -3
END;


PUBLIC VAR
  BDAY,BMON,BYEAR,SDAY,SMON: INTEGER;
  CURDAY: INTEGER;
  DAYS: ARRAY[1..12] OF INTEGER;
  WEEKS: ARRAY[0..6] OF PACKED ARRAY[1..4] OF CHAR;
  MONTHS:ARRAY[1..12] OF PACKED ARRAY[1..4] OF CHAR;
  LINE: PACKED ARRAY[1..67] OF CHAR;
  FIRST: BOOLEAN;
  MIN: INTEGER;
  IX: INTEGER;
  i,e,p,j,past,count: integer;
  NAME: STRING;
  CH: CHAR;

PROCEDURE INIT4;
BEGIN
  DAYS[1] := 31; DAYS[2] := 28; DAYS[3] := 31; DAYS[4] := 30; 
  DAYS[5] := 31; DAYS[6] := 30; DAYS[7] := 31; DAYS[8] := 31;
  DAYS[9] := 30; DAYS[10] := 31; DAYS[11] := 30; DAYS[12] := 31;
  weeks[0] := 'Sun '; weeks[1] := 'Mon '; weeks[2] := 'Tue '; weeks[3] := 'Wed ';
  weeks[4] := 'Thu '; weeks[5] := 'Fri '; weeks[6] := 'Sat ';
  months[1] := 'Jan '; months[2] := 'Feb '; months[3] := 'Mar ';
  months[4] := 'Apr '; months[5] := 'May '; months[6] := 'Jun ';
  months[7] := 'Jul '; months[8] := 'Aug '; months[9] := 'Sep ';
  months[10]:= 'Oct '; months[11]:= 'Nov '; months[12]:= 'Dec'
end;

procedure error;
begin
  writeln (tty,'What?'); break (tty); goto 1;
end;

begin
  rewrite (tty);
  rewrite (output,'BIO.LST');
  open (tty);
  init1;
  init2;
  init3;
  init4;
  loop
    1: writeln(tty); write (tty,' Chart for: ');
    break(tty);
    readln(tty);
    exit if eoln(tty);
    name := '';
while not eoln(tty) do begin
      read (tty,ch);
      name := name || ch;
    end;
    if name = '' then error;
    write (tty,' Birth date: ');
    break(tty);
    readln (tty);
    read (tty,bmon, bday, byear);
    if (byear>=78) orif (byear<0) orif (bmon>12) orif (bmon<1) orif
      (bday<=0) orif (bday>days[bmon]) then error;
    sday := 1; smon := 1;
    past := 0;
    curday := 0;
    for j := 77 downto byear+1 do begin
      past := past + 365;
      if (j mod 4) = 0 then past := past + 1;
    end;
    for j := 12 downto bmon+1 do
      past := past + days[j];
    if (bmon<=2) andif ((byear mod 4)=0) then past := past + 1;
    past := past + days[bmon]-bday+1;
    p := (past mod 23)*2;
    e := (past mod 28)*2;
    i := (past mod 33)*2;
    writeln (tty,' ',past:5,' days, ',past/365.0:4:1,' years'); break (tty);
    page(ttyoutput); page(ttyoutput);
    writeln (' ':15,'1978 Biorhythm Chart for: ',name);
    writeln(tty);
    writeln(tty);
    min := 33;
    for count := 1 to 365 do begin
      for first := true downto false do begin
	if first then begin
	  write (weeks[curday]);
	  curday := (curday+1) mod 7;
	  write (months[smon]); write (sday:3,'  ');
	end
	else write (' ':13);
	for j := 1 to min+34 do line[j] := ' ';
	min := 0;
	line[34] := '.';
	ix := pp[p];
	line[ix+34] := 'p'; 
	if ix>min then min := ix;
	ix := ee[e]; line[ix+34] := 'e';
	if ix>min then min := ix;
	ix := ii[i]; line[ix+34] := 'i';
	if ix>min then min := ix;
	writeln (line:min+67);
	p := (p+1)mod 46;
	e := (e+1)mod 56;
	i := (i+1)mod 66;
      end; (* downto *)
      sday := sday + 1;
      if sday > days[smon] then begin
	sday := 1;
	smon := smon+1;
      end;
    end; (* for *)
    page (ttyoutput);
    break(tty);
  end; (* loop *)
end.
   