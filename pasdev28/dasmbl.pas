$OPTIONS MAIN, SPECIAL, NOCHECK, PROGRESS, INFO

$LENGTH 44

const
    tab := chr (11b);
    pagelenth := 44;

type
    byte = 0..377b;

public var
    ch: char;
    i, ix: integer;
    f: file of integer;
    str: string[50];
    addr, disp: integer;
    left, right: byte;
    word: integer;
    linecount: integer;
    psectlen, len, count: integer;
    psectid, id: packed array[1..6] of char;

label
    900,
    901;

public procedure checklisting;

begin
  linecount := linecount + 1;
  if linecount >= pagelenth then begin
    page;
    linecount := 0;
  end;
end;

public function width (i: integer): integer;

begin
  if i < 0 then begin
    i := -i;
    width := 1;
  end
  else
    width := 0;
  repeat
    width := width + 1;
    i := i div 10
  until i = 0;
end;

public function bitset (w: integer; bit: integer): boolean;

var
    r: record
      case boolean of
	true: (
	  w1: integer);
	false: (
	  b1: packed array[0..35] of boolean)
    end;

begin
  r.w1 := w;
  bitset := r.b1[35-bit];
end;

public procedure nextword;

begin
  right := f^;
  get (f);
  left := f^;
  word := left*256+right;
  get (f);
end;

public function heading: integer;

begin
  nextword;
  nextword;
  count := (word-6) div 2;
  nextword;
  heading := word;
end;

function rx50 (c: byte): char;

begin
  case c of
    0:
      rx50 := ' ';
    1..26:
      rx50 := chr (c+ord('A')-1);
    27:
      rx50 := '$';
    28:
      rx50 := '.';
    29:
      rx50 := '_';
    30..39:
      rx50 := chr (c-30+ord('0'))
  end;
end;

public procedure r50;

var
    i: integer;

begin
  nextword;
  for i := 3 downto 1 do begin
    id[i] := rx50(word mod 50b);
    word := word div 50b;
  end;
  nextword;
  for i := 6 downto 4 do begin
    id[i] := rx50(word mod 50b);
    word := word div 50b;
  end;
  len := index (id,' ',7)-1;
end;

procedure from;

begin
  writeln (' from ',psectid:psectlen,'+',addr+disp-4:6:o);
  checklisting;
end;

public procedure printcode (i: integer; addrs: integer);

var
    ix,t: integer;

type
    itabform = array[0..122] of packed record
      class: integer;
      mnemonic: packed array[1..5] of char
    end;
    (*$Y        (* since PRETTY goes into convulsions indenting this constant *)
    CONST ITAB: ITABFORM := (
     ( 0,'HALT '),( 0,'WAIT '),( 0,'RTI  '),( 0,'BPT  '),
     ( 0,'IOT  '),( 0,'RESET'),( 0,'RTT  '),( 0,'.....'),
     ( 7,'JMP  '),
     ( 6,'RTS  '),( 1,'SPL  '),( 2,'CCC  '),( 2,'SCC  '),
     ( 7,'SWAB '),( 5,'BR   '),( 5,'BNE  '),( 5,'BEQ  '),
     ( 5,'BGE  '),( 5,'BLT  '),( 5,'BGT  '),( 5,'BLE  '),
     (11,'JSR  '),
     ( 7,'CLR  '),( 7,'COM  '),( 7,'INC  '),( 7,'DEC  '),
     ( 7,'NEG  '),( 7,'ADC  '),( 7,'SBC  '),( 7,'TST  '),
     ( 7,'ROR  '),( 7,'ROL  '),( 7,'ASR  '),( 7,'ASL  '),
     ( 3,'MARK '),( 7,'MFPI '),( 7,'MTPI '),( 7,'SXT  '),
     (15,'MOV  '),(15,'CMP  '),(15,'BIT  '),
     (15,'BIC  '),(15,'BIS  '),(15,'ADD  '),
     (10,'MUL  '),(10,'DIV  '),(10,'ASH  '),(10,'ASHC '),
     (11,'XOR  '),
     ( 6,'FADD '),( 6,'FSUB '),( 6,'FMUL '),( 6,'FDIV '),
     ( 9,'SOB  '),
     ( 5,'BPL  '),( 5,'BMI  '),( 5,'BHI  '),( 5,'BLOS '),
     ( 5,'BVC  '),( 5,'BVS  '),( 5,'BCC  '),( 5,'BCS  '),
     ( 4,'EMT  '),( 4,'TRAP '),
     ( 7,'CLRB '),( 7,'COMB '),( 7,'INCB '),( 7,'DECB '),
     ( 7,'NEGB '),( 7,'ADCB '),( 7,'SBCB '),( 7,'TSTB '),
     ( 7,'RORB '),( 7,'ROLB '),( 7,'ASRB '),( 7,'ASLB '),
     ( 0,'.....'),( 7,'MFPD '),( 7,'MTPD '),( 0,'.....'),
     (15,'MOVB '),(15,'CMPB '),(15,'BITB '),
     (15,'BICB '),(15,'BISB '),(15,'SUB  '),
     ( 0,'CFCC '),( 0,'SETF '),( 0,'SETI '),( 0,'LDUB '),
     ( 0,'LDSC '),( 0,'STA0 '),( 0,'MRS  '),( 0,'STQ0 '),
     ( 0,'.....'),( 0,'SETD '),( 0,'SETL '),( 0,'.....'),
     ( 0,'.....'),( 0,'.....'),( 0,'.....'),( 0,'.....'),
     ( 7,'LDFPS'),( 7,'STFPS'),( 7,'STST '),
     ( 8,'CLRF '),( 8,'TSTF '),( 8,'ABSF '),( 8,'NEGF '),
     (13,'MULF '),(13,'MODF '),(13,'ADDF '),(13,'LDF  '),
     (13,'SUBF '),(13,'CMPF '),(13,'STF  '),(13,'DIVF '),
     (14,'STEXP'),(14,'STCFI'),(14,'STCFD'),(12,'LDEXP'),
     (12,'LDCIF'),(13,'LDCFD'));
     *)





const
    merr := 7;
    mjmp := 8;
    mrts := 9;
    mccc := 11;
    mswab := 13;
    mbr := 14;
    mjsr := 21;
    mclr := 22;
    mmov := 38;
    mmul := 44;
    mfadd := 49;
    msob := 53;
    mbpl := 54;
    mclrb := 64;
    mmovb := 80;
    mcfcc := 86;
    mldfps := 102;
    mclrf := 105;
    mmulf := 109;


  procedure greg (i: integer);

  type
      rtab = array[0..7] of packed array[1..2] of char;

  const
      grtab: rtab := ('R0','R1','R2','R3','R4','R5','SP','PC');

  begin
    write (grtab[i mod 8]);
  end;


  procedure freg (i: integer);

  type
      ftab = array[0..7] of packed array[1..2] of char;

  const
      frtab: ftab := ('F0','F1','F2','F3','F4','F5','F6','F7');

  begin
    write (frtab[i mod 8]);
  end;


  procedure srcdst (i: integer; isfloat: boolean);


    procedure breg;

    begin
      write ('(');
      greg(i);
      write (')');
    end;

  begin
    case (i div 8) mod 8 of
      0:
	if isfloat then
	  freg(i)
	else
	  greg(i);
      1: begin
	write ('@');
	greg (i);
      end;
      2,3: begin
	if odd (i div 8) then
	  write ('@');
	breg;
	write ('+');
      end;
      4,5: begin
	if odd (i div 8) then
	  write ('@');
	write ('-');
	breg;
      end;
      6,7: begin
	if odd (i div 8) then
	  write ('@');
	breg
      end
    end;
  end;

begin
  if i < 100b then begin
    write (tab);
    if i < 7 then
      write (chr(i+ord('0')))
    else
      write (i:2:o);
    writeln;
    checklisting;
  end
  else begin
    case (i div 4096) mod 16 of
      0:
	case (i div 512) mod 8 of
	  0:
	    case (i div 64) mod 8 of
	      0:
		if ((i div 8) mod 8) = 0 then
		  ix := i mod 8
		else
		  ix := merr;
	      1:
		ix := mjmp;
	      2:
		case (i div 8) mod 8 of
		  0:
		    ix := mrts;
		  1,2:
		    ix := merr;
		  3:
		    ix := mrts+1;
		  4,5:
		    ix := mccc;
		  6,7:
		    ix := mccc+1
		end;
	      3:
		ix := mswab;
	      4..7:
		ix := mbr
	    end;
	  1..3:
	    ix := ((i div 256) mod 8)+(mbr-1);
	  4:
	    ix := mjsr;
	  5,6:
	    ix := ((i div 64 + 8) mod 16)+mclr;
	  7:
	    ix := merr
	end;
      1..6:
	ix := ((i div 4096) mod 8)+(mmov-1);
      7:
	case (i div 512) mod 8 of
	  0..4:
	    ix := ((i div 512) mod 8)+mmul;
	  5:
	    if ((i div 32) mod 16)=0 then
	      ix := ((i div 8) mod 4)+mfadd
	    else
	      ix := merr;
	  6:
	    ix := merr;
	  7:
	    ix := msob
	end;
      8:
	case (i div 512) mod 8 of
	  0..4:
	    ix := ((i div 256) mod 16)+mbpl;
	  5,6:
	    ix := ((i div 64+8) mod 16)+mclrb;
	  7:
	    ix := merr
	end;
      9..14:
	ix := ((i div 4096) mod 8)+(mmovb-1);
      15:
	case (i div 256) mod 16 of
	  0:
	    if ((i div 64) mod 4)=0 then
	      if (i mod 64) < 16 then
		ix := (i mod 64)+mcfcc
	      else
		ix := merr
	    else
	      ix := ((i div 64) mod 4)+(mldfps-1);
	  1:
	    ix := ((i div 64) mod 4)+mclrf;
	  2..15:
	    ix := ((i div 256) mod 16)+(mmulf-2)
	end
    end;
    write (tab,itab[ix].mnemonic);
    case itab[ix].class of
      1:
	write (i mod 8:1:o);
      2:
	write (i mod 16:2:o);
      3:
	write (i mod 64:2:o);
      4:
	write (i mod 256:3:o);
      5: begin
	t := i mod 256;
	if t > 127 then
	  t := t-256;
	write (psectid:psectlen,'+',addrs+t*2+2:6:o);
      end;
      6:
	greg(i);
      7:
	srcdst (i,false);
      8:
	srcdst (i,true);
      9: begin
	greg (i div 64);
	write (',?');
      end;
      10: begin
	srcdst (i,false);
	write (',');
	greg (i div 64);
      end;
      11: begin
	greg (i div 64);
	write (',');
	srcdst (i,false);
      end;
      12: begin
	srcdst (i,false);
	write (',');
	freg (i div 64 mod 4);
      end;
      13: begin
	srcdst (i,true);
	write (',');
	freg (i div 64 mod 4);
      end;
      14: begin
	freg (i div 64);
	write (',');
	srcdst (i, true);
      end;
      15: begin
	srcdst (i div 64,false);
	write (',');
	srcdst (i,false)
      end
    end;
    writeln;
    checklisting;
  end;
end;

begin
  rewrite (tty);
  open (tty);
  loop
    writeln (tty);
    write (tty,'FILE: ');
    break;
    readln (tty);
  exit if eoln (tty);
    str := '';
    while not eoln (tty) do begin
      read (tty,ch);
      str := str || ch;
    end;
    reset (f,'.OBJ '||str);
    if eof (f) then
      writeln (tty,'?CAN''T OPEN FILE ',str)
    else begin
      rewrite (output,str || '.LST[,]');
      writeln ('File ',str);
      writeln;
      linecount := 2;
      repeat
	case heading of
	  1: begin (* GSD *)
	    writeln ('GSD');
	    checklisting;
	    for ix := 1 to count div 4 do begin
	      r50;
	      nextword;
	      case left of
		0: begin
		  writeln ('  Module ',id:len);
		  checklisting;
		  nextword;
		end;
		1: begin
		  write ('  Csect ');
		  900:
		    write (id:len,' max length=');
		  nextword;
		  writeln (word:6:o);
		  checklisting;
		end;
		2: begin
		  writeln ('  Internal symbol ',id:len);
		  checklisting;
		end;
		3: begin
		  write ('  Transfer address for Psect ',id:len,' offset ');
		  nextword;
		  writeln (word:5:o);
		  checklisting;
		end;
		4: begin
		  write ('  Global');
		  if bitset (word,3) then
		    write (' definition of ')
		  else
		    write (' request for ');
		  write (id:len,' offset ');
		  nextword;
		  writeln (word:5:o);
		  checklisting;
		end;
		5: begin
		  write ('  Psect ');
		  goto 900;
		end;
		6: begin
		  writeln ('  Version ',id:len);
		  checklisting;
		  nextword;
		end;
		7: begin
		  write ('  Mapped array ',id:len,' of ');
		  nextword;
		  writeln (word: width(word),' blocks');
		  checklisting;
		end
	      end (*case left of *)
	    end; (* for *)
	  end; (* GSD *)
	  2: begin (* END OF GSD *)
	    writeln ('GSD ends');
	    checklisting;
	  end;
	  3: begin (* TXT *)
	    nextword;
	    writeln ('TEXT at location ',psectid:psectlen,'+',word:6:o);
	    checklisting;
	    addr := word;
	    for i := 0 to count-2 do begin
	      nextword;
	      write (' ':4,addr+2*i:6:o,' ':3,left:3:o,' ',right:3:o,' ':3,word:
		6:o);
	      printcode (word, addr+2*i);
	    end;
	  end;
	  4: begin (* RLD *)
	    writeln ('RLD');
	    checklisting;
	    writeln (' Displacement  Type');
	    checklisting;
	    ix := 0;
	    while ix < count do begin
	      nextword;
	      if not (left in [9,10]) then
		write (' ':4,left:3:o,' ':9)
	      else
		write (' ':16);
	      disp := left;
	      case right of
		1: begin
		  write ('Internal');
		  nextword;
		  if word <> 0 then begin
		    write (' to ',psectid:psectlen,'+',word:6:o);
		  end;
		  from;
		  ix := ix + 2;
		end;
		2: begin
		  r50;
		  write ('Global to ',id:len);
		  from;
		  ix := ix + 3;
		end;
		3: begin
		  nextword;
		  write ('Internal displaced to ',psectid:psectlen,'+',word:6:o)
		    ;
		  from;
		  ix := ix + 2;
		end;
		4: begin
		  r50;
		  ix := ix + 3;
		  write ('Global displaced to ',id:len);
		  from;
		end;
		5: begin
		  r50;
		  write ('Global additive to ',id:len);
		  901:
		    nextword;
		  if word <> 0 then begin
		    write (' by ');
		    write (word:6:o);
		  end;
		  from;
		  ix := ix + 4;
		end;
		6: begin
		  r50;
		  write ('Global additive displaced to ',id:len);
		  goto 901;
		end;
		7: begin
		  r50;
		  nextword;
		  writeln ('Location counter defined as ',id:len,'+',word:6:o);
		  checklisting;
		  psectid := id;
		  psectlen := len;
		  ix := ix + 4;
		end;
		10b: begin
		  nextword;
		  writeln ('Location counter set to ',word:6:o);
		  checklisting;
		  ix := ix + 2;
		end;
		11b: begin
		  ix := ix + 1;
		  writeln ('Program limits');
		  checklisting;
		end;
		12b: begin
		  r50;
		  ix := ix + 3;
		  write ('Psect relocation to ',id);
		  from;
		end;
		14b: begin
		  r50;
		  write ('Psect displaced in ',id);
		  from;
		  ix := ix + 3;
		end;
		15b: begin
		  r50;
		  nextword;
		  ix := ix + 4;
		  write ('Psect additive to ',id:len,' by ',word:6:o);
		  from;
		end;
		16b: begin
		  r50;
		  nextword;
		  ix := ix + 4;
		  write ('Psect additive displaced to ',id:len,' by ',word :6:o)
		    ;
		  from;
		end;
		17b: begin
		  writeln ('Complex relocation');
		  checklisting;
		end;
		20b: begin
		  nextword;
		  ix := ix + 2;
		  write ('Library additive by ',word:6:o);
		  from;
		end;
		others: begin
		  writeln (tty,'Illegal relocation type encountered');
		  stop
		end
	      end; (* case right of *)
	    end; (* while ix < count *)
	  end; (* rld case *)
	  5: begin (* isd *)
	    writeln ('ISD ignored');
	    checklisting;
	    for i := 1 to count-2 do
	      nextword;
	  end;
	  6: begin
	    writeln ('Module ends');
	    checklisting;
	  end
	end; (* case heading of *)
	nextword; (* eat checksum *)
      until eof (f);
      close (output);
    end; (* if not eof *)
  end; (* loop *)
end. (* program *)
 