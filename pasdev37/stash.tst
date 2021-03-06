LIFE    ?PAS     ?   .Q+:?nToc    ?LIFESV  ?PAS     ?   \Q+;?Q+;?    ?MONKEY  ?PAS     
   ?Q+;?nU)?    ?TPRIME  ?PAS        cQ+;?U#N    ?NADA1   ?REL         ?nTbnTb                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ld;
$OPTIONS storage=4096
PROGRAM life;

CONST
   nrows := 22;
   ncols := 80;

TYPE
   squarevalue = (void,birth,life,death);

   colindex = 1..ncols;
   colcount = 0..ncols;
   rowindex = 1..nrows;
   rowcount = 0..nrows;

   row = ARRAY[colindex] OF squarevalue;
   field = ARRAY[rowindex] OF row;

   representation = ARRAY[squarevalue] OF char;

CONST
   rep: representation := (' ','0','X','.');

TYPE
   gennum = 0..1000000;
   nneighbors = 0..8;
   sqset = SET OF squarevalue;

CONST
   aliveset: sqset := [birth,life];
   deadset: sqset := [void,death];
$PAGE get_start
FUNCTION get_start: field;

    
   VAR
      roff,rc: rowcount;
      ri: rowindex;
      coff,cc: colcount;
      ci: colindex;

BEGIN
FOR ri := minimum(rowindex) TO maximum(rowindex) DO
   FOR ci := minimum(colindex) TO maximum(colindex) DO
      get_start[ri,ci] := void;

write(tty,'#ROWS #COLS:  ');  
break;
readln(tty);  
read(tty,rc,cc);

IF (rc > 0) AND (cc > 0)
   THEN BEGIN
   roff := (nrows - rc) DIV 2 + minimum(rowindex) - 1;
   coff := (ncols - cc) DIV 2 + minimum(colindex) - 1;

   writeln(tty,'ENTER ROW & COLUMN FOR EACH SPOT; END WITH BLANK LINE.');
   break;

   LOOP
      readln(tty);
      EXIT IF eoln(tty);
     hr :  read(tty,ri,ci);
      IF ri > rc THEN writeln(tty,'ROW NUMBER TOO LARGE');
      IF ci > cc THEN writeln(tty,'COLUMN NUMBER TOO LARGE');
      break;
      IF (ri <= rc) AND (ci <= cc) THEN get_start[roff+ri,coff+ci] := life
      END
   END
END;
$PAGE next_gen
FUNCTION next_gen(f: field;  VAR g: gennum;  VAR quit: boolean): field;
$PAGE count_neigh
   FUNCTION count_neigh(f: field;  r: rowindex;  c: colindex): nneighbors;

      VAR
	 ri,lr,hr: rowindex;
	 ci,lc,hc: colindex;

   BEGIN
   IF r = minimum(rowindex) THEN lr := r 
   ELSE lr := pred(r);
   IF r = maximum(rowindex) THEN hr := r 
   ELSE hr :
   = succ(r);
   IF c = minimum(colindex) THEN lc := c 
   ELSE lc := pred(c);
   IF c = maximum(colindex) THEN hc := c 
   ELSE hc := succ(c);

   count_neigh := minimum(nneighbors);
   FOR ri := lr TO hr DO
      FOR ci := lc TO hc DO
	 IF ((ri <> r) ORIF (ci <> c)) ANDIF (f[ri,ci] IN aliveset)
	    THEN count_neigh := succ(count_neigh)
   END;
$PAGE next_gen2
   VAR
      ri: rowindex;
      ci: colindex;
      nn: nneighbors;

BEGIN
quit := false;

FOR ri := minimum(rowindex) TO maximum(rowindex) DO
   FOR ci := minimum(colindex) TO maximum(colindex) DO
      BEGIN
      nn := count_neigh(f,ri,ci);
    := c   IF (f[ri,ci] IN deadset)
	 THEN 
	 IF nn = 3
	    THEN next_gen[ri,ci] := birth
	 ELSE next_gen[ri,ci] := void
      ELSE				(* f[ri,ci] is in aliveset *)
	 IF nn IN [2..3]
	    THEN next_gen[ri,ci] := life
	 ELSE next_gen[ri,ci] := death;
      quit := quit ORIF ((ci = minimum(colindex)) ORIF (ci = maximum(colindex))
		    ORIF (ri = minimum(rowindex)) ORIF (ri = maximum(rowindex)))
		ANDIF (next_gen[ri,ci] IN aliveset)
      END;

g := succ(g)
END;
$PAGE write_field
PROCEDURE write_field(init: boolean; f: field;
		      g: gennum;  VAR outunit: text);

   CONST
      clear := chr(32b);
      home := c
brehr(36b);
      esc := chr(33b);

   VAR
      ri: rowindex;
      ci: colindex;
      cc: colcount;
      prow: ARRAY[colindex] OF char;
      ch: char;

BEGIN
IF init THEN write(outunit,clear) 
ELSE write(outunit,home);
writeln(outunit,'GENERATION ',g);

FOR ri := minimum(rowindex) TO maximum(rowindex) DO
   BEGIN
   FOR ci := minimum(colindex) TO maximum(colindex) DO
      IF (f[ri,ci] IN [birth,death]) OR (init AND (f[ri,ci] = life)) THEN
	 BEGIN
	 IF f[ri,ci] IN aliveset THEN ch := 'X' 
	 ELSE ch := ' ';
	 write(outunit,esc,'=',chr(ri+37b),chr(ci+37b),ch)
	 END
   END;
write(outunit,esc,'=7 ');
brereak;ak(outunit)
END;
$PAGE *main*
VAR
   sngens: -1000000..1000000;
   ngens,gen: gennum;
   i,gstep: 1..1000000;
   f: field;
   quit: boolean;
   init: boolean;

BEGIN
open(tty);    
rewrite(tty);
LOOP
   write(tty,'HOW MANY GENERATIONS (NEGATIVE TO QUIT)? ');     
   break;
   readln(tty);        
   read(tty,sngens);
   EXIT IF sngens < 0;
   ngens := sngens;
   write(tty,'STEP SIZE? ');   
   break;
   readln(tty);        
   read(tty,gstep);

   gen := 0;
   quit := false;
   f := get_start;

   LOOP
      IF (gen MOD gstep = 0) OR quit
	 THEN BEGIN
	 write(tty,'<ESC> WHEN READY.'); 
	 break;       
	 readln(tty);
	 init := NOT eoln(tty) OR (gen = 0);
	 write_field(init,f,gen,ttyoutput)
	 END;
      IF quit THEN writeln(tty,'HIT BORDER: STOP.');
      EXIT IF quit OR (gen >= ngens);
      f := next_gen(f,gen,quit)
      END
   END
END.
                                                                                                                                                                                                                                                                                                                                                                                            RI: R$OPTIONS storage=4096
PROGRAM LIFE;

CONST
	NROWS := 22;
	NCOLS := 80;

TYPE
	SQUAREVALUE = (VOID,BIRTH,LIFE,DEATH);

	COLINDEX = 1..NCOLS;
	COLCOUNT = 0..NCOLS;
	ROWINDEX = 1..NROWS;
	ROWCOUNT = 0..NROWS;

	ROW = ARRAY[COLINDEX] OF SQUAREVALUE;
	FIELD = ARRAY[ROWINDEX] OF ROW;

	REPRESENTATION = ARRAY[SQUAREVALUE] OF CHAR;

CONST
	REP: REPRESENTATION := (' ','0','X','.');

TYPE
	GENNUM = 0..1000000;
	NNEIGHBORS = 0..8;
	SQSET = SET OF SQUAREVALUE;

CONST
	ALIVESET: SQSET := [BIRTH,LIFE];
	DEADSET: SQSET := [VOID,DEATH];
$PAGE get_start
FUNCTION GET_START: FIELD;
VAR
	ROFF,RC: ROWCOUNT;
	RI: RELN(TOWINDEX;
	COFF,CC: COLCOUNT;
	CI: COLINDEX;

BEGIN
  FOR RI := MINIMUM(ROWINDEX) TO MAXIMUM(ROWINDEX) DO
    FOR CI := MINIMUM(COLINDEX) TO MAXIMUM(COLINDEX) DO
      GET_START[RI,CI] := VOID;

  WRITE(TTY,'#ROWS #COLS:  ');	BREAK;
  READLN(TTY);	READ(TTY,RC,CC);

  IF (RC > 0) AND (CC > 0)
  THEN BEGIN
    ROFF := (NROWS - RC) DIV 2 + MINIMUM(ROWINDEX) - 1;
    COFF := (NCOLS - CC) DIV 2 + MINIMUM(COLINDEX) - 1;

    WRITELN(TTY,'ENTER ROW & COLUMN FOR EACH SPOT; END WITH BLANK LINE.');
    BREAK;

    LOOP
      READLN(TTY);
     EXIT IF EOLN(TTY);
      READ(TTY,RI,CI);
      IF RI > RC THEN WRITELN(TPRED(TY,'ROW NUMBER TOO LARGE');
      IF CI > CC THEN WRITELN(TTY,'COLUMN NUMBER TOO LARGE');
      BREAK;
      IF (RI <= RC) AND (CI <= CC) THEN GET_START[ROFF+RI,COFF+CI] := LIFE
      END
    END
  END;
$PAGE next_gen
FUNCTION NEXT_GEN(F: FIELD;  VAR G: GENNUM;  VAR QUIT: BOOLEAN): FIELD;
$PAGE count_neigh
	FUNCTION COUNT_NEIGH(F: FIELD;  R: ROWINDEX;  C: COLINDEX): NNEIGHBORS;
	VAR
		RI,LR,HR: ROWINDEX;
		CI,LC,HC: COLINDEX;

	BEGIN
	  IF R = MINIMUM(ROWINDEX) THEN LR := R ELSE LR := PRED(R);
	  IF R = MAXIMUM(ROWINDEX) THEN HR := R ELSE HR := SUCC(R);
	  IF C = MINIMUM(COLINDEX) THEN LC := C ELSE LC := PRED(NEXT_C);
	  IF C = MAXIMUM(COLINDEX) THEN HC := C ELSE HC := SUCC(C);

	  COUNT_NEIGH := MINIMUM(NNEIGHBORS);
	  FOR RI := LR TO HR DO
	    FOR CI := LC TO HC DO
	      IF ((RI <> R) ORIF (CI <> C)) ANDIF (F[RI,CI] IN ALIVESET)
		THEN COUNT_NEIGH := SUCC(COUNT_NEIGH)
	  END;
$PAGE next_gen2
VAR
	RI: ROWINDEX;
	CI: COLINDEX;
	NN: NNEIGHBORS;

BEGIN
  QUIT := FALSE;

  FOR RI := MINIMUM(ROWINDEX) TO MAXIMUM(ROWINDEX) DO
    FOR CI := MINIMUM(COLINDEX) TO MAXIMUM(COLINDEX) DO
    BEGIN
      NN := COUNT_NEIGH(F,RI,CI);
      IF (F[RI,CI] IN DEADSET)
	THEN IF NN = 3
	  THEN NEXT_GEN[RI,CI] := BIRTH
	  ELSE NEXT_UM(ROGEN[RI,CI] := VOID
	ELSE					(* f[ri,ci] is in aliveset *)
	IF NN IN [2..3]
	  THEN NEXT_GEN[RI,CI] := LIFE
	  ELSE NEXT_GEN[RI,CI] := DEATH;
      QUIT := QUIT ORIF ((CI = MINIMUM(COLINDEX)) ORIF (CI = MAXIMUM(COLINDEX))
		    ORIF (RI = MINIMUM(ROWINDEX)) ORIF (RI = MAXIMUM(ROWINDEX)))
		ANDIF (NEXT_GEN[RI,CI] IN ALIVESET)
      END;

  G := SUCC(G)
  END;
$PAGE write_field
PROCEDURE WRITE_FIELD(F: FIELD;  G: GENNUM;  VAR OUTUNIT: TEXT);
VAR
	RI: ROWINDEX;
	CI: COLINDEX;
	CC: COLCOUNT;
	PROW: ARRAY[COLINDEX] OF CHAR;

BEGIN
  WRITELN(OUTUNIT,'GENERATION ',G);

  FOR RI := MINIMUM(ROWINDEX) TO MAXIMUM(RONS < WINDEX) DO
  BEGIN
    FOR CI := MINIMUM(COLINDEX) TO MAXIMUM(COLINDEX) DO
      PROW[CI] := REP[F[RI,CI]];
    CC := MAXIMUM(COLCOUNT);
    WHILE (CC > MINIMUM(COLCOUNT)) ANDIF (PROW[CC] = ' ') DO CC := PRED(CC);
    FOR CI := MINIMUM(COLINDEX) TO CC DO WRITE(OUTUNIT,PROW[CI]);
    WRITELN(OUTUNIT)
    END;
  BREAK(OUTUNIT)
  END;
$PAGE *main*
VAR
	SNGENS: -1000000..1000000;
	NGENS,GEN: GENNUM;
	I,GSTEP: 1..1000000;
	F: FIELD;
	QUIT: BOOLEAN;

BEGIN
  OPEN(TTY);	REWRITE(TTY);
  LOOP
    WRITE(TTY,'HOW MANY GENERATIONS (NEGATIVE TO QUIT)? ');	BREAK;
    READLN(TTY);	READ(TTY,SNGENS);
   EXIT IF SNGENS <      0;
    NGENS := SNGENS;
    WRITE(TTY,'STEP SIZE? ');	BREAK;
    READLN(TTY);	READ(TTY,GSTEP);

    GEN := 0;
    QUIT := FALSE;
    F := GET_START;

    LOOP
      IF (GEN MOD GSTEP = 0) OR QUIT
      THEN BEGIN
	WRITE(TTY,'<CR> WHEN READY.');	BREAK;	READLN(TTY);
	WRITE_FIELD(F,GEN,TTYOUTPUT)
	END;
      IF QUIT THEN WRITELN(TTY,'HIT BORDER: STOP.');
     EXIT IF QUIT OR (GEN >= NGENS);
      F := NEXT_GEN(F,GEN,QUIT)
      END
    END
  END.
                                                                                                                                                                       OF
PROGRAM monkey;				(* make like 3rd order eddington monkey *)

TYPE
   posint = 0..10000000;		(* your average nonnegative integer *)
   charcount = 0..262143;		(* 18 bits *)
   charindex = 0..27;			(* 0:bad; 1:space; 2..27:A-Z *)
   order1 = PACKED ARRAY[charindex] OF charcount;
   order2 = PACKED ARRAY[charindex] OF order1;
   order3 = PACKED ARRAY[charindex] OF order2;

   total3 = ARRAY[charindex,charindex] OF posint;
$PAGE ch_to_index
FUNCTION ch_to_index(ch: char): charindex;

   (* convert a character to its index in type CHARINDEX *)

   VAR
      tch: char;

BEGIN
tch := uppercase(ch);
CASE tch OF

      ' ': ch_to_index := 1;
   'A'..'Z': ch_to_index := ord(tch) - ord('A') + 2;
   OTHERS: ch_to_index := 0
   END					(* case *)
END;					(* ch_to_index *)
$PAGE index_to_ch
FUNCTION index_to_ch(i: charindex): char;

(* convert index to character *)

BEGIN
CASE i OF
   0: index_to_ch := '%';
   1: index_to_ch := ' ';
   2..27: index_to_ch := chr(ord('A') + i - 2)
   END					(* case *)
END;					(* index_to_ch *)
$PAGE getchar
FUNCTION getchar(VAR f: text; VAR chi: charindex): boolean;

   (* return index of next alphabetic or space read from file F
     in CHI--EOLN is space, alphabetics are uppercased.
   		(*   function value TRUE iff EOF(F). *)

   STATIC VAR
      spaceflag: boolean := true;	(* TRUE when last char was a space *)

   VAR
      ch: char;

BEGIN
REPEAT					(* look for good character *)
   IF eoln(f)
      THEN BEGIN			(* EOLN is space *)
      ch := ' ';
      readln(f)				(* set up for next line *)
      END
   ELSE read(f,ch);			(* just read a char *)
   chi := ch_to_index(ch)		(* convert *)
   UNTIL ((chi > 0)			(* good character *)
       AND NOT (spaceflag AND (chi = 1)))   (* but don't repeat space *)
       OR eof(f);			(* must quit! *)
spaceflag := chi = 1;
getchar := eof(f)
END;					(* dicesgetchar *)
$PAGE random_num
FUNCTION random_num(r: real): real OPTIONS special(word);

   (* generate pseudo-random number by linear congruential method *)

   CONST
      m := 4294967296;			(*  2**32  *)

   VAR
      x: 0..4294967295;

BEGIN
x := trunc(r*m);
random_num := ((5*x) MOD m)/m
END;					(* random_num *)
$PAGE *main*
VAR
   inname,outname: STRING;		(* input, output filename strings *)
   infile,outfile: text;		(* input, output file identifiers *)
   correl: order3;			(* correlation matrix *)
   cortot: total3;			(* sigma of CORREL over 3rd index *)
   i,j,k: charindex;			(* 1st, 2nd, 3rd indices;
   into CORREL *)
   countlen,outlen: posint;		(* character counters for output *)
   outcol: 1..81;			(* output column number *)
   ranno,seed: real;			(* for random numbers *)
   cum,randpoint: posint;		(* for weighted-average calculation *)
   word: STRING;			(* a random word *)
   ch: char;				(* all-purpose character *)
   ineof: boolean;			(* indicates input end-of-file *)

BEGIN

(* first fill in array, then generate gibberish *)

open(tty);    
rewrite(tty);
writeln(tty,'Output file=input file:');       
break;
readln(tty);
outname := '';				(* read and construct file names *)
LOOP
   read(tty,ch);
  )
   EXIT IF (ch = '=') OR eoln(tty);
   outname := outname||ch
   END;
inname := '';
WHILE NOT eoln(tty) DO
   BEGIN
   read(tty,ch);
   inname := inname||ch
   END;

reset(infile,inname);
FOR i := minimum(i) TO maximum(i) DO	(* initialize *)
   FOR j := minimum(j) TO maximum(j) DO
      FOR k := minimum(k) TO maximum(k) DO
	 correl[i][j][k] := 0;

i := 1;         
j := 1;					(* "last two" characters considered spaces *)

REPEAT					(* fill in CORREL *)
   ineof := getchar(infile,k);
   correl[i][j][k] := correl[i][j][k] + 1;  (* increment count *)
   i := j;       
   j := k;				(* move back indices *)
  many  UNTIL ineof OR (correl[i][j][k] = maximum(charcount));

(* stop at EOF or to prevent overflow *)

close(infile);

(*       summarize CORREL into CORTOT:

		 27
		 ---
  CORTOT[I,J] =   >    CORREL[I][J][K]
		 ---
		 K=1
 *)

FOR i := minimum(i) TO maximum(i) DO
   FOR j := minimum(j) TO maximum(j) DO
      BEGIN
      cortot[i,j] := 0;
      FOR k := minimum(k) TO maximum(k) DO
	 cortot[i,j] := cortot[i,j] + correl[i][j][k]
      END;

seed := (time MOD 1000)/1000.;		(* random number seed *)
IF seed <= 0.001 THEN seed := 0.002;	(* a zero seed is bad *)
ranno := random_num(seed);

write(tty,'How many    (*characters to generate?  ');      
break;
readln(tty);          
read(tty,outlen);
rewrite(outfile,outname);

word := '';     
i := 1;   
j := 1;   
outcol := 1;				(* initialize phase 2 *)
FOR countlen := 1 TO outlen DO		(* once per character generated *)
   IF cortot[i,j] = 0			(* pair I,J never occurred in source text-- *)
      THEN BEGIN			(* we shouldn't have generated it *)
      writeln(tty,'Help!  Impossible! I,J=',i,j);
      STOP
      END
   ELSE BEGIN				(* weighted average computation--integer arithmetic *)
      ranno := random_num(ranno);
      randpoint := trunc(ranno*cortot[i,j]);

      (*   ou RANDPOINT is in range 0 to CORTOT[I,J]-1 *)

      cum := 0;   
      k := minimum(k);
      LOOP				(* add to CUM until > RANDPOINT, from CORREL:
					   K that caused this is index of character
					   specified by RANDPOINT *)
	 cum := cum + correl[i][j][k];
	 EXIT IF cum > randpoint;
	 k := succ(k)			(* next K *)
	 END;

      ch := index_to_ch(k);		(* convert *)

      IF ch <> ' ' THEN word := word||ch
      ELSE BEGIN			(* end of silly word: try to fit on 80-char line *)
	 IF outcol + length(word) > 80
	    THEN BEGIN			(* need new line *)
	    writeln(outfile);     
	    write(outfile,word);
	    ou     tcol := length(word) + 1
	    END
	 ELSE BEGIN			(* fits *)
	    write(outfile,' ',word);
	    outcol := outcol + length(word) + 1
	    END;

	 word := ''			(* blank word *)
	 END;

      i := j;     
      j := k				(* advance for next random choice *)
      END;

writeln(outfile);			(* last carriage return *)
close(outfile)
END.
                                                                                                                                                                                                                                                                                        me!')program tprime;

var
	number,divisor: integer;
	prime: boolean;

begin
open(tty);rewrite(tty);
loop
  write(tty,'Number: ');break;
  readln(tty);
  exit if eoln(tty);
  read(tty,number);
  if number < 0 then writeln(tty,'Divisible by -1')
  else if number = 0 then writeln(tty,'Zero is not prime')
  else if number = 1 then writeln(tty,'One is not prime')
  else if not odd(number) then writeln(tty,'Divisible by 2')
  else begin
    prime := true;
    for divisor := 3 to trunc(sqrt(number)) do
      exit if odd(divisor) andif ((number mod divisor) = 0) do prime := false;
    if prime then writeln(tty,'Prime!')     
      else writeln(tty,'Divisible by',divisor)
    end
  end
end.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          0 ?      i   ?     @      ?          i       x      aJ1!# ?INX  M?J1!# ?INX    ( (                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ' J