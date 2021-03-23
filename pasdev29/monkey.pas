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
     function value TRUE iff EOF(F). *)

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
END;					(* getchar *)
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
   i,j,k: charindex;			(* 1st, 2nd, 3rd indices into CORREL *)
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
   UNTIL ineof OR (correl[i][j][k] = maximum(charcount));

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

write(tty,'How many characters to generate?  ');      
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

      (* RANDPOINT is in range 0 to CORTOT[I,J]-1 *)

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
	    outcol := length(word) + 1
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
