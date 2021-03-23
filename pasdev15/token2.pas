$LENGTH 44
(*$M-,O+,T+,D-*)
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        T O K E N 2                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED:  9-Mar-78

     PURPOSE: To parse user commands to ODMS.

     USAGE:
        procedure GETCMD (var CMD: CMDREC);

     OUTPUT:

        CMD        CMD is a parsing  record  describing  the  command
                   that  was  parsed.  Specifically,  its  fields are
                   loaded as follows:

        COMMAND    A scalar type indicating the imperative  that  was
                   parsed.

        FOUND      A   set  type  describing  the  keywords,  keyword
                   parameters,  and other information  found  on  the
                   line.

        MODNAME    The  module  name  found  on  the  line (the first
                   non-keyword and non-keyword parameter found on the
                   line).

        FNAME1     FNAME1  gets  the literal token found as parameter
                   to the TOWD and DB keywords.

        FNAME2     FNAME2 gets the  literal  token  found  as  either
                   parameter   to  the  FROM  keyword,  or  else  the
                   filename found on the line (the second non-keyword
                   and non-parameter found on the line).

        INTARG1    The  integer  argument  found  after  the  VERSION
                   keyword.

        INTARG2    The integer argument found after the AS keyword.

     REQUIREMENTS: TOKEN2 assumes that terminal I/O has  been  OPENed
        or RESET already.  It will prompt,  but does not flag errors.

     ALGORITHM: The  line  is  repeatedly   scanned   for   keywords,
        imperatives,  and remaining tokens.

     RESPONSIBLE: Jerry Rosen (with help from Todd Sanford).

     CHANGES: NONE.

     ---------------------------------------------------------------- *)
 
$INCLUDE USETYP.INC[52250,247]
 
$INCLUDE T2TYPE.INC[52250,247]
 
var	CSTRING : CMDWRD;
	INDEX : CMDTYP;
	I, LNKINDEX : INTEGER;
	RESNDX : RESWRD;

	CMDLEX: array[CMDTYP] of CMDWRD :=
	 ('USE                         ',
	  'BUILD                       ',
	  'CONVERT                     ',
	  'UPDATE		       ',
	  'DELETE                      ',
	  'PRINT                       ',
	  'CHANGE                      ',
	  'PACK                        ',
	  'EXIT                        ',
	  '                            ' ) ;

	LEXICON: array[RESWRD] of CMDWRD :=
	 ('VERSION                     ',
	  'AS			       ',
	  'FROM                        ',
	  'DATABASE                    ',
	  'TO                          ',
	  'USING                       ',
	  'VERSIONS                    ',
	  'SYMBOLS		       ',
	  'ALL                         ',
	  '                            ',
	  'RESIDENT	               ',
	  '                            ' ) ;


	SHOULD: array[RESWRD] of NEED := 
	 ( VERARG, ASARG, FROMARG, DBARG, TOARG, NONEABOVE, NONEABOVE,
	   NONEABOVE, NONEABOVE, NONEABOVE, NONEABOVE, NONEABOVE ) ;

	BLANKS: CMDWRD := '                            ';

	MAY: array[CMDTYP] of NEEDS :=
	 (
	  [FNAME],				(* USE *)
	  [MODNM, RESSW, USING, USINGARG,
	   DB, DBARG] ,				(* BUILD *)

	  [MODNM, TOWD, TOARG, FNAME],		(* CONVERT *)
	  [DB, DBARG, VER, VERARG, FROM,
	   FROMARG, MODNM, FNAME],		(* UPDATE *)
	  [MODNM, FROM, FROMARG, FNAME,
	   VER, VERARG, ALLSW],			(* DELETE *)
	  [MODNM, FROM, FROMARG, FNAME,
	   VERSW, SYMSW],			(* PRINT *)
	  [VER, ASARG, VERARG, NEWVER, MODNM,
	   DB, DBARG],				(* CHANGE *)
	  [FNAME],				(* PACK *)
	  [],					(* EXIT *)
	  [] )					(* ILLEGAL *);


	MUST: array[CMDTYP] of NEEDS :=
	 (
	  [FNAME],				(* USE *)
	  [USING, USINGARG],			(* BUILD *)
	  [MODNM],				(* CONVERT *)
	  [MODNM],				(* UPDATE *)
	  [MODNM],				(* DELETE *)
	  [MODNM],				(* PRINT *)
	  [NEWVER, ASARG, MODNM],		(* CHANGE *)
	  [],					(* PACK *)
	  [],					(* EXIT *)
	  []  );				(* ILLEGAL *)


	CH : char;
	LINEBUF : packed array[1..257] of char;
        LLEN, START, CURRCT : 1..257;
 

 
function CONVT(CSTRING : CMDWRD) : INTEGER;
var	BASE : 8..10;  
	TEMP : INTEGER;
begin
TEMP := 0;    BASE := 10;    I := 1;
while I <= 28 do
     if CSTRING[I] in ['0'..'9', ' '] then
	  I := I + 1
     else begin
	  if CSTRING[I] = 'B' then
	       BASE := 8
	  else TEMP := -1;
	  I := 29
	  end;
if TEMP = 0 then
     begin
     I := 1;
     while (CSTRING[I] = ' ') and (I < 28) do I := I + 1;
     if I = 28 then TEMP := -1;
     while (not(CSTRING[I] in [' ','B']) and (I <= 28)) do
          begin
	  TEMP := TEMP * BASE + ord(CSTRING[I]) - ord('0');
	  I := I + 1
	  end
     end;
CONVT := TEMP
end;
 
 
public procedure GETCMD(var CMD : CMDREC);
 
procedure GETLIN;
begin
write(tty,'>');
break(tty);
readln(tty);
LLEN := 1;
while not eoln(tty) do
     begin
     read(tty,CH);
     LINEBUF[LLEN] := uppercase(CH);
     LLEN := LLEN + 1
     end;
START := 1;   CURRCT := 1;
for I := LLEN to 257 do
     LINEBUF[I] := ' '
end;
 
 
procedure GETWRD(var CSTRING : CMDWRD);
var I : 1..29;
begin
for I := 1 to 28 do CSTRING[I] := ' ';
while (CURRCT < 257) and (LINEBUF[CURRCT] = ' ') do
     CURRCT := CURRCT + 1;
I := 1;   START := CURRCT;
while (LINEBUF[CURRCT] <> ' ') and (CURRCT < 257) and (I <= 28) do
     begin
     CSTRING[I] := LINEBUF[CURRCT];
     I := I + 1;
     CURRCT := CURRCT + 1
     end
end;
 
 
 
procedure EAT_IT;
begin
for I := START to CURRCT - 1 do
     LINEBUF[I] := ' '
end;
 
function LOOKUP(CSTRING : CMDWRD) : RESWRD;
var	INDEX : RESWRD;
 
begin
LOOKUP := NONEABOVE;
for INDEX := VER to RESSW do
     if LEXICON[INDEX] = CSTRING then LOOKUP := INDEX
end;
 
 
procedure NONRES(var CSTRING : CMDWRD);
var	INDEX : RESWRD;
begin
     loop
     GETWRD(CSTRING);
     exit if (CSTRING = BLANKS) or (LOOKUP(CSTRING) = NONEABOVE)
     end
end;
 
procedure GETMOD;
begin   CURRCT := 1;
NONRES(CSTRING);
if CSTRING <> BLANKS then
     begin
     CMD.MODNAME := '';
     for I := 1 to 6 do
	if CSTRING[I] <> ' ' then 
	CMD.MODNAME := CMD.MODNAME || CSTRING[I];
     CMD.FOUND := CMD.FOUND + [MODNM];
     EAT_IT
     end
end;
 
 
procedure GETFNAME;
begin   CURRCT := 1;
NONRES(CSTRING);
if CSTRING <> BLANKS then
     begin
     CMD.FNAME2 := CSTRING || ' ';
     CMD.FOUND := CMD.FOUND + [FNAME];
     EAT_IT
     end
end;
 
 
 
 
procedure ERROR_;
begin
CMD.COMMAND := ILLEGAL
end;
 
 
procedure RECINIT(var CMD : CMDREC);
begin
with CMD do
     begin
     FNAME1 := '';
     FNAME2 := '';
     MODNAME := '';
     for I := 1 to 256 do LNKTXT[I] := ' ';
     COMMAND := ILLEGAL;
     INTARG1 := 0;
     INTARG2 := 0
     end
end;
 
 
procedure RESWDSCAN;
var TMPINT: INTEGER;
begin
CURRCT := 1;
  loop
     loop
     GETWRD(CSTRING);
     RESNDX := LOOKUP(CSTRING);
     exit if RESNDX in [ENDLIN, NONEABOVE];
     EAT_IT;
     CMD.FOUND := CMD.FOUND + [RESNDX];
     case RESNDX of
 
VER, NEWVER	:begin
		 NONRES(CSTRING);
		 TMPINT := CONVT(CSTRING);
		 EAT_IT;
		 CURRCT := 1;
		 if TMPINT <> -1 then
		      CMD.FOUND := CMD.FOUND + [SHOULD[RESNDX]];
		 if RESNDX = VER then
		   CMD.INTARG1 := TMPINT
		 else
		   CMD.INTARG2 := TMPINT
		 end;
 
TOWD, FROM, DB	:begin
		 NONRES(CSTRING);
		 if CSTRING <> BLANKS then
		      begin
		      if RESNDX = FROM then 
                           CMD.FNAME2 := CSTRING || ' '
                      else CMD.FNAME1 := CSTRING || ' ';
		      CMD.FOUND := CMD.FOUND + [SHOULD[RESNDX]];
		      EAT_IT;
		      CURRCT := 1
		      end
		 end;
 
USING		:begin
		 CMD.FOUND := CMD.FOUND + [USINGARG];
			   LNKINDEX := 1;
			   loop
			   for I := CURRCT to LLEN - 1 do
				begin
				CMD.LNKTXT[I-CURRCT+LNKINDEX] := LINEBUF[I];
				LINEBUF[I] := ' '
				end;
			   exit if CMD.LNKTXT[LLEN - 1 + LNKINDEX - CURRCT] <> '&';
			   CMD.LNKTXT[LLEN - 1 + LNKINDEX - CURRCT] := ' ';
			   LNKINDEX := LNKINDEX + LLEN - CURRCT;
			   GETMOD;
			   GETLIN
			   end
		  end
	      end     (*CASE*)
	end (*LOOP*)
  exit if RESNDX = ENDLIN
  end   (* outer loop*)
end;
 
 
 
begin   (*START OF GETCMD MAINLINE*)
GETLIN;
GETWRD(CSTRING);   CMD.FOUND := [];   RECINIT(CMD);   
INDEX := USE;
while (CMD.COMMAND = ILLEGAL) and (INDEX <= EXITWD) do
     if CSTRING = CMDLEX[INDEX] then
	  CMD.COMMAND := INDEX
     else INDEX := succ(INDEX);
if CMD.COMMAND <> ILLEGAL then
     begin
     EAT_IT;
     RESWDSCAN;
     if (CMD.COMMAND in [USE, PACK]) then
	  GETFNAME;
     GETMOD;
     if not (FROMARG in CMD.FOUND) then
	  GETFNAME;
     IF (CMD.FOUND - MAY[CMD.COMMAND]) <> [] then
	  ERROR_
     else
     if (MUST[CMD.COMMAND] - CMD.FOUND) <> [] then
	  ERROR_;
     for RESNDX := VER to TOWD do
	  if (RESNDX in CMD.FOUND) and (not(SHOULD[RESNDX] in CMD.FOUND))
	  then ERROR_;
	  GETWRD(CSTRING);
          if CSTRING <> BLANKS then ERROR_
     end
end.
