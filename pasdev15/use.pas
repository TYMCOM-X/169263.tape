(*$X-,M-,D-,O-*)
$LENGTH 44
 
(*   +--------------------------------------------------------------+
     I                                                              I
     I                        U S E P R O                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 13-Jun-77

     PURPOSE: To process an MDL description of an overlayed  program,
        detecting any syntax errors in the MDL,  and returning to the
        caller lists of areas,  modules (with  all  symbols  in  that
        module),  and symbols for the main transfer vector.

     USAGE:
        MDLPROC(VAR noerrors : BOOLEAN;
                VAR modlist, ressymbollist, maintvlist, arealist : symptr);

     OUTPUT:

        noerrors   a  BOOLEAN  flag,  set to FALSE if any errors were
                   found in the MDL.

        <pointers> Heads  of  the  lists  described  above--must   be
                   initialized to NIL by the caller.

     REQUIREMENTS: Entry point SETUP requires a PACKED ARRAY[1..9] OF
        CHAR with filename-extension for initial opening of  the  MDL
        file.

     EFFECTS: If an error was found,  the states of the four pointers
        is uncertain--if additional processing is desired,  the lists
        should  be walked and the records on it DISPOSED.  Otherwise,
        the lists will have records of type  SYMREC.  Filler  entries
        in the MDL will be represented only by skippint in the module
        and/or main transfer  vector  list  indexing;  if  the  index
        jumps by two,  there is a hole laid down by a FILLER entry.

     NOTES: In  a  later  version,  the processor's symbol table will
        probably be made  available  to  the  caller  for  the  PRINT
        command.

     RESPONSIBLE: Jerry Rosen

     CHANGES: Altered for new transfer vector (ie. undifferentiated with
     respect to separate parts for resident and overlaid symbols) and
     quicker processing with better error reporting March 6, 1978, by
     Jerry Rosen.

     ---------------------------------------------------------------- *)
 
const	NUMKWD = 16;  (*NUMBER of LANGUAGE KEYWORDS*)
 
$INCLUDE USETYP.INC[52250,247]
 
type	FILENM = string[30];
	INPTOK = packed array[1..10] of char;
	TEMPLINE = packed array[1..257] of char;
	LINE = string[255];
	COUNT = 1..255;
	ARRINDEX = 1..10;
	TOKENS = set of TOKENTYPE;
	LIST = ^LISTREC;
	LISTREC = record
	     NAME : SYMBOLIC;
	     NEXTELL : LIST
	     end;
 
var	LASTMOD, LASTAREA,
        LASTMAINTV : SYMPTR;
	TOKEN, TYPEOF : TOKENTYPE;
	WORD, SYMNAME, MODNAME : SYMBOLIC;
	BLANKS: SYMBOLIC := '      ';
	VALTEMP, I, NUMVAL, STORKEEP : INTEGER;
        LINENUM : INTEGER := 0;
	NEWPTR, MODPTR, TEMPTR : SYMPTR;
	FOUND, FOUNDONE, NOERRORS : boolean;
	MDLFILE : TEXT;

	KEYLIST : array[1..NUMKWD] of INPTOK := (
	  'MACHINE   ',
	  'SYSTEM    ',
	  'AREAS     ',
	  'MODULES   ',
	  'SYMBOLS   ',
	  'STORAGE   ',
	  'END       ',
	  'SIZE      ',
	  'CONTAINS  ',
	  'IN        ',
	  'PROCEDURE ',
	  'FUNCTION  ',
	  'VAR	     ',
	  'CONST     ',
	  'RESIDENT  ',
	  'FILLER    '
	  );

	KEYVAL : array[1..NUMKWD] of TOKENTYPE := (
	  MACHINE, SYSTEM, AREAS, MODULES, SYMBOLS, STORAGE, ENDSYM,
	  SIZE, CONTAINS, INSYM, PROCSYM, FUNCTSYM, VARSYM, CONSYM,
	  RESIDENT, FILLER
	  );

	CURRLINE : LINE;
	PADDING, LINESIZE : COUNT;
	CURRCOUNT: COUNT := 1;
	LEGALCH: set of char := 
	  ['A'..'Z', '0'..'9', '%', '_', '.', '$'];
	WORDIN : INPTOK;
	STOPPERS, ALLSYM : TOKENS;
 
 
 
 
 
external procedure LOOKUP(    TABLE : SYMPTR;
		     WORD : SYMBOLIC;
		     KIND : KINDOFREC;
		 var FOUND : boolean;
		 var VALTEMP : INTEGER;
		 var WHERE : SYMPTR); 
 
 

external procedure PUT_ENT(var FIRST, LAST : SYMPTR;
		      WHICH : KINDOFREC;
		      INDEX : INTEGER;
		      NAME : SYMBOLIC;
		  var POINTER : SYMPTR);
 
 
 
external procedure ENDINORDERWALK(    TABLE : SYMPTR;
			 var TEST : boolean);
 
 
 
 
 
procedure GETTOKEN;
 
type	KEYNUM = 1..NUMKWD;
	BASECT = 8..10;
 
label	1,2,3;
 
var	CH, LOOKAHEAD : char;
	I : ARRINDEX;
	BASE : BASECT;
	KEYNDX : KEYNUM;
 
     FUNCTION NEXTCH : char;
     begin
     NEXTCH := substr (CURRLINE, CURRCOUNT, 1);
     CURRCOUNT := CURRCOUNT + 1;
     LOOKAHEAD := substr (CURRLINE, CURRCOUNT, 1)
     end;  (*TO GET NEXT CHARACTER in THE LINE BUFFER*)
 
	procedure CHUCKREST;
	begin
	while LOOKAHEAD <> ' ' do CH := NEXTCH;
	end; (*GETTOKEN'S HELPER TO DISCARD THE REST of A TOKEN*)
 
     procedure GETNEWLINE;
     var     TEMP : char;
	     TMPLIN: TEMPLINE;
     begin
     if EOLN(MDLFILE) then
	if EOF(MDLFILE) then goto 1
	else READLN(MDLFILE);
     for CURRCOUNT := 1 TO 255 do
	  if EOLN(MDLFILE) then TMPLIN[CURRCOUNT] := ' '
          else begin
	       LINESIZE := CURRCOUNT;
	       READ(MDLFILE, TEMP);
	       TMPLIN[CURRCOUNT] := uppercase(TEMP)
	       end;
     CURRCOUNT := 1;  LINENUM := LINENUM + 1;
     LOOKAHEAD := TMPLIN[1];
     CURRLINE := TMPLIN
     end;  (*TO GET A NEW LINE in THE LINE BUFFER CURRLINE*)
 
begin
1: LOOKAHEAD := substr (CURRLINE, CURRCOUNT, 1);
if eof(MDLFILE) then TOKEN := NOMOR
else begin
     for I := 1 TO 10 do WORDIN[I] := ' ';
     while LOOKAHEAD = ' ' do
	  begin
	  if CURRCOUNT >= LINESIZE then GETNEWLINE
	                      else CH := NEXTCH
	  end;
 
     case LOOKAHEAD of
'('	       :begin
		CH := NEXTCH;
		if LOOKAHEAD <> '*' then
		     begin
		     TOKEN := BADSYM;
		     CHUCKREST
		     end
		else begin
		     CH := NEXTCH;
			  loop
			  if CURRCOUNT >= LINESIZE then GETNEWLINE;
			  CH := NEXTCH;
			  exit if ((CH = '*') and (LOOKAHEAD = ')' ))
					or eof(MDLFILE)
			  end;  (*loop*)
		     if eof(MDLFILE) then 
			  writeln(tty,'UNCLOSED COMMENT--**GOBBLE**')
		     else CH := NEXTCH;
		     goto 1
		     end  (*else*)
		end;  (*CASE*)
 
'0'..'9'       :begin
		(*MAYBE OCTAL COMVERSION*)
		I := 1;     BASE := 10;
		while LOOKAHEAD in ['0'..'9', 'B'] do
		     begin
		     WORDIN[I] := LOOKAHEAD;
		     CH := NEXTCH; I := I + 1
		     end;
		if LOOKAHEAD <> ' ' then
		     begin
		     TOKEN := BADSYM;
		     CHUCKREST
		     end
		else begin
		     NUMVAL := 0;
		     if CH = 'B' then BASE := 8;
		     for I := 1 TO 10 do
			  if not(WORDIN[I] in [' ','B']) then
			       NUMVAL := (BASE*NUMVAL) + 
			       (ord(WORDIN[I]) - ord('0'))
		     end;
	        TOKEN := NUMERIC
		end;
 
','	       :begin
		TOKEN := COMMA;
		CH := NEXTCH
		end;
 
OTHERS	       :begin
		for I := 1 TO 10 do
		     begin
		     CH := NEXTCH;
		     if not(LOOKAHEAD in (LEGALCH+[' ',','])) then
			  begin
			  TOKEN := BADSYM;
			  CHUCKREST;
			  goto 3
			  end
		     else WORDIN[I] := CH;
		     if CH = '_' then WORDIN[I] := '%';
		     if LOOKAHEAD in [' ',','] then goto 2
		     end;  (*for*)
	      2:if not (LOOKAHEAD in [' ', ',']) then
		     begin
		     writeln(tty,'LONG IDENTIFIER IS NOW ', WORDIN);
		     CHUCKREST
		     end;
		for KEYNDX := 1 TO NUMKWD do
		     begin
		     if KEYLIST[KEYNDX] = WORDIN then
			  begin
			  TOKEN := KEYVAL[KEYNDX];
			  goto 3
			  end
		     end;
		TOKEN := LEGALSYM;
		WORD := WORDIN;
		WORD := substr(WORD,1,index(WORD,' ',(length(WORD)+1))-1)
		(* To catch only good part of identifier (no blanks) *)
		end  (*OTHERS*)
	  end; (*case*)
   3:end; (*else*)
end;  (*procedure*)
$PAGE
procedure EATUP(STOPPERS : TOKENS);
begin
while not (TOKEN in STOPPERS) do
     GETTOKEN
end;
 
 
 
 
 
procedure MDLERR(WHICH : MDLERRTYPE;  STOPPERS : TOKENS);
(*  ERROR MESSAGE ROUTINE***
    SETS THE NOERRORS FLAG TO false for MDLPROC and THE CALLING
    PROGRAM.  GETS WORD and MODWORD, SET BY MDLPROC.*)
 
var  ONEASSUME, FIRSTUSED : boolean;
 
begin
ONEASSUME := false;
NOERRORS := false;
FIRSTUSED := false;
writeln(tty);
write(tty, LINENUM:5, '***  ');
for PADDING :=1 TO LINESIZE do write(tty,CURRLINE[PADDING]);
writeln(tty);
for PADDING := 1 TO CURRCOUNT + 9 do write(tty,' ');
writeln(tty,'^');
EATUP(STOPPERS);
 
case WHICH of
 
GARBAGE		:writeln(tty,'GARBAGE FOUND AT END OF LINE');
 
NOAREA		:writeln(tty,'NO AREAS SECTION SEEN');
 
MULTDEFAREA	:begin
		 writeln(tty,'AREA ', WORD, 'MULTIPLY DEFINED');
		 FIRSTUSED := true
		 end;
 
SIZERR		:begin
		 writeln(tty,'INVALID OR MISSING OVERLAY AREA SIZE');
		writeln(tty,' 1000 (OCTAL) ASSUMED')
		 end;
 
NOMOD		:writeln(tty,' NO MODULES SECTION SEEN');
 
MULTDEFMOD	:begin
		 writeln(tty,'MODULE "',MODNAME, '" MULTIPLY DEFINED');
		 FIRSTUSED := true
		 end;
 
INEXPECT        :begin
		 writeln(tty,'"IN" CLAUSE EXPECTED IN PREVIOUS DEF''N');
		 writeln(tty,'SYMBOL APPEARS IN "CONTAINS" AND CANNOT BE RES.')
		 end;
 
MISAREANAME	:writeln(tty,'INVALID AREA NAME ');
 
NOAREADEFS	:writeln(tty,'  NULL AREAS SECTION');
 
BADAREANAME	:begin
		 writeln(tty,'AREA "',WORD,'" UNDEFINED IN MODULE DEF''N');
		 ONEASSUME := true
		 end;
 
BADSYMBOL	:writeln(tty,'INVALID SYMBOL NAME IN CONTAINS CLAUSE');
 
BADINWD		:writeln(tty,'INVALID WORD IN "IN" PHRASE--RESIDENT ASSUMED');
 
BADSYMTYPE	:writeln(tty,'NEED "PROCEDURE", "VAR", "CONST", OR "FUNCTION"');
 
NOMODDEF	:writeln(tty,'NULL MODULES SECTION');
 
MULTSYM		:begin
		 writeln(tty,'SYMBOL MULTIPLY DEFINED IN CONTAINS CLAUSE ');
		 FIRSTUSED := true
		 end;
 
NOSYM		:writeln(tty,'NO SYMBOLS SECTION SEEN');
 
UNDEFMOD	:begin
		 write(tty,'INVALID OR UNDEFINED MODULE NAME "',WORD);
		 writeln(tty,'" IN SYMBOL DEFINITION')
		 end;
 
DISAGREE	:begin
		 write(tty,'SYMBOL "',SYMNAME,'" DEFINED DIFFERENTLY ');
		 writeln(tty,'IN MODULES AND SYMBOLS SECTIONS');
		 FIRSTUSED := true
		 end;
 
 
OVFLOW		:writeln(tty,'OVERFLOW OF DECLARED TOTAL STATIC STORAGE');
 
NOSTORAGE	:writeln(tty,'NO STORAGE SECTION SEEN');
 
NOSTOSIZ	:writeln(tty,' INVALID TOTAL STATIC SIZE--1000(OCTAL) ASSUMED');
 
BADSTOMOD	:writeln(tty,'INVALID MODULE NAME ',WORD, 'IN STORAGE SECTION');
 
UNDEFSTOMOD	:writeln(tty,'MODULE ',WORD,' IN STORAGE SECTION IS UNDEFINED');
 
STOSIZERR	:writeln(tty,'INVALID STORAGE SIZE--200 (OCTAL) ASSUMED');
 
MACHGARB	:writeln(tty,'GARBAGE FOR MACHINE IDENTIFIER');
 
NOMACHID	:writeln(tty,'NO MACHINE IDENTIFIER');
 
SYSGARB		:writeln(tty,'GARBAGE FOR SYSTEM IDENTIFIER');
 
NOSYSID		:writeln(tty,'NO SYSTEM IDENTIFIER');
 
BADEND		:writeln(tty,'NO END IN SIGHT')
	end;
if FIRSTUSED then writeln(tty,'FIRST APPEARANCE USED');
if ONEASSUME then writeln(tty,' NUMBER ONE ASSUMED')
end;
 
 
 
procedure EAT_UP(STOPPERS : TOKENS);   (*THIS ONE CHECKS AND FLAGS JUNK AT END*)
begin
if not (TOKEN in STOPPERS) then
     MDLERR(GARBAGE, STOPPERS)
end;
 
 
public procedure SETUP(FILENAME : FILENM;  var NOERRORS : boolean);
begin
reset(MDLFILE, '.MDL ' || FILENAME);
CURRLINE := ' ';
NOERRORS := not eof(MDLFILE)
end;
 
 
 
 
 
$PAGE
public procedure MDLPROC(var ERRETURN : boolean;  var STATSIZE, RESSTATORG : INTEGER;
		  var FIRSTMOD, FIRSTMAINTV,
		      FIRSTAREA, SYMTABL : SYMPTR);
 
var	MACHTYPE, SYSTYPE : SYMBOLIC;
 
 
 
 
function CCLAUSE(STOPPERS : TOKENS;  VAR CCLIST : LIST) : boolean;
(*
	<cclause> ::= CONTAINS <idlist>
	<idlist> ::=  <id> [ <comma> <idlist>]
								*)
var	CCEND, CCTEMP : LIST;
begin
CCLAUSE := false;  CCLIST := nil;  CCEND := nil;
if TOKEN = CONTAINS then
     repeat
     GETTOKEN;
     CCLAUSE := true;
     if TOKEN <> LEGALSYM then
	  MDLERR(BADSYMBOL, STOPPERS + [COMMA])
     else begin
	  LOOKUP(SYMTABL, WORD, SYMBOL, FOUND, VALTEMP, TEMPTR);
	  IF FOUND THEN
	       MDLERR(MULTSYM, ALLSYM)
	  ELSE BEGIN
               new(CCTEMP);
               CCTEMP^.NEXTELL := NIL;
	       CCTEMP^.NAME := WORD;
               if CCLIST = nil then CCLIST := CCTEMP
               else CCEND^.NEXTELL := CCTEMP;
               CCEND := CCTEMP;
               end;
          GETTOKEN
	  END
     until TOKEN <> COMMA;
EAT_UP(STOPPERS)
end;
 
 
$page
function INCLAUSE(STOPPERS : TOKENS;  VAR MODPTR : SYMPTR) : boolean;
(*
	<inclause> ::= IN { <id> | RESIDENT }
									*)
begin
INCLAUSE := false;
if TOKEN = INSYM then
     begin
     INCLAUSE := true;
     GETTOKEN;
     MODPTR := nil;
     if TOKEN <> RESIDENT then
	  begin
	  if TOKEN <> LEGALSYM then
	       MDLERR(BADINWD, STOPPERS)
	  else begin
	       LOOKUP(SYMTABL, WORD, OVMODULE, FOUND, VALTEMP, MODPTR);
	       if not FOUND then
		    begin
		    MODPTR := nil;
		    MDLERR(UNDEFMOD, STOPPERS)
		    end;
	       GETTOKEN
	       end
	  end
     else GETTOKEN
     end;
EAT_UP(STOPPERS)
end;
 
 
 
 
 
 
function AREADEF(STOPPERS : TOKENS;  var SIZEMOD : INTEGER;  var NAME : SYMBOLIC)
				: boolean;
(*
	<areadef> ::= <id> [SIZE] <num>
							*)
begin
AREADEF := false;
if TOKEN = LEGALSYM then
     begin
     AREADEF := true;
     NAME := WORD;
     GETTOKEN;
     if TOKEN = SIZE then GETTOKEN;
     if TOKEN <> NUMERIC then
	  begin
	  MDLERR(SIZERR, STOPPERS);
	  SIZEMOD := 1000B
	  end
     else begin
	  SIZEMOD := NUMVAL;
	  GETTOKEN
	  end
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
function STORDEF(STOPPERS : TOKENS;  var NAME : SYMBOLIC;  var SIZEMOD : INTEGER)
				: boolean;
(*
	<stordef> ::= { FILLER | <id> } [SIZE] <num>
							*)
begin
STORDEF := false;
if TOKEN in [LEGALSYM, FILLER] then
     begin
     STORDEF := true;
     if TOKEN = FILLER then
	  NAME := ''
     else NAME := WORD;
     GETTOKEN;
     if TOKEN = SIZE then GETTOKEN;
     if TOKEN <> NUMERIC then
	  begin
	  MDLERR(STOSIZERR, STOPPERS);
	  SIZEMOD := 200B
	  end
     else begin
	  SIZEMOD := NUMVAL;
	  GETTOKEN
	  end
     end;
EAT_UP(STOPPERS)
end; 
 
 
$page
function MODDEF(STOPPERS : TOKENS;  var NAME : SYMBOLIC;
	        var AREAPTR : SYMPTR;  var CCLIST : LIST) : boolean;
(*
	<moddef> ::= { FILLER | <id> [IN] <id> [<cclause>] }
								*)
begin
MODDEF := false;
if TOKEN in [FILLER, LEGALSYM] then
     begin
     MODDEF := true;
     if TOKEN = FILLER then
	  begin
	  GETTOKEN;
	  NAME := ''
	  end
     else begin
	  NAME := WORD;
	  GETTOKEN;
	  if TOKEN = INSYM then GETTOKEN;
	  if TOKEN <> LEGALSYM then
	       begin
	       MDLERR(MISAREANAME, STOPPERS + [CONTAINS]);
	       AREAPTR := nil
	       end
	  else begin
	       LOOKUP(SYMTABL, WORD, AREA, FOUND, VALTEMP, TEMPTR);
	       if not FOUND then
		    begin
		    MDLERR(BADAREANAME, STOPPERS + [CONTAINS]);
		    AREAPTR := nil
		    end
	       else AREAPTR := TEMPTR;
	       GETTOKEN
	       end;
	  if not CCLAUSE(STOPPERS, CCLIST) then CCLIST := nil
	  end
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
function SYMDEF(STOPPERS : TOKENS;  var NAME : SYMBOLIC; var RESFILLER: boolean;
		var TYPEOF : TOKENTYPE;  var MODPTR : SYMPTR) : boolean;
(*
<symdef> ::= { FILLER | <id> {PROCEDURE|FUNCTION|VAR|CONST} [<inclause>] }
									   *)
begin
SYMDEF := false; RESFILLER := false;
if TOKEN in [FILLER, LEGALSYM] then
     begin
     SYMDEF := true;
     if TOKEN = FILLER then
	  begin
	  NAME := '';
	  GETTOKEN
	  end
     else begin
	  NAME := WORD;
	  GETTOKEN;
	  if TOKEN in [PROCSYM, FUNCTSYM, VARSYM, CONSYM] then
	       begin
	       TYPEOF := TOKEN;
	       GETTOKEN
	       end
	  else begin
	       MDLERR(BADSYMTYPE, STOPPERS + [INSYM]);
	       TYPEOF := PROCSYM
	       end
	  end;
     if not INCLAUSE(STOPPERS, MODPTR) then
	  MODPTR := nil
     else RESFILLER :=   MODPTR = nil;
     end;
EAT_UP(STOPPERS)
end;
$PAGE
function MACHSECT(STOPPERS : TOKENS;  var WORDIN : SYMBOLIC) : boolean;
(*
	<machsect> ::= MACHINE <id>
					*)
begin
MACHSECT := false;
if TOKEN = MACHINE then
     begin
     MACHSECT := true;
     GETTOKEN;
     if TOKEN <> LEGALSYM then
	  MDLERR(MACHGARB, STOPPERS + [LEGALSYM]);
     if TOKEN = LEGALSYM then
	  begin
	  WORDIN := WORD;
	  GETTOKEN
	  end
     else MDLERR(NOMACHID, STOPPERS)
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
function SYSSECT(STOPPERS : TOKENS;  var WORDIN : SYMBOLIC) : boolean;
(*
	<syssect> ::= SYSTEM <id>
					*)
begin
SYSSECT := false;
if TOKEN = SYSTEM then
     begin
     GETTOKEN;
     SYSSECT := true;
     if TOKEN <> LEGALSYM then
	  MDLERR(SYSGARB, STOPPERS + [LEGALSYM]);
     if TOKEN = LEGALSYM then
	  begin
	  WORDIN := WORD;
	  GETTOKEN
	  end
     else MDLERR(NOSYSID, STOPPERS)
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
function AREASECT(STOPPERS : TOKENS) : boolean;
(*
	<areasect> ::= AREAS <areadeflist>
	<areadeflist> ::= <areadef> [<areadeflist>]
							*)
var	SIZEMOD, AREAINDEX : INTEGER;
begin
AREASECT := false;   AREAINDEX := 1;
if TOKEN = AREAS then
     begin
     AREASECT := true;
     GETTOKEN;
     FOUNDONE := false;
     while not (TOKEN in STOPPERS) do
	  begin
	  if AREADEF(STOPPERS + [LEGALSYM], SIZEMOD, MODNAME) then
	       begin
	       FOUNDONE := true;
	       LOOKUP(SYMTABL, MODNAME, AREA, FOUND, VALTEMP, TEMPTR);
	       if FOUND then
		    MDLERR(MULTDEFAREA, ALLSYM)
	       else begin
		    PUT_ENT(FIRSTAREA, LASTAREA, AREA, AREAINDEX,
			    MODNAME, TEMPTR);
		    if SYMTABL = nil then SYMTABL := TEMPTR;
		    TEMPTR^.SIZE := SIZEMOD;
		    AREAINDEX := AREAINDEX + 1
		    end
	       end
	  end;
     if not FOUNDONE then
	  MDLERR(NOAREADEFS, STOPPERS)
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
function MODSECT(STOPPERS : TOKENS) : boolean;
(*
	<modsect> ::= MODULES <moddeflist>
	<moddeflist> ::= <moddef> [<moddeflist>]
							*)
var	MODINDEX: INTEGER;
	CCLIST, CCTEMP : LIST;
	ARPTR: SYMPTR;
begin
MODSECT := false;   MODINDEX := 1; CCLIST := NIL;
if TOKEN = MODULES then
     begin
     MODSECT := true;
     GETTOKEN;
     FOUNDONE := false;
     while not (TOKEN in STOPPERS) do
	  begin
	  FOUNDONE := true;
	  if MODDEF(STOPPERS + [LEGALSYM, FILLER], MODNAME,
		    ARPTR, CCLIST) then
	       begin
	       if MODNAME = 'n
		    MODINDEX := MODINDEX + 1
	       else begin
		    LOOKUP(SYMTABL, MODNAME, OVMODULE, FOUND, VALTEMP, MODPTR);
		    if FOUND then
			 MDLERR(MULTDEFMOD, STOPPERS + [LEGALSYM, FILLER])
		    else begin
			 PUT_ENT(FIRSTMOD, LASTMOD, OVMODULE, MODINDEX,
				 MODNAME, MODPTR);
			 MODINDEX := MODINDEX + 1;
			 MODPTR^.MYAREA := ARPTR
			 end;
		    while CCLIST <> nil do
			 begin
			 LOOKUP(SYMTABL, CCLIST^.NAME, SYMBOL, FOUND,
				VALTEMP, TEMPTR);
			 PUT_ENT(MODPTR^.FIRSTSYM, MODPTR^.LASTSYM,
				      SYMBOL, 0, CCLIST^.NAME, TEMPTR);
			 TEMPTR^.MYMODULE := MODPTR;
			 CCTEMP := CCLIST^.NEXTELL;
			 DISPOSE(CCLIST);
			 CCLIST := CCTEMP
			 end
		    end
	       end
	  end;
     if not FOUNDONE then MDLERR(NOMODDEF, STOPPERS)
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
function SYMSECT(STOPPERS : TOKENS) : boolean;
(*
	<symsect> ::= SYMBOLS <symdeflist>
	<symdeflist> ::= <symdef> [<symdeflist>]
							*)
var	TVCOUNT : INTEGER;
	RESFILLER: boolean;
	FIRSTRESSYM, LASTRESSYM: SYMPTR; (* DUMMIES *)
begin
SYMSECT := false;  TVCOUNT := 400012B;
if TOKEN = SYMBOLS then
     begin
     SYMSECT := true;
     GETTOKEN;
     FOUNDONE := false;
     while not (TOKEN in STOPPERS) do
	  begin
	  if SYMDEF(STOPPERS + [LEGALSYM, FILLER], SYMNAME, RESFILLER,
		    TYPEOF, MODPTR) then
	       begin
	       FOUNDONE := true; FIRSTRESSYM := nil; LASTRESSYM := nil;
	       if SYMNAME = '' then 
		    if RESFILLER then
			TVCOUNT := TVCOUNT + 1
		    else TVCOUNT := TVCOUNT + 2
	       else begin
		    LOOKUP(SYMTABL, SYMNAME, SYMBOL, FOUND, VALTEMP, TEMPTR);
		    if FOUND then
			 begin
			 if (TEMPTR^.MYMODULE = nil) or (MODPTR = nil) then
			      MDLERR(INEXPECT, STOPPERS + [LEGALSYM, FILLER])
			 else if MODPTR <> TEMPTR^.MYMODULE then
				   MDLERR(DISAGREE,STOPPERS+[LEGALSYM,FILLER])
			 end
		    else begin
			 if MODPTR = nil then
			   begin
			     PUT_ENT(FIRSTRESSYM, LASTRESSYM, SYMBOL, TVCOUNT,
				     SYMNAME, TEMPTR);
			     TEMPTR^.NEXTMODSYM := nil
			   end
			 else
			   begin
			     PUT_ENT (MODPTR^.FIRSTSYM, MODPTR^.LASTSYM,
				SYMBOL, TVCOUNT, SYMNAME, TEMPTR);
			     TEMPTR^.MYMODULE := MODPTR
			   end
			 end;
			 if FIRSTMAINTV = nil then FIRSTMAINTV := TEMPTR
			 else LASTMAINTV^.NEXTMAINSYM := TEMPTR;
			 TEMPTR^.TVLOC := TVCOUNT;
			 if MODPTR = nil then TVCOUNT := TVCOUNT + 1
			 else TVCOUNT := TVCOUNT + 2;
			 LASTMAINTV := TEMPTR;
		    TEMPTR^.MYKIND := TYPEOF
		    end
	       end
	  end
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
function STORSECT(STOPPERS : TOKENS;  var STATSIZE : INTEGER) : boolean;
(*
	<storsect> ::= STORAGE [SIZE] <num> <stordeflist>
	<stordeflist> ::= <stordef> [<stordeflist>]
							*)
var	MODSIZE : INTEGER;
begin
STORSECT := false;
if TOKEN = STORAGE then
     begin
     GETTOKEN;
     STORSECT := true;
     if TOKEN = SIZE then GETTOKEN;
     if TOKEN <> NUMERIC then
	  begin
	  MDLERR(NOSTOSIZ, STOPPERS + [LEGALSYM]);
	  STATSIZE := 1000B
	  end
     else begin
	  STATSIZE := NUMVAL;
	  GETTOKEN
	  end;
     FOUNDONE := false;
     STORKEEP := 140B;
     while STORDEF(STOPPERS + [LEGALSYM, FILLER], MODNAME, MODSIZE) do
	  begin
	  FOUNDONE := true;    (*FILLER*)
	  if MODNAME = '' then STORKEEP := STORKEEP + MODSIZE
          else begin
               LOOKUP(SYMTABL, MODNAME, OVMODULE, FOUND, VALTEMP, TEMPTR);
               if not FOUND then
                    MDLERR(BADSTOMOD, ALLSYM)
               else begin
                    TEMPTR^.STATICSIZE := MODSIZE;
		    TEMPTR^.STATORIG := STORKEEP;
                    STORKEEP := STORKEEP + MODSIZE
		    end
               end;
               if STORKEEP > STATSIZE + 140B then MDLERR(OVFLOW, STOPPERS)
          end
     end;
EAT_UP(STOPPERS)
end;
 
 
$page
(*
		A FEW FINAL DEFINITIONS
 
	<mdldesc> ::= [<machsect>] [<syssect>] <areasect> <modsect>
		      <symsect> <storsect> END
 
	<id> ::= <<any valid PASCAL identifier of max length six characters>>
 
	<num> ::= <<a valid PASCAL integer constant, B for octal is ok >>
 
	<comma> ::=  ,
 
							*)
 
begin    (*MAINLINE FOR PROCEDURE MDLPROC*)
CURRCOUNT := 1;   LINENUM := 0;
CURRLINE := ' ';  LINESIZE := 1;
NOERRORS := true;  SYMTABL := nil;
LASTMAINTV := nil;
LASTAREA := nil;      LASTMOD := nil;
GETTOKEN;
STOPPERS := [SYSTEM, AREAS, MODULES, SYMBOLS, STORAGE, ENDSYM, NOMOR];
if not MACHSECT(STOPPERS, MACHTYPE)  then
	MACHTYPE := BLANKS;
STOPPERS := STOPPERS - [SYSTEM];
if not SYSSECT(STOPPERS, SYSTYPE) then
	SYSTYPE := BLANKS;
STOPPERS := STOPPERS - [AREAS];
if not AREASECT(STOPPERS) then
     MDLERR(NOAREA, STOPPERS);
STOPPERS := STOPPERS - [MODULES];
if not MODSECT(STOPPERS) then
     MDLERR(NOMOD, STOPPERS);
STOPPERS := STOPPERS - [SYMBOLS];
if not SYMSECT(STOPPERS) then
     MDLERR(NOSYM, STOPPERS);
STOPPERS := STOPPERS - [STORAGE];
if not STORSECT(STOPPERS, STATSIZE) then 
     MDLERR(NOSTORAGE, STOPPERS);
if TOKEN <> ENDSYM then MDLERR(BADEND, STOPPERS);
ENDINORDERWALK(SYMTABL, NOERRORS);
 
(*$X  writeln(tty);
     writeln(tty,'WALK of AREA LIST');   *)
     RESSTATORG := (STATSIZE + 1137B) DIV 1000B * 1000B;
     NEWPTR := FIRSTAREA;
     while NEWPTR <> nil do
	  begin
          NEWPTR^.ORIGIN := RESSTATORG;
          RESSTATORG := (RESSTATORG + NEWPTR^.SIZE + 777B) DIV 1000B * 1000B;
   (*$X   with NEWPTR^ do
	       writeln(tty,SYMNAME,SIZE:6, ORIGIN:9, INDEX:4);   *)
	  NEWPTR := NEWPTR^.NEXTAREA
	  end;
     NEWPTR := FIRSTMOD;   STORKEEP := 140B;
(*$X writeln(tty);
     writeln(tty,'WALK of MODULE LIST');     *)
     while  NEWPTR <> nil do
	       begin
	       if NEWPTR^.STATICSIZE = 0 then
		    NEWPTR^.STATORIG := STORKEEP
	       else STORKEEP := NEWPTR^.STATORIG;
	(*$X   writeln(tty);
	       with NEWPTR^ do
	       writeln(tty,SYMNAME,STATORIG:7,STATICSIZE:7,MODNUMBER:3,AREANUM:4);
	       TEMPTR := NEWPTR^.FIRSTSYM;
	       writeln(tty,'         with SYMBOLS');
	       while TEMPTR <> nil do
		    begin
		    with TEMPTR^ do
		         writeln(tty, SYMNAME, MYMODNUM:4, SYMNUM:4);
		    TEMPTR := TEMPTR^.NEXTMODSYM
		    end;     *)
	       NEWPTR := NEWPTR^.NEXTMOD
	       end;
(*$X writeln(tty);
     writeln(tty,'WALK of MASTER TRANSFER VECTOR LIST');
     NEWPTR := FIRSTMAINTV;
     while NEWPTR <> nil do
	  begin
	  with NEWPTR^ do
	       writeln(tty, SYMNAME, MYMODNUM:4, SYMNUM:4);
	  NEWPTR := NEWPTR^.NEXTMAINSYM
	  end;
				*)

ERRETURN := NOERRORS;
if NOERRORS then writeln(tty,'NO ERRORS DETECTED in MDL DESCRIPTION')
	    else writeln(tty,' UNSUCCESSFUL MDL PROCESSING')
end.
 Q@n÷