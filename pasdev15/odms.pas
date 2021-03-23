$LENGTH 44
(*$D-,M+*)
(*   +--------------------------------------------------------------+
     I                                                              I
     I                          O D M S                             I
     I                          - - - -                             I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 20-Jun-77

     PURPOSE: Main program for updating and constructing modules  for
        database memory management.

     USAGE:
        RU ODMS[52250,247]

     INPUT: 

                   Enter commands interactively from the terminal

     REQUIREMENTS: The  first  command  to  ODMS must be a USE with a
        valid memory description of the program to be overlaid.

     EFFECTS: Will    generate     command,     symbol,     database,
        relocatable,  and save files for the program.

     RESPONSIBLE: Jerry Rosen

     CHANGES: NONE.

     ---------------------------------------------------------------- *)
 
 
$INCLUDE USETYP.INC
 
$INCLUDE T2TYPE.INC

$INCLUDE rlb:PRGDIR.INC
 

type
	EXT = packed array[1..3] of char;
	BIGSTRING = packed array[1..256] of char;
	CMDERRTYPE = (USEFILERR, UPERR, WHICHONE, NO_MOD, NOT_HERE,NODB);
	SIXCHAR = packed array[1..6] of char;
	SETOFCMD = set of CMDTYP;
 
 
var	FIRSTMAINTV, MODPTR, FIRSTMOD,
	FIRSTAREA : SYMPTR;
	SYMTABL: SYMPTR := nil;
	THISFILE : FILENAME;
	STATSIZE, MODNUM, VERNUM, I, RESSTATORG, INPUTVAL : INTEGER;
	MDLFILE: EXT := 'MDL';
	BLDFILE: EXT := 'SYM';
	CMDFILE: EXT := 'CMD';
	MODFILE: EXT := 'MOD';
	OVLFILE: EXT := 'OVL';
	DBFILE : EXT := 'ODB';
	FLAG : boolean;
	MODNAME, PGMNAME : SYMBOLIC;
	CMD : CMDREC;
	LEGALCMDS: SETOFCMD := [USE, EXITWD, ILLEGAL];
	NEXTCMD: array[CMDTYP, boolean] of SETOFCMD :=
	  ( ( [USE, EXITWD, ILLEGAL],
	      [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
		PRINT, PACK, EXITWD, ILLEGAL] ) ,

	  ( [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ,
	    [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ) ,
	  ( [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ,
	    [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ) ,
	  ( [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ,
	    [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ) ,
	  ( [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ,
	    [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ) ,
	  ( [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ,
	    [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ) ,
	  ( [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ,
	    [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ) ,
	  ( [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ,
	    [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
	     PRINT, PACK, EXITWD, ILLEGAL] ) ,
	  (  []   ,   []   ),  		(* EXITWD *)
	  (  []   ,   []   ) );		(* ILLEGAL *)


	MODSTAT, AREASIZE : INTEGER;
	BLANKS : SYMBOLIC := '      ';
	CMDFL : text;
	TEMPTR : SYMPTR;



 
(*
initprocedure;
begin
BLANKS := '      ';   SYMTABL := nil;
MDLFILE := 'MDL';  BLDFILE := 'SYM';  CMDFILE := 'CMD';
MODFILE := 'MOD';  OVLFILE := 'OVL';  DBFILE :=  'ODB';
LEGALCMDS := [USE, EXITWD, ILLEGAL];
 
NEXTCMD[USE, FALSE] := [USE, EXITWD, ILLEGAL];
NEXTCMD[USE, TRUE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE, PRINT,
		      PACK, EXITWD, ILLEGAL];
NEXTCMD[BUILD, FALSE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			  PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[BUILD, TRUE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE, PRINT,
			 PACK, EXITWD, ILLEGAL];
NEXTCMD[CONVERT, FALSE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			    PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[CONVERT, TRUE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE, PRINT,
			   PACK, EXITWD, ILLEGAL];
NEXTCMD[UPDATEWD, FALSE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE, PRINT,
			   PACK, EXITWD, ILLEGAL];
NEXTCMD[UPDATEWD, TRUE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			  PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[DELETE, FALSE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			   PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[DELETE, TRUE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			  PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[PRINT, FALSE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			  PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[PRINT, TRUE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE, 
			 PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[PACK, FALSE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			 PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[PACK, TRUE] := [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[CHANGE, TRUE]:= [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[CHANGE,FALSE]:= [USE, CHANGE, BUILD, CONVERT, UPDATEWD, DELETE,
			PRINT, PACK, EXITWD, ILLEGAL];
NEXTCMD[EXITWD, TRUE] := [];
NEXTCMD[EXITWD, FALSE] := []
end;
*)
 
 

$page
external procedure MODBLD(MODVAR, MAIN : SYMPTR);  
 
external procedure RESBLD(MAIN, AREAS, MODULES : SYMPTR;
		 STATSIZE : INTEGER;
		 DBNAME : FILENAME);  
 
external procedure BLDINIT(FNAME : SYMBOLIC);  

external procedure VERCHN (OLD, NEW, OVMODULE: INTEGER; FN: FILENAME;
			   var AOK: boolean);
 
external procedure BLDCLS;  
 
external procedure UPINIT(FNAME, DBNAME : FILENAME;
		 var ERROR : boolean);   
 
external procedure UPDATE(MODNUM, VERNUM, OVSIZE, STATSIZE : INTEGER;
		 var ERROR : boolean);  
 
external procedure LOOKUP(    SYMTABL : SYMPTR;  WORD : SYMBOLIC;
		     KIND : KINDOFREC;  var FOUND : BOOLEAN;
		 var VALTEMP : INTEGER;  var WHERE : SYMPTR);  
 
external procedure MDLPROC(var ERROR : boolean;  var STATSIZE, RESSTATORG : INTEGER;
		  var FIRSTMOD, MAINTV, AREA, SYMTAB : SYMPTR);  
 
external procedure GETCMD(var CMD : CMDREC);  
 
external procedure SETUP(FNAA : FILENAME;  var NOERRORS : boolean);  
 
external procedure DELMOD(MODNUM, VERNUM : INTEGER;  var SUCCESS : boolean;
		 FILNAM : FILENAME);   
 
external procedure MODMAK(MODFILE: FILENAME; CIMAGE: SIXCHAR);
 
external procedure PRVERS(MODNUM : INTEGER;  DBNAME : FILENAME);  
 
external procedure PACKDB(FILNAM : FILENAME;  LASTMODINDEX : INTEGER);  
$PAGE
function FCHECK (FN: FILENAME; EXTN: EXT): FILENAME;
  var	BRACKET: INTEGER;

  begin
    FCHECK := '';
    if index (FN,'.') = 0 then
      begin
	BRACKET := index(FN,'[');
	if BRACKET = 0 then
	  begin
	    FCHECK := FN;
	    while substr(FCHECK,length(FCHECK),1) = ' ' do
	      FCHECK := substr(FCHECK,1,length(FCHECK)-1);
	    FCHECK := FCHECK || '.' || EXTN || ' '
	  end
	else
	  begin
	    FCHECK := substr (FN,1,(BRACKET-1) );
	    FCHECK := FCHECK || '.' || EXTN || substr(FN,BRACKET)
	  end
      end
    else
      FCHECK := FN
  end;



procedure PURGE(THIS : SYMPTR);
var	TEMP : SYMPTR;
begin
if THIS <> nil then
     begin
     PURGE(THIS^.LEFTKIN);
     TEMP := THIS^.RIGHTKIN;
     DISPOSE(THIS);
     PURGE(TEMP)
     end
end;
 
 
procedure CMDBLD(FNM : SYMBOLIC;  LOW, HIGH : INTEGER;
		 ISITRES : boolean;  LINK : BIGSTRING);
var	FNAME : FILENAME;
begin
rewrite (CMDFL, FCHECK (FNM, CMDFILE) );
writeln(CMDFL,'/SET:.LOW.:', LOW:6:O, ', /SET:.HIGH.:', HIGH:6:O);
write (CMDFL, FNM);
writeln(CMDFL, '.SRL');
if ISITRES then
     writeln(CMDFL, 'NEWOVL.REL' || PRGM_DIR );
writeln(CMDFL, LINK);
if ISITRES then
		writeln(CMDFL, FNM, '/SSAVE')
	   else begin
		writeln(CMDFL, '/START:', LOW:6:O);
		writeln(CMDFL, FNM, '/SAVE')
		end;
writeln(CMDFL, '/G');
close(CMDFL)
end;
 
procedure MODLST(FIRSTMOD : SYMPTR;  FILEN:SYMBOLIC);
var	LINECT, MODINDEX : INTEGER;
	TEMPMOD: SYMPTR;
 
     procedure WRTMDN(var  FIL : text;  NUMBER : INTEGER;  NAME : SYMBOLIC);
     begin
     if LINECT < 6 then
	  begin
	  if NUMBER <> 0 then
	       write(FIL, ',' , 'FIL', NUMBER:3:O )
	  else write(FIL, ',' , NAME );
	  LINECT := LINECT + 1
	  end
     else begin
	  if NUMBER <> 0 then
	       writeln(FIL, ',' ,'FIL', NUMBER:3:O )
	  else writeln(FIL, ',' , NAME );
	  write(FIL,'                    ');
	  LINECT := 1
	  end
     end;
 
begin
LINECT := 1;  MODINDEX := 1;
rewrite(CMDFL, '.MOD ' || FILEN);
write(CMDFL, 'type  MODULETYPE = (NO_MOD');
TEMPMOD := FIRSTMOD;
while TEMPMOD <> nil do
     begin
     while MODINDEX < TEMPMOD^.MODNUMBER do
	  begin
	  WRTMDN(CMDFL, MODINDEX, '      ');
	  MODINDEX := MODINDEX + 1
	  end;
     WRTMDN(CMDFL, 0, TEMPMOD^.SYMNAME);
     TEMPMOD := TEMPMOD^.NEXTMOD;
     MODINDEX := MODINDEX + 1
     end;
writeln(CMDFL, ');  ');
close(CMDFL)
end;
 
 

procedure CMDERR(WHICH : CMDERRTYPE);
begin
case WHICH of

USEFILERR	:writeln(tty,'NULL OR INVALID MDL FILE SPECIFIED');
 
UPERR		:writeln(tty,'INVALID OR NULL OVL FILE SPECIFIED');
 
WHICHONE	:writeln(tty,'MULTIPLE SWITCHES OR FILENAMES SPECIFIED');
 
NO_MOD		:writeln(tty,'UNDEFINED MODULE  ',CMD.MODNAME);

NODB		:writeln(tty,'DATABASE SPEC NOT APPLICABLE');
 
NOT_HERE	:writeln(tty,'COMMAND NOT VALID AT THIS TIME')
	end
end;
 
 
procedure PROCESS(var NOERRORS : boolean);
 
procedure SEMERR;
begin
writeln(tty,' SEMERR');
NOERRORS := false
end;
 
 
begin
NOERRORS := true;

case CMD.COMMAND of
 
USE		:begin
                 SETUP(FCHECK (CMD.FNAME2, MDLFILE), NOERRORS);
		 if NOERRORS then
		      begin
		      PURGE(SYMTABL);  SYMTABL := nil;
		      FIRSTMAINTV := nil;
		      FIRSTMOD := nil;
		      FIRSTAREA := nil;
		      MDLPROC(NOERRORS, STATSIZE, RESSTATORG,
			      FIRSTMOD, FIRSTMAINTV,
			      FIRSTAREA, SYMTABL);
		      if NOERRORS then
			   begin
			     I := search (CMD.FNAME2, [' ','[','.'],
					  length(CMD.FNAME2) + 1);
			     PGMNAME := substr (CMD.FNAME2,1,I-1);
			     MODLST (FIRSTMOD, PGMNAME)
			   end
		      end
		 else CMDERR(USEFILERR)
		 end;
 
BUILD		:if (DBARG in CMD.FOUND) and
		not (RESSW in CMD.FOUND) then CMDERR(NODB) else
		 begin
		 if RESSW in CMD.FOUND then
		      begin
		      BLDINIT(PGMNAME);
			if not (DBARG in CMD.FOUND) then
			  CMD.FNAME1 := PGMNAME;
		      RESBLD(FIRSTMAINTV, FIRSTAREA, FIRSTMOD,
			     STATSIZE, FCHECK (CMD.FNAME1, DBFILE));
		      CMDBLD(PGMNAME, RESSTATORG, 400000B, TRUE, CMD.LNKTXT)
		      end
		 else begin
		      BLDINIT(CMD.MODNAME);
		      MODBLD(MODPTR, FIRSTMAINTV);
		      CMDBLD(CMD.MODNAME, MODPTR^.STATORIG, 
			     MODPTR^.MYAREA^.ORIGIN, FALSE, CMD.LNKTXT)
		      end;
		 BLDCLS
		 end;
 
UPDATEWD		:begin
		 if CMD.FNAME1 = '' then
		      CMD.FNAME1 := PGMNAME;
		 if CMD.FNAME2 = '' then
		      begin
		      MODMAK(CMD.MODNAME, CMD.MODNAME);
		      CMD.FNAME2 := CMD.MODNAME;
		      end;
		 NOERRORS := false;
		 UPINIT ( FCHECK (CMD.FNAME2,OVLFILE) || ' ',
			  FCHECK (CMD.FNAME1,DBFILE) || ' ',
			  NOERRORS );
		 if NOERRORS then CMDERR(UPERR)
		 else begin
		      UPDATE(MODPTR^.MODNUMBER, CMD.INTARG1, MODPTR^.MYAREA^.SIZE,
			     MODPTR^.STATICSIZE, FLAG)
		      end
		 end;
 
CONVERT		:begin
		 if [TOARG, FNAME] <= CMD.FOUND then
		      CMDERR(WHICHONE)
		 else begin
		      if TOWD in CMD.FOUND then
			   MODMAK(FCHECK(CMD.FNAME1,OVLFILE),CMD.MODNAME)
		      else
			begin
			  if CMD.FNAME2 = '' then
			     CMD.FNAME2 := CMD.MODNAME;
			  MODMAK(FCHECK(CMD.FNAME2,OVLFILE), CMD.MODNAME)
			end
		      end
		 end;


CHANGE		:begin
		  if CMD.FNAME1 = '' then
		     CMD.FNAME1 := PGMNAME;
		  if not (VER in CMD.FOUND) then
		     CMD.INTARG1 := 0;
		  VERCHN (CMD.INTARG1, CMD.INTARG2,
			MODPTR^.MODNUMBER,
			FCHECK (CMD.FNAME1, DBFILE), NOERRORS)
		 end;


DELETE		:begin
		 if [FROMARG, FNAME] <= CMD.FOUND then
		      CMDERR(WHICHONE)
		 else begin
		      NOERRORS := true;
		      if ALLSW in CMD.FOUND then
			   CMD.INTARG1 := -1;
		      if CMD.FNAME2 = '' then
			   CMD.FNAME2 := PGMNAME;
		      DELMOD(MODPTR^.MODNUMBER, CMD.INTARG1,
			     NOERRORS,FCHECK(CMD.FNAME2,DBFILE))
		      end
		 end;
 
PRINT		:begin
		 if [VERSW, SYMSW] <= CMD.FOUND then
		      CMDERR(WHICHONE)
		 else begin
		      if not (SYMSW in CMD.FOUND) then
			   begin
			   if CMD.FNAME2 = '' then
				CMD.FNAME2 := PGMNAME;
			   PRVERS(MODPTR^.MODNUMBER, FCHECK(CMD.FNAME2,DBFILE))
			   end;
		      if not(VERSW in CMD.FOUND) then
			   begin
			   TEMPTR := MODPTR^.FIRSTSYM;
			   while TEMPTR <> nil do
				begin
				with TEMPTR^ do
				     writeln(tty,' SYMBOL ', SYMNAME,
				     '  ON MTV AT ', TVLOC:6:O);
				TEMPTR := TEMPTR^.NEXTMODSYM
				end
			   end
		      end
		 end;
 
 
PACK		:begin
		 if not (FNAME in CMD.FOUND) then
		      CMD.FNAME2 := PGMNAME;
		 TEMPTR := FIRSTMOD;
		 while TEMPTR^.NEXTMOD <> nil do
		      TEMPTR := TEMPTR^.NEXTMOD;
		 PACKDB(FCHECK(CMD.FNAME2,DBFILE),TEMPTR^.MODNUMBER)
		 end;
 
ILLEGAL		:writeln(tty,'HUH?')
 
	end
end;
 
 
procedure SEM_ERR;
begin
writeln(tty,'ERROR IN MAIN')
end;
 
 
begin   (*MAINLINE COMMAND PROCESSOR*)
open(tty);    rewrite(ttyoutput);
writeln(tty,' ODMS, version 1.5(NC), ', compdate );
while LEGALCMDS <> [] do
     begin
     GETCMD(CMD);	FLAG:=False;
     if CMD.COMMAND in LEGALCMDS then
	  begin
	  if CMD.MODNAME <> BLANKS then
	       LOOKUP(SYMTABL, CMD.MODNAME, OVMODULE, FLAG, I, MODPTR)
	  else FLAG := true;
	  if not FLAG then CMDERR(NO_MOD)
	  else PROCESS(FLAG);
	  if CMD.COMMAND <> ILLEGAL then LEGALCMDS := NEXTCMD[CMD.COMMAND, FLAG]
	  end
     else CMDERR(NOT_HERE)
     end
end.
    