$OPTIONS SPECIAL

(*
-----------------------------------------------------------------------------

				   C O P R E N

-----------------------------------------------------------------------------

MDSI, COMPANY CONFIDENTIAL

STARTED OCTOBER 17, 1977

PURPOSE: TO PARSE AND EXECUTE A SUBSET OF THE 940 OP SYSTEM COMMANDS WHICH
	PERFORM MAGIC ON FILES.  THE FILE NAMES MUST BE CONVERTED TO THEIR
	PDP-10 EQUIVALENTS FIRST, BEFORE ANY TRICKIES.

USAGE:   PROCEDURE COPY(LINE: CMDLINE; IDX: CMDLINEIDX);
	LINE--LINE FROM WHICH THE COMMAND IMPERATIVE HAS BEEN PARSED
	IDX---POINTER TO THE END OF IMPERATIVE PARSING


	 PROCEUDRE RENAME(  SAME AS COPY  );


EFFECTS:  BOTH COPY AND RENAME MAKE SURE THAT THE ORIGINAL ('FROM' FILE,
	OR 'NEW' FILE) EXIST (IMPLYING THAT THEY ARE TRANSLATABLE INTO
	DEC-10 LINGO).  ON ANY PROMPT EXCEPT 'RETYPE', THE USER MAY ENTER
	THE REST OF HER LINE IN ONE FELL SWOOP.  ONCE WE HAVE TWO LEGITIMATE
	DEC-10 FILE NAMES, THE TENIO UUO'S ARE USED TO OPEN, COPY, CLOSE,
	AND RENAME AS NECESSARY.

RESPONSIBLE:  JERRY ROSEN

CHANGES: NONE
--------------------------------------------------------------------------*)
$PAGE
const
  BLI = 1;
  BLO = 3;
  ASNU = 5;
  DSNU = 6;
  UUO_MAXAC = 2;


$OPTIONS NOSOURCE
$INCLUDE DOUUO.INC[52250,237]
$INCLUDE EXCCHN.INC[52250,237]
$INCLUDE CMDUTL.TYP[52250,245]
$INCLUDE EXC940.INC[52250,237]
$INCLUDE QUERY.INC[52250,245]
$INCLUDE END940.INC[52250,237]
$OPTIONS SOURCE

external function GET940FN(var LINE: CMDLINE;
			   var IDX : CMDLINEIDX) : FILE_ID;




static var
  CURRIDX : CMDLINEIDX;                         (* COUNTER ALONG LINE BEING PARSED *)
  CURRLINE : CMDLINE;                           (* LINE BEING PARSED *)
  FROMFILE, TOFILE : FILE_ID;                   (* 940 FILE NAMES *)
  TOUNIT, FROMUNIT : FILE_UNIT;                 (* GETCHN UNITS ASSOCIATED WITH TWO FILES *)
  BUFR : packed array [1..128] of INTEGER;       (* FOR COPY *)
  EOFSW : BOOLEAN;                              (* COPY, TO REMEMBER EOF ON SOURCE *)
  FROM10, TO10 : FILE_ID;                       (* PDP-10 EQUIVALENT FILE NAMES *)
  TENCODE : FNRETTYPE;                          (* USED WITH FN940TO10 *)
  UCODE : UUO_RET;                              (* USED WITH DOUUO *)
  INBLK, OUTBLK : UUO_ACBLK;                    (* SET OF ACCUMS FOR EACH *)
$PAGE
procedure SETLINE(LINE: CMDLINE);

begin
  CURRLINE := LINE;
  CURRIDX := 1
end;
$PAGE
procedure READNEWLINE(PROMPT: CMDLINE);

begin
  WRITE(TTY, PROMPT||': ');
  BREAK(TTY);
  SETLINE(EDITREAD(''))
end;






function EATSPACE : BOOLEAN;

begin
  EATSPACE := (VERIFY (CURRLINE, [' ', ','] ) = 0);

  (* TRUE ON AN END-OF-LINE CONDITION *)

  if EATSPACE then
    CURRLINE := ''
  else CURRLINE := SUBSTR(CURRLINE, VERIFY(CURRLINE, [' ', ','] ));
  CURRIDX := 1
end;
$PAGE
procedure EATNOISE(NOISE: FILE_ID);

begin
  if (not EATSPACE)
    andif (LENGTH(CURRLINE) >= CURRIDX + LENGTH(NOISE) - 1)
    andif
    (UPPERCASE(SUBSTR(CURRLINE, CURRIDX, LENGTH(NOISE))) = UPPERCASE(NOISE))

  (* THIS CHECKS FOR 1: LINE IS LONG ENOUGH, 2: NOISE WORD IS THERE *)

  then
    CURRLINE := SUBSTR(CURRLINE, CURRIDX + LENGTH(NOISE))

(* CHOP THE NOISE OFF *)
(* ELSE NOTHING *)

end;
$PAGE
function GETFILE(PROMPT: CMDLINE) : FILE_ID;

begin
  GETFILE := ''; 
  repeat
    if not EATSPACE then
      begin
      if SEARCH(CURRLINE, [' ', ','] ) = 0 then
	begin                           (* TOKEN EXTENDS TO *)
	GETFILE := CURRLINE;            (* END OF LINE *)
	CURRLINE := ''
      end

      (* ALL THE WAY TO THE END OF THE LINE *)

      else
	begin
	GETFILE := SUBSTR(CURRLINE, CURRIDX,
		   SEARCH(CURRLINE, [' ', ','] ) - CURRIDX + 1);

	(* GET TOKEN UP TO BLANK *)

	CURRLINE := SUBSTR(CURRLINE, SEARCH(CURRLINE, [' ', ','] ))

      (* CHOP OFF THAT PART OF LINE *)

      end
    end
    else
      READNEWLINE(PROMPT);                (* NOTHING LEFT *)
  until GETFILE <> '';
  GETFILE := GETFILE || ' '                 (* TO MAKE CONVRT HAPPY *)
end;





function RETRY(PROMPT: CMDLINE): FILE_ID;

begin
  READNEWLINE(PROMPT);                      (* THROW OUT OLD LINE *)
  RETRY := GETFILE(PROMPT)                  (* AND LET GETFILE TAKE OVER *)
end;
$PAGE
public procedure COPY(INPUTLINE: CMDLINE; LINDX: CMDLINEIDX);

  var PROMPT: CMDLINE;                          (* FOR QUERY VERIFICATION *)

  label 1, 2;                                    (* NASTY, BUT LOGICAL *)

  procedure ERROR(WHY: CMDLINE);

  begin
    WRITELN(TTY, '*ERROR ON  ', WHY);
    goto 2                                    (* SHUT EVERYTHING DOWN *)
  end;



begin
  SETLINE(SUBSTR(INPUTLINE, LINDX));          (* CHOP OFF IMPERATIVE *)
  FROMFILE := GETFILE('FROM FILE');           (* PROMPT IF NECESSARY *)
  EATNOISE('TO');                             (* YOU NEVER KNOW *)
  TOFILE := GETFILE ('TO FILE');
  if not EATSPACE then ERROR('JUNK AT END OF LINE');

  (* LOOKS OK SO FAR, SO INITIALIZE A FEW NECESSARIES *)

  INBLK [1] := - 377777777777B + ORD(ADDRESS(FROM10));

  (* THIS TELLS ASNU TO OPEN FOR INPUT, WITH A WORD THAT LOOKS LIKE
  400000,,ADDRESS OF TEXT OF FILENAME + 1 WORD (TO SKIP LENGTH) *)

  INBLK [2] := 0;                              (* USE YOUR OWN BUFFER, ASNU *)
  GETCHN(TOUNIT);
  GETCHN(FROMUNIT);
$PAGE

  (* CHECK TO SEE IF 940 FILE NAMES MAKE SENSE *)

  (* FIRST, THE FROM FILE *)

  loop
    FROM10 := '';
    INBLK [2] := 0;
    FN940TO10(FROMFILE, FROM10, TENCODE);    (* TRANSLATE *)
    FROM10 := FROM10 || CHR(0);             (* NULL FOR ASNU *)
  exit if (TENCODE = ISFN)                (* TRANSLATION OK *)
    andif
    (DOUUO(ASNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(FROMUNIT))) = SKIP1);

    (* AND IF THE FILE EXISTS (OPENABLE FOR INPUT) *)
  (* OTHERWISE *)

    FROMFILE := RETRY('RETYPE FROM FILE')
  end;                                      (* LOOP *)

  (* FROM FILE NOW OPEN THE WAY WE WANT IT *)

  INBLK [1] := - 377777777777B + ORD(ADDRESS(TO10));

  (* SET INPUT POINTER TO THE TO FILE NOW, AND SEE IF IT'S A
     NEW FILE, OR AN OLD FILE *)

  OUTBLK [1] := 0;                           (*DSNU, DON'T DELETE *)

1:                                        (* IT'LL MAKE SENSE FURTHER DOWN *)
  FN940TO10(TOFILE, TO10, TENCODE);         (* XLATE *)
  if TENCODE <> ISFN then
    begin
    TOFILE := RETRY('RETYPE TO FILE');    (* NO XLATE *)
    goto 1                                (* TRY AGAIN *)
  end;                                    (* OTHERWISE *)

  (* OPEN FILE FOR INPUT TO CHECK EXISTENCE *)

  INBLK [2] := 0;
  TO10 := TO10 || CHR(0);                   (* NULL FOR ASNU *)
  if DOUUO(ASNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT))) = NOSKIP 
  then PROMPT := 'NEW FILE'                 (* DIDN'T EXIST *)
  else PROMPT := 'OLD FILE';                (* DID EXIST *)
  if not QUERY(PROMPT) then
    begin                                   (* USER DENIES HER FOOLISH ERROR *)
    TOFILE := RETRY('RETYPE TO FILE');
    UCODE := DOUUO(DSNU, OUTBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT)));
    goto 1                                (* BACK TO XLATE *)
  end;

$PAGE
  (* IF WE MADE IT HERE, THEN USER IS HAPPY WITH WHAT WE'VE GOT *)

  OUTBLK [1] := 0;                           (* CLOSE, DON'T DELETE *)
  UCODE := DOUUO(DSNU, OUTBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT)));

  (* DSNU CAN'T ERROR, SO DO IT IN ANY CASE *)

  OUTBLK [1] := 200000000001B + ORD(ADDRESS(TO10));

  (* OPEN FOR OUTPUT, DELETE CONTENTS IF ANY, WITH POINTER TO SECOND
  WORD OF STRING VARIABLE (SKIPPING LENGTH WORD)  *)

  OUTBLK [2] := 0;                           (* OWN BUFFER, ASNU *)
  UCODE := DOUUO(ASNU, OUTBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT)));
  if UCODE = NOSKIP then ERROR('TO FILE');

  (* BUT THAT SHOULD NEVER HAPPEN! *)


  (* NOW, JUST COPY FILE OVER, BLOCK BY BLOCK *)


  INBLK [1] := ORD(ADDRESS(BUFR));
  OUTBLK [1] := INBLK [1] ;
  INBLK [2] := 128;
  OUTBLK [2] := 128;
  EOFSW := FALSE;

  loop
    UCODE := DOUUO(BLI, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(FROMUNIT)));
    if UCODE = NOSKIP then ERROR('FROM FILE ');
    if UCODE = SKIP1 then
      begin
      EOFSW := TRUE;
      OUTBLK [2] := 128 - INBLK [2]
    end;

    UCODE := DOUUO(BLO, OUTBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT)));
    if UCODE = NOSKIP then ERROR ('TO FILE ');
  exit if EOFSW;
    OUTBLK [1] := ORD(ADDRESS(BUFR));
    INBLK [1] := OUTBLK [1] ;
    OUTBLK [2] := 128;
    INBLK [2] := 128
  end;

$PAGE
  (* NOW CLOSE UP AND GO HOME *)

  INBLK [1] := 0;
  UCODE := DOUUO(DSNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT)));
  UCODE := DOUUO(DSNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(FROMUNIT)));
2: 
  FRECHN(TOUNIT);
  FRECHN(FROMUNIT)
end;
$PAGE
public procedure RENAME(INPUTLINE: CMDLINE; LIDX: CMDLINEIDX);

  label 1;                                      (* FOR ERROR KICKOUTS *)

  procedure ERROR(WHY: CMDLINE);

  begin
    WRITELN(TTY, '*ERROR ON  ', WHY);
    goto 1                                    (* SHUT EVERYTHING DOWN *)
  end;



begin
  SETLINE(SUBSTR(INPUTLINE, LIDX));           (* CHOP OFF IMPERATIVE *)
  FROMFILE := GETFILE('OLD FILE');            (* PROMPT IF NECESSARY *)
  EATNOISE('AS');                             (* YOU NEVER KNOW *)
  TOFILE := GETFILE ('NEW FILE');
  if not EATSPACE then ERROR('JUNK AT END OF LINE');

  (* LOOKS OK SO FAR, SO INITIALIZE A FEW NECESSARIES *)

  INBLK [1] := - 377777777777B + ORD(ADDRESS(FROM10));

  (* THIS TELLS ASNU TO OPEN FOR INPUT, WITH A WORD THAT LOOKS LIKE
  400000,,ADDRESS OF TEXT OF FILENAME + 1 WORD (TO SKIP LENGTH) *)

  INBLK [2] := 0;                              (* USE YOUR OWN BUFFER, ASNU *)
  GETCHN(TOUNIT);
  GETCHN(FROMUNIT);
$PAGE

  (* CHECK TO SEE IF 940 FILE NAMES MAKE SENSE *)

  (* FIRST, THE FROM FILE *)

  loop
    FROM10 := '';
    INBLK [2] := 0;
    FN940TO10(FROMFILE, FROM10, TENCODE);    (* TRANSLATE *)
    FROM10 := FROM10 || CHR(0);             (* NULL FOR ASNU *)
  exit if (TENCODE = ISFN)                (* TRANSLATION OK *)
    andif
    (DOUUO(ASNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(FROMUNIT))) = SKIP1);

    (* AND IF THE FILE EXISTS (OPENABLE FOR INPUT) *)
  (* OTHERWISE *)

    FROMFILE := RETRY('RETYPE OLD FILE')
  end;                                      (* LOOP *)

  INBLK [1] := - 377777777777B + ORD(ADDRESS(TO10));

  (* SET UP FILE TO10 FOR INPUT, NO DELETE, TO SEE IF IT'S THERE *)

  INBLK [2] := 0;                              (* NO BUFFER, ASNU *)

  loop                                      (* TO GET A TRANSLATABLE FILENAME *)
    TO10 := '';                             (* ZAP OLD RESULT *)
    FN940TO10(TOFILE, TO10, TENCODE);       (* XLATE *)
  exit if TENCODE = ISFN;                 (* A GOOD XLATE *)
    TOFILE := RETRY('RETYPE NEW FILE')
  end;


  (* NOW, WE'RE READY TO TRY THE BIG WORLD *)

  TO10 := TO10 || CHR(0);                     (* GET THAT NULL CHARACTER *)
  if DOUUO(ASNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT))) = SKIP1 then
    begin

    (* ERROR--CAN'T RENAME IT TO A FILE THAT ALREADY EXISTS *)

    INBLK [1] := 0;                          (* DON'T DELETE *)
    UCODE := DOUUO(DSNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(TOUNIT)));
    ERROR('FILE ALREADY EXISTS')
  end;

  (* OTHERWISE...*)

  INBLK [1] := (ORD(ADDRESS(TO10)) + 1) * 1000000B + 3;

(* TELLS DSNU TO USE RENAME OPTION (3) WITH FILE NAME IN TOFILE *)

1: 
  UCODE := DOUUO(DSNU, INBLK, UUO_MAXAC, 0, ORD(ADDRESS(FROMUNIT)));
  FRECHN(TOUNIT);
  FRECHN(FROMUNIT)
end.
