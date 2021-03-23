PROGRAM SCRIBE OPTIONS STORAGE(3072);

$INCLUDE CMDUTL.TYP
PUBLIC
$INCLUDE SCRVAR.INC
$INCLUDE TEMPFI.INC[31024,320156]

EXTERNAL PROCEDURE DO_SCRIBE (LINE: CMDLINE; ECHO_CMD: BOOLEAN);
EXTERNAL PROCEDURE RUN(PACKED ARRAY[1..*] OF CHAR; INTEGER);

CONST MAXCMDIN = 4;    (* maximum depth of indirect (@) file nesting *)

VAR
  LINE: CMDLINE;				(* input line *)
  CMDFILE: BOOLEAN;				(* tells whether to prompt for  command input *)
  CMDIN: ARRAY[1..MAXCMDIN] OF TEXT;   (* vector of input file(s) *)
  CURCMDIN: 0..MAXCMDIN;   (* current entry in cmdin vector *)

BEGIN

  REWRITE(TTY);  OPEN(TTY);   (* first, open up the controlling terminal *)
  OPEN (CMDIN[1], temp_file_name('SCR'));			(* open input, check first for cmd file *)
  IF EOF (CMDIN[1]) THEN BEGIN   (* if eof high, no cmd file present *)
    CMDIN[1]:= TTY;
    CMDFILE := FALSE;				(* read commands from the terminal  *)
  END
  ELSE CMDFILE := TRUE;
  CURCMDIN:= 1;   (* set current entry in file vector *)
  WRITELN(TTY,'SCRIBE Version 4.3 ',COMPDATE);

  WHILE (CURCMDIN > 0) DO BEGIN   (* read until first level file ended *)
    IF (CMDIN[CURCMDIN] = TTY) THEN BEGIN   (* if reading from tty, prompt *)
      WRITE (TTY, '*');
      BREAK
    END;
    READLN (CMDIN[CURCMDIN]);				(* advance to next line of input *)
    IF EOF(CMDIN[CURCMDIN]) THEN BEGIN   (* on eof, pop up 1 level *)
      IF (CURCMDIN > 1) AND (CMDIN[CURCMDIN]<>TTY) THEN
	CLOSE(CMDIN[CURCMDIN]);  (* and close, unless tty or 1st level file *)
      CURCMDIN:= CURCMDIN - 1
    END
    ELSE IF NOT EOLN(CMDIN[CURCMDIN]) THEN BEGIN   (* non-null line *)
      READ(CMDIN[CURCMDIN],LINE);   (* read the line *)
      IF LINE[1] = '@' THEN   (* indirect file line *)
	IF CURCMDIN < MAXCMDIN THEN BEGIN   (* can go deeper *)
	  OPEN(CMDIN[CURCMDIN+1], '.ccl ' || SUBSTR(LINE,2));
	  IF EOF(CMDIN[CURCMDIN+1]) THEN  (* try other extension *)
	    OPEN(CMDIN[CURCMDIN+1], '.cmd ' || SUBSTR(LINE,2));
	  IF EOF(CMDIN[CURCMDIN+1]) THEN BEGIN   (* file didn't open *)
	    WRITELN(TTY,'? Unable to open indirect file: ',SUBSTR(LINE,2),'.');
	    BREAK
	  END
	  ELSE CURCMDIN:= CURCMDIN + 1   (* down a level *)
	END
	ELSE BEGIN   (* we are already too deep in files *)
	  WRITELN(TTY,'? Too many nested indirect files.');
	  BREAK
	END
      ELSE IF CMDIN[CURCMDIN] = TTY THEN DO_SCRIBE(LINE, FALSE)
           ELSE DO_SCRIBE(LINE, TRUE)  (* Normal input line...off to scribe *)
    END
    ELSE   (* null line...terminate if highest level and reading tty *)
      IF (CURCMDIN=1) AND (NOT CMDFILE) THEN CURCMDIN:= 0;   (* else...ignore it *)
    IF RUN_FLAG THEN BEGIN
      CLOSE;
      IF CMDFILE THEN BEGIN 				(* delete the command file *)
        RESET (CMDIN[1],TEMP_FILE_NAME('SCR'));
        SCRATCH(CMDIN[1]);
      END;
      RUN(RUN_PGM_NAME,RUNOFF_VAL); (*NEED TO INCLUDE RUN FILE*)
    END;
    EXIT IF EXIT_FLAG;
  END;

  IF CMDFILE THEN BEGIN				(* delete the command file *)
    RESET(CMDIN[1],TEMP_FILE_NAME('SCR'));
    SCRATCH(CMDIN[1]);
  END;
END.
