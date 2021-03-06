(* SPATPARSE extracts a string pattern from an input line.  The parse begins
   at a caller supplied index. Preceding blanks are ignored. If the first 
   nonblank character is not a valid string pattern delimiter, then false is
   returned.  The index is not advanced.  If a valid delimiter is found, then
   the line is scanned to complete the pattern. An error occurs if there is
   no closing delimiter or if the default pattern is used and there it has no
   current value.  If the pattern is valid, the pattern record is filled in;
   the parsing index is advanced to the character past the closing delimiter;
   and true is returned. If there is an error, true is returned with an error
   code set. The index is set to the openning delimiter; the pattern record is
   not modified. *)


EXTERNAL FUNCTION SPATPARSE
	    (	LINE: CMDLINE;		(* LINE TO BE PARSED *)
		VAR IDX: CMDLINEIDX;	(* PARSING CURSOR, ADVANCED IF TRUE RETURNED *)
		VAR PAT: SPATTERN;	(* PATTERN DESCRIPTION, SET IF COMPLETE PATTERN FOUND *)
		WILDSWITCH: BOOLEAN;	(* WILDCARDING SET SWITCH *)
		VAR ERR: QERRCODE	(* ERROR CODE, SET IF SOMETHING FOUND *)
				): BOOLEAN;



(* SPATMATCH attempts to find a substring matching a pattern somewhere within
   a caller supplied line.  If no match is found, then false is returned.  If
   a match is found, then the starting index and length of the substring are
   set, and true is returned. *)


EXTERNAL FUNCTION SPATMATCH
	    (	LINE: QSTRING;		(* LINE TO SCAN FOR A MATCHING SUBSTRING *)
		PAT: SPATTERN;		(* PATTERN TO BE MATCHED *)
		VAR POS, LEN: QSTRINGIDX;  (* STARTING POSITION AND LENGTH OF SUBSTRING *)
		VAR ERR: QERRCODE	(* ERROR REPORT *)
				): BOOLEAN;




(* SPATDISPOSE disposes of the doubly-linked list in the given SPATTERN *)

EXTERNAL PROCEDURE SPATDISPOSE (PATTERN: SPATTERN);

(* SPATSETDEFAULT sets the default string pattern - i.e. // or equivalent.
   Initially it has no value; an attempt to reference it causes an error to
   be reported by SPATMATCH. *)


EXTERNAL PROCEDURE SPATSETDEFAULT (PATTERN: SPATTERN);

(* SPATGETDEFAULT returns the value of the current
default spattern. Useful for preventing sub-commands of FIND from
changing it. Will not report an error if the pattern
is null. *)

EXTERNAL FUNCTION SPATGETDEFAULT: SPATTERN;

(* QTOKENGET searches for the next non-blank character in line and sets idx
to that position. The function returns FALSE if no token is found *)

EXTERNAL FUNCTION QTOKENGET(LINE : CMDLINE; VAR IDX : CMDLINEIDX) : BOOLEAN;
  