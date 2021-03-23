(* QSUBSTITUTE replaces zero or more occurrences of a pattern within a line
   with a caller supplied string.  The caller may select among the following
   options:

     confirm - display the line, bracketing the matched pattern with BACKSLASH,
       and query the user to confirm the substitution. It is assumed that
       the teletype has been opened.

     all - substitute for all occurences of the pattern in the line; if not
       selected, only the first occurrence may be replaced.

     print - after applying all substitutions, display the resulting line
       if modifications have been made.

     number - if a match for the pattern is found, display a caller supplied
       line number before the first substitution.

   This routine returns the modified line, a count of the number of substitutions
   performed, and a flag indication that a match was found for the pattern. The
   flag may be true when the count is zero if the substitutions were not confirmed
   by the user.

   An error is reported if insertion of the replacement string would result in
   a line that is too long. If on return, the error code is nonnull, then
   the flag function value is true, but the values of the other values are
   undefined. 
   N_PAR is the Nth occurrance parameter. This parameter instructs QSUBST on 
   which occurrance of matching the pattern substitution(s) should begin.
   Variable NTH is a count of the number of matches found and not 
   substituted for. If NTH is less than N_PAR, then no substitution will be
   made and QSUBST will return false.
   CMD must be either SUBSTITUTE, BEFORE, or AFTER qedcmd. This instructs
   QSUBST whether to substitute or insert the replacement string in the
   line.         *)

EXTERNAL FUNCTION QSUBSTITUTE
	    (	VAR LINE: QSTRING;		(* LINE TO BE MODIFIED *)
		LINENO: QLINENO;		(* NUMBER OF ABOVE *)
		PAT: SPATTERN;			(* PATTERN TO BE REPLACED *)
		RPLMTSTR: QSTRING;		(* STRING TO REPLACE PATTERN *)
		OPTION_LIST: SUB_OPT_SET;
		VAR CNT: QSTRINGIDX;		(* COUNT OF SUBSTITUTIONS PERFORMED *)
                CMD : QEDCMDS;                  (* PARSED COMMAND *)
                VAR NTH : QLINENO;              (* COUNT OF OCCURRANCES *)
                N_PAR : QLINENO;                (* OCCURRANCE PARAMETER *)
		VAR ERR: QERRCODE		(* INDICATES IF RESULT TOO LONG *)
					): BOOLEAN;	(* INDICATES MATCH FOUND *)

