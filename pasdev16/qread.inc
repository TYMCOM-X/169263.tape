(* QREAD reads a line of input, but will no longer allow intraline editing.
   Naturally assumes that the terminal is open. This routine will also
   print a '?' when a control-G is type at the terminal.      *)

EXTERNAL FUNCTION QREAD : QSTRING;
  