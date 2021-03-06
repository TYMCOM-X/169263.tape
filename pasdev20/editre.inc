(* QREAD is the actual read routine used by QED 1.6 and EDITREAD . *)

EXTERNAL FUNCTION QREAD
					 : QSTRING;	(* RESULT *)

(* EDITREAD (1.6) reads a line of input. The oldline input is just a stub
   that in version 1.5 was an argument string for intra-line editing. *)

EXTERNAL FUNCTION EDITREAD
		    (  OLDLINE: QSTRING			
					 ): QSTRING;	(* RESULT *)


    