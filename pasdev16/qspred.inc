(* SPREDPARSE extracts a string predicate from an input line.  The parse begins
   at a caller supplied index. If the first token at or following the index is
   not a valid initial token for a string predicate, false is returned. No
   additional parameters, including the line index, are modified. If the
   token is acceptable, an attempt is made to parse a complete string predicate
   An error occurs if there is a syntactic error, an error in a contained
   string pattern, or an AND operator whose operands are both ::, @@, or ##
   form patterns.  If there is no error, then a pointer to the base of the
   predicate tree is filled in; the index is set to the character position
   following the end of the predicate; and true is returned.  If there is an
   error, true is returned with a nonnull error code.  The index is set to
   the start of the token at which the error occurred. The predicate tree
   base is not set. *)

EXTERNAL FUNCTION SPREDPARSE
	    (	LINE: QSTRING;			(* STRING TO BE PARSED *)
		VAR IDX: QSTRINGIDX;		(* PARSING CURSOR, ADVANCED IF SOMETHING FOUND *)
		VAR PRED: SPRED;		(* PREDICATE TREE, SET IF COMPLETE PATTERN FOUND *)
		WILDSWITCH: BOOLEAN;		(* ON/OFF SWITCH FOR WILDCARDING *)
		VAR ERR: QERRCODE		(* ERROR CODE, SET IF SOMETHING FOUND *)
				): BOOLEAN;



(* SPREDMATCH attempts to match a line against a string predicate. The rules
   for interpreting a predicate is given in the 'Definition of QED'.  A flag is
   returned indicating whether or not there was a match. *)

EXTERNAL FUNCTION SPREDMATCH
	    (	LINE: QSTRING;			(* LINE TO MATCH AGAINST *)
		PRED: SPRED;			(* PREDICATE TO MATCH *)
		VAR ERR: QERRCODE		(* ERROR REPORT *)
				  ): BOOLEAN;	(* TRUE IF MATCH FOUND *)



(* SPREDDISPOSE deletes the nodes of a string predicate tree. It is passed
   a possibly nil pointer to the base of a tree. *)

EXTERNAL PROCEDURE SPREDDISPOSE (PRED: SPRED);
    