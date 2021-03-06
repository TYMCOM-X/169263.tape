external function qlabelfind
(	var buffer:	qbuffer;	  
	start:		qlineno;	(* where to start search *)
	top:		qlineno;	(* upper search limit *)
	var qlabel:	qstring;	(* label name, if found *)
	var displ:	qlineno;	(* disp. of label from start *)
	var err:	qerrcode):	(* error code *)
			boolean;	(* true if label found, else false *)

(* QLABELFIND searches the buffer from the start line backwards to the
   TOP line for a line with a label. If one is found, the label's name
   and its displacement from the START line are returned, with the value
   TRUE. Otherwise, if no label is found, FALSE is returned. 
   A label begins in column one with a character in the set
   ['A'..'Z','a'..'z','0'..'9','$'] and ends with the character
   preceding the next tab, blank, or end of line.  *)
 