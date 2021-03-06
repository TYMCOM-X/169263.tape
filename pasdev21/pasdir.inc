(* PASDIR.INC external procedure/function definitions *)

(* NOTICE -- DTIME.TYP must be included before including this file! *)

external procedure DIR_OPEN ( var DIR_ERRORS;
			      var DIR_INT_ID;
			      DIR_EXT_ID);

external procedure DIR_CLOSE ( var DIR_ERRORS;
			       DIR_INT_ID);

external procedure DIR_NEXT ( var DIR_ERRORS;
			      DIR_INT_ID;
			      var DIR_FNAME);

external function DIR_MATCH (TARGET: DIR_FNAME;
			     PATTERN: DIR_M_STR): boolean;

external procedure DIR_ATTR
  ( var DIR_ERRORS;
    DIR_FNAME;
    var DIR_ATTRS );

(* end of PASDIR.INC *)
  