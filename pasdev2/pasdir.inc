(* PASDIR.INC external procedure/function definitions *)

(* NOTICE -- DTIME.TYP must be included before including this file! *)

external procedure DIR_OPEN ( var err: DIR_ERRORS;
			      var id: DIR_INT_ID;
			      ext_id: DIR_EXT_ID);

external procedure DIR_CLOSE ( var err: DIR_ERRORS;
			       id: DIR_INT_ID);

external procedure DIR_NEXT ( var ERR: DIR_ERRORS;
			      id: DIR_INT_ID;
			      var fname: DIR_FNAME);

external function DIR_MATCH (TARGET: DIR_FNAME;
			     PATTERN: DIR_M_STR): boolean;

external procedure DIR_ATTR
  ( var err: DIR_ERRORS;
    fname: DIR_FNAME;
    var attr: DIR_ATTRS );

(* end of PASDIR.INC *)
  