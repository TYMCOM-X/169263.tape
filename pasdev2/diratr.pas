module DIRATR options special(coercions), nolibrary;

$HEADER DIRATR.HDR

$SYSTEM DTIME.TYP[31024,320156]
$SYSTEM DTIME.INC[31024,320156]
$SYSTEM PASDIR.TYP[31024,320156]


external procedure DO_ATTR (var DIR_ATTRS;
			    DIR_FNAME;
			    var DIR_ERRORS);

public procedure DIR_ATTR (var ERR: DIR_ERRORS;
			   FNAME: DIR_FNAME;
			   var FILE_INFO: DIR_ATTRS);

  (* DIR_ATTR gets various attributes returned by the 5-word extended
     LOOKUP monitor call. An external MACRO routine is used to perform
     the LOOKUP, and to assign most of the fields of the record. *)
type
  FULLWORD = packed record
    L, R: 0..777777b
    end;

var
  FW: ^ FULLWORD;

begin 
  DO_ATTR(FILE_INFO, uppercase(FNAME), ERR);
  if ERR <> DIR_OK then
    ERR := DIR_NO_FILE 
  else with FILE_INFO do begin  (* a few coercions *)
    FW := address (ACCESSED);
    ACCESSED := EC_TSDATE (FW^.R);
    FW := address (CREATION);
    CREATION := DT_COMBINE (EC_TSDATE (ord (FW^.L)),
			    EC_DCTIME (ord (FW^.R) * 60000) )
    end
end(* procedure DIR_ATTR *).
