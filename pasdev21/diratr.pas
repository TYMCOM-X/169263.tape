module DIRATR;

$HEADER DIRATR.HDR

$include dtime.typ
$include dtime.inc
$INCLUDE PASDIR.TYP


external procedure DO_ATTR (var DIR_ATTRS;
			    DIR_FNAME;
			    var DIR_ERRORS);

public procedure DIR_ATTR (var ERR: DIR_ERRORS;
			   FNAME: DIR_FNAME;
			   var FILE_INFO: DIR_ATTRS);

  (* DIR_ATTR gets various attributes returned by the 5-word extended
     LOOKUP monitor call. An external MACRO routine is used to perform
     the LOOKUP, and to assign most of the fields of the record. *)

begin 
  DO_ATTR(FILE_INFO, uppercase(FNAME), ERR);
  if ERR <> DIR_OK then
    ERR := DIR_NO_FILE 
  else with FILE_INFO do begin  (* a few coercions *)
    ACCESSED := EC_TSDATE (ord(ACCESSED)); (* convert decdate *)
    CREATION := DT_COMBINE (EC_TSDATE(ord(CREATION) div 1000000b),
			    EC_DCTIME((ord(CREATION) mod 1000000b) * 60000))
    end
end(* procedure DIR_ATTR *).
  