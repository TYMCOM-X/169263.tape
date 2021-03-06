(* VBUF/VALLOC error handler *)

$INCLUDE VBUF.TYP


type
  ERRORSTR = string[80];		(* holds error message *)
  ERRORTABLE = array[VERROR] of ERRORSTR;

external procedure VKILLR(ERRORSTR);	(* MACRO routine to go to DIE. *)

const
  ERRTAB: ERRORTABLE := (			(* messages are *)
'',						(* VOK *)
'VBUF no unlocked buffers available.',	(* VNOBUF *)
'VBUF invalid realpointer to VFREE.',	(* VBADRP *)
'VBUF invalid VIRTUALPOINTER.',		(* VBADVP *)
'VBUF address space not initialized.',	(* VNOTINIT *)
'VBUF can''t VNEW beyond page size.',	(* VTOOLONGNEW *)
'VBUF fatal internal error!',		(* VINTERR *)
'VBUF LRU page locked.',			(* VLASTLOCKED *)
'VBUF attempt to VNEW zero-length record', (* VZERONEW *)
'VBUF attempt to VDISPOSE a record twice.', (* VDISPTWICE *)
'VBUF must have at least one buffer.',	(* VNEEDONE *)
'VBUF I/O error in paging op''n',	(* VIOERR *)
'VBUF bad paging file'			(* VBADFILE *)
);

public procedure ZAPERR (ERR: VERROR);
  begin
  VKILLR(ERRTAB[ERR])				(* call MACRO routine *)
  end.
    