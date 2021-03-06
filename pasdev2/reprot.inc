
Type PRO_ERR = ( PRO_OK,	(* File successfully re-protected. *)
		 PRO_BAD_FILE,	(* Can't open file, either does not 
				   exist or cannot be opened write access *)
		 PRO_NO_WRITE );(* Couldn't reprotect even though the file
				   could be opened write access. If this
				   happens contact the programming languages
				   group. *)

External Procedure REPROTECT ( FNAME	: File_name;
				PRO	: DIR_PROT;	(* from PASDIR.TYP *)
			    Var ERR	: PRO_ERR );
   