
$IFANY (VAX,P10)
Type PRO_ERR = ( PRO_OK,	(* File successfully re-protected. *)
		 PRO_BAD_FILE,	(* Can't open file, either does not 
				   exist or cannot be opened write access *)
		 PRO_NO_WRITE );(* Couldn't reprotect even though the file
				   could be opened write access. If this
				   happens contact the programming languages
				   group. *)

$ENDIF

$IF M68
Type PRO_ERR = (PRO_OK,          (* File successfully reprotected      *)   
                PRO_INV_RLENGTH, (* Read code is invalid length        *)     
                PRO_INV_WLENGTH, (* Write code is invalid length       *)    
                PRO_INV_RCODE,   (* Read code not between 'AA' & 'PP'  *)
                PRO_INV_WCODE,   (* Write code not between 'AA' & 'PP' *)
                PRO_BAD_FILE,    (* File couldn't be opened            *)         
                PRO_NO_PROTECT); (* Catch-all, unable to reprotect     *)  
$ENDIF


External Procedure REPROTECT ( FNAME	: File_name;
				PRO	: DIR_PROT;	(* from PASDIR.TYP *)
			    Var ERR	: PRO_ERR );
 