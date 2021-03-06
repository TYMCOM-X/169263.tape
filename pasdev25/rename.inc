(* Include file for RENAME VAX procedure *)

Type RNM_ERR = (
	RNM_OK,		(* Successful renaming process *)
	RNM_DIR_NOT_FOUND, (* Bad directory specified  *)
	RNM_FILE_EXISTS,(* To file already exists.     *)
	RNM_FILE_NOT_FOUND, (* From file not found     *)
	RNM_PROT_VIOLATION, (* Protection violation    *)

$IFANY (VAX,P10)
	RNM_RENAME_FAIL );  (* Catcall in case         *)
$ENDIF

$IF M68
        RNM_INVALID_SYNTAX, (* Invalid file syntax for one of the file names *)
        RNM_RENAME_FAIL,    (* Catch-all, just in case *)
        RNM_WARNING );      (* Warning--Reprotection not permitted on a rename *)
$ENDIF

External Procedure RENAME ( File_name; File_name; Var RNM_ERR );
  