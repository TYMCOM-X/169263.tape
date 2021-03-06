(* Include file for RENAME VAX procedure *)

Type RNM_ERR = (
	RNM_OK,		(* Successful renaming process *)
	RNM_DIR_NOT_FOUND, (* Bad directory specified  *)
	RNM_FILE_EXISTS,(* To file already exists.     *)
	RNM_FILE_NOT_FOUND, (* From file not found     *)
	RNM_PROT_VIOLATION, (* Protection violation    *)
	RNM_RENAME_FAIL );  (* Catcall in case         *)

External Procedure RENAME ( File_name; File_name; Var RNM_ERR );
  