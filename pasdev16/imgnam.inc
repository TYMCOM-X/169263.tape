(* VAX IMAGE_FILE_NAME external procedure declaration *)

Type IMAGE_ERROR = (
	IMAGE_OK,
	IMAGE_ERR );

External Procedure IMAGE_FILE_NAME (
	Var IMAGE_NAME : File_name;
	Var IMGERR : IMAGE_ERROR );
 