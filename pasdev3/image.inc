(* External include file for IMAGE_FILE_NAME.
   This routine is a VAX RDLIB routine which returns the
   full pathname of the image file that the user is currently
   running. When running QED this routine will return:
          _DRA1:[250220.QED]QED.EXE;1                         *)

type IMAGE_ERROR = ( IMAGE_OK, IMAGE_ERR );

external procedure image_file_name( var name : file_name;
				    var err  : image_error );

