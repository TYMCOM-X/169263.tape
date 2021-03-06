(**********  FILUTL.inc  last modified 6/19/78 **********)



(* PR FILE ID extracts a file title form an input string. If the title parses
   correctly, file_id information is set, the string cursor is advanced past
   the title and true is returned.  If the title is incorrectly formed, false
   is returned, and the cursor is left pointing to the character which is in
   error.  The file_id information is not changed. *)

EXTERNAL FUNCTION PR_FILE_ID
	    (	LINE: CMDLINE; VAR IDX: CMDLINEIDX;
		VAR FID: FILE_ID		      ): BOOLEAN;



(* OPEN FILE opens a text file for input or output. The mode is specified by
   the caller. For an output file the user can also request append mode and
   old/new file prompting. The caller may supply a default extension to be used
   in the file_id if none appears.  A flag is returned indicating if the open
   was successful. *)

TYPE
  IO_MODE = ( INPUT_MODE, OUTPUT_MODE );
  IO_OPTION_SET = SET OF ( APPEND_MODE, CONFIRM_OPEN, ASCII_MODE );

EXTERNAL FUNCTION OPEN_FILE
	    (   VAR F: TEXT;
		FID: FILE_ID;
		EXT: EXTENSION;
		MODE: IO_MODE;
		OPTION_SET: IO_OPTION_SET   ): BOOLEAN;



(* PATHNAME returns the actual file_id of an opened PASCAL file.  This file_id
   reflects any directory searching in effect. *)

EXTERNAL FUNCTION PATHNAME  ( VAR F: TEXT ):  FILE_ID;

    