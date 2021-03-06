(* external procedures called by qed for 940 conversion work *)

	(* routine to convert string containing some sort of 940 file name
	   to equivalent PDP-10 file name.  If modifier specifies that file
	   is actually PDP-10 file, wtenfile is returned.  If conversion
	   fails, wbadname is returned, else wok. *)


EXTERNAL PROCEDURE WFILECONVERT( FILE_ID;	(*name to convert*)
				 WMODIFIER;	(*string indicating conversion*)
				 VAR FILE_ID;	(*converted name*)
				 VAR WCODES;	(*return code*)
				 VAR BOOLEAN);	(*add r for revision if true *)

	(* routine to open a 940 file for I/O *)

EXTERNAL PROCEDURE WOPEN( WCHANNEL;		(*channel on which to open file*)
			  VAR WCODES;		(*wok=success; wbadfile=failure*)
			  WIOMODES;		(*direction of data transfer*)
			  VAR FILE_ID);		(*PDP-10 file name*)

	(* routine to close a 940 file *)

EXTERNAL PROCEDURE WCLOSE( WCHANNEL;		(*channel on which to close file*)
			   VAR WCODES);		(*wok=success, wouterror=failure*)

	(* routine to read a line from 940 file *)

EXTERNAL PROCEDURE WINLINE( WCHANNEL;		(*channel to read from*)
			    VAR WCODES;		(*wok=success, winerror=failure,
						  weof=end of file*)
			    VAR QSTRING);	(*line read (no cr on end)*)

	(* routine to write a line *)

EXTERNAL PROCEDURE WOUTLINE( WCHANNEL;		(*channel to write to*)
			     VAR WCODES;	(*wok=success; wouterror=failure*)
			     QSTRING);		(*line to write (w/o final cr)*)

	(* routine to return PDP-10 file name open on channel *)

EXTERNAL PROCEDURE WFILENAME( WCHANNEL;
			      VAR FILE_ID;	(*name of file*)
				VAR BOOLEAN);

(**********************************************************************)

    