(*  Declarations for QED routines to read and write 940 style files *)
(*  RLD:CMDUTL.TYP must be $INCLUDEd previously to define "file_id" *)
(*  RND:QSTR.TYP must be $INCLUDEd previously to define "qstring"   *)

TYPE WCHANNEL = 0..15;				(* channel number obtained from Pascal runtime *)

     (* error codes returned by routines in this package *)

     WCODES = (WOK, WTENFILE, WBADNAME, WBADFILE, WEOF, WINERROR, WOUTERROR);

     (* following type is for the file name modifier wtring passed to
	wfileconvert.  Its length is 6 to force the old compiler to
	pass it by address for compatibility with the new compiler *)

     WMODIFIER = STRING[6];

     (* argument to wopen specifying data transfer direction *)

     WIOMODES = (WINPUT, WOUTPUT);
 