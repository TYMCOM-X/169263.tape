(* QEDLN.INC - last modified 9/21/81 by djm to remove external references to
               procedures QTAG, QTAGSET, and QTAGCLEAR.  These procedures are 
               still present in the ANC version. *)

EXTERNAL PROCEDURE QINITBUF			(* call once for each buffer *)
(	VAR BUFFER: QBUFFER);			(* buffer to be created *)

EXTERNAL PROCEDURE QDELBUF			(* clear and re-initialize buffer *)
(	VAR BUFFER: QBUFFER);			(* buffer to be purged *)

EXTERNAL FUNCTION QGETLINE			(* returns text of specified line *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	L: QLINENO;				(* number of line to be gotten *)
	VAR ERR: QERRCODE			(* what went wrong? *)
		): QSTRING;			(* text of line *)

EXTERNAL PROCEDURE QMODLINE			(* change text of specified line *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	L: QLINENO;				(* line to be changed *)
	NEWTEXT: QSTRING;			(* new line's text *)
	VAR ERR: QERRCODE);			(* in case of trouble *)

EXTERNAL PROCEDURE QDELLINES			(* delete specified range of lines *)
(	VAR BUFFER: QBUFFER;			(* from this buffer *)
	F, L: QLINENO;				(* from F to L *)
	VAR ERR: QERRCODE);			(* everything QOK? *)

EXTERNAL PROCEDURE QADDLINE			(* add line of text to specified place *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	L: QLINENO;				(* after this line *)
	TEXT: QSTRING;				(* line to add *)
	VAR ERR: QERRCODE);			(* did it work? *)

EXTERNAL FUNCTION QBUFLENGTH			(* return length of bounded buffer *)
(	VAR BUFFER: QBUFFER
		): QLINENO;

EXTERNAL FUNCTION QDOLLAR_VAL			(* return the value of a $ ld *)
(       VAR BUFFER: QBUFFER
		): QLINENO;

EXTERNAL FUNCTION QFIRST_VAL			(* return the value of the 1st bounded line*)
(       VAR BUFFER: QBUFFER
		): QLINENO;

EXTERNAL PROCEDURE QMOVELINES			(* relocate a section of the buffer *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to be moved *)
	DEST: QLINENO;				(* where to move them to *)
	VAR ERR: QERRCODE);			(* error report *)

EXTERNAL PROCEDURE QCOPYLINES
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to copy *)
	DEST: QLINENO;				(* where to copy them to *)
	VAR ERR: QERRCODE);			(* error report *)

EXTERNAL PROCEDURE QSETOFFSET			(* sets offset for addressing bounded lines *)
(	NEWOFFSET: QLINENO;			(* new buffer offset *)
	VAR BUFFER: QBUFFER);			(* working buffer *)

EXTERNAL PROCEDURE QSETBOUNDS			(* set up bounded region in buffer *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	F, L: QLINENO;				(* bounds (in bounded linenos) *)
	ABSOLUTE: BOOLEAN;			(* flag for line addressing mode *)
	VAR ERR: QERRCODE);			(* any problem? *)

EXTERNAL PROCEDURE QUNBOUND			(* resets buffer bounds to whole buffer *)
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	VAR ERR: QERRCODE);			(* done? *)

EXTERNAL PROCEDURE QFILEAPPEND			(* read text from file *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	s940id: file_id;			(* from this file	*)
	wmod: wmodifier;			(* 940 file type modifier	*)
	WHERE: QLINENO;				(* append to this line *)
	VAR CNT: QLINENO;			(* number of lines appended *)
	VAR ERR: QERRCODE);			(* and report any problems *)

EXTERNAL PROCEDURE QTTYAPPEND			(* append text from the TTY *)
(	VAR BUFFER: QBUFFER;			(* in this buffer *)
	WHERE: QLINENO;				(* append to this line *)
	VAR CNT: QLINENO;			(* number of lines appended *)
	VAR ERR: QERRCODE);			(* report failures *)

EXTERNAL PROCEDURE QFILEWRITE			(* write text to file *)
(	VAR BUFFER: QBUFFER;			(* buffer to write from *)
	s940id: file_id;			(* file to write to	*)
	wmod: wmodifier;			(* 940 file type modifier	*)
	FNO, LNO: QLINENO;			(* range to write *)
	CONFIRM: BOOLEAN;			(* new/old file prompting desired? *)
	VAR ERR: QERRCODE);			(* error report *)
   