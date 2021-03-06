		(* Basic QED buffer management routines *)

external procedure qinitbuf	(* call once for each buffer *)
(	var buffer: qbuffer);	(* buffer to be created *)

external procedure qdelbuf	(* clear and re-initialize buffer *)
(	var buffer: qbuffer);	(* buffer to be purged *)

external function qgetline	(* returns text of specified line *)
(	var buffer: qbuffer;	(* working buffer *)
	l: qlineno;		(* number of line to be gotten *)
	var err: qerrcode	(* what went wrong? *)
		): qstring;	(* text of line *)

external procedure qmodline	(* change text of specified line *)
(	var buffer: qbuffer;	(* in this buffer *)
	l: qlineno;		(* line to be changed *)
	newtext: qstring;	(* new line's text *)
	var err: qerrcode);	(* in case of trouble *)

external procedure qdellines	(* delete specified range of lines *)
(	var buffer: qbuffer;	(* from this buffer *)
	f, l: qlineno;		(* from F to L *)
	var err: qerrcode);	(* everything QOK? *)

external procedure qaddline	(* add line of text to specified place *)
(	var buffer: qbuffer;	(* in this buffer *)
	l: qlineno;		(* after this line *)
	text: qstring;		(* line to add *)
	var err: qerrcode);	(* did it work? *)

external function qbuflength	(* return length of bounded buffer *)
(	var buffer: qbuffer
		): qlineno;

external function qdollar_val   (* return the value of a $ ld *)
(       var buffer: qbuffer
                ): qlineno;

external function qfirst_val    (* return the value of the 1st bounded line*)
(       var buffer: qbuffer
                ): qlineno;

external procedure qmovelines	(* relocate a section of the buffer *)
(	var buffer: qbuffer;	(* working buffer *)
	first, last: qlineno;	(* range of lines to be moved *)
	dest: qlineno;		(* where to move them to *)
	var err: qerrcode);	(* error report *)

external procedure qcopylines
(	var buffer: qbuffer;	(* working buffer *)
	first, last: qlineno;	(* range of lines to copy *)
	dest: qlineno;		(* where to copy them to *)
	var err: qerrcode);	(* error report *)

external procedure qsetoffset		(* sets offset for addressing bounded lines *)
(	newoffset: qlineno;		(* new buffer offset *)
	var buffer: qbuffer);		(* working buffer *)

external procedure qsetbounds	(* set up bounded region in buffer *)
(	var buffer: qbuffer;	(* working buffer *)
	f, l: qlineno;		(* bounds (in bounded linenos) *)
	absolute: boolean;	(* flag for line addressing mode *)
	var err: qerrcode);	(* any problem? *)

external procedure qunbound	(* resets buffer bounds to whole buffer *)
(	var buffer: qbuffer;	(* working buffer *)
	var err: qerrcode);	(* done? *)

external procedure qfileappend	(* read text from file *)
(	var buffer: qbuffer;	(* in this buffer *)
	s940_id: file_id;	(* from this file *)
	wmod: wmodifier;	(* WIERD 940 file modifier *)
	where: qlineno;		(* append to this line *)
	var cnt: qlineno;	(* number of lines appended *)
	var err: qerrcode);	(* and report any problems *)

external procedure qttyappend	(* append text from the TTY *)
(	var buffer: qbuffer;	(* in this buffer *)
	where: qlineno;		(* append to this line *)
	var cnt: qlineno;	(* number of lines appended *)
	var err: qerrcode);	(* report failures *)

external procedure qfilewrite	(* write text to file *)
(	var buffer: qbuffer;	(* buffer to write from *)
	s940_id: file_id;	(* file to write to *)
	wmod: wmodifier;	(* WIERD 940 file modifier *)
	fno, lno: qlineno;	(* range to write *)
        confirm: boolean;	(* new/old file prompting desired? *)
	var err: qerrcode);	(* error report *)
 