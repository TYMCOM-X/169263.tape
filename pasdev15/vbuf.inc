(*********************************************************************)
(* INCLUDE file for VBUF and VALLOC routines *)

type					(* a few *)
  WORD = -1..377777777777B;		(* basic data unit *)
  VIRTUALPOINTER = WORD;		(* as far as user is concerned *)
  FILENAME = packed array[1..30] of char;


(***************** NOTICE **************
the user should define type REALPOINTER
before $INCLUDEing this file!!!       *)

external procedure VBUFINIT (
    WORD;				(* number of core buffers to use *)
    FILENAME;				(* paging file to use *)
    var boolean);			(* error return *)

(* VBUFINIT must be called prior to any use of the other routines in
   the VBUF or VALLOC packages.  It is also called to clear and reset
   the address space.  WORD indicates the number of in-core buffers to
   be allocated.  Any former address space is deallocated, and its
   paging file deleted.  FILENAME is the name of the paging file to
   be used; the user must have total access to the paging file.  If the
   null string is passed, a default file name will be supplied.  The
   following error codes can be returned: 

	VNEEDONE---WORD is 0 or negative,
	VBADFILE---FILENAME cannot be opened, and
	VOK---if all went well.
*)

external procedure VBUFCLS(KEEP: boolean);

(* VBUFCLS is called to deallocate the currently active address space
   without activating a new one.  It is an error to call VBUFCLS when
   there is no active address space. If KEEP is true, then the
   paging file, if not a temp file (created by VBUFINIT for '' as
   a file name), is not deleted. Temp files are always deleted.
*)

external procedure VHOLD (
    VIRTUALPOINTER;			(* which page to hold *)
    var REALPOINTER;			(* hard address returned *)
    boolean);				(* clean or dirty? *)

(* The VIRTUALPOINTER is translated into a REALPOINTER, and the 
   corresponding page is locked in a core buffer.  Boolean indicates
   the type of lock, TRUE if the contents of the page will be changed
   (and must be written out later), false otherwise.  A run-time error
   will occur if: (1)the number of currently valid REALPOINTERS 
   exceeds the number of page buffers; (2)an I/O error occured; (3)
   address space not initialized.
*)

external procedure VFREE (
    REALPOINTER);			(* hard pointer within page *)

(* The REALPOINTER is considered no longer in use.  If no other pointers
   to the same virtual page were obtained by VHOLD, the page is unlocked.
   Run-time errors are (1) internal error(s); (2) REALPOINTER not 
   within a core buffer; (3) address space not initialized.
*)

external procedure VFREEALL;

(* VFREEALL is called to free all currently active REALPOINTERS.  It 
   effectively resets the address space control information.  It is an
   error to call VFREEALL when there is no active address space.
*)

external procedure VFETCH (
   VIRTUALPOINTER;			(* which datum to get *)
   REALPOINTER);			(* pointer to the data *)

(* The data contained in the virtual record pointed to by VIRTUALPOINTER
   is placed in the cell pointed to by REALPOINTER.  Errors can occur
   if (1)too many locks; (2)address space not initialized; (3)invalid
   virtual pointer; and (4) I/O or internal error.
*)

external procedure VCHANGE (
    VIRTUALPOINTER;			(* where to change *)
    REALPOINTER);			(* what to change it to *)

(* The data pointed to by REAPOINTER is placed in the record pointed
   to by VIRTUALPOINTER.  Errors include: (1)too many locks; (2)
   invalid virtual pointer; (3)address space not initialized; (4) I/O
   or other internal error.
*)

external procedure VNEW (
    var VIRTUALPOINTER;			(* address of new record *)
    WORD);				(* length of record *)

(* A record of length WORD is created in the virtual address space,
   and its VIRTUALPOINTER is returned.  Errors are: (1)too many locks;
   (2)address space not initialized; (3)I/O or internal error; (4)
   desired size larger than buffer size.
*)

external procedure VDISPOSE (
    VIRTUALPOINTER);			(* what to dispose of *)

(* The record at VIRTUALPOINTER is returned to the virtual heap.  Errors
   are (1)address space not initialized; (2)attempt to dispose a single
   record twice; (3)invalid virtual pointer.
*)
(*********************************************************************)
 