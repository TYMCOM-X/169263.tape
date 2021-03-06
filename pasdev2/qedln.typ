(********************************************************************* *)

		      (* QED type definitions *)
		      (* -------------------- *)


CONST QMAXLINES = 99999;			(* maximum line number *)

TYPE
  QLINEP = ^QLINE;				(* ptr to line descriptor *)
  QLINENO = 0..QMAXLINES;			(* QED line number *)
  QLNOFFSET = -99999..QMAXLINES;		(* line offset *)

  (* BUFFER DESCRIPTOR - callers should only access the following fields:
     curlineno, lbound, hbound, lastlineno, curfile, curfileok, mark, changes.
     All other fields are implementation specific. *)

  QBUFFER = RECORD
    FIRSTLINEP,					(* ptr to dummy 0th line desc *)
    LASTLINEP,					(* to last line desc *)
    GETLINEP: QLINEP;				(* ptr to last line gotten *)
    GETLINENO: QLINENO;				(* line number of last line gotten *)
    CURLINENO,					(* current line number *)
    LASTLINENO: QLINENO;			(* last line number *)
    GARBLINEP: QLINEP;				(* lines to be deleted *)
    LBOUND,
    HBOUND: QLINENO;				(* bounded range *)
    LBOUNDP,
    HBOUNDP: QLINEP;				(* pointers to bounds *)
    OFFSET: QLINENO;				(* ANC line bounding offset *)
    OLDOFFSET: QLINENO;				(* offset previous to above *)
    MARK: SPRED;				(* boundary mark *)
    CURFILE: FILE_ID;				(* name of defaul file *)
    CURFILEOK: BOOLEAN;				(* valid filename in above ? *)
    CHANGES: BOOLEAN;				(* unwritten changes in buffer *)
    S940: BOOLEAN				(* true if current file is 940 file	*)
  END;

(********************************************************************* *)
    