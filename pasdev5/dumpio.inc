(* INCLUDE file for DUMPIO routines *)
(* Please define INTEGER as a subrange before this point *)

type fnstr = string[90];

external procedure OPN$FILE(
	FN: fnstr;
	var CHN: INTEGER;	(* returns channel *)
	var AOK: boolean;	(* returns true iff OK *)
	INPUTONLY: boolean;	(* set true for input only *)
	DELETECON: boolean);	(* set true for delete contents *)

external procedure WRIT$PAGE(
	ADDR: INTEGER;		(* core addr of buffer *)
	LEN: INTEGER;		(* num of words to write *)
	BLOCK: INTEGER;		(* block number in file *)
        CHN: INTEGER;		(* channel number from OPEN$FILE *)
	var IOOK: boolean);	(* true iff all is OK *)

external procedure READ$PAGE (
	ADDR,LEN,BLOCK,CHN: INTEGER; (* same as WRIT$PAGE *)
	var IOOK: boolean);

external procedure CLOS$FILE(
	CHN: INTEGER;		(* channel number assoc. w/file *)
	DELETE: boolean);	(* set true to delete contents *)

(*******************************************************************)
