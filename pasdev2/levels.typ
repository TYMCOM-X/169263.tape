
(**** Type Definitions for data structures for Section numbering ****)

const maxlevels = 8;   (* maximum number of section numbering levels *)
type
  posarray = array[1..maxlevels] of lineptr;   (* indentation positions *)
  levarray = array[1..maxlevels] of integer;   (* section numbers *)

  