
(**** Type Definitions for data structures for Section numbering ****)

const maxlevels = 8;   (* maximum number of section numbering levels *)
type
  lev_rec = record
            posn : integer;
            relative : boolean
            end;
  posarray = array[1..maxlevels] of lev_rec;   (* indentation positions *)
  levarray = array[1..maxlevels] of integer;   (* section numbers *)

   