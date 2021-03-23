(* MMDBPR.TYP database primitive declarations *)

type
  directoryblock = record
    nextdirect: integer;		(* pointer to next, if any *)
    modptrs: array [1..127] of integer	(* pointer to first of modules *)
    end (* directoryblock *);

  coerce_rec = packed record		(* to coerce words in database *)
    case 0..2 of
      0: (lh: -1 .. #o377777; rh: -1 .. #o377777);
      1: (int: integer);
      2: (dt: dtime_int)
    end (* coerce *);

const
  max_page_size = 127;		(* length of page. *)
  versionid = 1;			(* offset of ODMS control info *)
  timestamp = 2;
  nextptr = 3;

(* END of MMDBPR.TYP *)
