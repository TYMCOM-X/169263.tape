(*********  LOOKUP.typ  last modified 8/13/77  **********)

type
  cmdlist =
    record
      name: packed array[1..10] of char;  (* full name *)
      abbrev: 1..10			  (* min length *)
    end;

    