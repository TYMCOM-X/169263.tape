(*********  LOOKUP.typ  last modified 8/13/77  **********)

TYPE
  CMDLIST =
    RECORD
      NAME: PACKED ARRAY[1..10] OF CHAR;	(* full name *)
      ABBREV: 1..10				(* min length *)
    END;

 