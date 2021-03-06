(* OVLDEF.TYP - include file defining Pascal overlay system's 
   standard types. *)

TYPE
     OVL_CODE = (                    (* codes returned from routines *)
       OV_OK,
       OV_FILNOTFND,                 (* OVEXTF file not found *)
       OV_TOOMANFIL,                 (* OVEXTF too many files *)
       OV_BADFILIND,                 (* invalid file table index *)
       OV_UNKMODIND,                 (* unknown module index *)
       OV_BADNSOVL,                  (* OVMODF verification fails *)
       OV_NOTDB,                     (* OVLVER file is not database *)
       OV_VERNOTFND,                 (* OVLVER version not found *)
       OV_BADDBFOR,                  (* OVLVER bad database format *)
       OV_FILISDB,                   (* OVMODF file is database *)
       OV_OVLACTIVE,                 (* current o'lay in file call *)
       OV_NOCANDEL                   (* can't delete defaults *)
       );

     VERSION = integer;
     FILEINDEX = integer;
    