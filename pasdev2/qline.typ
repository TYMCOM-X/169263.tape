(* QLINE.TYP - last modified 9/21/81 by djm to remove $IF ANC tag field from
               this version.  The field is still present in the ANC version. *)

TYPE
  QLINE = PACKED RECORD				(* QED line descriptor *)
    PREVLINEP,					(* previous line pointer *)
    NEXTLINEP: QLINEP;				(* next line pointer *)
    SOURCE: QSTRING				(* text of line *)
  END;						(* note: users of QED routines
						should never mess with qlines *)
   