(* QLINE.TYP - created 10/09/81 by djm *)

  qline = packed record				(* QED line descriptor *)
    tag: boolean;				(* ANC's "tagged" line marker *)
    prevlinep,					(* previous line pointer *)
    nextlinep: qlinep;				(* next line pointer *)
    source: qstring				(* text of line *)
  end;	(* note: users of QED routines should never mess with qlines *)
  