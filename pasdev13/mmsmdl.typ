(* MMSMDL.TYP basic type definitions for ODMS' MDL processing. *)

type
  mdltoken = (machtok, systok, areatok, modtok, symtok, stortok,
    endtok, sizetok, containstok, intok, proctok, functok, shartok,
    vartok, constok, restok, filltok, debtok, idtok, numtok,
    commatok, eoftok, errtok);

  mdlwords = machtok .. idtok;

(* End of MMSMDL.TYP *)
