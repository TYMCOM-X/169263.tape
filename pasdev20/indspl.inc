(* INDSPL.INC Selected public vars for INDSPL *)

external var
  EOL_CT,
  TOK_IND,
  ORIG_IND: LINE_IDX;

  TOP_FLAG: STK_FLAG;

  READER,
  FORMAT: ENVIRONMENT;

  THIS_TOKEN,
  NEXT_TOKEN: TOKENTYPE;

  NEXT_PAGE,				(* true if NEXT_TOKEN is $PAGE *)
  SPREAD: boolean;			(* true if SPREAD option picked *)

  LINE: TEXTLINE;
  CURSTART,
  CUREND: LINE_IDX;

(* End of INDSPL.INC *)
 