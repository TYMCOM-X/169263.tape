(* INDOUT.INC Selected public vars for INDOUT *)

external var
  LINE: TEXTLINE;
  CURSTART,
  CUREND,
  EOL_CT,
  COM_COL,
  TOK_LEN,
  OUT_MAX,
  QUANTUM,
  COM_IND,
  OUT_IND: LINE_IDX;

  ALIGN_COM: boolean;

  THIS_SYM,
  NEXT_SYM: SYMBOLS;

  SAFIRST,
  SALAST: SAPTR;

  OUTFILE: text;

  READER: ENVIRONMENT;

(* End of INDOUT.INC *)
 