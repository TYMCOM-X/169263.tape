$PAGE PASPT.INC template
(* Information from scanner, available to rest of parse *)

$include paspt.typ

external var
  token: token_type;			(* last token scanned *)
  last_token: token_type;		(* previous token *)
   