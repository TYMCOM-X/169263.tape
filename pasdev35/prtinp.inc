
external procedure getinit;
external procedure get_token;
external function get_comment: boolean;

(*  ^variables for the output module.  *)

external var
    spacing: space_codes;
    token_text: string [max_text];
    cmt_column: text_index;
    cmt_level: 0 .. 1000;

(*  ^variables for the analyzer module.  *)

external var
    this_token: token_kind;
    next_token: token_kind;
 