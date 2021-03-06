(*
 *  include file of definitions required by all cl/1 modules.
 *)

const max_id_len = 10;  (* max signifigant length of id's *)
   max_int = 377777777777b;

type token_type = (id,cons,r_paren,l_paren,plus,minus,
      times,divide,assign_op,eol,error); (* types of tokens *)
   identifier = string[max_id_len]; (* identifier definition *)
   token_rec = record  (* token definition *)
      t_type: token_type;  (*   - type of token *)
      id_text: identifier;  (*   - text of token if id *)
      cons_val: 0..max_int  (*   - value of token if constant *)
   end;
   err_types = (success, syn_err, lex_err, sym_err, id_err, div_err);
      (* error types used by parser and main routine *)
   int = -max_int..max_int;
   