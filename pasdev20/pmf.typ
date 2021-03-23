(**********   General Integer Subranges and String Types   **********)


(*  The string index types are used when we want to be sure that a string
    index will stay in range.  String_index and string_range variables are
    representable in a half-word.  *)

const
    max_string_length = 262143;
    neg_string_length = - 262144;

type
    string_index = 0 .. max_string_length;
    index_range = neg_string_length .. max_string_length;


(*  The integer type is used for arithmetic with values read from the source
    file.  Integer variables use a full word, so anything that we can do at
    all, we can do in an integer.  *)

type
    integer = -377777777777b .. 377777777777b;


(*  The string_parm type is used for declaring procedures that take string
    parameters whose maximum size is not known in advance.  Ideally, this
    type would be declared as 'string [max_string_index]'.  Unfortunately,
    the compiler always allocates enough space for a temporary of the maximum
    possible string parameter size.  This would be impractical if that size
    were >50000 words.  *)

type
    string_parm = string [256];


(*  Sym_strings are used for macro, parameter, and file names.  *)

const
    sym_size = 40;

type
    sym_string = string [sym_size];


(*  An argument index may be used to access a macro call argument.  *)

const
    max_args = 64;

type
    arg_index = 0 .. max_args;


(*  The evaluation stack text block type is publicly available only so that
    it can be used to declare the parameter of the blk_back procedure in
    the pmfinp module.  *)

const
    eval_blk_size = 2500;
    eval_blk_lim = 2501;			(* eval_blk_size + 1 *)

type
    eval_index = 0 .. eval_blk_lim;
    eval_text = packed array [1..eval_blk_size] of char;


(**********   Special Character Definitions   **********)

(*  NUL is used during the processing of macro definitions to replace
	characters in the definition text which are deleted in the stored
	form of the definition.
    ETX is returned by the 'arg_char' function when its index is outside
	the selected argument.
    END_EXP is stored in the input stack to delimit the end of the
	expansion text for a user macro call.
    TAB is the standard text file tab character.
    EOL is used internally to represent the end of a text line.
    ARG_MARKER is used to mark a point for argument substitution in a
	user macro definition.  It is followed by a character specifying
	which argument is to be inserted.
    EOF_CH is used internally to represent an end-of-file condition.
    SPACE is simply the space character.
    FIRST_ARG.  The character following an ARG_MARKER in a macro definition
	is chr(ord(FIRST_ARG) + <the argument number>).
    ARG_DELIMITER is used to set off parameter names in definitions.  *)

const
    nul = chr(0);				(* ascii NUL *)
    stx = chr(2);				(* ascii STX *)
    etx = chr(3);				(* ascii ETX *)
    end_exp = chr(4);				(* ascii EOT *)
    tab = chr(9);				(* ascii HT *)
    eol = chr(13);				(* ascii CR *)
    arg_marker = chr(26);			(* ascii SUB *)
    eof_ch = chr(28);				(* ascii FS *)
    space = ' ';				(* ascii space *)
    first_arg = ' ';
    arg_delimiter = '\';


(*  The sets of alphas and alphanumerics are used in symbolic processing.  *)

type
    char_set = set of char;

const
  alphas: char_set :=  ['A'..'Z','_','#'];
  alphanumerics: char_set :=  ['A'..'Z','_','#','0'..'9'];


(**********   Command Line Options   **********)

type
    option_switches =
      ( process_switch, input_switch, macro_switch, pascal_switch, lib_switch,
	save_switch, compile_switch, dump_switch, set_switch, exit_switch,
	bad_option );

    option_set = set of option_switches;

    option_record =
      record
	options: option_set;
	switch_values: array [lib_switch..compile_switch] of sym_string;
	dmp_list: string [256]
      end;


(**********   Definition Nodes   **********)

(*  A definition node contains a definition chain pointer, which is used in
    looking up macro names; the length and text of the name of the defined
    macro; the kind of macro it is; and, if it is a user-defined macro, a
    pointer to a string block containing the text of the definition.  *)

type
    macro_kinds =
      ( user_macro, user_literal, def_mac, lit_mac, asgn_mac, undef_mac,
	opsyn_mac, num_mac, if_mac, eqn_mac, nen_mac, ltn_mac, len_mac,
	gtn_mac, gen_mac, eqc_mac, nec_mac, ltc_mac, lec_mac, gtc_mac,
	gec_mac, substr_mac, index_mac, search_mac, verify_mac, length_mac,
	upc_mac, lwc_mac, and_mac, or_mac, not_mac, save_mac, lib_mac,
	str_mac, eval_mac, error_mac, macro_mac );

    str_block =
      record
	str_length: string_index;
	str_text: packed array [1..1] of char
      end;

    str_pointer = ^ str_block;

    definition = ^ def_node;

    def_node =
      packed record
	chain: definition;
	def_text: str_pointer;
	kind: macro_kinds;
	name_len: string_index;
	name_text: packed array [1..1] of char
      end;


(**********   Error Codes   **********)

type
    error_codes =
      ( er_tok_len, er_lit_len, er_cmt_len, er_lit_eol, er_cmt_eof,
	er_arg_eof, er_arg_num, er_bad_sym, er_und_mac, er_few_arg,
	er_inp_ofl, er_tmp_ofl, er_arg_ind, er_bad_num, er_lib_fil,
	er_str_eof, er_err_mac );
