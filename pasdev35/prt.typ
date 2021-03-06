const
    max_text = 500;

type
    text_index = 0 .. max_text;

const
    nul = chr(0);				(* ascii ^n^u^l *)
    dollar_flag = chr(1);			(* ascii ^s^o^h *)
    tab = chr(9);				(* ascii ^h^t *)
    cmt_brk = chr(10);				(* ascii ^l^f *)
    eol = chr(13);				(* ascii ^c^r *)
    eof_ch = chr(28);				(* ascii ^f^s *)

type
    token_kind =
      ( beginsy, ofsy, loopsy, repeatsy, endsy, untilsy, dosy, colonsy,
	exitsy, semicolonsy, thensy, elsesy, commentsy, lparensy, rparensy,
	ifsy, procedursy, functionsy, constsy, typesy, varsy, recordsy,
	labelsy, commasy, publicsy, externalsy, staticsy, forwardsy,
	externsy, fortransy, orsy, andsy, orifsy, andifsy, casesy, eofsy,
	initprocsy, pascalsy, dollarsy, lbracketsy, rbracketsy, etc );

    space_codes =
      ( space, nospace, comment );

    stack_flags =
      ( one_flag, many_flag, then_flag );

    flag_set = set of stack_flags;

external procedure error ( msg: string );
  