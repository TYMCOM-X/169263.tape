(* INDENT.TYP Type definitions for INDENT program *)

const
  LINLEN := 500;			(* max size of input lines *)
  NEGLINLEN := -500;			(* negative of LINLEN *)
  STK_MAX := 200;			(* max size of INDSTK stack *)
  MAX_OPT := 5;				(* indent options 1..MAX_OPT *)
  MAX_SYM := 9;				(* number of 2-character specialsyms *)

type
  TEXTLINE = packed array[1..LINLEN]
    of char;
  LINE_IDX = NEGLINLEN .. LINLEN;

$INCLUDE COROUT.INC

(* Notice that the coroutine control routines are decapitalized
   throughout the INDENT code. *)


type
  TOKENTYPE =
    ( AND_OP, ANDIF_OP, OR_OP, ORIF_OP, NOT_OP, ARRAY_OP, BEGIN_OP,
      CASE_OP, COND_OP, CONST_OP, MODULE_OP, DATAMOD_OP, PROGRAM_OP,
      DIV_OP, MOD_OP, DO_OP, DOWNTO_OP, TO_OP, ELSE_OP, END_OP, EXIT_OP,
      EXTERNAL_OP, PUBLIC_OP, STATIC_OP, FILE_OP, SET_OP, FOR_OP,
      FORWARD_OP, EXTERN_OP, FUNCTION_OP, GOTO_OP, IF_OP, IN_OP,
      LABEL_OP, LOOP_OP, REPEAT_OP, OF_OP, ON_OP, OPTION_OP, OTHERS_OP,
      PACKED_OP, PREC_OP, PROCED_OP, RECORD_OP, RETURN_OP, STOP_OP,
      STRING_OP, THEN_OP, TYPE_OP, UNTIL_OP, VAR_OP, WHILE_OP, WITH_OP,
      ASSI_OP, EXPO_OP, CONCAT_OP, NE_OP, LE_OP, GE_OP, LT_OP,
      GT_OP, AST_OP, PLUS_OP, SUB_OP, DIVIDE_OP, ELLIPS_OP, COLON_OP, EQ_OP,
      LPAREN_OP, RPAREN_OP, COMMA_OP, LBRACK_OP, RBRACK_OP, SEMI_OP,
      UPARR_OP, PERIOD_OP, OPEN_COM_OP, CLOSE_COM_OP, ETC_OP,
      NO_GOOD
    );

  TOKSET = set of TOKENTYPE;

  SYMBOLS = 
    ( LOGCONSY, NOTSY, ARRAYSY, BEGINSY, CASESY, CONDSY, CONSTSY,
      COMPILSY, ARITHWORDSY, DOSY, TOSY, ELSESY, ENDSY, EXITSY, EXTERNALSY,
      CLASSY, FILSETSY, FORSY, PSEUDOSY, FUNCTIONSY, GOTOSY, IFSY,
      INSY, LABELSY, ITERSY, OFSY, ONSY, OPTIONSY, OTHERSY, PACKEDSY,
      PRECSY, PROCSY, RECORDSY, TERMINSY, STRINGSY, THENSY, TYPESY,
      UNTILSY, VARSY, W_DOSY, ASSIGNSY, ARITHSY, ELLIPSISY, COLONSY, EQUALSY,
      LPARENSY, RPARENSY, COMMASY, LBRACKSY, RBRACKSY, SEMISY,
      DEREFSY, PERIODSY, NUMERIC, LITERALSY, DIRECTIVE, OPEN_COM,
      CLOSE_COM, SA_COM, CONT_COM, ETC
    );

  SYMSET = set of SYMBOLS;

  OPTION = 1..MAX_OPT;
  OPTIONSET = set of OPTION;

  STK_FLAG = (ONE_FLAG,			(* single statement mark *)
	      MANY_FLAG,		(* compound statement mark *)
	      THEN_FLAG,		(* special flag for 'then' *)
	      CASE_FLAG			(* special flag for 'case' *)
	      );

  FLAG_SET = set of STK_FLAG;

  STK_REC = record
    STK_MARK: STK_FLAG;
    STK_IND: LINE_IDX			(* save former indent and call flag *)
  end (* STK_REC def'n *);

  CASE_TYPE = (NO_CHANGE, DECAP, CAP, MIXED);

  SAPTR = ^SAREC;			(* pointer to comment records *)

  SAREC = record			(* standalone comment record *)
    SALEN,				(* length of comment text *)
    SAEOL,				(* number of EOL's following line *)
    SAOFFSET: LINE_IDX;			(* relative offset WRT first line *)
    SANEXT: SAPTR;			(* next in list *)
    SATEXT: packed array[1..*]
      of char				(* the text (variable NEW call) *)
    end;

(* end of INDENT.TYP *)
    