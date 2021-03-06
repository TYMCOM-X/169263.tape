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
    ( OFSY, TOSY, INSY, IFSY, ORSY, DOSY, BYSY,
      VARSY, ENDSY, ANDSY, FORSY, SETSY, DIVSY, MODSY, NOTSY,
      TYPESY, THENSY, ELSESY, EXITSY, WITHSY, LOOPSY, ORIFSY,
      CASESY, GOTOSY, FILESY, STOPSY,
      CONSTSY, BEGINSY, WHILESY, UNTILSY, LABELSY, ANDIFSY, ARRAYSY, ALGOLSY,
      STATICSY, REPEATSY, PASCALSY, MODULESY, EXTERNSY, PACKEDSY,
      RECORDSY, PUBLICSY, DOWNTOSY, OTHERSSY, STRINGSY, RETURNSY,
      FORTRANSY, FORWARDSY,
      FUNCTSY, EXTERNALSY,
      PROCSY,
      ETC, NUMERIC, COLONSY, OPENPARENSY, CLOSEPARENSY, COMMASY, SYMSY,
      OPENBRACKSY, CLOSEBRACKSY, ASSIGNSY, SEMICOLONSY, DIRECTIVE,
      OPEN_COM, CLOSE_COM, SA_COM, CONT_COM, INITPROCSY, EQUALSY,
      ELLIPSISY, PERIODSY, LITERALSY
    );

  TOKSET = set of TOKENTYPE;

  SYMBOLS = 
    ( COLON_OP, EQ_OP, LT_OP, GT_OP, MUL_OP, ADD_OP, SUB_OP,
      DIV_OP, RPAREN_OP, LPAREN_OP, RBRACK_OP, LBRACK_OP, SEMI_OP, UPARR_OP,
      CLOSE_COM_OP, OPEN_COM_OP, COMMA_OP, PERIOD_OP,
      ELLIPS_OP, GE_OP, LE_OP, NE_OP, CONCAT_OP, EXP_OP, ASSI_OP,
      NO_GOOD
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
    SATEXT: packed array[1..10]
      of char				(* the text (variable NEW call) *)
    end;

(* end of INDENT.TYP *)
    