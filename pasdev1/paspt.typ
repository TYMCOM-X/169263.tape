(* Basic symbol and operator type definitions *)

$include pasbnf.sym

type
  symbol_set = set of badsymbol..nonterminal;

type
  operators =
      (	mul, rdiv, idiv, expon, imod, andop, andifop, plus, minus, orop,
	orifop, catop, leop, ltop, gtop, geop, eqop, neop, inop,
	readsy, writesy, readlnsy, writelnsy, readrnsy, writernsy,
	getstrsy, putstrsy, noop );


(* Token descriptor *)

type
  parse_node = ^ token_type;		(* token's are the leaves of the parse tree *)

  token_type =
      packed record
	next: parse_node;		(* next node at current level *)
	defn: parse_node;		(* defining or qualifying nodes of current node;
					   chains through parse_node^.next *)
	source: source_id;		(* id of line on which token appears *)
	column: line_index;		(* starting position of token in line *)
	length: line_index;		(* length of token text in line *)
	dummy: boolean;			(* true => token has no text *)
	case sym: symbols of		(* class token belongs to *)
	  ident:
	    (  name: nam  );		(* name table entry giving name *)
	  intconst, realconst, stringconst:
	    (  value: val  );		(* constant table entry giving value *)
	  notsy, powerop, mulop, addop, relop, iosy:
	    (  op: operators  )		(* operator code, undiscriminated *)
      end;
    