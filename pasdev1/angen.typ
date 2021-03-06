(**********************************************************************)
	   (* analyzer semantic routine type definitions *)

(* the first analyzer pass constructs a symbol table and grammar representation
with the following structure:  the symbol table is an unbalanced binary
tree and contains records for terminal symbols, non-terminal symbols,
and $s/$f codes.  non-terminal records contain a pointer to their definition
in the grammar.  the definition consists of a singly linked list of
records for symbols, metabrackets, and alternative lists occuring in
their definition.  some number of initial symbol records may be marked
as yparts, indicating that the non-terminal has alternate definitions.
metabrackets and alternatives also contain a pointer to their definition,
in the same form as a non-terminal definition list.  finally, symbol
records for $s/$f codes, terminals, and non-terminals are on singly linked lists in
order, aiding the generation of parsing tables and associated information
in the second pass.     *)

const
  maxgencode = 1000;				(*max number of unique $s/$f codes*)
  maxsymlen = 20;				(*max length of symbol text in table*)
  maxvindex = 1000;				(*max number of terminal symbols*)
  errorpmret = -3;				(* special parsing machine addresses *)
  falsepmret = -2;
  truepmret = -1;

type
  pmaddr = -3..1000;
  gramrecptr = ^gramrec;			(*pointer to record in grammar definition*)
  symrecptr = ^symrec;				(*pointer to record in symbol table*)

  gramrectype = (gramsym, grammb, gramalt);	(*possible grammar records*)

  gramrec = record
    nextptr: gramrecptr;			(*ptr to next record in definition*)
    instaddr: -3..1000;				(*address of corresponding parsing machine
						instruction, assigned in first pass*)
    case rectype: gramrectype of
      gramsym: ( ypart: boolean;		(*ypart/zpart indication*)
		 symptr: symrecptr;		(*ptr to symtab entry for this symbol*)
		 at,af: pmaddr;
		 sgptr,
		 fgptr: symrecptr);		(*ptrs to symrec's of $s/$f codes*)
      grammb,
      gramalt: ( defnptr: gramrecptr;		(*ptr to this mb/alt definition*)
		 closure: boolean)		(*kleene star flag (mb only)*)
  end;

  symrectype = (symundef, symter, symnonter, symgencode);
  symtexttype = (symlit, symclass);

  symrec = record
    aptr, zptr: symrecptr;			(*binary tree pointers*)
    symtext: string[maxsymlen];			(*text of symbol*)
    scalar_name: string[maxsymlen];		(* for terminals and nonterminals, name of symbol value *)
    texttype: symtexttype;			(*flags nature of symbol text*)
    nextsymptr: symrecptr;			(*chain word for lists of the following variants*)
      (* symrec's should be new'ed without tagfield, as rectype changes *)
    case rectype: symrectype of
      symter: ( vindex: 1..maxvindex);		(*terminal symbol vocabulary index*)
      symnonter: ( defnptr: gramrecptr;		(*pointer to definition of symbol*)
		   scalar_chain: symrecptr );	(* ptr to next named nonterminal *)
      symgencode: ( gencode: 1..maxgencode)
  end;

    