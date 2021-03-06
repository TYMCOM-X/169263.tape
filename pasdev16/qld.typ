(* LINE DESIGNATOR defintions.  An LD is represented as a linked list of nodes
   giving the atomic parts of a line designator such as predicates, symbolic
   addresses, and punctuation. Details may be found in the 'Definition of QED.' *)


TYPE
  LD_FORMS =
     (	NUM_LD,				(* LINE OFFSET FROM LAST ADDRESSED LINE *)
        FORWARD_LD,                     (* ADDRESS SPECIFIED BY PREDICATE *)
        BACKWARD_LD,                    (* ADDRESS SPECIFIED BY ^PREDICATE *)
	DOT_LD,				(* CURRENT LINE *)
	DOLLAR_LD,			(* LAST LINE *)
	STAR_LD,			(* FIRST LINE OF RANGE *)
	COMMA_LD	);		(* RANGE SEPARATOR *)


  LDCHAIN = ^ LDPART;			(* NODE ON LDCHAIN LIST *)
  LDPART =
    RECORD
      NEXT: LDCHAIN;			(* POINTER TO NEXT NODE ON LIST, OR NIL *)
      CASE LDKIND : LD_FORMS OF		(* VARIOUS ATOMIC PARTS *)
	NUM_LD:	     (  OFFSET: QLNOFFSET  );	(* OFFSET OR ABSOLUTE LINENO *)
	FORWARD_LD,BACKWARD_LD:  (  PRED: SPRED  );	(* PREDICATES TO SEARCH FOR *)
	DOT_LD,DOLLAR_LD,STAR_LD,COMMA_LD: ()
    END;

  LDCOUNT = 0..2;			(* NUMBER OF ADDRESSES IN AN LD *)
  LDRANGE =				(* EVALUATED LD CHAIN, GIVES LIMITS OF RANGE *)
    RECORD
      LBOUND: QLINENO;			(* FIRST LINE ADDRESSED *)
      HBOUND: QLINENO			(* SECOND LINE, SAME AS FIRST IF LD NOT RANGE *)
    END;
   