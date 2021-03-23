$TITLE INDSPL Semantic line splitting module for INDENT

module INDSPL;

$HEADER INDSPL.HDR
$PAGE GLOBAL declarations and stuff

$INCLUDE INDENT.TYP

$INCLUDE INDSPL.INC

(* initializing constants for split sets *)

const
  INI_SPL_BEFORES: SYMSET :=
    [UNTILSY, ELSESY, NUMERIC, LABELSY, TYPESY, EXTERNALSY,
     CLASSY, CASESY, VARSY, PROCSY, FUNCTIONSY, CONSTSY, ENDSY, FORSY,
     W_DOSY, EXITSY, ITERSY, DIRECTIVE, NUMERIC, IFSY
    ];

  INI_SPL_AFTERS: SYMSET :=
    [SEMISY, BEGINSY, ITERSY, COLONSY, CLOSE_COM,
     RECORDSY, DIRECTIVE
    ];

  INI_BL_BEFORES: SYMSET :=
    [SA_COM];

  INI_BL_AFTERS: SYMSET :=
    [SA_COM];

static var
  SPL_BEFORES,				(* tokens to split before *)
  SPL_AFTERS,				(* and after *)
  BL_BEFORES,				(* tokens to force blank line before *)
  BL_AFTERS: SYMSET;			(* and after *)

  PASSED_ONE,				(* true if DO_SPLIT passed a NEVERBEFORE *)
  PAGE_PRECEDES,			(* true if a $PAGE was the last passed *)
  DID_SPLIT: boolean;			(* true if line split on prev. token *)
  OFFSET: LINE_IDX;			(* for adjusting TOK_IND on splitlines*)
  PAREN_LEVEL: LINE_IDX;		(* to help detect end of proc. decl. *)
  BEGIN_LEVEL: LINE_IDX;		(* to detect the first 'begin' of body*)

const
  DECLS: SYMSET :=			(* tokens which may have stor. class *)
    [VARSY, CONSTSY, PROCSY, FUNCTIONSY];

  (* set of compound statement header keywords which may be
  preceded by a 'case' label (thus we must turn off splitting) *)
  CSHKEYWDS: SYMSET :=
    [ITERSY, IFSY, CASESY, FORSY, W_DOSY];

  SPREADERS: SYMSET :=			(* tokens taking blank line before
					them with SPREAD option *)

    [LABELSY, TYPESY, CONSTSY, VARSY, PROCSY, FUNCTIONSY,
     EXTERNALSY, CLASSY];

  NEVERBEFORES: SYMSET :=		(* never split before these *)
    [COMMASY, SEMISY, OPEN_COM, PSEUDOSY];
$PAGE DO_SPLIT routine to insure minimum spacing
procedure DO_SPLIT ( HOW_MANY: LINE_IDX );

(* DO_SPLIT guarantees that the line is split after THIS_SYM, by insuring
   that EOL_CT is >= HOW_MANY. Thus, the caller can guarantee a blank
   line before THIS_SYM by passing HOW_MANY = 2.  In addition,
   DO_SPLIT will set DID_SPLIT true if it really did split the line,
   so that the caller in INDSPL can fix up ORIG_IND and TOK_IND to
   give the illusion that the token before which the split was made
   appeared first on an input line with a zero indentation. *)

  begin
  if EOL_CT < HOW_MANY then
    begin
    while (EOL_CT = 0) and (NEXT_SYM in NEVERBEFORES) do
      begin
      resume (FORMAT);			(* hold off on the split *)
      PAGE_PRECEDES := false;		(* if it did, it doesn't now *)
      PASSED_ONE := true
      end;
    DID_SPLIT := (EOL_CT = 0) or DID_SPLIT; (* if new split, set DID_SPL*)
    if not NEXT_PAGE and
	not PAGE_PRECEDES then
      EOL_CT := max (EOL_CT, HOW_MANY);
    end
  end (* procedure DO_SPLIT *);
$PAGE INDSPL the coroutine SPLITR
public procedure INDSPL;

(* INDSPL is the coroutine living in the environment SPLITR. It uses
   four sets of type TOKENTYPE to determine which tokens need line
   splits or blank lines before or after them.  Since, for a given
   token, this decision depends on context, INDSPL modifies these
   sets on the fly. For example, a line is NOT split before the
   word 'Procedure' if (1)a storage class precedes it; (2)within
   a type definition.
   INDSPL is a loop, determining splits after THIS_SYM in a single
   top-to-bottom pass. First we do all context adjustments necessary.
   Then we determine split/blank after THIS_SYM, and split/blank
   before NEXT_SYM. The 'resume' to FORMAT is at the bottom of 
   the loop. *)


  procedure SPL_INIT;			(* reinitializes the splitting sets *)
    begin
    SPL_BEFORES := INI_SPL_BEFORES;
    SPL_AFTERS := INI_SPL_AFTERS;
    BL_BEFORES := INI_BL_BEFORES;
    BL_AFTERS := INI_BL_AFTERS;
    if SPREAD then
      begin
      if BEGIN_LEVEL = 0 then
	BL_BEFORES := BL_BEFORES + [BEGINSY]; (* blank before level 0 begin *)
      BL_BEFORES := BL_BEFORES + SPREADERS
      end
    end (* procedure SPL_INIT *);



  label 2;				(* for treating numerics like case labels *)

  begin					(* procedure INDSPL *)
  DID_SPLIT := false;
  OFFSET := 0;
  BEGIN_LEVEL := 0;
  SPL_INIT;

  detach;				(* once for initializing CREATE *)
  resume (READER);			(* second, go back to reader *)

    loop

    (* The best way to describe the actions of this loop is in terms of
       the state described by the sets of splitting tokens. The 'default'
       state (that set by the INI's) splits before CSH keywords and
       'section' keywords (things like 'var', 'external', etc.). Also,
       the default state splits BEFORE NUMERICS, assuming they are
       labelled statements. Every semicolon restores the default state
       (except within procedure headers, handled as a special case below).
       The appearance of a numeric token itself does nothing to the
       current state. A colon, if within the context of a case statement
       (and here we look at the global TOP_FLAG in FORMAT), disables
       its own SPLIT_AFTER, and the SPLIT_BEFORE for the compound
       statement keyword headers which may follow it. 
       The appearance of any other token will disable the line splitting
       before NUMERIC and after COLON; the next semicolon will reinstate
       this processing. *)

      case THIS_SYM of

	ARRAYSY,FILSETSY:
	  SPL_AFTERS := SPL_AFTERS - [OFSY];

	COLONSY: if (TOP_FLAG = CASE_FLAG) then
	  begin
	  SPL_AFTERS := SPL_AFTERS - [COLONSY];
	  SPL_BEFORES := SPL_BEFORES - CSHKEYWDS
	  end;

	ENDSY: 
	  begin
	  if NEXT_SYM = SEMISY then
	    SPL_AFTERS := SPL_AFTERS - [ENDSY];
	  BEGIN_LEVEL := max(BEGIN_LEVEL -1, 0)
	  end;

	SEMISY: begin
	  SPL_INIT;
	  if NEXT_SYM = SEMISY then
	    SPL_AFTERS := SPL_AFTERS - [SEMISY]
	end;

	NUMERIC: if (TOP_FLAG = CASE_FLAG) then
	  goto 2;			(* else do nothing, context already set **)

	OPEN_COM,CLOSE_COM,DIRECTIVE:;	(* and let simple comments slip by *)

	SA_COM: 			(* blank after last one *)
	  while NEXT_SYM = SA_COM do
	    resume (FORMAT);		(* munch until last line *)

	PROCSY, FUNCTIONSY: begin
	  PAREN_LEVEL := 0;
	    repeat
	      if THIS_SYM = LPARENSY then
		PAREN_LEVEL := PAREN_LEVEL + 1
	      else if THIS_SYM = RPARENSY then
		PAREN_LEVEL := PAREN_LEVEL - 1;
	      resume (FORMAT)		(* no line splitting within pr decls *)
	    until (PAREN_LEVEL <= 0) and
		  (THIS_SYM in [SEMISY,ENDSY,RPARENSY]);

	  SPL_INIT;
	end;

	(* The following tokens may be considered 'statement terminators'
	   for our purposes, since a declared label may follow them.
	   Thus we restore the 'default' state. Notice that we don't
	   want to split after an 'if' if followed by 'else'. *)

	BEGINSY, ITERSY:
	  begin
	  BEGIN_LEVEL := BEGIN_LEVEL + 1;
	  SPL_INIT
	  end;

	CASESY:
	  begin
	  SPL_AFTERS := SPL_AFTERS + [OFSY];
	  if BEGIN_LEVEL > 0 then
	    BEGIN_LEVEL := BEGIN_LEVEL + 1
	  end;

	DOSY, THENSY, ELSESY: begin
	  SPL_INIT;
	  if THIS_SYM = ELSESY then
	    SPL_BEFORES := SPL_BEFORES - [IFSY]
	end;

	EXITSY:				(* turn off split before if *)
	  SPL_BEFORES := SPL_BEFORES - [IFSY];

	(* Anything else signals the start of a declaration section,
	   a declaration, or a simple statement. So we disable the
	   declared label and colon splitting. Also, we could be
	   looking at a storage class declaration, so remove those
	   guys that take a storage class (DECLS). Finally, we may
	   want to split the line anyway if we're looking at a
	   CSH keyword (maybe not, also, if we saw a declared label
	   earlier; the 'colon' already disabled this), but if not
	   then we DO want to reinstate CSH keyword splitting. *)

	OTHERS: begin
	  2:SPL_BEFORES := SPL_BEFORES - DECLS - [NUMERIC];
	  SPL_AFTERS := SPL_AFTERS - [COLONSY];
	  BL_BEFORES := BL_BEFORES - DECLS;
	  if not (THIS_SYM in CSHKEYWDS) then
	    SPL_BEFORES := SPL_BEFORES + CSHKEYWDS
	end

      end; (* case *)

      PASSED_ONE := false;
      PAGE_PRECEDES := (THIS_SYM = DIRECTIVE) andif
			(substr(LINE,CURSTART,5) = '$PAGE');
      if (NEXT_SYM in BL_BEFORES)
 	then DO_SPLIT(2)
      else if NEXT_SYM in SPL_BEFORES 
	then DO_SPLIT(1);

      if THIS_SYM in BL_AFTERS then
	DO_SPLIT(2)
      else if THIS_SYM in SPL_AFTERS then
	DO_SPLIT(1);

      if not DID_SPLIT and (EOL_CT > 0) then
	OFFSET := 0;			(* 'natural' endofline, reset offset *)

      if not PASSED_ONE then
	resume (FORMAT);
      if DID_SPLIT then begin		(* we split, massage token values *)
	ORIG_IND := 0;			(* this line lines up to minimum *)
	OFFSET := TOK_IND;		(* offset for future tokens *)
	end;

      DID_SPLIT := false;		(* reset for next pass *)
      TOK_IND := TOK_IND - OFFSET	(* finally, adjust TOK_IND if needed *)
    end (* loop *)
  end (* procedure INISPL and module *).
  