$TITLE pasexp - PASCAL compiler expression evaluation
$OPTIONS SPECIAL, NOCHECK

(*	Conditional compilation switches used:

	Y4:  Unless disabled, support double precision reals  *)

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASCMP.INC

$INCLUDE PASSTR.INC

$INCLUDE PASCG.INC

$INCLUDE PASLEX.INC
$PAGE declarations

external function finscope (fcp: ctp): boolean;

external procedure selector (fsys: setofsys; fcp: ctp);

external procedure constant (fsys:setofsys; var fsp:stp; var fvalu:valu);

external procedure call (fsys: setofsys; fcp: ctp);

$PAGE makereal
public procedure makereal(var fattr: attr);

var i: integer;

begin
  if fattr.typtr=intptr then
    with fattr do begin
      if kind=cst then
	with cval do begin
	  i := ival;
	  new (valp,reel);
	  valp^.rval := i;
	  (* Assume RVAL2 zeroed by NEW *)
	  valp^.selfcsp := nil
	end
      else begin
	load(fattr);
	mac3(201b (*MOVEI*),tac,reg);
(*$Y4	IF DBLREAL THEN BEGIN
	  SUPPORT (DCONVERTINTEGERTOREAL);
	  REG := REG + 1;
	END
	ELSE	*)
	  support(convertintegertoreal)
      end;
      fattr.typtr := realptr
    end;
  if gattr.typtr=intptr then
    makereal(gattr)
end;

type
    valuekind = (onregc, onfixedregc, truejmp, falsejmp);

(*$Y4 
TYPE INST_ARRAY = ARRAY[BOOLEAN] OF INSTRANGE;
STATIC VAR
  ADD: INST_ARRAY := (144B (* FADR *), 110B (* DFAD *) );
  SUBTRACT: INST_ARRAY := (154B (* FSBR *), 111B (* DFSB *) );
  MULTIPLY: INST_ARRAY := (164B (* FMPR *), 112B (* DFMP *) );
  DIVIDE: INST_ARRAY := (174B (* FDVR *), 113B (* DFDV *) );	*)

$PAGE expression
public procedure expression (fsys: setofsys; fvalue:valuekind);

var
    lattr: attr;
    lop: operator;
    lsize: addrrange;
    loffset: integer;
    default: boolean;
    boolregc,testregc:acrange;
    linstr,linstr1: instrange;
    lregc1,lregc2: acrange;
    setinclusion : boolean;
    ldplmt: addrrange;
    stringcomparison: boolean;

  procedure changebool(var finstr: instrange);

  begin
    if (finstr>=311b) andif (finstr<=313b) then
      finstr := finstr+4			(*CAML,CAME,CAMLE --> CAMGE,CAMN,CAMG*)
    else if (finstr>=315b) andif (finstr<=317b) then
      finstr := finstr-4 (*SAME IN THE OTHER WAY*);
  end;

$PAGE searchcode
  procedure searchcode(finstr:instrange; fattr: attr);

    procedure changeoperands(var finstr:instrange);

    begin
      if finstr=311b				(*CAML*)
      then
	finstr := 317b				(*CAMG*)
      else if finstr = 313b			(*CAMLE*)
      then
	finstr := 315b				(*CAMGE*)
      else if finstr=315b			(*CAMGE*)
      then
	finstr := 313b				(*CAMLE*)
      else if finstr = 317b			(*CAMG*)
      then
	finstr := 311b				(*CAML*)
      else if finstr = 420b			(*ANDCM*)
      then
	finstr := 410b				(*ANDCA*)
      else if finstr = 410b			(*ANDCA*)
      then
	finstr := 420b (*ANDCM*);
    end;

  begin
    with gattr do
      if fattr.kind = expr then begin
	makecode(finstr,fattr.reg,gattr);
	reg := fattr.reg
      end
      else if kind = expr then begin
	changeoperands(finstr);
	makecode(finstr,reg,fattr)
      end
      else if (kind=varbl) andif ((packfg<>notpack) orif (indexr>regin) andif
	(indexr<=regcmax) andif ((fattr.indexr<=regin) orif
	  (fattr.indexr>regcmax))) then begin
	    load(gattr);
	    changeoperands(finstr);
	    makecode(finstr,reg,fattr)
	  end
	  else begin
	    load(fattr);
	    makecode(finstr,fattr.reg,gattr);
	    reg := fattr.reg
	  end;
  end;

$PAGE simpleexpression, term, factor
  procedure simpleexpression(fsys: setofsys);

  var
      lattr: attr;
      lop: operator;
      signed : boolean;
      saveregc: acrange;
      lcix1 : coderange;

    procedure term(fsys: setofsys);

    var
	lattr: attr;
	lop: operator;
	t1, t2 : integer;
	lcix2: coderange;

      procedure factor(fsys: setofsys);

      var
	  lcp: ctp;
	  lvp: csp;
	  varpart: boolean;
	  cstpart: set of 0..71;
	  lsp: stp;
	  rangepart: boolean;
	  lrmin: integer;

      begin
	if not (sy in facbegsys) then begin
	  errandskip(173,fsys or facbegsys);
	  gattr.typtr := nil
	end;
	if sy in facbegsys then begin
	  case sy of
	  (*ID*)
	    ident: begin
	      srchid([konst,vars,field,func],lcp);
	      insymbol;
	      if lcp^.klass = func then begin
		if (sy <> lparent) andif finscope (lcp) then
		  selector (fsys, lcp)
		else
		  call (fsys, lcp)
	      end
	      else if lcp^.klass = konst then
		with gattr, lcp^ do begin
		  typtr := idtype;
		  kind := cst;
		  cval := values
		end
	      else
		selector(fsys,lcp);
	      if gattr.typtr <> nil then	(*ELIM. SUBR. TYPES TO*)
		with gattr, typtr^ do		(*SIMPLIFY LATER TESTS*)
		  if form = subrange then
		    typtr := rangetype
	    end;
	    (*CST*)
	    intconst: begin
	      with gattr do begin
		typtr := intptr;
		kind := cst;
		cval := val;
	      end;
	      insymbol
	    end;
	    realconst: begin
	      with gattr do begin
		typtr := realptr;
		kind := cst;
		cval := val
	      end;
	      insymbol
	    end;
	    stringconst: begin
	      with gattr do begin
		constant(fsys,typtr,cval) ;
		kind := cst ;
	      end;
	    end;
	    (* ( *)
	    lparent: begin
	      insymbol;
	      expression(fsys or [rparent],onregc);
	      if sy = rparent then
		insymbol
	      else
		error(152)
	    end;
	    (* NOT *)
	    notsy: begin
	      insymbol;
	      factor(fsys);
	      if gattr.typtr = boolptr then begin
		load(gattr);
		mac3(411b (*ANDCAI*),regc,1)
	      end
	      else begin
		error(359);
		gattr.typtr := nil
	      end;
	    end;
	    (*[*)
	    lbrack: begin
	      insymbol;
	      cstpart := [ ];
	      varpart := false;
	      rangepart:=false;
	      new(lsp,power);
	      with lsp^ do begin
		elset:=nil;
		size:= 2
	      end;
	      if sy = rbrack then begin
		with gattr do begin
		  typtr:=lsp;
		  kind:=cst;
		  new(lvp,pset);
		  lvp^.pval := cstpart;
		  cval.valp := lvp
		end;
		insymbol
	      end
	      else begin
		loop
		  incrementregc;
		  incrementregc;
		  expression(fsys or [comma,rbrack,elipsis],onregc);
		  if gattr.typtr <> nil then
		    if gattr.typtr^.form <> scalar then begin
		      error(461);
		      gattr.typtr := nil
		    end
		    else if comptypes(lsp^.elset,gattr.typtr) then begin
		      if gattr.kind = cst then begin
			if gattr.typtr=charptr then
			  gattr.cval.ival := gattr.cval.ival-offset;
			if (gattr.cval.ival<0) or (gattr.cval.ival>basemax) then
			  error(352) ;
			cstpart := cstpart or [gattr.cval.ival];
			regc := regc - 2;
			if sy=elipsis then begin
			  rangepart:=true;
			  lrmin:=gattr.cval.ival
			end
			else if rangepart then begin
			  lrmin:=lrmin+1;
			  while (lrmin<gattr.cval.ival) do begin
			    cstpart:=cstpart or [lrmin];
			    lrmin:=lrmin+1
			  end;
			  rangepart:=false
			end
		      end
		      else begin
			if (sy=elipsis) or rangepart then begin
			  error(207);
			  rangepart := not rangepart
			end;
			load(gattr);
			mac3(210b (*MOVN*),regc,regc);
			regc := regc - 1;
			mac3(515b (*HRLZI*),regc-1,400000b);
			mac3(400b (*SETZ*),regc,0);
			if gattr.typtr=charptr then
			  mac4(246b (*LSHC*),regc-1,regc+1,offset)
			else
			  mac4(246b (*LSHC*),regc-1,regc+1,0);
			if varpart then begin
			  mac3(434b (*IOR*),regc-3,regc-1);
			  mac3(434b (*IOR*),regc-2,regc);
			  regc := regc-2;
			end
			else
			  varpart := true;
			gattr.kind := expr;
			gattr.reg := regc
		      end;
		      lsp^.elset := gattr.typtr;
		      gattr.typtr :=lsp
		    end
		    else
		      error(360);
		exit if not(sy in [comma,elipsis]);
		  insymbol
		end;
		if sy = rbrack then
		  insymbol
		else
		  error(155);
		if varpart then begin
		  if cstpart <> [ ] then begin
		    new(lvp,pset);
		    lvp^.pval := cstpart;
		    lvp^.selfcsp := nil;
		    gattr.cval.valp := lvp;
		    gattr.kind := cst;
		    mac3(434b (*IOR*),regc,0);
		    mac3(434b (*IOR*),regc-1,0);
		    depcst(pset,gattr);
		    gattr.kind := expr;
		    gattr.reg := regc;
		  end
		end
		else begin
		  new(lvp,pset);
		  lvp^.pval := cstpart;
		  lvp^.selfcsp := nil;
		  gattr.cval.valp := lvp
		end
	      end;
	    end
	  end (*CASE*);
	  iferrskip(166,fsys)
	end;
	(*IF SY IN FACBEGSYS*)
      end (*FACTOR*);

$PAGE expsupport
      procedure expsupport (fsupport : supports);

      var par_reg: acrange;

      begin
	load (lattr);
	load (gattr);
(*$Y4	IF DBLREAL ANDIF (GATTR.TYPTR = REALPTR) THEN
	  PAR_REG := GATTR.REG-1
	ELSE	*)
	  par_reg := gattr.reg;
	mac3 (201b (*MOVEI*),hac,par_reg);
(*$Y4	IF DBLREAL ANDIF (LATTR.TYPTR = REALPTR) THEN
	  PAR_REG := LATTR.REG - 1
	ELSE	*)
	  par_reg := lattr.reg;
	mac3 (201b (*MOVEI*),tac,par_reg);
	support (fsupport)
      end;					(*EXPSUPPORT*)

$PAGE term - body
    begin
    (*TERM*)
      factor(fsys or [mulop]);
      while sy = mulop do begin
	if op in [rdiv,idiv,imod] then
	  load(gattr);				(*BECAUSE OPERANDS ARE NOT
						 ALLOWED TO BE CHOSEN*)
	if op = andop then
	  if gattr.typtr <> nil then
	    if gattr.typtr^.form = power then
	      if not specialsw then
		error (466);
	if op = andifop then
	  if gattr.typtr = boolptr then begin
	    load (gattr);
	    mac3 (322b (*JUMPE*),gattr.reg,0);
	    regc := gattr.reg - 1;
	    lcix2 := cix
	  end
	  else if gattr.typtr <> nil then begin
	    error (369);			(*MUST BE BOOLEAN*)
	    gattr.typtr := nil
	  end;
	lattr := gattr;
	lop := op;
	insymbol;
	factor(fsys or [mulop]);
	if (lattr.typtr <> nil) andif (gattr.typtr <> nil) then
	  case lop of
	  (***)
	    mul:
	      if (lattr.typtr = intptr) andif (gattr.typtr = intptr) then
		searchcode(220b (*IMUL*),lattr)
	      else begin
(*$Y4		IF DBLREAL ANDIF ((LATTR.TYPTR=REALPTR) ORIF (GATTR.TYPTR=REALPTR))
		  ANDIF (LATTR.TYPTR <> GATTR.TYPTR) THEN ERROR (565);	*)
		makereal(lattr);
		if (lattr.typtr = realptr) andif (gattr.typtr = realptr) then
(*$X4		  SEARCHCODE(164B (*FMPR*),LATTR)	*)
(*$Y4		  SEARCHCODE (MULTIPLY[DBLREAL],LATTR)	*)
		else if comptypes(lattr.typtr,gattr.typtr) andif
		  (lattr.typtr^.form=power) then
		    searchcode(404b (*AND*),lattr)
		  else begin
		    error(311);
		    gattr.typtr := nil
		  end
	      end;
	      (* / *)
	    rdiv: begin
(*$Y4	      IF DBLREAL ANDIF ((LATTR.TYPTR <> REALPTR) ORIF (GATTR.TYPTR <> REALPTR))
		THEN ERROR (565);	*)
	      makereal(lattr);
	      if (lattr.typtr = realptr) andif (gattr.typtr = realptr) then
(*$X4		SEARCHCODE(174B (*FDVR*),LATTR)	*)
(*$Y4		SEARCHCODE (DIVIDE[DBLREAL],LATTR)	*)
	      else begin
		error(311);
		gattr.typtr := nil
	      end
	    end;
	    (*DIV*)
	    idiv:
	      if (lattr.typtr = intptr) andif (gattr.typtr = intptr) then
		searchcode(230b (*IDIV*),lattr)
	      else begin
		error(311);
		gattr.typtr := nil
	      end;
	      (*MOD*)
	    imod:
	      if (lattr.typtr = intptr) and (gattr.typtr = intptr) then begin
		searchcode(230b (*IDIV*),lattr);
		(* IDIV PUTS REMAINDER IN AC+1, SO
		  MOVE IT TO AC SO THINGS LIKE PROC
		  CALLS DON'T GET CONFUSED*)
		mac3(200b (*MOVE*),gattr.reg,gattr.reg+1)
	      end
	      else begin
		error(311);
		gattr.typtr := nil
	      end;
	      (* AND *)
	    andop:
	      if comptypes(lattr.typtr,gattr.typtr) and
		( (gattr.typtr = boolptr) orif (lattr.typtr^.form = power)) then
		  searchcode(404b (*AND*),lattr)
		else begin
		  error(311);
		  gattr.typtr := nil
		end;

		(* ANDIF*)
	    andifop: begin
	      if gattr.typtr = boolptr then begin
		regc := lattr.reg - 1;
		load (gattr);
		regc := lattr.reg;
		if gattr.reg <> regc then begin
		  mac3 (200b (*MOVE*),regc,gattr.reg);
		  gattr.reg := regc
		end;
		insertaddr (right,lcix2,ic)
	      end
	      else begin
		error (359);			(*MUST BE BOOLEAN*)
		gattr.typtr := nil
	      end
	    end;

	    (* ** *)
	    expn: begin
	      if (lattr.typtr=intptr) andif (gattr.typtr=intptr) then begin
		if gattr.kind=cst then begin
		  t1 := gattr.cval.ival;	(* E.G.: X**4  *)
		  if t1 < 1 then begin
		    if lattr.kind <> expr then begin
		      incrementregc;
		      lattr.kind := expr;
		      lattr.reg := regc
		    end;
		    if t1 < 0 then
		      mac3(400b (*SETZ*),lattr.reg,0)	(*N**-1=0*)
		    else
		      mac3(201b (*MOVEI*),lattr.reg,1)	(*N**0=1*)
		  end
		  else begin
		    load(lattr);
		    if t1 > 1 then
		      if t1 = 2 then
			mac3 (220b (*IMUL*),lattr.reg,lattr.reg)    (* X**2 *)
		      else begin
			incrementregc;
			mac3 (200b (*MOVE*),regc,lattr.reg);	(*COPY VALUE FOR MULT*)
			if t1 < 5 then
			  for t2 := 2 to t1 do
			    mac3 (220b (*IMUL*),lattr.reg,regc)
			else begin
			  mac3 (201b (*MOVEI*),tac,t1-1);
			  mac3 (220b (*IMUL*),lattr.reg,regc);
			  mac3r(367b (*SOJG*),tac,ic-1)
			end;
			regc := regc - 1;
		      end
		  end
		end
		else
		  expsupport(expii)
	      end
	      else if (lattr.typtr=realptr) andif (gattr.typtr=intptr) then
		(*$Y4 IF DBLREAL THEN EXPSUPPORT(DXPRI) ELSE *)
		  expsupport(expri)
	      else begin
(*$Y4		IF DBLREAL THEN
		  IF LATTR.TYPTR <> REALPTR THEN ERROR (565);	*)
		makereal(lattr);
		if (lattr.typtr=realptr) and (gattr.typtr=realptr) then begin
(*$Y4		  IF DBLREAL THEN EXPSUPPORT (DXPRR) ELSE *)
		  expsupport(exprr);
		  library[fortransy].called := true
		end
		else begin
		  error(260);			(* TYPE CONFLICT? *)
		  lattr.typtr := nil
		end
	      end;
	      gattr := lattr
	    end					(* CASE EXPN: *)

	  end					(*CASE*)
	else
	  gattr.typtr := nil;
	regc:=gattr.reg
      end					(*WHILE*)
    end (*TERM*);

$PAGE simpleexpression - body
  begin
  (*SIMPLEEXPRESSION*)
    saveregc := regc;
    signed := false;
    if (sy = addop) andif (op in [plus,minus]) then begin
      signed := op = minus;
      insymbol
    end;
    term(fsys or [addop]);
    if signed then
      with gattr do
	if typtr <> nil then
	  if (typtr = intptr) orif (typtr = realptr) then
	    if kind = cst then
	      if typtr = intptr then
		cval.ival := - cval.ival
	      else begin			(*CREATE NEW VALU AND NEGATE AS
				      GATTR VALU MAY BELONG TO CONST ID*)
	      (*USE LATTR VALU RATHER THAN DEFINING NEW LOCAL CELL*)
		new(lattr.cval.valp,reel);
		with lattr.cval.valp^ do begin
		  rval:= gattr.cval.valp^.rval;
		  (* Assume RVAL2 = 0 *)
		  selfcsp:= nil
		end;
		cval.valp:= lattr.cval.valp;	(*RESET GATTR VALU PTR*)
		cval.valp^.rval:= - cval.valp^.rval (*NEGATE*)
	      end
	    else begin
	      load(gattr) ;
	      with code, instruction[cix] do
		if instr=200b			(*MOVE*)
		then
		  instr := 210b			(*MOVN*)
		else
(*$Y4		  IF DBLREAL ANDIF (GATTR.TYPTR = REALPTR) THEN BEGIN
		  IF INSTR = 120B (* DMOVE *) THEN
		    INSTR := 121B	(* DMOVN *)
		  ELSE
		    MAC3 (121B(*DMOVN*),GATTR.REG-1,GATTR.REG-1)
		  END
		ELSE	*)
		  mac3(210b (*MOVN*),gattr.reg,gattr.reg)
	    end
	  else begin
	    error(311) ;
	    gattr.typtr := nil
	  end ;
    while sy = addop do begin
      if op=minus then
	load(gattr);				(*BECAUSE OPD MAY NOT BE CHOSEN*)
      if op = orop then
	if gattr.typtr <> nil then
	  if gattr.typtr^.form = power then
	    if not specialsw then
	      error (466);
      if op = orifop then
	if gattr.typtr = boolptr then begin
	  load (gattr);
	  mac3 (326b (*JUMPN*),gattr.reg,0);
	  regc := gattr.reg - 1;
	  lcix1 := cix
	end
	else if gattr.typtr <> nil then begin
	  error (359);				(*MUST BE BOOLEAN*)
	  gattr.typtr := nil
	end;
      lattr := gattr;
      lop := op;
      if lop = concat then
	regc := saveregc + 2;			(* SAVE SPACE FOR OPERAND 1 DESC *)
      insymbol;
      term(fsys or [addop]);
      if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
	case lop of
	(*+*)
	  plus:
	    if (lattr.typtr = intptr) andif (gattr.typtr = intptr) then
	      searchcode(270b (*ADD*),lattr)
	    else begin
(*$Y4	      IF DBLREAL ANDIF ((LATTR.TYPTR=REALPTR) ORIF (GATTR.TYPTR=REALPTR)) ANDIF (LATTR.TYPTR <> GATTR.TYPTR) THEN ERROR (565); *)
	      makereal(lattr);
	      if (lattr.typtr = realptr) andif (gattr.typtr = realptr) then
(*$X4		SEARCHCODE(144B (*FADR*),LATTR)	*)
(*$Y4		SEARCHCODE (ADD[DBLREAL],LATTR) *)
	      else if comptypes(lattr.typtr,gattr.typtr) and
		(lattr.typtr^.form=power) then
		  searchcode(434b (*IOR*),lattr)
		else begin
		  error(311);
		  gattr.typtr := nil
		end
	    end;
	    (*-*)
	  minus:
	    if (lattr.typtr = intptr) andif (gattr.typtr = intptr) then
	      searchcode(274b (*SUB*),lattr)
	    else begin
(*$Y4	      IF DBLREAL ANDIF ((LATTR.TYPTR=REALPTR) ORIF (GATTR.TYPTR=REALPTR)) ANDIF (LATTR.TYPTR <> GATTR.TYPTR) THEN ERROR (565);	*)
	      makereal(lattr);
	      if (lattr.typtr = realptr) andif (gattr.typtr = realptr) then
(*$X4		SEARCHCODE(154B (*FSBR*),LATTR)	*)
(*$Y4		SEARCHCODE(SUBTRACT[DBLREAL],LATTR)	*)
	      else if comptypes(lattr.typtr,gattr.typtr) and
		(lattr.typtr^.form = power) then
		  searchcode(420b (*ANDCM*),lattr)
		else begin
		  error(311);
		  gattr.typtr := nil
		end
	    end;
	    (* OR *)
	  orop:
	    if comptypes(lattr.typtr,gattr.typtr) and ( (gattr.typtr = boolptr)
	      orif (lattr.typtr^.form = power) ) then
		searchcode(434b (*IOR*),lattr)
	      else begin
		error(311);
		gattr.typtr := nil
	      end;

	      (* ORIF *)
	  orifop:
	    if gattr.typtr = boolptr then begin
	      regc := lattr.reg - 1;
	      load (gattr);
	      regc := lattr.reg;
	      if gattr.reg <> regc then begin
		mac3 (200b (*MOVE*),regc,gattr.reg);
		gattr.reg := regc
	      end;
	      insertaddr (right,lcix1,ic)
	    end
	    else begin
	      error (359);			(*MUST BE BOOLEAN*)
	      gattr.typtr := nil
	    end;

	    (* || *)
	  concat: begin
	    if not (strform (lattr.typtr) and strform (gattr.typtr)) then begin
	      error (311);
	      gattr.typtr := nil
	    end
	    else
	      strcat (saveregc+1, lattr, gattr)	(* PUTS RESULT ATTR IN GATTR *)
	  end
	end					(*CASE*)
      else
	gattr.typtr := nil;
      if gattr.kind = expr then
	regc := gattr.reg
      else
	regc := saveregc
  						(*WHILE*)
  end (*SIMPLEEXPRESSION*);

$PAGE expression - body
begin
(*EXPRESSION*)
  testregc := regc+1;
  simpleexpression(fsys or [relop]);
  if sy = relop then begin
    if strform (gattr.typtr) then begin
      lattr := gattr;
      lop := op;
      regc := testregc+1;			(* RESERVE PLACE FOR OPERAND 1 DESC *)
      insymbol;
      simpleexpression(fsys);
      if comptypes (lattr.typtr, charptr) andif
	(comptypes (gattr.typtr, charptr) orif (not strform (gattr.typtr)   (*SET?*)
	)) then begin				(* CHAR COMPARE *)
	  stringcomparison := false;
	  if fvalue in [onregc, onfixedregc] then begin
	    incrementregc;
	    boolregc := regc;
	    mac3(201b (*MOVEI*),boolregc,1)
	  end
	end
	else begin				(* FULL STRING COMPARE *)
	  stringcomparison := true;
	  boolregc := testregc + 1		(* KNOW RUNTIME WILL LEAVE A 1 HERE *)
	end
    end
    else begin
      stringcomparison := false;
      if fvalue in [onregc,onfixedregc] then begin
	incrementregc;
	mac3(201b (*MOVEI*),regc,1);
	boolregc := regc
      end;
      if gattr.typtr <> nil then
	if gattr.typtr^.size > 2 then
	  loadaddress;
      lregc1 := regc;
      lattr := gattr;
      lop := op;
      if (fvalue in [onregc,onfixedregc]) andif (regc < boolregc) then
	regc := boolregc;
      insymbol;
      simpleexpression(fsys);
      if gattr.typtr <> nil then
	if gattr.typtr^.size > 2 then
	  loadaddress;
      lregc2 := regc;
    end;
    if (lattr.typtr <> nil) and (gattr.typtr <> nil) then begin
      if lop = inop then
	if gattr.typtr^.form = power then
	  if comptypes(lattr.typtr,gattr.typtr^.elset) then begin
	    load(lattr);
	    if (fvalue in [onregc,onfixedregc]) andif (regc < boolregc) then
	      regc := boolregc;
	    load(gattr);
	    regc := gattr.reg - 1;
	    if lattr.typtr=charptr then
	      mac4(246b (*LSHC*),regc,lattr.reg,-offset)
	    else
	      mac4(246b (*LSHC*),regc,lattr.reg,0);
	    if fvalue = truejmp then
	      linstr := 305b			(*CAIGE*)
	    else
	      linstr := 301b (*CAIL*);
	    mac3(linstr,regc,0);
	  end
	  else begin
	    error(260);
	    gattr.typtr := nil
	  end
	else begin
	  error(213);
	  gattr.typtr := nil
	end
      else begin
	if lattr.typtr <> gattr.typtr then
	  makereal(lattr);
	if comptypes(lattr.typtr,gattr.typtr) orif (strform (lattr.typtr) andif
	  strform (gattr.typtr)) then begin
	    lsize := lattr.typtr^.size;
	    case lattr.typtr^.form of
	      pointer:
		if lop in [ltop,leop,gtop,geop] then
		  error (312);
	      power:
		if lop in [ltop,gtop] then
		  error(313);
	      arrays:
		if not string(lattr.typtr) and (lop in [ltop,leop,gtop,geop])
		  then
		    error(312);
	      records:
		if lop in [ltop,leop,gtop,geop] then
		  error(312);
	      files:
		error(314)
	    end;
	    with lattr.typtr^ do begin
	      if (size <= 2) andif (not stringcomparison) then begin
		default := true;
		loffset := 3;
		setinclusion := false;
		case lop of
		  ltop: begin
		    linstr := 311b (*CAML*);
		    linstr1 := 313b
		  end;
		  leop:
		    if form = power then begin
		      searchcode(420b (*ANDCM*),lattr);
		      setinclusion := true
		    end
		    else begin
		      linstr := 313b (*CAMLE*);
		      linstr1 := 313b
		    end;
		  gtop: begin
		    linstr := 317b (*CAMG*);
		    linstr1 := 315b
		  end;
		  geop:
		    if form = power then begin
		      searchcode(410b (*ANDCA*),lattr);
		      setinclusion := true
		    end
		    else begin
		      linstr := 315b (*CAMGE*);
		      linstr1 := 315b
		    end;
		  neop: begin
		    linstr := 316b (*CAMN*);
		    default := false
		  end;
		  eqop: begin
		    linstr := 312b (*CAME*);
		    default := false;
		    loffset := 2
		  end
		end;
		if fvalue = truejmp then
		  changebool(linstr);
		if size = 1 then
		  searchcode(linstr,lattr)
		else if setinclusion then begin
		  mac3(336b (*SKIPN*),0,gattr.reg);
		  mac3(332b (*SKIPE*),0,gattr.reg-1);
		  if fvalue = truejmp then
		    mac3r(254b (*JRST*),0,ic+2)
		end
		else begin
		  load(lattr);
		  if (fvalue in [onregc,onfixedregc]) andif (regc<boolregc) then
		    regc := boolregc;
		  load(gattr);
		  if default then begin
		    mac3(linstr1,lattr.reg-1,gattr.reg-1);
		    mac3r(254b (*JRST*),0,ic+4)	(*FALSE*)
		  end;
		  mac3(312b (*CAME*),lattr.reg-1,gattr.reg-1);
		  mac3r(254b (*JRST*),0,ic+loffset);
		  mac3(linstr,lattr.reg,gattr.reg)
		end
	      end
	      else begin
		if stringcomparison then begin
		  strdesc (testregc, lattr);
		  strdesc (testregc+2, gattr);
		  strop (cmpss, testregc);
		  lregc2 := 0;
		  ldplmt := 1;
		  regc := 0
		end
		else begin
		  mac3(201b (*MOVEI*),hac,lsize);
		  incrementregc ;
		  mac4(200b (*MOVE*),regc,lregc1,0);
		  mac4(312b (*CAME*),regc,lregc2,0);
		  mac3r(344b (*AOJA*), lregc2, ic+4);
		  mac3(340b (*AOJ*),lregc1,0);
		  mac3(340b (*AOJ*),lregc2,0);
		  mac3r(367b (*SOJG*),hac,ic-5);
		  ldplmt := 1000000b-1
		end;
		case lop of
		  ltop:
		    linstr := 311b (* CAML *);
		  leop:
		    linstr := 313b (* CAMLE *);
		  eqop:
		    linstr := 312b (* CAME *);
		  gtop:
		    linstr := 317b (* CAMG *);
		  geop:
		    linstr := 315b (* CAMGE *);
		  neop:
		    linstr := 316b		(* CAMN *)
		end;
		if fvalue=truejmp then
		  changebool(linstr);
		mac4(linstr,regc,lregc2,ldplmt);
		regc:=testregc-1		(* RESET REGC TO THAT AT BEGINNING *)
	      end
	    end
	  end
	  else
	    error(260)
      end;
      if fvalue in [onregc,onfixedregc] then begin
	mac3(400b (*SETZ*),boolregc,0);
	regc := boolregc
      end
      else
	mac3(254b (*JRST*),0,0);
    end;
    (*(IF LATTR.TYPTR#NIL) AND (GATTR.TYPTR#NIL) THEN *)
    gattr.typtr := boolptr;
    gattr.kind := expr;
    gattr.reg := regc
  end						(*SY = RELOP*)
  else if fvalue in [truejmp,falsejmp] then begin
    load(gattr);
    if gattr.typtr<>boolptr then
      error (359);
    if fvalue = truejmp then
      linstr := 326b				(*JUMPN*)
    else
      linstr := 322b (*JUMPE*);
    mac3(linstr,gattr.reg,0)
  end
  else if gattr.kind=expr then
    regc := gattr.reg;
  if gattr.typtr <> nil then
    with gattr,typtr^ do
      if (fvalue = onfixedregc) andif (kind=expr) then begin
	if size = 2 then
	  testregc := testregc + 1;
	if testregc <> regc then begin
	  if dmove and (size = 2) then
	    mac3(120b (*DMOVE*),testregc-1,regc-1)
	  else begin
	    if size = 2 then
	      mac3(200b (*MOVE*),testregc-1,regc-1);
	    mac3(200b (*MOVE*),testregc,regc)
	  end;
	  regc := testregc;
	  reg := regc
	end
      end
end (*EXPRESSION*).
  V|\¦