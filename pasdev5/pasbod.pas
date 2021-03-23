$title body - PASCAL compiler statement routines
$OPTIONS SPECIAL, NOCHECK

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASLEX.INC

$INCLUDE PASEXP.INC

$INCLUDE PASCG.INC

$INCLUDE PASMC.INC

$INCLUDE PASCMP.INC

$INCLUDE PASSTR.INC

$PAGE body
public procedure body(fsys: setofsys);

var
    construct: boolean;
    exitcix: coderange;

$PAGE statement - in body
  procedure statement(fsys,statends: setofsys);

  var
      lcp: ctp;
      rlev: disprange;
      lab: labp;
      ix,j: integer;
      savelc: addrrange;
      saveexitcix, lexitcix: coderange;
      saveconstruct, couldexit: boolean;

$PAGE assignment - in statement
    procedure assignment(fcp: ctp);

    var
	lattr,slattr: attr;
	srmin,srmax: integer;

(*$X5      PROCEDURE STOREGLOBALS ;

      TYPE
	  WANDELFORM = (PTRW,INTW,REELW,PSETW,STRGW,INSTW) ;

      VAR
	  WANDEL : RECORD
	    CASE KW : WANDELFORM OF
	      PTRW: (
		WPTR :GTP (*TO ALLOW NIL*)) ;
	      INTW: (
		WINT : INTEGER ;
		WINT1 : INTEGER (*TO PICK UP SECOND WORD OF SET*)) ;
	      REELW: (
		WREEL: REAL) ;
	      PSETW: (
		WSET : SET OF 0..71) ;
	      STRGW: (
		WSTRG: CHARWORD) ;
	      INSTW: (
		WINST: PDP10INSTR)
	  END ;
	  I,J : INTEGER ;
	  SGLOBPTR: GTP;
	  SCIX : CODERANGE;
	  FOUND : BOOLEAN;

	PROCEDURE STOREWORD ;

	BEGIN
	  CASE LATTR.PACKFG OF
	    HWORDL:
	      WANDEL.WINT := WANDEL.WINT*262144; (*LEFT HALF*)
	    HWORDR, NOTPACK:
	      ; (*RIGHT HALF OR FULL WORD *)
	    OTHERS:
	      ERROR(507) (*DON'T KNOW HOW TO DO THAT YET*)
	  END (* CASE *);
	  CIX := CIX + 1 ;
	  IF CIX > CIXMAX THEN BEGIN
	    CIX := 0 ;
	    ERRORWITHTEXT(356,'INITPROCD.')
	  END ;
	  IF FOUND THEN (* ADD TO AN ALREADY SET WORD *)
	    CODE.WORD[CIX] := CODE.WORD[CIX] + WANDEL.WINT
	  ELSE (* ELSE MAKE ONE *)
	    WITH CGLOBPTR^ DO BEGIN
	      CODE.INSTRUCTION[CIX] := WANDEL.WINST ;
	      LASTGLOB := LASTGLOB + 1 ;
	    END ;
	END ;

	PROCEDURE GETNEWGLOBPTR ;

	VAR
	    LGLOBPTR : GTP ;

	BEGIN
	  NEW(LGLOBPTR) ;
	  WITH LGLOBPTR^ DO BEGIN
	    NEXTGLOBPTR := NIL ;
	    FIRSTGLOB := LATTR.DPLMT ;
	    LASTGLOB := FIRSTGLOB - 1;
	    FCIX := CIX + 1;
	  END ;
	  IF CGLOBPTR <> NIL THEN
	    CGLOBPTR^.NEXTGLOBPTR := LGLOBPTR ;
	  CGLOBPTR := LGLOBPTR ;
	END;

      BEGIN
      (*STOREGLOBALS*)
	IF NOT (LATTR.VSCLASS IN [PUBLICSC, STATICSC]) THEN
	  ERROR (467);
	IF STRFORM (LATTR.TYPTR) THEN
	  IF NOT COMPTYPES (LATTR.TYPTR, GATTR.TYPTR) THEN BEGIN
	    WITH LATTR.TYPTR^ DO BEGIN
	      IF STRKIND (LATTR.TYPTR) = VARYING THEN BEGIN
		IF GATTR.TYPTR = CHARPTR THEN
		  ALTERSTRINGCST (GATTR, DCLSTRING (1, VARYING))
		ELSE BEGIN
		  IF GATTR.TYPTR^.MAXLENGTH > MAXLENGTH THEN
		    ALTERSTRINGCST (GATTR, DCLSTRING (MAXLENGTH, VARYING))
		  ELSE
		    ALTERSTRINGCST (GATTR, DCLSTRING (GATTR.TYPTR^.MAXLENGTH,
		      VARYING))
		END
	      END
	      ELSE
		ALTERSTRINGCST (GATTR, LATTR.TYPTR)
	    END
	  END;
	SGLOBPTR := FGLOBPTR;
	FOUND := FALSE;
	WHILE NOT(FOUND OR (SGLOBPTR = NIL)) DO
	  WITH LATTR, SGLOBPTR^ DO BEGIN
	    FOUND := (DPLMT >= FIRSTGLOB) ANDIF (DPLMT <= LASTGLOB);
	    IF NOT FOUND THEN
	      SGLOBPTR := NEXTGLOBPTR
	  END (* WHILE, WITH *);
	IF FOUND THEN BEGIN (* THIS IS AN ADD ON TO AN ALREADY STARTED WORD*)
	  SCIX := CIX;
	  WITH LATTR, SGLOBPTR^ DO
	    CIX := FCIX+DPLMT-FIRSTGLOB-1;
	END
	ELSE (* BUILD A NEW WORD *)
	BEGIN
	  IF FGLOBPTR = NIL THEN BEGIN
	    GETNEWGLOBPTR ;
	    FGLOBPTR := CGLOBPTR ;
	  END
	  ELSE IF LATTR.DPLMT <> CGLOBPTR^.LASTGLOB + 1 THEN
	    GETNEWGLOBPTR ;
	END;
	WITH WANDEL,CGLOBPTR^,GATTR,CVAL DO
	  IF TYPTR <> NIL THEN BEGIN
	    CASE TYPTR^.FORM OF
	      SCALAR, SUBRANGE: BEGIN
		IF TYPTR = REALPTR THEN
		  IF LATTR.TYPTR=INTPTR THEN
		    WREEL := IVAL
		  ELSE
		    WREEL := VALP^.RVAL
		ELSE IF (LATTR.TYPTR = REALPTR) ANDIF (TYPTR = INTPTR) THEN
		  WREEL := IVAL (*CONVERTS IT TO REAL*)
		ELSE
		  WINT := IVAL ;
		STOREWORD ;
	      END ;
	      POINTER: BEGIN
		WPTR := NIL ;
		STOREWORD
	      END ;
	      POWER : BEGIN
		WSET := VALP^.PVAL ;
		STOREWORD ;
		WINT := WINT1 (*GET SECOND WORD OF SET*);
		STOREWORD ;
	      END ;
	      STRINGS, ARRAYS :
		WITH VALP^,WANDEL DO BEGIN
		  IF TYPTR^.SKIND = VARYING THEN BEGIN
		    WINT := SLGTH;
		    STOREWORD
		  END;
		  J := 0;
		  WINT := 0;
		  FOR I := 1 TO SLGTH DO BEGIN
		    J := J + 1;
		    WSTRG[J] := SVAL[I];
		    IF J=5 THEN BEGIN
		      J := 0;
		      STOREWORD;
		      WINT := 0
		    END
		  END;
		  IF J<>0 THEN
		    STOREWORD
		END;
	      RECORDS, FILES :
		ERROR(411)
	    END (*CASE*);
	  END (* WITH *);
	IF FOUND THEN
	  CIX := SCIX (* RESTORE CIX *);
      END (* STOREGLOBALS *);	*)

    begin
    (*ASSIGNMENT*)
      with lcp^ do
	if (klass=vars) andif (vconst) andif ((not specialsw) orif (vclass <> staticsc)) then
	  error (510);
      selector(fsys or [becomes],fcp);
      if sy = becomes then begin
	lattr := gattr;
	insymbol;
	expression(fsys,onregc);
	if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
	  if comptypes(lattr.typtr,gattr.typtr) orif (((realptr=lattr.typtr)
	    andif (gattr.typtr=intptr)) orif (strform (lattr.typtr) andif
	      strform (gattr.typtr))) then
(*$X5		IF INITGLOBALS THEN
		  IF GATTR.KIND = CST THEN
		    STOREGLOBALS
		  ELSE
		    ERROR(504)
		ELSE *) begin
		  if (lattr.vsclass = valparmsc) orif
		    (lattr.vsclass = spvalparmsc) then
		      if not specialsw then
			error (372);
		  if strform (lattr.typtr) then
		    mvstring (gattr, lattr)
		  else if (gattr.kind=cst) andif (gattr.cval.ival=0) andif
		    (lattr.packfg=notpack) 
(*$Y4		    ANDIF NOT (DBLREAL AND (LATTR.TYPTR=REALPTR))	*)
		    then begin
		      if lattr.typtr^.form = subrange then begin
			(* Assure that zero is in bounds *)
			getbounds (lattr.typtr,srmin,srmax);
			if (srmin>0) orif (srmax<0) then
			  error (367);
		      end;
		      fetchbasis(lattr);
		      with lattr do begin
			mac(vrelbyte,402b (*SETZM*),0,indbit,indexr,dplmt)
		      end
		    end
		    else
		      case lattr.typtr^.form of
			pointer: begin
			  load (gattr);
			  if runtmcheck then begin
			    if (lattr.typtr^.v_offset) andif (gattr.typtr <> nilptr) then begin
			      if lattr.packfg <> notpack then begin
				mac3b (612b(*tdne*),gattr.reg,ovfl_mask);
				ovfl_mask := ic - 1;
				support (offsetoverflow);
			      end;
			      fetchbasis (lattr);
			      support (areacheck);
			      fullword (no,lattr.indexr,gattr.reg);
			    end;
			  end;
			  if (lattr.typtr^.v_offset) andif (lattr.packfg = notpack) then begin
			    mac3b (630b(*tdz*),gattr.reg,area_mask);
			    area_mask := ic-1;
			  end;
			  store (gattr.reg,lattr);
			end;
			scalar, power: begin
			  if (gattr.typtr=intptr) andif
			    comptypes(realptr,lattr.typtr) then
			      makereal(gattr);
			  load (gattr);
			  store(gattr.reg,lattr)
			end;
			subrange: begin
			  getbounds(lattr.typtr,srmin,srmax);
			  if (gattr.kind=cst) then
			    if (gattr.cval.ival >= srmin) and
			      (gattr.cval.ival <=srmax) then
				load (gattr)
			      else
				error (367)
			  else begin
			    if runtmcheck andif (( gattr.kind<>varbl) orif
			      (gattr.subkind <> lattr.typtr)) then begin
				load (gattr);
				with slattr do begin
				  typtr:=intptr;
				  kind :=cst;
				  cval.ival:=srmax
				end;
				makecode(317b (*CAMG*),regc,slattr);
				slattr.kind:=cst;
				slattr.cval.ival:=srmin;
				makecode(315b (*CAMGE*),regc,slattr);
				support(errorinassignment)
			      end
			      else
				load (gattr);
			  end;
			  store(gattr.reg,lattr)
			end;
			arrays, records:
			  if gattr.typtr^.size <= 2 then begin
			    load(gattr) ;
			    store(gattr.reg,lattr)
			  end
			  else
			    with lattr do begin
			      loadaddress ;
			      code.instruction[cix].instr := 505b(*hrli*);
			      fetchbasis(lattr);
			      mac(vrelbyte,541b (*HRRI*),regc,indbit,indexr,
				dplmt) ;
			      if indbit=0 then
				mac5(vrelbyte,251b (*BLT *),regc,indexr,dplmt+
				  typtr^.size-1)
			      else begin
				incrementregc ;
				mac3(200b (*MOVE*),regc,regc-1);
				mac4(251b (*BLT *),regc,regc-1,typtr^.size-1)
			      end;
			    end;
			files:
			  error(361)
		      end
		end				(* NOT INITGLOBALS *)
	      else
		error(260)
      end					(*SY = BECOMES*)
      else
	error(159);
    end (*ASSIGNMENT*);

$PAGE gotostatement - in statement
    procedure gotostatement;

    var
	rlev: disprange;
	i: disprange;
	lab: labp;

    begin
      if sy = intconst then begin
	lab := findlabel (val.ival, rlev);
	if lab = nil then begin			(* NOT PREVIOUS DEFINED *)
	  lab := dcllabel (val.ival, standard);
	  rlev := 0
	end;
	if (level = 2) andif (rlev = 1) and main then begin
	(* GOTO from top level procedure into mainline *)
	  if dmove then
	    mac4 (120b (*DMOVE*),basis,basis,1)
	  else begin
	    mac4 (120b (*MOVE*),topp,basis,2);
	    mac4 (120b (*MOVE*),basis,basis,1);
	  end;
	  mac3 (603b (*TLNE*),basis,777777b);
	  mac3r(254b (*JRST*),0,ic-2);
	  mac3 (262b (*POP*),topp,0);
	end
	else if rlev > 0 then begin		(* POP BACK TO FRAME OF BLOCK CONTAINING LABEL *)
	  mac4 (550b (*HRRZ*), tac, basis, 1);	(* GET PARENT'S BASIS *)
	  for i := 2 to rlev do			(* GET PARENT'S PARENT'S BASIS *)
	    mac4 (550b (*HRRZ*), tac, tac, 1);
	  support (unwind)			(* SET BASIS AND TOPP TO THIS FRAME *)
	end;
	with lab^ do				(* PERFORM JUMP *)
	  if defined then
	    mac3r (254b (*JRST*), 0, labaddr)
	  else begin				(* ADDR NOT KNOWN, LET LINKER FIX IT UP *)
	    mac3b (254b (*JRST*), 0, labchain);
	    labchain := ic - 1;
	    code.information[cix] := 'F'
	  end;
	insymbol
      end
      else
	error (255)
    end (*GOTOSTATEMENT*);

$PAGE compoundstatement - in statement
    procedure compoundstatement;

    begin
      incnest;
      loop
	repeat
	  statement(fsys,statends)
	until not (sy in statbegsys);
      exit if not (sy in [semicolon,exitsy]);
	if sy = semicolon then
	  insymbol
      end;
      decnest;
      setnest;
      if sy = endsy then begin
	if debug then
	  newliner;
	insymbol
      end
      else
	error(163)
    end (*COMPOUNDSTATEMENET*);

$PAGE ifstatement - in statement
    procedure ifstatement;

    var
	lcix1,lcix2: coderange;

    begin
      expression(fsys or [thensy],falsejmp);
      lcix1 := cix;
      if sy = thensy then
	insymbol
      else
	error(164);
      incnest;
      statement(fsys or [elsesy],statends or [elsesy]);
      decnest;
      if sy = elsesy then begin
	mac3(254b (*JRST*),0,0);
	lcix2 := cix;
	insertaddr(right,lcix1,ic);
	insymbol;
	statement(fsys,statends);
	insertaddr(right,lcix2,ic)
      end
      else
	insertaddr(right,lcix1,ic)
    end (*IFSTATEMENT*);

$PAGE casestatement - in statement
    procedure casestatement;

    type
	cip = ^caseinfo;
	caseinfo = packed record
	  next: cip;
	  csstart: addrrange;
	  csend: coderange;
	  cslab: integer
	end;

    var
	lsp,lsp1,llsp1: stp;
	fstptr,lpt1,lpt2,lpt3,othersptr: cip;
	lval,lstval: valu;
	lic,laddr,jumpaddr: addrrange;
	lcix: coderange;
	lmin,lmax: integer;

      procedure insertbound(fcix:coderange;fic: addrrange;bound:integer);

      var
	  lcix1:coderange;
	  lic1: addrrange;
	  lattr:attr;

      begin
	if bound>=0 then
	  insertaddr(no,fcix,bound)
	else begin
	  lcix1:=cix;
	  lic1 := ic;
	  cix:=fcix;
	  ic := fic;
	  with lattr do begin
	    kind:=cst;
	    cval.ival:=bound;
	    typtr:=nil
	  end;
	  depcst(int,lattr);
	  cix:=lcix1;
	  ic:= lic1;
	  with code.instruction[fcix] do
	    instr:=instr+10b			(*CAILE-->CAMLE, CAIL-->CAML*)
	end
      end;

    begin
      incnest;
      othersptr:=nil;
      expression(fsys or [ofsy,comma,colon],onregc);
      load(gattr);
      mac3(301b (*CAIL*),regc,0);		(*<<<---------- LMIN IS INSERTED HERE*)
      mac3(303b (*CAILE*),regc,0);		(*<<<--------- LMAX IS INSERTED HERE*)
      mac3(254b (*JRST*),0,0);			(*<<<------------- START OF "OTHERS" IS INSERTED HERE*)
      mac(no,254b (*JRST*),0,1,regc,0);		(*<<<---- START OF JUMP TABLE IS INSERTED HERE*)
      lcix := cix;
      lic := ic;
      lsp := gattr.typtr;
      if lsp <> nil then
	if (lsp^.form <> scalar) or (lsp = realptr) then begin
	  error(315);
	  lsp := nil
	end;
      if sy = ofsy then
	insymbol
      else
	error(160);
      fstptr := nil;
      lpt3 := nil;
      loop
	loop
	  constant(fsys or [comma,colon,elipsis],lsp1,lval);
	  if not comptypes (lsp, lsp1) then begin
	    error (505);
	    lsp1 := nil
	  end;
	  lstval := lval;
	  llsp1 := lsp1;
	  if sy = elipsis then begin
	    insymbol;
	    constant (fsys + [comma,colon],llsp1,lstval);
	    if not comptypes (lsp,llsp1) then begin
	      error (505);
	      llsp1 := nil
	    end
	  end;
	  if (lsp <> nil) and (llsp1 <> nil) and (lsp1 <> nil) then begin
	    if lstval.ival < lval.ival then
	      error (451)
	    else if abs (lstval.ival) > hwcstmax then
	      error (451)
	    else
	      while lval.ival <= lstval.ival do begin
		lpt1 := fstptr;
		lpt2 := nil;
		while lpt1 <> nil do
		  with lpt1^ do begin
		    if cslab <= lval.ival then begin
		      if cslab = lval.ival then
			error(261);
		      goto 1
		    end;
		    lpt2 := lpt1;
		    lpt1 := next
		  end;
		1:
		  new(lpt3);
		with lpt3^ do begin
		  next := lpt1;
		  cslab := lval.ival;
		  csstart := ic;
		  csend := 0
		end;
		if lpt2 = nil then
		  fstptr := lpt3
		else
		  lpt2^.next := lpt3;
		lval.ival := lval.ival + 1
	      end
	  end;
	exit if sy <> comma;
	  insymbol
	end;
	if sy = colon then
	  insymbol
	else
	  error(151);
	repeat
	  statement(fsys,statends)
	until not (sy in statbegsys);
	if lpt3 <> nil then begin
	  mac3(254b (*JRST*),0,0);
	  lpt3^.csend := cix
	end;
      exit if sy <> semicolon;
	insymbol;
	if sy=otherssy then begin
	  insymbol;
	  if sy=colon then
	    insymbol
	  else
	    error(151);
	  new(othersptr);
	  with othersptr^ do begin
	    csstart:=ic;
	    repeat
	      statement(fsys,statends)
	    until not(sy in statbegsys);
	    mac3(254b (*JRST*),0,0);
	    csend:=cix;
	    goto 2
	  end
	end
      end;
      2:
	if fstptr <> nil then begin
	  lmax := fstptr^.cslab;
	  (*REVERSE POINTERS*)
	  lpt1 := fstptr;
	  fstptr := nil;
	  repeat
	    lpt2 := lpt1^.next;
	    lpt1^.next := fstptr;
	    fstptr := lpt1;
	    lpt1 := lpt2
	  until lpt1 = nil;
	  lmin := fstptr^.cslab;
	  insertbound(lcix-2,lic-2,lmax);
	  insertbound(lcix-3,lic-3,lmin);
	  insertaddr(right,lcix,ic-lmin);
	  if lmax - lmin < cixmax-cix then begin
	    laddr := ic + lmax - lmin + 1;
	    if othersptr=nil then
	      jumpaddr:=laddr
	    else begin
	      insertaddr(right,othersptr^.csend,laddr);
	      jumpaddr:=othersptr^.csstart
	    end;
	    insertaddr(right,lcix-1,jumpaddr);
	    repeat
	      with fstptr^ do begin
		while cslab > lmin do begin
		  fullword(right,0,jumpaddr);
		  lmin := lmin + 1
		end;
		fullword(right,0,csstart);
		if csend <> 0 then
		  insertaddr(right,csend,laddr);
		fstptr := next;
		lmin := lmin + 1
	      end
	    until fstptr = nil
	  end
	  else
	    error(363)
	end;
      decnest;
      setnest;
      if sy = endsy then
	insymbol
      else
	error(163)
    end (*CASESTATEMENT*);

$PAGE repeatstatement
    procedure repeatstatement;

    var
	laddr: addrrange;

    begin
      incnest;
      laddr := ic;
      loop
	repeat
	  statement(fsys or [untilsy],statends or [untilsy])
	until not (sy in statbegsys);
      exit if not (sy in [semicolon,exitsy]);
	if sy = semicolon then
	  insymbol
      end;
      decnest;
      setnest;
      if sy = untilsy then begin
	if debug then
	  newliner;
	insymbol;
	expression(fsys,falsejmp);
	insertaddr(right,cix,laddr);
      end
      else
	error(202)
    end (*REPEATSTATEMENT*);

$PAGE whilestatement
    procedure whilestatement;

    var
	laddr: addrrange;
	lcix: coderange;

    begin
      laddr := ic;
      expression(fsys or [dosy],falsejmp);
      lcix := cix;
      if sy = dosy then
	insymbol
      else
	error(161);
      statement(fsys,statends);
      mac3r(254b (*JRST*),0,laddr);
      insertaddr(right,lcix,ic)
    end (*WHILESTATEMENT*);

$PAGE forstatement
    procedure forstatement;

    var
	lattr, lattr2, hattr, battr: attr;
	byseen: boolean;
	lsp: stp;
	lsy: symbol;
	lcix: coderange;
	laddr: addrrange;
	linstr: instrange;
	lregc: acrange;
	addtolc: integer;

    begin
      if sy = ident then begin
	srchid ([vars,field,func],lcp);
	insymbol;
	selector (fsys + [becomes], lcp);
	if lcp^.klass = vars then begin
	  lattr := gattr;
	  if lcp^.vkind <> actual then begin
	    error (364);
	    lattr.typtr := nil
	  end;
	end
	else begin
	  error (365);
	  lattr.typtr := nil
	end;
	if lattr.typtr <> nil then
	  if comptypes (realptr, lattr.typtr) or (lattr.typtr^.form > subrange)
	    then begin
	      error (365);
	      lattr.typtr := nil
	    end;
      end
      else begin
	errandskip(209,fsys or [becomes,tosy,downtosy,dosy]);
	lattr.typtr := nil
      end;
      if sy = becomes then begin
	insymbol;
	expression(fsys or [tosy,downtosy,dosy,bysy],onregc);
	if gattr.typtr <> nil then
	  if gattr.typtr^.form <> scalar then
	    error(315)
	  else if comptypes(lattr.typtr,gattr.typtr) then
	    load(gattr)
	  else
	    error(556);
	lregc := gattr.reg
      end
      else
	errandskip(159,fsys or [tosy,downtosy,dosy]);
      if sy in [tosy,downtosy] then begin
	lsy := sy;
	insymbol;
	expression(fsys or [dosy,bysy],onregc);
	if gattr.typtr <> nil then
	  if gattr.typtr^.form <> scalar then
	    error(315)
	  else if comptypes(lattr.typtr,gattr.typtr) then begin
	    addtolc := 0 ;
	    with gattr do
	      if ( (kind = varbl) andif ( (vlevel > 1) andif (vlevel < level)
		orif (packfg <> notpack) orif (indexr > 0) andif
		  (indexr <= regcmax) ) ) orif (kind = expr) then begin
		 ad(gattr);
		    mac4(202b (*MOVEM*),regc,basis,lc);
		    addtolc := 1;
		    kind := varbl ;
		    indbit := 0 ;
		    indexr := basis ;
		    vlevel := 1;
		    dplmt := lc ;
		    packfg := notpack ;
		    vrelbyte := no
		  end ;
	    hattr := gattr;
	    if sy = bysy then begin
	      byseen := true;
	      insymbol;
	      expression (fsys or [dosy],onregc);
	      if gattr.typtr <> nil then
		if gattr.typtr^.form <> scalar then
		  error (315)
		else if comptypes (lattr.typtr, gattr.typtr) then begin
		  with gattr do begin
		    load (gattr);
		    mac4 (202b (*MOVEM*),regc,basis,lc+addtolc);
		    kind := varbl;
		    indbit := 0;
		    indexr := basis;
		    vlevel := 1;
		    dplmt := lc+addtolc;
		    packfg := notpack;
		    vrelbyte := no;
		    addtolc := addtolc + 1;
		  end;
		  battr := gattr;
		end;
	    end
	    else
	      byseen := false;
	    lattr2 := lattr;			(* MUST SAVE TO REFERENCE INDEX AGAIN *)
	    fetchbasis(lattr);
	    with lattr do begin
	      if (indexr>0) and (indexr<=regcmax) then begin
		mac(vrelbyte,201b (*MOVEI*),indexr,indbit,indexr,dplmt);
		indbit := 1;
		dplmt := lc+addtolc;
		vsclass := localsc;
		vrelbyte := no;
		mac4(202b (*MOVEM*),indexr,basis,dplmt);
		indexr := basis;
		vlevel := 1;
		addtolc := addtolc + 1 ;
		lattr2 := lattr			(* SAVE TO REF INDEX *)
	      end;
	      mac(vrelbyte,202b,lregc,indbit,indexr,dplmt);
	    end;
	    if lsy = tosy then
	      linstr := 313b			(*CAMLE*)
	    else
	      linstr := 315b (*CAMGE*);
	    laddr := ic;
	    makecode(linstr,lregc,hattr) ;
	  end
	  else
	    error(556)
      end
      else
	errandskip(251,fsys or [dosy]);
      mac3(254b (*JRST*),0,0);
      lcix :=cix;
      if sy = dosy then
	insymbol
      else
	error(161);
      checklc (addtolc);
      statement(fsys,statends);
      lc := lc - addtolc;
      if byseen then
	with battr do begin
	  if lsy = tosy then
	    linstr := 200b
	  else
	    linstr := 210b (* MOVN *);
	  mac4 (linstr,lregc,basis,dplmt);
	  linstr := 273b (* ADDB *);
	end
      else if lsy = tosy then
	linstr := 350b				(*AOS*)
      else
	linstr := 370b (*SOS*);
      fetchbasis (lattr2);
      with lattr2 do
	mac (vrelbyte,linstr,lregc,indbit,indexr,dplmt);
      mac3r(254b (*JRST*),0,laddr);
      insertaddr(right,lcix,ic)
    end (*FORSTATEMENT*);

$PAGE loopstatement
    procedure loopstatement;

    var
	laddr: addrrange;

    begin
      laddr := ic;
      compoundstatement;
      mac3r (254b (*JRST*),0,laddr);
    end;

$PAGE exitstatement
    procedure exitstatement;

    var
	testcix: addrrange;

    begin
      if construct then begin
	if sy = ifsy then begin
	  insymbol;
	  expression (fsys or [semicolon,endsy,dosy],falsejmp);
	  testcix := cix;
	  if sy = dosy then begin
	    insymbol;
	    statement (fsys,statends);
	  end;
	  mac3r (254b (*JRST*),0,exitcix);
	  exitcix := cix;
	  insertaddr (right,testcix,ic);
	end
	else
	  errandskip (162,fsys or [semicolon,endsy]);
      end
      else
	error (413);				(* MUST BE IN LOOP, REPEAT, WHILE, OR FOR *)
    end;

$PAGE withstatement
    procedure withstatement;

    var
	lcp: ctp;
	oldlc: addrrange;
	lcnt1: disprange;
	oldregc: acrange;

    begin
      lcnt1 := 0;
      oldregc := regcmax;
      oldlc := lc;
      loop
	if sy = ident then begin
	  srchid([vars,func,field],lcp);
	  insymbol
	end
	else begin
	  error(209);
	  lcp := uvarptr
	end;
	selector(fsys or [comma,dosy],lcp);
	if gattr.typtr <> nil then
	  if gattr.typtr^.form = records then
	    if top < displimit then begin
	      top := top + 1;
	      lcnt1 := lcnt1 + 1;
	      withix := withix + 1;
	      display[top].fname := gattr.typtr^.fstfld;
	      with display[top],gattr do begin
		occur := crec;
		if indbit = 1 then
		  getparaddr;
		if (vlevel <> 0) or incore then	(* if not virtual ptr *)
		  fetchbasis(gattr);
		if (vsclass = externalsc) orif  (indexr<>0) andif (indexr <> basis) then begin
		  if (vsclass=externalsc) then begin
		    mac (vrelbyte,201b (*MOVEI*),regcmax,indbit,indexr,dplmt);
		    dplmt := 0;
		    indbit := 0;
		    vrelbyte := no;
		  end
		  else
		    mac3(200b (*MOVE*),regcmax,indexr);
		  indexr := regcmax;
		  regcmax := regcmax-1;
		  if regcmax<regc then begin
		    error(317);
		    regc := regcmax
		  end
		end;
		clev := vlevel;
		crelbyte := vrelbyte;
		cindr := indexr;
		cindb:=indbit;
		cdspl := dplmt;
		clc := lc;
		if (cindr<>0) and (cindr<>basis) then begin
		  checklc (1);
		end
	      end
	    end
	    else
	      error(404)
	  else
	    error(308);
      exit if sy <> comma;
	insymbol
      end;
      if sy = dosy then
	insymbol
      else
	error(161);
      statement(fsys,statends);
      regcmax:=oldregc;
      top := top - lcnt1;
      lc := oldlc;
      withix := withix - lcnt1;
    end (*WITHSTATEMENT*);

  begin
  (*STATEMENT*)
    savelc := lc;				(* STMT MAY ALLOCATE TEMPS *)
    if sy = intconst then			(*LABEL*)
    begin
      lab := findlabel (val.ival, rlev);	(* SEARCH FOR ANY LABEL WITH THIS NAME *)
      if lab = nil then
	lab := dcllabel (val.ival, standard)
      else
	with lab^ do
	  if defined and (rlev = 0) then
	    error (211)				(* DUPLICATED LOCAL LABEL *)
	  else if (rlev > 0)			(* DCL GLOBAL LABEL WITH SAME NAME *)
	  then
	    if (labchain > pfstart)		(* FORWARD REF RESOLVED TO THE GLOBAL LABEL *)
	    then begin
	      error (508);
	      if not specialsw then
		error (265)
	    end
	    else
	      lab := dcllabel (val.ival, standard);
      with lab ^ do begin
	defined := (lkind = standard) or (rlev = 0);
	labaddr := ic;
	if lkind = standard then
	  if not specialsw then
	    error (265);
      end;
      insymbol;
      if sy = colon then
	insymbol
      else
	error(151)
    end;
    if not (sy in fsys or [ident,exitsy]) then
      errandskip(166,fsys);
    if sy in statbegsys or [ident,exitsy] then begin
      if debug andif (*$X5 (NOT INITGLOBALS) ANDIF *) not (sy in [beginsy,repeatsy,loopsy]) then
	newliner;
      setnest;
      regc:=regin ;
(*$X5      IF INITGLOBALS ANDIF (SY <> IDENT) THEN
	ERROR(462)
      ELSE *) begin
	saveconstruct := construct;
	couldexit := sy in [whilesy,repeatsy,loopsy,forsy];
	if sy = casesy then
	  construct := false
	else if couldexit then begin
	  construct := true;
	  saveexitcix := exitcix;
	  exitcix := 0;
	end;
	(* Allow EXIT IFs from WHILE, REPEAT, LOOP and FOR constructs
	   even if nested within BEGIN and WITH statements but
	   disallow any such exits from CASE *)
	case sy of
	  ident: begin
	    srchid([vars,field,func,proc],lcp);
	    insymbol;
	    if lcp^.klass = proc then
(*$X5	      IF INITGLOBALS THEN
		ERROR(462)
	      ELSE	*)
		call(fsys,lcp)
	    else
	      assignment(lcp)
	  end;
	  beginsy: begin
	    insymbol;
	    compoundstatement
	  end;
	  gotosy: begin
	    insymbol;
	    gotostatement
	  end;
	  ifsy: begin
	    insymbol;
	    ifstatement
	  end;
	  casesy: begin
	    insymbol;
	    casestatement
	  end;
	  whilesy: begin
	    insymbol;
	    whilestatement
	  end;
	  repeatsy: begin
	    insymbol;
	    repeatstatement
	  end;
	  loopsy: begin
	    insymbol;
	    loopstatement
	  end;
	  exitsy: begin
	    insymbol;
	    exitstatement
	  end;
	  forsy: begin
	    insymbol;
	    forstatement
	  end;
	  withsy: begin
	    insymbol;
	    withstatement
	  end
	end;
	if couldexit andif not errorflag then begin
	  while exitcix <> 0 do begin
	    lexitcix := code.instruction[exitcix].address;
	    insertaddr (right,exitcix,ic);
	    exitcix := lexitcix
	  end;
	  exitcix := saveexitcix;
	end;
	construct := saveconstruct;
      end;
      skipiferr(statends or [exitsy],506,fsys)
    end;
    regc := regin				(*RE-INITIALIZE REGISTER COUNTER TO AVOID OVERFLOW DURING SUBSEQUENT
						 EXPRESSION EVALUATIONS IN REPEATSTATEMENT OR LOOPSTATEMENT *)
    ;
    lc := savelc
  end (*STATEMENT*);

$PAGE body - body
var save_inclnest: 0..maxinclusion;
begin
(*BODY*)
  set_ptr_type (func);	(* PTR to be treated as a function *)
  regcmax:=within;
  withix := -1;
  firstkonst := nil;
  construct := false;
  cix := -1 ;
(*$X5  IF INITGLOBALS THEN BEGIN
    CGLOBPTR := NIL ;
    LOOP
      IF SY <> ENDSY THEN
	STATEMENT([SEMICOLON,ENDSY],[SEMICOLON,ENDSY]) ;
    EXIT IF SY <> SEMICOLON ;
      INSYMBOL
    END ;
    IF SY = ENDSY THEN
      INSYMBOL
    ELSE
      ERROR(163) ;
    WRITEMC(WRITEGLOBALS)
  END
  ELSE BEGIN	*)
  (*BODY PROPER*)
    enterbody;
    if curproc <> nil then
      curproc^.pfaddr:= pfstart
    else
      lc:= progrst;
    lcmax:=lc;
    if main or (level > 1) then begin
      loop
	repeat
	  statement(fsys or [semicolon,endsy],[semicolon,endsy])
	until not (sy in statbegsys);
      exit if sy <> semicolon;
	insymbol
      end;
      if sy = endsy then
	insymbol
      else
	error(163);
    end;
    leavebody;
    if main or (level > 1) then
      insertaddr(no,patchsize,lcmax);
    if debug andif (level = 1) then begin
      save_inclnest := inclnest;	(* make sure forward file/page refs are satisfied *)
      while inclnest >= 0 do begin
	fwdpageref;
	fwdfileref;
	inclnest := inclnest - 1;
      end;
      inclnest := save_inclnest;
    end;
    writemc(writecode);
    if (firstkonst <> nil) orif (display[top].flabel <> nil) orif
      (localpfptr <> nil) andif (localpfptr^.pflev = level) then
	writemc (writeinternals);
    if level = 1 then begin
      if debug then
	writemc (writeprogblock);
      writemc(writefileblocks);
      writemc(writesymbols);
      writemc(writelibrary);
      writemc(writestart);
      writemc(writeend)
    end
  (*$X5 END *) (* BODY PROPER*);
  set_ptr_type (types);	(* PTR again a type *)
end (*BODY*).
    u@^