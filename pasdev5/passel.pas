$TITLE selector
$OPTIONS SPECIAL, NOCHECK

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASLEX.INC

$INCLUDE PASCMP.INC

$INCLUDE PASSTR.INC

$INCLUDE PASCG.INC

$PAGE declarations
type
    valuekind = (onregc, onfixedregc, truejmp, falsejmp);

external procedure expression (fsys: setofsys; fvalue: valuekind);

type
    bp_array = array[0..4] of addrrange;

const
    ssbyteptrs: bp_array := (100700b,440700b,350700b,260700b,170700b);

(* FUNCTION TO DETERMINE IF A FUNCTION NAME IS REFERENCED WITHIN
   THE BODY OF THAT FUNCTION *)

$PAGE finscope
public function finscope (fcp: ctp): boolean;

var
    d: disprange;

begin
  finscope := false;
  d := top;
  loop
    with display[d] do begin
      if occur = blck then
	if blkid = fcp then
	  finscope := true
    end;
  exit if finscope orif (d <= 0);
    d := d - 1
  end;
end;

$PAGE selector
public procedure selector(fsys: setofsys; fcp: ctp);

var
    lattr: attr;
    lcp: ctp;
    lsp, old_typtr: stp;
    lmin,lmax,indexvalue,indexoffset: integer;
    oldic: acrange;
    inxtyp: stp;
    breg: acrange;
    lenreg: acrange;
    sskind: stringkind;

  function chkspecial (fsy: symbol): boolean;

  begin
    if sy = fsy then begin
      chkspecial := true;
      if not specialsw then
	error (466)
    end
    else
      chkspecial := false
  end;

  procedure sublowbound;

  begin
    if lmin > 0 then
      mac3(275b (*SUBI*),regc,lmin)
    else if lmin < 0 then
      mac3(271b (*ADDI*),regc,-lmin);
    if runtmcheck then begin
      mac3(301b (*CAIL*),regc,0);
      mac3(303b (*CAILE*),regc,lmax-lmin);
      support(indexerror)
    end
  end;

begin
  with fcp^, gattr do begin
    typtr := idtype;
    kind := varbl;
    packfg := notpack;
    case klass of
      vars: begin
	vlevel := max (vlev,1);
	indexr := 0;
	vsclass := vclass;
	vid := fcp;
	indbit := 0;
	case vsclass of
	  varparmsc, spvalparmsc: begin
	    dplmt := vaddr;
	    vrelbyte := no;
	    if virtual and not incore then begin
	      getparaddr;
	      vlevel := 0;	(* mark as virtual *)
	    end
	    else indbit := 1;
	  end;
	  localsc, valparmsc: begin
	    dplmt := vaddr;
	    vrelbyte := no
	  end;
	  staticsc, publicsc: begin
	    vlevel := 1;			(* GIVE FETCHBASIS WHAT IT WANTS *)
	    dplmt := vaddr;
	    vrelbyte := right
	  end;
	  externalsc:begin
	    dplmt := 0;
	    vrelbyte := right;
	    (* BACKLINKING ADDR FIXED UP IN FETCHBASIS *)
	    indbit := ord (overlaysw);
	  end
	end;
      end;
      field:
	with display[disx] do
	  if occur = crec then begin
	    vlevel := clev;
	    packfg := packf;
	    vrelbyte:= crelbyte;
	    vsclass := localsc;
	    if packfg = packk then begin
	      vid := fcp;
	      bpaddr := fldaddr;
	      dplmt := cdspl
	    end
	    else
	      dplmt := cdspl+fldaddr;
	    indexr := cindr;
	    indbit:=cindb
	  end
	  else
	    error(171);
      func:
	if pfdeckind = standard then
	  error(502)
	else if pflev = 0 then
	  error(502)				(*EXTERNAL FCT*)
	else if pfkind = formal then
	  error(456)
	else begin
	  if not finscope (fcp) then
	    error (209);
	  vlevel := pflev + 1;
	  vrelbyte := no;
	  vsclass := localsc;
	  dplmt := retloc;			(*IMPL. RELAT. ADDR. OF FCT. RESULT*)
	  indexr :=0;
	  indbit := 0;
	  if idtype <> nil then
	    if idtype^.size > 2 then
	      indbit := 1;
	end
    end;
    (*CASE*)
  end (*WITH*);
  iferrskip(166,selsys or fsys);
  while sy in selsys do begin
  (*[*)
    if (sy = lbrack) orif chkspecial (lparent) then begin
      if gattr.indbit = 1 then
	getparaddr;
      oldic := gattr.indexr;
      indexoffset := 0 ;
      loop
	lattr := gattr;
	indexvalue := 0 ;
	with lattr do
	  if typtr <> nil then begin
	    if not (typtr^.form in [arrays, strings]) then begin
	      error(307);
	      typtr := nil
	    end;
	    lsp := typtr
	  end;
	insymbol;
	expression(fsys or [comma,colon,rbrack,rparent],onregc);
	if gattr.kind<>cst then
	  load(gattr)
	else
	  indexvalue := gattr.cval.ival ;
	if gattr.typtr <> nil then
	  if gattr.typtr^.form <> scalar then
	    error(403);
	if lattr.typtr <> nil then
	  with lattr,typtr^ do begin
	    if typtr^.form = arrays then begin
	      inxtyp := inxtype;
	      if inxtyp <> nil then
		getbounds (inxtyp,lmin,lmax);
	      typtr := aeltype
	    end
	    else (* FORM = STRINGS *)begin
	      inxtyp := intptr;
	      lmin := 1;
	      lmax := typtr^.maxlength;
	      typtr := charptr;
	    end;
	    if comptypes(inxtyp,gattr.typtr) then begin
	      if inxtype <> nil then begin
		if gattr.kind = cst then
		  if (indexvalue < lmin) or (indexvalue > lmax) then
		    error(263)
	      end
	    end
	    else
	      error(457);
	  end ;
      exit if sy <> comma;
	with lattr do
	  if typtr<>nil then
	    if gattr.kind = cst then
	      dplmt := dplmt +( indexvalue - lmin ) * typtr^.size
	    else begin
	      sublowbound;
	      if typtr^.size > 1 then
		mac3(221b (*IMULI*),regc,typtr^.size);
	      if oldic = 0 then
		oldic := regc
	      else if oldic > regcmax then begin
		mac3(270b (*ADD*),regc,oldic);
		oldic := regc
	      end
	      else begin
		mac3(270b (*ADD*),oldic,regc) ;
		regc := regc - 1
	      end;
	      indexr := oldic
	    end ;
	gattr := lattr ;
      end;
      (*LOOP*)
      with lattr do
	if typtr <> nil then begin
	  if strform (lsp) then begin		(* SUBSTRING OF A STRING *)

	    lenreg := 0;			(* NO LENGTH LOADED *)
	    if strkind (lsp) = varying then begin
	      if runtmcheck then begin		(* LOAD LENGTH *)
		lenreg := regcmax;		(* BORROW A WITH REG *)
		regcmax := regcmax - 1;
		if regcmax < regc then begin
		  error (317);
		  regc := regcmax
		end;
		fetchbasis (lattr);
		mac5 (vrelbyte, 200b (*MOVE*), lenreg, indexr, dplmt)
	      end;
	      if (vsclass = externalsc) andif (indexr = 0) then begin
		dplmt := 1;			(* SHOULD FORCE ADDRESS LOAD NEXT TIME *)
		vrelbyte := right		(* FETCHBASIS IS A BITCH *)
	      end
	      else
		dplmt := dplmt + 1		(* STRING 1 WORD PAST LENGTH *)
	    end;

	    (* IF GATTR IS CST, THEN BYTE OFFSET IS FIXED *)
	    if gattr.kind = cst then begin
	      if lenreg <> 0 then begin		(* VARYING STRING, CHECK LEN *)
		if runtmcheck then begin
		  mac3 (305b (*CAIGE*), lenreg, indexvalue);
		  support (indexerror)
		end;
		if (indexvalue <> 1) andif (sy = colon) then
		  mac3 (275b (*SUBI*), lenreg, indexvalue-1);
	      end;
	      lmax := lmax - indexvalue + 1;	(* RESTRIC SIZE OF SUBSTR *)
	      if (indexr <= 1) orif (indexr > regcmax)	(* PICK REG IN WHICH TO PUT BP *)
	      then begin
		incrementregc;			(* INDEXR IS WITH PTR OR BASIS *)
		breg := regc			(* GET A NEW REG *)
	      end
	      else
		breg := indexr;			(* REUSE REG ACCUM. INDEX *)
	      dplmt := dplmt + ((indexvalue-1) div 5);	(* ADD WORD OFFSET *)
	      fetchbasis (lattr);
	      mac5(vrelbyte,201b (*MOVEI*),breg,indexr,dplmt);	(* ADDR IN RHALF *)
	      mac3 (505b (*HRLI*), breg, ssbyteptrs [indexvalue mod 5]);
	    end

	    (* GATTR NOT CST, MUST COMPUTE BYTE OFFSET *)
	    else begin
	      if runtmcheck then begin
		mac3 (301b (*CAIL*), regc, 1);
		if lenreg <> 0 then
		  mac3 (313b (*CAMLE*), regc, lenreg)
		else
		  mac3 (303b (*CAILE*), regc, lmax);
		support (indexerror);
		if sy = colon then begin	(* SAVE LB FOR SUBSTR LEN CHK *)
		  if lenreg = 0 then begin
		    lenreg := regcmax;
		    regcmax := regcmax - 1;
		    if regcmax < regc then begin
		      error (317);
		      regc := regcmax
		    end;
		    mac3 (201b (*MOVEI*), lenreg, lmax)
		  end;
		  mac4 (275b (*SUBI*), lenreg, regc, -1)
		end
	      end;
	      if (indexr <= 1) or (regc < indexr) then
		breg := regc
	      else
		breg := indexr;
	      mac3 (231b (*IDIVI*), regc, 5);
	      fetchbasis (lattr);
	      if breg = regc then
		mac5 (vrelbyte, 271b (*ADDI*), breg, indexr, dplmt)
	      else
		mac5 (vrelbyte, 271b (*ADDI*), breg, regc, dplmt);
	      if virtual then
		mac3 (505b(*hrli*),breg,0);
	      if overlaysw then
		mac5 (right, 270b, regc+1, 0, cbpssaddr)
	      else mac5 (right, 270b, breg, regc+1, cbpssaddr);
	      if cbpssaddr = 0 then
		insertaddr (no,cix,0);
	      cbpssaddr := ic - 1;
	      code.information [cix] := 'E';
	      if overlaysw then
		mac4 (270b, breg, regc+1, 0);
	    end;
	    bpaddr := breg;			(* SET UP AS SINGLE CHAR REF *)
	    packfg := packr;
	    regc := breg;
	    if sy = colon then begin		(* SUBSTR REFERENCE, GET LENGTH *)
	      kind := substrng;
	      reg := breg;
	      sskind := varying;		(* IF LENGTH NOT CONSTANT *)
	      insymbol;				(* GO GET LENGTH *)
	      expression (fsys+[rbrack],onfixedregc);	(* WANT LENGTH IN BREG+1 *)
	      if not comptypes (gattr.typtr, intptr) then
		error(457)
	      else if gattr.kind = cst then begin
		with gattr do begin
		  if (cval.ival < 1) or (cval.ival > lmax) then
		    error (263);
		  sskind := nonvarying;		(* NOW KNOWN LENGTH *)
		  lmax := cval.ival
		end
	      end;
	      typtr := dclstring (lmax, sskind);    (* RESTRICT SIZE OF TEMPS, ASSIGNED CSTS *)
	      load (gattr);
	      if runtmcheck then begin
		mac3 (301b (*CAIL*), regc, 0);
		if lenreg <> 0 then
		  mac3 (313b (*CAMLE*), regc, lenreg)
		else
		  mac3 (303b (*CAILE*), regc, lmax);
		support (indexerror)
	      end
	    end;
	    if lenreg <> 0 then
	      regcmax := lenreg			(* RESTORE IF USED *)
	  end
	  else begin
	    if gattr.kind = cst then
	      indexoffset := ( indexvalue - lmin ) * typtr^.size
	    else begin
	      if runtmcheck orif (typtr^.size > 1) then
		sublowbound
	      else
		indexoffset := -lmin;
	      if typtr^.size > 1 then
		mac3(221b (*IMULI*),regc,typtr^.size);
	      indexr := regc ;
	    end ;
	    if lsp^.arraypf then begin
	      incrementregc;
	      if indexr=oldic then begin
		incrementregc;
		indexr := 0
	      end;
	      mac4(571b (*HRREI*),regc,indexr,indexoffset);
	      incrementregc;			(*TEST FOR IDIVI-INSTRUCTION*)
	      regc := regc-1;
	      indexoffset := 0;
	      mac3(515b (*HRLZI*),regc-1,440001b+(typtr^.bitsize*100b));
	      mac3(231b (*IDIVI*),regc,bitmax div lsp^.aeltype^.bitsize);
	      mac3(133b (*IBP*),0,regc-1);
	      mac3r(365b (*SOJGE*),regc+1,ic-1);
	      bpaddr := regc-1;
	      packfg := packk;
	      indexr := regc;
	    end;
	    dplmt := dplmt + indexoffset ;
	    kind := varbl ;
	    if ( oldic <> indexr ) andif ( oldic <> 0 ) then begin
	      if oldic > regcmax then begin
		if oldic = basis then
		  mac4 (271b (*addi*),indexr,oldic,0)
		else mac3(270b (*ADD*),indexr,oldic)
	      end
	      else begin
		if indexr = basis then
		  mac4 (271b(*addi*),oldic,indexr,0)
		else mac3(270b (*ADD*),oldic,indexr);
		regc := regc - 1;
		indexr := oldic
	      end
	    end
	  end
	end (*WITH.. IF TYPTR # NIL*);
      gattr := lattr ;
      if (sy = rbrack) orif chkspecial (rparent) then
	insymbol
      else
	error(155)
    end						(*IF SY = LBRACK*)
    else
    (*.*)
    if sy = period then begin
      with gattr do begin
	if typtr <> nil then
	  if typtr^.form <> records then begin
	    error(308);
	    typtr := nil
	  end;
	if indbit=1 then
	  getparaddr
	else if virtual then
	  if vsclass = externalsc then
	      loadaddress;
	insymbol;
	if sy = ident then begin
	  if typtr <> nil then begin
	    srchsection(typtr^.fstfld,lcp);
	    if lcp = nil then begin
	      error(309);
	      typtr := nil
	    end
	    else
	      with lcp^ do begin
		typtr := idtype;
		packfg := packf;
		if packfg = packk then begin
		  if virtual then vid := lcp;
		  bpaddr := fldaddr
		end
		else
		  dplmt := dplmt + fldaddr;
	      end
	  end;
	  insymbol
	end					(*SY = IDENT*)
	else
	  error(209)
      end					(*WITH GATTR*)
    end						(*IF SY = PERIOD*)
    else
    (*^*)
    begin
      if gattr.typtr <> nil then
	with gattr,typtr^ do
	  if (form = pointer) orif (form = files) then begin
	    old_typtr := typtr;
	    if form = pointer then
	      typtr := eltype
	    else
	      typtr := filtype;
	    if typtr <> nil then
	      with gattr do begin
		loadnoptr := false;
		if (old_typtr^.form = pointer) andif (old_typtr^.v_offset) then
		  vload (gattr)
		else
		  load (gattr);
		loadnoptr := true;
		if runtmcheck then begin
		  mac3(332b (*SKIPE*),0,reg);
		  if virtual then begin
		    mac3b (316b(*camn*),reg,v_nil);
		    v_nil := ic - 1;
		  end
		  else begin
		    mac3(306b (*CAIN*),reg,377777b (*NIL*));
		  end;
		  support(pointererror);
		end;
		indexr := reg;
		dplmt := 0;
		indbit:=0;
		if virtual then vlevel := 0;
		packfg := notpack;
		kind := varbl;
		vrelbyte:= no;
		vsclass := localsc
	      end
	  end
	  else
	    error(407);
      insymbol
    end;
    iferrskip(166,fsys or selsys)
  end;
  (*WHILE*)
  with gattr do
   if kind = varbl then
    if typtr<>nil then
      if typtr^.size = 2 then begin
	if indbit = 1 then
	  getparaddr;
	if (indexr>regin) andif (indexr<=regcmax) then
	  incrementregc
      end
end (*SELECTOR*).
   