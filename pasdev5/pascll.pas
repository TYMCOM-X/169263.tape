$OPTIONS SPECIAL, NOCHECK
$TITLE call
(*      Conditional compilation switches used:
	Y4:  Unless disabled, support double precision reals    *)
$INCLUDE PASDCL.INC
$INCLUDE PASLEX.INC
$INCLUDE PASCG.INC
$INCLUDE PASSTR.INC
$INCLUDE PASCEX.INC
$INCLUDE PASFB.INC
$INCLUDE PASCMP.INC

public procedure call(fsys: setofsys; fcp: ctp);

var
    lkey: pfkey;
    lfollowerror, norightparent : boolean;
$PAGE getfilename


  procedure getfilename(defaultname:alfa; textreq:boolean) ;

  var
      lcp : ctp ;
      lvlev: levrange;
      default,defaulttty : boolean ;
      lsy : symbol;
      lid: alfa;
      indirect: boolean;
      filestp: stp;

  begin
    default := true ;
    defaulttty := false;
    norightparent := true;
    indirect := false;
    if sy = lparent then begin
      norightparent := false;
      insymbol ;
      if sy = ident then begin
	srchid([konst,vars,field,proc,func],lcp);
	if lcp <> nil then
	  with lcp^ do
	    if idtype <> nil then
	      with idtype^ do begin
		if form = files then begin
		  filestp := filtype;
		  default := false;
		end
		else if form = pointer then begin
		  if eltype <> nil then
		    if eltype^.form = files then begin
		      filestp := eltype^.filtype;
		      default := false;
		      indirect := true;
		    end;
		end;
		if not default then begin (* FOUND A FILE *)
		  if textreq then
		    if not (comptypes (filestp, charptr)) then
		      error (366)
		end;
		if klass = vars then
		  lvlev := vlev
		else
		  lvlev := 1
	      end;
	if (lvlev = 0) andif (id = 'TTY             ') andif
	  ((defaultname = 'OUTPUT    ') orif (defaultname = 'TTYOUTPUT '))
	    then begin
	      default := true;
	      defaulttty := true;
	      defaultname := 'TTYOUTPUT '
	    end
      end ;
    end ;
    if default then begin
      lid := id;
      id := defaultname;
      srchid([vars],lcp);
      id := lid;
      if lcp^.idtype^.form <> files then begin
	lcp := display[0].fname;
	while lcp^.name <> defaultname do
	  with lcp^ do
	    if name < defaultname then
	      lcp := rlink
	    else
	      lcp := llink
      end
    end ;
    lsy := sy;
    if indirect then
      insymbol
    else
      sy := comma;
    lfollowerror := followerror; (*TO SUPRESS DOUBLE ERRORMESSAGE
				  IF NAME DETECTED HERE NOT USED IN GETFILENAME  *)
    selector(fsys or [comma,rparent],lcp) ;
    loadaddress ;
    if gattr.typtr <> nil then
      if gattr.typtr^.form <> files then
	errandskip (458, fsys or [rparent, comma]);
    followerror := lfollowerror;
    if not indirect then
      sy := lsy;
    if not default or defaulttty then begin
      if not indirect then
	insymbol;
      if sy = comma then
	insymbol
      else if sy <> rparent then
	error(158)
    end ;
  end (*GETFILENAME*);
$PAGE variable


  procedure variable(fsys: setofsys);

  var
      lcp: ctp;

  begin
    if sy = ident then begin
      srchid([vars,field],lcp);
      insymbol
    end
    else begin
      error(209);
      lcp := uvarptr
    end;
    selector(fsys,lcp)
  end (*VARIABLE*);
$PAGE getputresetrewrite


  procedure getputresetrewrite;

  var
      adr : supports ;
      default : array [1..5] of boolean;
      i : integer;
      lcp : ctp;


    procedure getstringaddress ;

    begin
      if sy=comma then begin
	insymbol;
	expression(fsys or [comma,rparent],onfixedregc);
	if not strform (gattr.typtr) then
	  error (212)
	else begin
	  strdesc (reginp2, gattr);
	  regc := reginp3
	end
      end
      else begin (* DEFAULT IS NULL FILE NAME *)
	regc := reginp3; (* LOC OF LENGTH WORD *)
	mac3 (201b (*MOVEI*), regc, 0)
      end
    end ;

  begin
    if (lkey = 6) andif (id = 'TTY       ') then begin
      srchid([vars],lcp);
      if lcp <> nil then
	if lcp^.vlev = 0 then
	  id := 'TTYOUTPUT '
    end;
    variable( fsys or [rparent,comma] ) ;
    loadaddress ;
    if gattr.typtr <> nil then
      if gattr.typtr^.form <> files then
	errandskip(458,fsys or [rparent])
      else begin
	if lkey>=5 then begin
	  getstringaddress (* OF FILENAME *);
	  if lkey = 6 (* REWRITE *)then begin
	    if sy = comma then begin (* HAVE APPEND FLAG *)
	      insymbol;
	      expression (fsys or [rparent], onfixedregc);
	      if gattr.typtr <> nil then begin
		load (gattr)
	      end
	    end
	    else begin
	      mac3 (201b (*MOVEI*), reginp4, 0); (* DEFAULT APPEND = FALSE *)
	    end
	  end
	end;
	case lkey of
	  1:
	    if comptypes(gattr.typtr^.filtype,charptr) then
	      adr := getcharacter
	    else
	      adr:= getfile ;
	  2:
	    adr:= getline ;
	  3:
	    adr:= putfile ;
	  4:
	    adr:= putline ;
	  5:
	    adr:= resetfile ;
	  6:
	    adr:= rewritefile;
	  25:
	    adr := openfile
	end ;
	support(adr) ;
      end ;
  end;
$PAGE readreadln


  procedure readreadln;

  var
      slattr : attr;
      srmin, srmax : integer;
      laddr : supports;

  begin
    getfilename('INPUT     ',true);
    if (lkey = 7) or ((lkey = 8) and ((sy <> rparent) and
      (sy in (facbegsys-[lbrack]) or [addop]))) then
	loop
	  laddr := readinteger; (* IN CASE OF ERRORS *)
	  variable(fsys or [comma,rparent]);
	  loadaddress;
	  if gattr.typtr<>nil then begin
	    if gattr.typtr^.form<=subrange then
	      if comptypes(intptr,gattr.typtr) then
		laddr := readinteger
	      else if comptypes(realptr,gattr.typtr) then begin
	      (*$Y4           IF DBLREAL THEN
				LADDR := READDREAL
			      ELSE    *)
		laddr := readreal;
	      end
	      else if comptypes(charptr,gattr.typtr) then
		laddr := readcharacter
	      else
		error(169)
	    else
	      error(458);
	    regc := regc - 1;
	    support(laddr);
	    if runtmcheck andif (gattr.typtr^.form = subrange) then begin
	      getbounds(gattr.typtr,srmin,srmax);
	      with slattr do begin
		typtr := intptr;
		kind := cst;
		cval.ival := srmax
	      end;
	      mac4 (200b (*MOVE*),regc+1,gattr.indexr,0);
	      makecode(317b (*CAMG*),regc+1,slattr);
	      slattr.kind := cst;
	      slattr.typtr := intptr;
	      slattr.cval.ival := srmin;
	      makecode(315b (*CAMGE*),regc+1,slattr);
	      support(errorinassignment)
	    end (*RUNTMCHECK*);
	  end;
	exit if sy <> comma;
	  insymbol
	end;
    if lkey = 8 then
      support(getline)
  end (*READREADLN*);
$PAGE close, break


  procedure close;

  begin
    getfilename('INPUT     ',false);
    support(closeop);
  end;


  procedure break;

  begin
    getfilename('TTYOUTPUT ',false);
    support(breakoutput) ;
  end ;
$PAGE writewriteln


  procedure writewriteln;

  var
      lsp: stp;
      default,realformat,writeoct: boolean;
      lsize,lmin,lmax: integer;
      laddr: supports;
      lac: acrange;
      llc: addrrange;
      wrsubstr: boolean;

  begin
    llc := lc;
    getfilename('OUTPUT    ',true);
    if (lkey = 10) orif ((lkey = 11) andif ((sy <> rparent) andif
      (sy in (facbegsys-[lbrack]) or [addop]))) then
	loop
	  laddr := writeinteger; (* IN CASE OF ERRORS *)
	  expression(fsys or [comma,colon,rparent],onfixedregc);
	  lsp := gattr.typtr;
	  lsize := lgth;
	  writeoct := false;
	  wrsubstr := false;
	  if lsp <> nil then
	    if lsp^.form <= power then begin
	      load(gattr);
	      (*$Y4         IF DBLREAL ANDIF (LSP = REALPTR) THEN BEGIN
			      WITH CODE, INSTRUCTION[CIX] DO
				IF INSTR = 120B (*DMOVE*) THEN
				  INSTR := 200B (*MOVE*);
			      REGC := REGC - 1;
			    END;                *)
	    end
	    else begin
	      if gattr.kind = expr then begin
		with gattr do begin
		  lac := reg;
		  packfg := notpack;
		  kind := varbl;
		  indexr := basis;
		  indbit := 0;
		  dplmt := lc;
		  vlevel := 1;
		  vsclass := localsc;
		  vrelbyte := no;
		  checklc (lsp^.size);
		  store (lac, gattr);
		  regc := reginp1
		end;
		loadaddress
	      end
	      else if gattr.kind in [varbl, cst] then begin
		if lsp^.form = strings then
		  regc := reginp1; (* ASSURE ADDRESS GOES IN REGINP2 *)
		loadaddress
	      end
	      else (* KIND = SUBSTR *)begin
		regc := reginp3; (* USER LENGTH GOES IN REGIN + 4 *)
		wrsubstr := true
	      end
	    end;
	  if sy = colon then begin
	    insymbol;
	    expression(fsys or [comma,colon,rparent],onfixedregc);
	    if gattr.typtr <> nil then
	      if gattr.typtr <> intptr then
		error(458);
	    load(gattr);
	    default := false;
	  end
	  else begin
	    default := true;
	    incrementregc (*RESERVE REGISTER FOR DEFAULT VALUE*)
	  end ;
	  if lsp = intptr then begin
	    laddr := writeinteger ;
	    lsize := 12
	  end;
	  if sy = colon then begin
	    insymbol;
	    if (sy = ident) andif ((id='O       ') orif (id='H       '))
	      then begin
		if not comptypes(lsp,intptr) then
		  error(262);
		if id = 'O         ' then
		  laddr := writeoctal
		else begin
		  laddr := writehexadecimal;
		  lsize := 11
		end;
		insymbol
	      end
	    else begin
	      expression(fsys or [comma,rparent],onfixedregc);
	      if gattr.typtr <> nil then
		if gattr.typtr <> intptr then
		  error(458);
	      if lsp <> realptr then
		error(258);
	      load(gattr);
	      realformat := false
	    end
	  end
	  else
	    realformat := true;
	  if lsp = intptr then
	    goto 1;
	  if lsp = charptr then begin
	    lsize := 1;
	    laddr := writecharacter
	  end
	  else if lsp = realptr then begin
	    lsize := 16;
	    laddr := writereal;
	    if realformat then
	      mac3(201b (*MOVEI*),reginp4,123456b);
	  end
	  else if lsp = boolptr then begin
	    lsize := 6;
	    laddr := writeboolean
	  end
	  else if lsp <> nil then begin
	    if lsp^.form = scalar then
	      error(169)
	    else if string(lsp) then begin
	      if lsp^.inxtype<>nil then begin
		getbounds(lsp^.inxtype,lmin,lmax);
		lsize := lmax-lmin+1;
	      end;
	      mac3(201b (*MOVEI*),reginp4,lsize);
	      if lsp^.arraypf then
		laddr := writepackedstring
	      else
		laddr := writestring ;
	    end
	    else if lsp^.form = strings then begin
	      if wrsubstr then begin
		laddr := writesubstring;
		if default then
		  mac3 (200b (*MOVE*), reginp4, reginp3);
		default := false
	      end
	      else if lsp^.skind = varying then begin
		laddr := writevaryingstring;
		if default then
		  mac4 (200b (*MOVE*), reginp3, reginp2, 0); (* GET CURRENT LENGTH *)
		default := false;
	      end
	      else (* SKIND = NONVARYING *)begin
		laddr := writepackedstring;
		lsize := lsp^.maxlength;
		mac3 (201b (*MOVEI*),reginp4,lsize)
	      end
	    end
	    else
	      error(458)
	  end;
	  1:
	    if default then
	      mac3(201b (*MOVEI*),reginp3,lsize);
	  support(laddr);
	  regc :=reginp1;
	  lc := llc;
	exit if sy <> comma;
	  insymbol
	end;
    if lkey = 11 then
      support(putline) ;
  end (*WRITE*);
  (* HELPING FUNCTION FOR SIZE  -- RETURNS THE
     SIZE OF A TYPE. IF THE NEXT SYMBOL IS A
     COMMA, A VARIANT VALUE IS ASSUMED TO FOLLOW,
     AND THE SIZE IS ADJUSTED ACCORDINGLY *)
$PAGE get_a_file_name

procedure saveregs; forward;

procedure restoreregs; forward;

procedure get_a_file_name;

var
  lregc: acrange;

begin
  saveregs;
  lregc := regc;
  regc := regin;
  variable (fsys or [rparent]);
  loadaddress;
  if gattr.typtr <> nil then
    if gattr.typtr^.form <> files then
      error (458)
    else with gattr do begin
      typtr := dclstring (80, varying);
      kind := varbl;
      packfg := notpack;
      vrelbyte := no;
      vsclass := localsc;
      vlevel := 1;
      indbit := 0;
      indexr := basis;
      dplmt := lc;
      checklc (typtr^.size);
      mac4 (201b(*movei*), hac, basis, dplmt);
      support (getfname);
    end;
  restoreregs;
  regc := lregc;
end;
$PAGE getsize


  function getsize (fsp: stp): addrrange;

  var
      lsize: addrrange;
      varp, vstp: stp;
      vval: valu;
      err: boolean;

  begin
    if fsp = nil then
      lsize := 0
    else
      lsize := fsp^.size;
    if sy = comma then begin
      insymbol;
      if fsp = nil then
	errandskip (408, fsys + [rparent])
      else begin
	with fsp^ do
	  case form of
	    records:
	      varp := recvar;
	    variant:
	      varp := subvar;
	    others:
	      varp := nil
	  end;
	constant (fsys + [comma, rparent], vstp, vval);
	if varp = nil then
	  errandskip (408, fsys + [rparent])
	else if string (vstp) or (vstp = realptr) then
	  errandskip (460, fsys + [rparent])
	else begin
	  with varp^ do
	    if form = tagfwithid then
	      if tagfieldp = nil then
		err := true
	      else
		err := not comptypes (vstp, tagfieldp^.idtype)
	    else
	      err := not comptypes (vstp, tagfieldtype);
	  if err then
	    errandskip (459, fsys + [rparent])
	  else begin (* FIND MATCHING VARIANT *)
	    lsize := varp^.size;
	    varp := varp^.fstvar;
	    while varp <> nil do
	      with varp^ do begin
		if varval.ival = vval.ival then begin
		  lsize := getsize (varp);
		  goto 1
		end;
		varp := nxtvar
	      end;
	  end
	end
      end
    end (* SY = COMMA *);
    1:
      getsize := lsize
  end;
$PAGE size


  procedure size;

  var
      lcp: ctp;
      lsize: addrrange;

  begin
    lsize := 0;
    if sy <> ident then
      error (459)
    else begin
      srchid ([vars, types, field], lcp);
      insymbol;
      with lcp^ do begin
	if idtype <> nil then
	  lsize := getsize (idtype)
      end;
    end;
    with gattr do begin
      typtr := intptr;
      kind := cst;
      cval.ival := lsize
    end;
  end;
$PAGE uplowcase


  procedure ulcase (reg: acrange);

  begin
    if lkey = 17 then begin
      mac3 (301b (*CAIL*), reg, ord ('a'));
      mac3 (303b (*CAILE*), reg, ord ('z'));
      mac3 (304b (*CAIA*), 0, 0); (* CHAR NOT LOWER CASE SKIP *)
      mac3 (275b (*SUBI*), reg, 40b)
    end
    else begin
      mac3 (301b (*CAIL*), reg, ord ('A'));
      mac3 (303b (*CAILE*), reg, ord ('Z'));
      mac3 (304b (*CAIA*), 0, 0); (* CHAR NOT UPPER CASE, SKIP *)
      mac3 (271b (*ADDI*), reg, 40b)
    end
  end;


  procedure uplowcase;

  var
      lattr: attr;
      pix: integer;

  begin
    with gattr do begin
      if not strform (typtr) then
	error (459)
      else begin
	if comptypes (typtr, charptr) then begin
	  load (gattr);
	  ulcase (reg)
	end
	else (* EXPR IS FULL STRING *)
	begin
	  if gattr.kind <> substrng then begin (* MAKE IT A DESCRIPTOR *)
	    incrementregc;
	    strdesc (regc, gattr);
	    incrementregc
	  end;
	  with lattr do begin (* ALLOC TEMP RESULT *)
	    typtr := gattr.typtr; (* RESULT HAS SMAE 'SHAPE' AS ORIGINAL *)
	    kind := varbl;
	    vrelbyte := no;
	    packfg := notpack;
	    vsclass := localsc;
	    dplmt := lc;
	    indexr := basis;
	    indbit := 0;
	    vlevel := 1;
	    checklc (typtr^.size);
	  end;
	  if strkind (typtr) = varying (* STORE LENGTH *)
	  then
	    mac4 (202b (*MOVEM*), (*GATTR*)reg+1, basis, lattr.dplmt);
	  mac3r (322b (*JUMPE*), (*GATTR*)reg+1, 0); (* SKIP IF LENGTH = 0 *)
	  pix := cix; (* FILL IN ADDRESS LATER *)
	  incrementregc; (* LOAD BYTE PTR TO TEMP *)
	  mac4 (201b (*MOVEI*), regc, basis, lattr.dplmt + ord (strkind (typtr)
	    = varying));
	    (* HACK GIVES OFFSET 1 IF VARYING *)
	  mac3 (505b (*HRLI*), regc, 440700b); (* MAKE IT A BYTE PTR *)
	  incrementregc; (* REG TO PUT CHARS IN *)
	  mac3 (134b (*ILDB*), regc, (*GATTR*)reg); (* GET NEXT CHAR FROM STRING *)
	  ulcase (regc); (* PERFORM FUNCTION *)
	  mac3 (136b (*IDPB*), regc, regc-1); (* STORE IN RESULT *)
	  mac3r (367b (*SOJG*), (*GATTR*)reg+1, ic-6);
	  insertaddr (right,pix,ic); (* TARGET OF BRANCH ON ZERO LENGTH *)
	  regc := reg-1; (* THROW AWAY UNUSED REGS *)
	  gattr := lattr (* RETURN TEMP AS RESULT *)
	end
      end
    end;
  end;
$PAGE upperlowerbound


  procedure upperlowerbound;

  var
      i, bound, min: integer;
      lcp: ctp;
      lsp: stp;
      result_type: stp;

  begin
    result_type := intptr;
    if sy = ident then begin
      srchid ([types,vars,func],lcp);
      insymbol;
      lsp := lcp^.idtype;
      if lsp <> nil then begin
	if lsp^.form = strings then
	  if lkey = 26 then
	    bound := lsp^.maxlength
	  else
	    bound := 1
	else if lsp^.form = arrays then begin
	  if sy = comma then begin
	    insymbol;
	    if sy = intconst then
	      if val.ival < 1 then
		error (459)
	      else
		for i := 2 to val.ival do
		  if lsp^.aeltype <> nil then
		    lsp := lsp^.aeltype
		  else
		    error (557)
	    else
	      error (255);
	    insymbol;
	  end;
	  if lsp <> nil then begin
	    result_type := lsp^.inxtype;
	    getbounds (lsp^.inxtype,min,bound);
	    if lkey = 27 then
	      bound := min;
	  end
	end
	else
	  errandskip (319,fsys or [rparent])
      end;
    end
    else
      errandskip (209,fsys or [rparent]);
    with gattr do begin
      typtr := result_type;
      kind := cst;
      cval.ival := bound
    end
  end;
$PAGE new


  procedure new;

  const
      tagfmax=5;

  var
      base_reg: acrange;
      lsp,lsp1: stp;
      lcp: ctp;
      varts,lmin,lmax,correction: integer;
      firstload, area_specified, rescan: boolean;
      lsize,lsz: addrrange;
      lval: valu;
      lattr, area_attr: attr;
      i,tagfc: integer;
      tagfsav: array[0..tagfmax] of record
	tagfval: integer;
	tagfaddr: addrrange;
	lpackkind:packkind;
	tagfexists: boolean
      end;

  begin
    alc_used := true;
    incrementregc;
  area_specified := false;
  rescan := false;
  if virtual then begin
    if sy = intconst then begin
      area_specified := true;
      with area_attr do begin
	typtr := intptr;
	kind := cst;
	cval.ival := val.ival;
	insymbol;
      end;
    end
    else if sy = ident then begin
      srchid ([vars,field,func,konst],lcp);
      insymbol;
      if lcp^.klass = konst then begin
	area_specified := true;
	with lcp^, area_attr do begin
	  if comptypes (idtype,intptr) then begin
	    typtr := idtype;
	    kind := cst;
	    cval.iv values.ival;
	  end
	  else begin
	    error (459);
	    typtr := nil;
	  end
	end;
      end
    end
    else begin
      error (209);
      lcp := uvarptr;
    end;
    if not area_specified then begin
      selector (fsys or [rparent,comma,colon],lcp);
      if comptypes (gattr.typtr,intptr) then begin
	area_specified := true;
	area_attr := gattr;
      end
      else rescan := true;
    end;
    if area_specified then
      if area_attr.kind <> cst then
	load (area_attr);
    if (sy = comma) and not rescan then
      insymbol;
  end;
  if area_specified or not rescan then begin
    if sy = ident then begin
      srchid ([vars,field,func],lcp);
      insymbol
    end
    else begin
      error (209);
      lcp := uvarptr;
    end;
    selector (fsys or [rparent,comma,colon],lcp);
  end;
    lsp := nil;
    varts := 0;
    lsize := 0;
    tagfc := -1;
    lattr := gattr;
    if gattr.typtr <> nil then
      with gattr.typtr^ do
	if form = pointer then begin
	  if eltype <> nil then begin
	    lsize := eltype^.size;
	    if eltype^.form = files then
	      lsize := lsize + sizeoffileblock;
	    if eltype^.form = records then begin
	      lsp := eltype^.recvar;
	    end
	    else if eltype^.form = arrays then
	      lsp := eltype
	  end
	end
	else
	  error(458);
    while sy = comma do begin
      insymbol;
      constant(fsys or [comma,colon,rparent],lsp1,lval);
      varts := varts + 1;
      (*CHECK TO INSERTADDR HERE: IS CONSTANT IN TAGFIELDTYPE RANGE*)
      if lsp = nil then
	error(408)
      else if string(lsp1) or (lsp1=realptr) then
	error(460)
      else begin
	tagfc := tagfc + 1;
	if tagfc > tagfmax then begin
	  error(409);
	  tagfc := tagfmax;
	  goto 1
	end;
	tagfsav[tagfc].tagfexists := true;
	if lsp^.form = tagfwithid then begin
	  if lsp^.tagfieldp <> nil then
	    if comptypes(lsp^.tagfieldp^.idtype,lsp1) then
	      with tagfsav[tagfc],lsp^.tagfieldp^ do begin
		tagfval := lval.ival;
		tagfaddr:= fldaddr;
		lpackkind:= packf
	      end
	    else begin
	      error(458);
	      goto 1
	    end
	end
	else if lsp^.form=tagfwithoutid then begin
	  tagfsav[tagfc].tagfexists := false;
	  if not comptypes(lsp^.tagfieldtype,lsp1) then begin
	    error(458);
	    goto 1
	  end
	end
	else begin
	  error(358);
	  goto 1
	end;
	lsp1 := lsp^.fstvar;
	while lsp1 <> nil do
	  with lsp1^ do
	    if varval.ival = lval.ival then begin
	      lsize :=size;
	      lsp := subvar;
	      goto 1
	    end
	    else
	      lsp1:=nxtvar;
	lsize := lsp^.size;
	lsp := nil
      end;
      1:
    end (*WHILE*);
    if sy = colon then begin
      insymbol;
      expression(fsys or [rparent],onregc);
      if lsp = nil then
	error(408)
      else if lsp^.form <> arrays then
	error(259)
      else begin
	if not comptypes(gattr.typtr,lsp^.inxtype) then
	  error(458);
	lsz := 1;
	lmin := 1;
	if lsp^.inxtype <> nil then
	  getbounds(lsp^.inxtype,lmin,lmax);
	if lsp^.aeltype <> nil then
	  lsz := lsp^.aeltype^.size;
	load(gattr);
	if lsz <> 1 then
	  mac3(221b (*IMULI*),regc,lsz);
	if lsp^.arraypf then begin
	  lsz := bitmax div lsp^.aeltype^.bitsize; (* NUMBER ELEMENTS PER WORD *)
	  correction := lsz-1; (* ADD TO INDEX PRIOR TO IDIVI TO DETERMINE # WORDS REQUIRED *)
	  (* MUST ALSO CORRECT INDEX IF LOWERBOUND <> 1 TO DETERMINE # ELEMENTS ALLOCATED *)
	  if lmin > 1 then
	    correction := correction - (lmin-1)
	  else if lmin < 1 then
	    correction := correction + (1-lmin);
	  mac3(271b (*ADDI*),regc,correction);
	  incrementregc;
	  regc := regc - 1;
	  (*FOR TESTING BECAUSE IDIV WORKS ON AC+1 TOO*)
	  mac3 (231b (*IDIVI*),regc,lsz);
	  lsz := lsize - lsp^.size;
	end
	else
	  lsz := lsize - lsp^.size - lsz*(lmin - 1);
	mac4(201b (*MOVEI*),reginp1,regc,lsz);
      end;
      if not specialsw then
	error (466)
    end
    else begin
      mac3(201b (*MOVEI*),reginp1,lsize);
    end;
    if area_specified then begin
      if area_attr.kind = cst then
	mac3 (201b(*movei*),tac,area_attr.cval.ival)
      else mac3 (200b(*move*),tac,area_attr.reg);
    end;
    if (lattr.indexr <> 0) andif (lattr.indexr <= regcmax) then
      mac3(261b (*PUSH*),topp,lattr.indexr);
    if area_specified then
      support (a_new)
    else if virtual then
      support (v_new)
    else support (allocate);
    regc := reginp1;
    if lsize > 0 (* ASSURES FOLLOWING POINTERS ARE NON-NIL *)
    then
      if lattr.typtr^.eltype^.form = files then
	with lattr.typtr^ do begin
	(* Parameter in AC3 to INITFILEBLOCK is:
	    left-hw  = negative component size
	    right-hw = zero if text file, otherwise 14B *)
	  mac3 (515b (*HRLZI*),reginp2,-eltype^.size); (* OK if text file *)
	  if not comptypes (eltype, textptr) then
	    mac3 (541b (*HRRI*), reginp2, 14b); (* identify as non-text *)
	  if virtual then begin
	    (* have virtual address rather than real address in ac2: save
	       virtual address and pass real address *)
	    mac3 (261b (*push*),topp,reginp1);
	    mac3 (200b (*move*),reginp1,tac);
	  end;
	  support (initfileblock);
	  if virtual then
	    mac3 (262b (*pop*),topp,reginp1);	(* restore virtual address *)
	    (* need not restore real address since no tagfields possible *)
	end;
    firstload := true;
    if virtual then base_reg := tac	(* holds real address *)
    else base_reg := regc;
    for i := 0 to tagfc do
      with tagfsav[i] do
	if tagfexists then begin
	  mac3(201b (*MOVEI*),hac,tagfval);
	  case lpackkind of
	    notpack:
	      mac4(202b (*MOVEM*),hac,base_reg,tagfaddr);
	    hwordr:
	      mac4(542b (*HRRM*),hac,base_reg,tagfaddr);
	    hwordl:
	      mac4(506b (*HRLM*),hac,base_reg,tagfaddr);
	    packk : begin
	      if firstload then begin
		if not virtual then mac3(200b (*MOVE*),tac,regc);
		firstload := false
	      end;
	      mac3r(137b (*DPB*),hac,tagfaddr)
	    end
	  end (*CASE*)
	end;
    if (lattr.indexr <> 0) andif (lattr.indexr <= regcmax) then
      mac3(262b (*POP*),topp,lattr.indexr);
    store(regc,lattr)
  end (*NEW*);
$PAGE mark
procedure mark;

begin
  if virtual then support (v_mark)
  else support (markop);
  regc := reginp1;
  variable (fsys or [rparent]);
  if comptypes (intptr, gattr.typtr) then
    store (reginp1, gattr)
  else error (458);
end (* mark *);

(* following version only if mark/release allowed in areas *)

(*
var area_specified, skip: boolean;
    area_attr: attr;
    lcp: ctp;

begin
  alc_used := true;
  regc := reginp1;
  area_specified := false;
  skip := false;
  if virtual then begin
    if sy = intconst then begin
      area_specified := true;
      with area_attr do begin
	typtr := intptr;
	kind := cst;
	cval.ival := val.ival;
	insymbol;
      end;
    end
    else if sy = ident then begin
      srchid ([vars,func,konst],lcp);
      insymbol;
      if lcp^.klass = konst then begin
	area_specified := true;
	with lcp^, area_attr do begin
	  typtr := idtype;
	  kind := cst;
	  cval.ival := values.ival;
	end;
      end
    end
    else begin
      error (209);
      lcp := uvarptr;
    end;
    if not area_specified then
      selector (fsys or [rparent,comma],lcp);
    if sy = comma then begin
      insymbol;
      if not area_specified then
	area_attr := gattr;
      area_specified := true;
    end
    else if area_specified then
      error (158)
    else skip := true;
  end;
  if area_specified then
    if not comptypes (area_attr.typtr,intptr) then
      error (459);
  if area_specified or not skip then begin
    variable (fsys or [rparent]);
  end;
  if gattr.indexr <> 0 then
    mac3 (261b(*push*),topp,gattr.indexr);
  if virtual then
    if area_specified then begin
      if area_attr.kind = cst then
	mac3 (201b(*movei*),tac,area_attr.cval.ival)
      else begin
	load (area_attr);
	mac3 (200b(*move*),tac,area_attr.reg);
      end;
      support (a_mark)
    end
    else support (v_mark)
  else support (markop);
  if gattr.indexr <> 0 then 
    mac3 (262b(*pop*),topp,gattr.indexr);
  store (reginp1,gattr);
    if not specialsw then
      error (466)
  end (*MARK*); *)
$PAGE release
  procedure release;

  begin
    alc_used := true;
    expression (fsys or [rparent],onregc);
    if gattr.typtr = intptr then begin
      makecode (200b (*move*), reginp1, gattr);
      if gattr.reg <> reginp1 then
	mac3 (200b (*move*), reginp1, gattr.reg);
      regc := reginp1;
      if virtual then
	support (v_release)
      else support (releaseop);
    end
    else
      error(458);
    if not specialsw then
      error (466)
  end (*RELEASE*);
$PAGE dispose


  procedure dispose;



  begin
    alc_used := true;
    expression (fsys or [rparent], onregc);
    if (gattr.typtr <> nil) andif (gattr.typtr^.form = pointer) then begin
      load (gattr);
      if gattr.reg <> reginp1 then
	mac3 (200b, reginp1, gattr.reg);
      if gattr.typtr^.eltype <> nil then
	if gattr.typtr^.eltype^.form = files then (* UNCHAIN FROM RUNTIME *)
	  support (unchainfile);
      if virtual then
	support (v_dispose)
      else support (disposeop);
    end
    else
      error (458)
  end;
$PAGE get_root, set_root

procedure get_root;

begin
  if gattr.typtr <> nil then begin
    if comptypes (gattr.typtr,intptr) then begin
      mac3 (201b(*movei*),tac,gattr.reg);
      support (getroot);
      gattr.typtr := nilptr;
    end
    else begin
      error (459);
      gattr.typtr := nil;
    end;
  end;
end;

procedure set_root;

begin
  expression (fsys or [rparent,comma],onfixedregc);
  load (gattr);
  if not comptypes (gattr.typtr,intptr) then
    error (459);
  if sy = comma then
    insymbol
  else
    error (1158);
  expression (fsys or [rparent,comma],onfixedregc);
  load (gattr);
  if gattr.typtr <> nil then begin
    if gattr.typtr^.form <> pointer then
      error (459)
    else
      support (setroot);
  end;
end;
$PAGE areaid_or_offset

procedure areaid_or_offset;

var reloc: no..both;

begin
  if gattr.typtr <> nil then begin
    if gattr.typtr^.form <> pointer then
      error (459)
    else begin
      if lkey = 34 (* offset *)then begin
	mac3b (630b (* tdz *), gattr.reg,area_mask);
	area_mask := ic - 1;
      end
      else begin
	if shr_area = 0 then
	  reloc := no
	else reloc := right;
	mac (reloc,242b(*lsh*),gattr.reg,1,0,shr_area);
	shr_area := ic-1;
      end
    end;
    gattr.typtr := intptr;
  end;
end;
$PAGE make_pointer

procedure make_pointer;

var
    lattr: attr;
    instr: instrange;
    reloc: no..both;

begin
  if not comptypes (gattr.typtr,intptr) then
    error (459);
  load (gattr);
  lattr := gattr;
  if sy = comma then
    insymbol
  else
    error (158);
  expression (fsys or [rparent],onregc);
  if gattr.typtr <> nil then begin
    if gattr.typtr^.form <> pointer then
      error (459)
    else
      with gattr do begin
	load (gattr);
	mac3b (316b(*camn*),reg,v_nil);
	v_nil := ic-1;
	mac3r (254b(*jrst*),0,ic+5);
	mac3 (250b(*exch*),reg,lattr.reg);
	if shl_area = 0 then reloc := no
	else reloc := right;
	mac (reloc,242b(*lsh*),reg,1,0,shl_area);
	shl_area := ic-1;
	mac3b (630b(*tdz*),lattr.reg,area_mask);
	area_mask := ic-1;
	mac3 (270b(*add*),reg,lattr.reg);
	kind := expr;
	typtr := nilptr;
      end;
  end;
end;
$PAGE channel, getchannel, freechannel


  procedure channel;

  begin
    getfilename ('TTY       ',false);
    with gattr do begin
      kind := expr;
      reg := indexr;
      mac4 (554b (*HLRZ*), reg, reg, filchn);
      typtr := intptr
    end
  end;


  procedure getchannel;

  begin
    variable (fsys or [rparent]);
    if gattr.typtr <> nil then
      if not comptypes (intptr, gattr.typtr) then
	error (458)
      else begin
	support (getchan);
	store (0, gattr)
      end
  end;


  procedure freechannel;

  begin
    expression (fsys or [rparent], onregc);
    if gattr.typtr <> nil then
      if not comptypes (intptr, gattr.typtr) then
	error (458)
      else begin
	makecode (200b (*MOVE*), 0, gattr);
	if gattr.reg <> 0 then
	  mac3 (200b, 0, gattr.reg);
	support (freechan)
      end
  end;
$PAGE getlinenr, getintegerfilename


  procedure getlinenr;

  begin
    getfilename('INPUT     ',true);
    variable(fsys or [rparent]);
    if gattr.typtr <> nil then
      if comptypes(charptr,gattr.typtr^.aeltype) and
	(gattr.typtr^.form = arrays) then begin
	  mac4(200b (*MOVE*),regc,regc,fillnr);
	  store(regc,gattr)
	end
	else
	  error(458);
  end;


  procedure getintegerfilename(defaultname : alfa);

  var
      lcp : ctp;
      lid : alfa;

  begin
    lid := id;
    id := defaultname;
    srchid([vars],lcp);
    selector(fsys or facbegsys or [comma], lcp);
    loadaddress;
    with lcp^, idtype^ do
      if (form = files) and (vlev = 0) and (not main) then begin
	if vaddr=0 then
	  insertaddr(no,cix,0);
	vaddr:= ic-1;
	code.information[cix] := 'E'
      end;
    id := lid
  end;
$PAGE page, abs

  procedure page;

  begin
    getfilename('OUTPUT    ',true);
    support(putpage)
  end;

  procedure abs;

  begin
    with gattr do
      if (typtr = intptr) orif (typtr = realptr) then
	with code.instruction[cix] do
	  if instr = 200b (*MOVE*)
	  then
	    instr := 214b (*MOVM*)
	  else
	  (*$Y4       IF DBLREAL ANDIF (TYPTR = REALPTR) THEN BEGIN
			MAC3R (325B(*JUMPGE*),REG-1,IC+2);
			MAC3  (121B(*DMOVN*),REG-1,REG-1);
		      END
		      ELSE        *)
	    mac3(214b (*MOVM*),reg,reg)
      else begin
	error(459);
	typtr:= intptr
      end
  end (*ABS*);
$PAGE sqr, trunc, round


  procedure sqr;

  begin
    with gattr do
      if typtr = intptr then
	mac3(220b (*IMUL*),reg,reg)
      else if typtr = realptr then begin
      (*$Y4   IF DBLREAL THEN
		MAC3 (112B(*DFMP*),REG-1,REG-1)
	      ELSE    *)
	mac3 (164b (*FMPR*),reg,reg);
      end
      else begin
	error(459);
	typtr := intptr
      end
  end (*SQR*);


  procedure trunc;

  begin
    if gattr.typtr <> realptr then
      error(459)
    else
    (*$Y4 IF DBLREAL THEN BEGIN
	    MAC3 (201B(*MOVEI*),TAC,GATTR.REG-1);
	    SUPPORT (CONVERTDREALTOINTEGER);
	    REGC := REGC - 1;
	    GATTR.REG := REGC;
	  END
	  ELSE *)begin
      mac3 (201b (*MOVEI*),tac,gattr.reg);
      support(convertrealtointeger);
    end;
    gattr.typtr := intptr
  end (*TRUNC*);


  procedure round;

  begin
    if gattr.typtr<>realptr then
      error(459)
    else
    (*$Y4 IF DBLREAL THEN BEGIN
	    REGC := REGC - 1;
	  MAC3 (201B(*MOVEI*),TAC,REGC);
	    SUPPORT (ROUNDDREAL);
	    GATTR.REG := REGC; (* := GATTR.REG-1 *)
	  END
	  ELSE *)begin
      mac3 (201b (*MOVEI*),tac,gattr.reg);
      support(roundrealtointeger)
    end;
    gattr.typtr:= intptr
  end (*ROUND*);
$PAGE fortran_functions

procedure fortran_functions;

var reg: acrange;
    lattr: attr;
    convert: record
      case boolean of
	true:  (fortran_support: supports);
	false: (key: pfkey)
      end;

begin
  if gattr.typtr <> nil then begin
    if lkey <> 52 (* random *) then
      library[fortransy].called := true;
    makereal (gattr);
    if gattr.typtr = realptr then begin
      convert.key := 2*(lkey-36(*ln*))+ord(ln_function);
        reg := gattr.reg - ord(dblreal);
      if (convert.fortran_support = atan_function) andif (sy = comma) then begin
	lattr := gattr;
	insymbol;
	expression (fsys or [rparent],onregc);
	load (gattr);
	makereal (gattr);
	if dblreal then support (atan2_double)
	else support (atan2_function);
	fullword (no,reg*40b,reg);
	fullword (no,reg*40b,gattr.reg-ord(dblreal));
	gattr := lattr;
      end
      else begin
	if dblreal then if convert.fortran_support <> random_function then
	  convert.fortran_support := succ (convert.fortran_support);
	support (convert.fortran_support);
        fullword (no,reg*40b,reg);
      end;
      regc := reg + ord (dblreal);	(* reuse expression registers if necessary *)
    end;
  end;
end;
$PAGE odd, float


  procedure odd;

  begin
    with gattr do begin
      if typtr <> intptr then
	error(459);
      mac3(405b (*ANDI*),reg,1);
      typtr := boolptr
    end
  end (*ODD*);


  procedure float;

  begin
    if gattr.typtr <> intptr then
      error (459)
    else begin
    (*$Y4    MAC3 (201B(*MOVEI*),TAC,GATTR.REG);
	IF DBLREAL THEN BEGIN
	  SUPPORT (DCONVERTINTEGERTOREAL);
	  INCREMENTREGC;
	  WITH GATTR DO
	    REG := REG + 1;
	END
	ELSE        *)
      support (convertintegertoreal);
    end;
    gattr.typtr := realptr;
  end;
$PAGE minmax


  procedure minmax;

  const
      minkey := 29;
      maxkey := 30;
      move = 200b;

  var
      typeptr: stp;
      havereg, constread: boolean;
      instr: instrange;
      register, lreg: acrange;
      lattr, cstattr: attr;
      arguments: integer;

  begin
    typeptr := nil;
    constread := false;
    havereg := false;
    if lkey = minkey then
      instr := 311b (* CAML *)
    else
      instr := 317b;
    arguments := 0;
    loop
      expression (fsys or [comma,rparent],onregc);
      arguments := arguments + 1;
      with gattr do
	if typtr <> nil then begin
	  if typeptr = nil then begin
	    if typtr^.form <= subrange then
	      typeptr := typtr
	    else
	      error (459);
	  end
	  else if (typeptr=realptr) andif (typtr=intptr) then
	    makereal (gattr)
	  else if not comptypes (typeptr,typtr) then
	    error (459);
	  if kind = cst then
	    if constread then begin
	      if (lkey=minkey) andif (cval.ival<cstattr.cval.ival) orif
		(lkey=maxkey) andif (cval.ival>cstattr.cval.ival) then
		  cstattr := gattr;
	    end
	    else begin
	      cstattr := gattr;
	      constread := true;
	    end
	  else if havereg then begin
	    if kind=varbl then begin
	      if (vsclass=externalsc) orif (indbit=1) orif (packfg<>notpack)
		then
		  load (gattr)
		else
		  fetchbasis (gattr);
	    end;
	    if kind=expr then begin
	      mac3 (instr,register,reg);
	      mac3 (move,register,reg);
	    end
	    else begin
	      mac5 (vrelbyte,instr,register,indexr,dplmt);
	      mac5 (vrelbyte,move,register,indexr,dplmt);
	    end;
	    regc := register;
	  end
	  else begin
	    load (gattr);
	    register := reg;
	    havereg := true;
	  end
	end; (* WITH GATTR *)
    exit if sy <> comma;
      insymbol;
    end; (* LOOP *)
    if havereg then begin
      if constread then begin
	lattr := cstattr;
	makecode (instr,register,lattr);
	makecode (move,register,cstattr);
      end;
      with gattr do begin
	typtr := typeptr;
	reg := register;
	kind := expr;
      end;
    end
    else if constread then begin
      gattr := cstattr;
    end
    else begin
      error (459);
      gattr.typtr := nil;
    end;
    if arguments < 2 then
      error (459);
  end;
$PAGE minimaximum


  procedure minimaximum;

  var
      minval, maxval: integer;
      lcp : ctp;

  begin
    gattr.typtr := nil;
    if sy <> ident then
      error (459)
    else begin
      srchid ([vars, types, field], lcp);
      with lcp^ do begin
	if idtype <> nil then begin
	  if (comptypes (idtype, realptr)) or (idtype^.form > subrange) then
	    error (459)
	  else begin
	    getbounds (idtype, minval, maxval);
	    with gattr do begin
	      kind := cst;
	      if idtype^.form=subrange then
		typtr:= idtype^.rangetype
	      else
		typtr:= idtype;
	      if lkey = 14 then
		cval.ival := minval
	      else
		cval.ival := maxval
	    end
	  end
	end
      end
    end;
    insymbol;
  end;
$PAGE ord, chr, pointr


  procedure ord;

  begin
    if gattr.typtr <> nil then
      if (gattr.typtr = realptr) orif (gattr.typtr^.form >= power) then
	error(459);
    gattr.typtr := intptr
  end (*ORD*);


  procedure chr;

  begin
    if gattr.typtr <> intptr then
      error(459);
    gattr.typtr := charptr
  end (*CHR*);


  procedure pointr;

  begin
    if not comptypes (gattr.typtr, intptr) then
      error (459);
    load (gattr);
    gattr.typtr := arbptrtyp;
  end;
$PAGE predsucc


  procedure predsucc;

  var
      lstrptr:stp;
      lattr: attr;
      instr: instrange;

  begin
    if lkey = 9 then
      instr := 275b (* SUBI *)
    else
      instr := 271b; (* ADDI *)
    if gattr.typtr <> nil then
      if (gattr.typtr^.form>subrange) or (gattr.typtr=realptr) then
	error(459)
      else if runtmcheck then begin
	lstrptr:=gattr.typtr;
	if (lstrptr^.form=subrange) and (lstrptr^.rangetype <>nil) then
	  lstrptr:=lstrptr^.rangetype;
	if lstrptr=intptr then begin
	  mac3r (255b (*JFCL*),10b,ic+1);
	  mac3 (instr (* SUBI OR ADDI *),regc,1);
	  mac3r (255b (*JFCL*),10b,ic+2);
	  mac3 (334b (*SKIPA*),0,0);
	  support (errorinassignment);
	end
	else if lkey=9 then
	(*  CHAR OR DECLARED *)
	begin
	  mac3r(365b (*SOJGE*),regc,ic+2);
	  support(errorinassignment)
	end
	else (* LKEY = 10 *)
	  with lattr do begin
	    typtr := lstrptr;
	    kind := cst;
	    cval.ival := 0;
	    if lstrptr=charptr then
	      cval.ival := 177b
	    else if lstrptr^.fconst <> nil then
	      cval.ival:=lstrptr^.fconst^.values.ival;
	    makecode(311b (*CAML*),regc,lattr);
	    support(errorinassignment);
	    mac3(271b (*ADDI *),regc,1 );
	  end (* LKEY = 10 *);
      end (* RUNTMCHECK *)
      else
	mac3(instr (*SUBI/ADDI*),regc,1)
  end (*PREDSUCC*);
$PAGE eofeoln, protection


  procedure eofeoln;

  begin
    getfilename('INPUT     ',(lkey=12));
    with gattr do begin
      kind := expr;
      reg := indexr;
      if lkey=11 then begin
	mac4(332b (*SKIPE*),reg,reg,fileof) ;
	mac3(201b (*MOVEI*),reg,1) ;
      end
      else
	mac4(200b (*MOVE*),reg,reg,fileol);
      typtr := boolptr
    end
  end (*EOF*);


  procedure protection;
  (* FOR DETAILS SEE  DEC-SYSTEM-10 MONITOR CALLS MANUAL, 3.2.4 *)

  begin
    expression ( fsys or [rparent], onregc );
    if gattr.typtr = boolptr then begin
      load(gattr);
      mac3(047b (*CALLI*),regc,36b (*SETUWP*));
      mac3(254b (*HALT*),4,0)
    end
    else
      error(458)
  end;
$PAGE stop, return, address


  procedure stop;

  begin
    support(exitprogram)
  end;


  procedure return;

  begin
    if level = 1 then
      stop (* MAIN PROGRAM, DO A STOP *)
    else begin
    (*  PROCEDURE OR FUNCTION, EXIT IT *)
      support(procreturn)
    end;
  end; (* RETURN *)


  procedure address; (*ADDRESS OF SYMBOL*)

  begin
    variable(fsys+[rparent]);
    ldaddress;
    with gattr do begin
      kind:= expr;
      reg:= regc;
      typtr:= arbptrtyp (*COMPATIBLE WITH ANY POINTER*)
    end
  end (*ADDRESS*);
$PAGE saveregs, restoreregs

var
    savecount: acrange;
    llc: addrrange;


  procedure saveregs;

  var
      i: integer;

  begin
    savecount := regc - regin;
    if savecount > 0 then begin
      llc := lc;
      checklc (savecount);
      if savecount > 3 then begin
	mac3(505b (*HRLI*),tac,2);
	mac4(541b (*HRRI*),tac,basis,llc);
	mac4(251b (*BLT*),tac,basis,llc+savecount-1)
      end
      else
	for i := 1 to savecount do
	  mac4(202b (*MOVEM*),regin+i,basis,llc+i-1)
    end;
  end;


  procedure restoreregs;

  var
      i: integer;

  begin
    if savecount > 0 then begin
      if savecount > 3 then begin
	mac4(505b (*HRLI*),tac,basis,llc);
	mac3(541b (*HRRI*),tac,2);
	mac3(251b (*BLT*),tac,savecount+1)
      end
      else
	for i := 1 to savecount do
	  mac4(200b (*MOVE*),regin+i,basis,llc+i-1) ;
    end
  end;
$PAGE lengthfun


  procedure lengthfun;

  var
      maxl: integer;

  begin
    expression (fsys+[rparent], onregc);
    with gattr do begin
      if strform (typtr) then begin
	if strkind (typtr) = nonvarying then begin
	  maxl := maxlen (gattr);
	  kind := cst;
	  cval.ival := maxl
	end
	else (* STRKIND = VARYING *)begin
	  if kind = cst then
	    cval.ival := cval.valp^.slgth
	  else if kind = expr then
	    reg := reg - 1 (* LENGTH IF FIRST OF PAIR *)
	  else if kind = varbl then (* LENGTH IS FIRST WORD, JUST CHANGE TYPTR *)
	  else if kind = substrng then begin
	    reg := reg + 1;
	    kind := expr
	  end
	end;
	typtr := intptr
      end
      else (* NOT STRING *)begin
	error (459);
	typtr := nil
      end
    end
  end;
$PAGE indexfun


  procedure indexfun;

  var
      lregc: acrange;

  begin
    lregc := regc;
    saveregs;
    regc := regin;
    expression (fsys+[rparent,comma],onfixedregc);
    if strform (gattr.typtr) then
      strdesc (reginp1, gattr)
    else
      error (459);
    if sy <> comma then
      errandskip (158,fsys+[comma])
    else
      insymbol;
    regc := reginp2; (* FORCE EXPREESION INTO REGIN + 3 *)
    expression(fsys+[comma,rparent],onfixedregc);
    if strform (gattr.typtr) then
      strdesc (reginp3, gattr)
    else
      error (459);
    if sy = comma then begin
      regc := reginp4;
      insymbol;
      expression (fsys+[rparent],onfixedregc);
      if not comptypes (gattr.typtr, intptr) then
	error (459)
      else
	load (gattr);
      support (ix3ss)
    end
    else
      support (ixss);
    regc := lregc;
    incrementregc;
    if regc <> reginp5 then
      mac3 (200b (*MOVE*), regc, reginp5);
    with gattr do begin
      kind := expr;
      typtr := intptr;
      reg := regc
    end;
    restoreregs
  end;
$PAGE searchverify


  procedure searchverify;

  var
      lregc: acrange;

  begin
    lregc := regc;
    saveregs;
    regc := regin;
    expression (fsys+[rparent,comma],onfixedregc);
    if strform (gattr.typtr) then
      strdesc (reginp1, gattr)
    else
      error (459);
    if sy <> comma then
      errandskip (158,fsys+[comma])
    else
      insymbol;
    regc := reginp2; (* FORCE EXPRESSION INTO REGIN + 3 *)
    expression (fsys+[rparent,comma], onfixedregc);
    if gattr.typtr <> nil then
      if gattr.typtr^.form <> power then
	error (459)
      else if not comptypes (gattr.typtr^.elset, charptr) then
	error (459)
      else
	load (gattr);
    if sy = comma then begin
      insymbol;
      expression (fsys+[rparent],onfixedregc);
      if not comptypes (gattr.typtr, intptr) then
	error (459)
      else
	load (gattr);
      if lkey = 24 then
	support (sr3ss)
      else
	support (vf3ss)
    end
    else if lkey = 24 then
      support (srss)
    else
      support (vfss);
    regc := lregc;
    incrementregc;
    if regc <> reginp5 then
      mac3 (200b (*MOVE*), regc, reginp5);
    with gattr do begin
      kind := expr;
      typtr := intptr;
      reg := regc
    end;
    restoreregs
  end;
$PAGE substrfun


  procedure substrfun;

  var
      lmax: addrrange;
      breg: acrange;
      lenreg: acrange;
      lattr: attr;
      sskind: stringkind;

  begin
    expression (fsys+[comma],onfixedregc);
    with gattr do begin
      if not strform (typtr) then
	error (459)
      else if comptypes (typtr, charptr) then
	strparm (gattr, dclstring (1, nonvarying), 0);
      if kind = expr then
	saveexpr (gattr);
      if kind = cst then
	loadaddress;
      if indbit = 1 then
	getparaddr;
    end;
    lattr := gattr;
    lmax := maxlen (lattr);
    sskind := strkind (lattr.typtr);
    with lattr do begin
      lenreg := 0; (* NO LENGTH LOADED *)
      if sy <> comma then
	errandskip (158, fsys+[comma])
      else
	insymbol;
      expression (fsys+[rparent,comma],onfixedregc);
      if gattr.kind=varbl then
	load(gattr);
      if kind = varbl then begin (* GET BYTE TO ADDRESSED CHAR *)
	if sskind = varying then begin
	  if runtmcheck or (sy = rparent) then begin (* LOAD LENGTH *)
	    lenreg := regcmax; (* BORROW A WITH REG *)
	    regcmax := regcmax - 1;
	    if regcmax < regc then begin
	      error (317);
	      regc := regcmax
	    end;
	    fetchbasis (lattr);
	    mac5 (vrelbyte, 200b (*MOVE*), lenreg, indexr, dplmt)
	  end;
	  if (vsclass = externalsc) and (indexr = 0) then begin
	    dplmt := 1; (* FORCE LOAD OF ADDRESS LATER *)
	    vrelbyte := right
	  end
	  else
	    dplmt := dplmt + 1 (* STRING 1 WORD PAST LENGTH *)
	end;
	(* IF GATTR IS CST, THEN BYTE OFFSET IS FIXED *)
	if gattr.kind = cst then begin
	  if lenreg <> 0 then begin (* VARYING STRING, CHECK LEN *)
	    if runtmcheck then begin
	      mac3 (305b (*CAIGE*), lenreg, gattr.cval.ival-1);
	      support (indexerror)
	    end;
	    if (gattr.cval.ival <> 1) and (sy = rparent) then
	      mac3 (275b (*SUBI*), lenreg, gattr.cval.ival-1);
	  end;
	  lmax := lmax - gattr.cval.ival + 1; (* RESTRIC SIZE OF SUBSTR *)
	  if (indexr <= 1) or (indexr > regcmax) (* PICK REG IN WHICH TO PUT BP *)
	  then begin
	    incrementregc; (* INDEXR IS WITH PTR OR BASIS *)
	    breg := regc (* GET A NEW REG *)
	  end
	  else
	    breg := indexr; (* REUSE REG ACCUM. INDEX *)
	  dplmt := dplmt + ((gattr.cval.ival-1) div 5); (* ADD WORD OFFSET *)
	  fetchbasis (lattr);
	  mac5(vrelbyte,201b (*MOVEI*),breg,indexr,dplmt); (* ADDR IN RHALF *)
	  if (gattr.cval.ival mod 5) = 0 then
	    mac3 (505b (*HRLI*), breg, 100700b)
	  else
	    mac3 (505b (*HRLI*), breg, 530700b - ((gattr.cval.ival mod 5)
	      * 070000b));
	end
	(* GATTR NOT CST, MUST COMPUTE BYTE OFFSET *)
	else begin
	  if runtmcheck then begin
	    mac3 (301b (*CAIL*), regc, 0);
	    if lenreg <> 0 then
	      mac4 (303b (*CAILE*), regc, lenreg, 1)
	    else
	      mac3 (303b (*CAILE*), regc, lmax+1);
	    support (indexerror);
	  end;
	  if (sy = rparent) or runtmcheck then begin
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
	  if overlaysw then (* MUST INDIRECT *)
	    mac5 (right, 270b (* ADD *), regc+1, 0, cbpssaddr)
	  else
	    mac5 (right, 270b, breg, regc+1, cbpssaddr);
	  if cbpssaddr = 0 then
	    insertaddr (no,cix,0);
	  cbpssaddr := ic - 1;
	  code.information [cix] := 'E';
	  if overlaysw then
	    mac4 (270b, breg, regc+1, 0);
	end;
      end
      else (* KIND = SUBSTR *)begin
	breg := reg;
	if gattr.kind = cst then begin
	  with gattr do begin
	    if ((cval.ival < 0) or (cval.ival > lmax+1)) andif (runtmcheck and
	      not specialsw) then
		error (263);
	    lmax := lmax - cval.ival + 1
	  end
	end;
	load (gattr);
	if runtmcheck then begin
	  mac3 (301b (*CAIL*), regc, 0);
	  mac4 (303b (*CAILE*), regc, breg+1, 1);
	  support (indexerror)
	end;
	if runtmcheck or (sy = rparent) then begin
	  mac4 (275b (*SUBI*), breg+1, regc, -1);
	  if runtmcheck then begin
	    lenreg := regcmax;
	    regcmax := regcmax - 1;
	    if regcmax < regc then begin
	      error (317);
	      regc := regcmax
	    end;
	    mac3 (200b (*MOVE*), lenreg, breg+1)
	  end
	  else
	    lenreg := breg+1
	end;
	if kl10sw then begin (* USE "ADJBP" *)
	  mac3 (133b (* IBP *), regc, breg);
	  mac3 (200b (* MOVE *), breg, regc)
	end
	else begin
	  incrementregc;
	  mac3 (231b (*IDIVI*), regc-1, 5);
	  mac4 (271b (*ADDI*), breg, regc-1, 0);
	  mac3 (133b (*IBP*), 0, breg);
	  mac3r (365b (*SOJGE*), regc, ic-1)
	end
      end;
      regc := breg;
      if sy = comma then begin (* GET LENGTH *)
	sskind := varying; (* IF LENGTH NOT CONSTANT *)
	insymbol; (* GO GET LENGTH *)
	expression (fsys+[rparent],onfixedregc); (* WANT LENGTH IN BREG+1 *)
	if not comptypes (gattr.typtr, intptr) then
	  error(457)
	else if gattr.kind = cst then begin
	  with gattr do begin
	    if ((cval.ival < 1) or (cval.ival > lmax)) andif (runtmcheck and
	      not specialsw) then
		error (263);
	    sskind := nonvarying; (* NOW KNOWN LENGTH *)
	    lmax := cval.ival
	  end
	end;
	load (gattr);
	if runtmcheck then begin
	  mac3 (301b (*CAIL*), regc, 0);
	  if lenreg <> 0 then
	    mac3 (313b (*CAMLE*), regc, lenreg)
	  else
	    mac3 (303b (*CAILE*), regc, lmax);
	  support (indexerror)
	end
      end
      else begin (* NO 3RD ARG, ASSUME REST OF STRING *)
	incrementregc;
	if lenreg = 0 then
	  mac3 (201b (*MOVEI*), regc, lmax)
	else if regc <> lenreg then
	  mac3 (200b (*MOVE*), regc, lenreg);
      end;
    end (* WITH LATTR *);
    if lenreg > regcmax then
      regcmax := lenreg; (* RESTORE IF USED *)
    with gattr do begin
      kind := substrng;
      reg := breg;
      typtr := dclstring (lmax, sskind)
    end;
    regc := breg + 1
  end;
$PAGE callnonstandard


  procedure callnonstandard;

  var
      nxt,lnxt,lcp: ctp;
      lsp: stp;
      tolevel: levrange;
      lkind: idkind;
      lb, parok, size2: boolean;
      p,i,nofpar: integer;
      toppoffset,offset,parlist,actualpar,firstpar,llc: addrrange;
      lregc, saveregc: acrange;
      retvaloffset: addrrange;


    function compatible (lcp1: ctp; lcp2: ctp): boolean;
    (* Determine whether parameter list LCP2 can be passed to a procedure
       expecting LCP1 *)

    begin
      compatible := true;
      while (lcp1 <> nil) and (lcp2 <> nil) do
	if lcp1 <> lcp2 then
	  with lcp1^ do begin
	    if (idtype <> nil) andif (idtype^.form = formalprocfunc) then begin
	      if (lcp2^.idtype <> nil) andif
		(lcp2^.idtype^.form = formalprocfunc) then begin
		  if not comptypes (idtype^.proctyptr^.idtype,lcp2^.idtype^.
		    proctyptr^.idtype) then
		      compatible := false
		    else if not compatible (idtype^.proctyptr^.parameters, lcp2^
		      .idtype^.proctyptr^.parameters) then
			compatible := false
		end
		else
		  compatible := false
	    end
	    else if vkind <> lcp2^.vkind then
	      compatible := false
	    else if vkind = formal then begin
	      if not comptypes (idtype,lcp2^.idtype) then
		compatible := false
	    end
	    else if strform (idtype) then begin
	      if not strform (lcp2^.idtype) then
		compatible := false
	    end
	    else if comptypes (idtype,realptr) then begin
	      if not (comptypes (lcp2^.idtype,realptr) orif
		comptypes (lcp2^.idtype,intptr)) then
		  compatible := false
	    end
	    else if not comptypes (idtype,lcp2^.idtype) then
	      compatible := false;
	    lcp1 := next;
	    lcp2 := lcp2^.next
	  end;
      if not ((lcp1 = nil) and (lcp2 = nil)) then
	compatible := false
    end;


    function filter (lcp: ctp): ctp;
    (* returns pointer to first parameter, if any *)

    begin
      with lcp^ do begin
	if pfkind = formal then
	  filter := parameters
	else if debug then begin
	  if (next <> nil) andif ((next^.klass = vars) andif
	    (next^.vclass in [valparmsc,varparmsc,spvalparmsc]) orif
	      (next^.klass in [proc,func]) andif (next^.pfkind = formal)) then
		filter := next
	      else
		filter := nil;
	end (* if debug *)
	else
	  filter := next;
      end (* with *)
    end (* filter *);

  begin
    nofpar:= 0;
    toppoffset := 0;
    parlist := 0;
    actualpar := 0;
    retvaloffset := 0;
    with fcp^ do begin
      lkind := pfkind;
      nxt := filter (fcp); (* first parameter or nil *)
      if klass = func then
	firstpar := 2
      else
	firstpar := 1;
      if pfclass = externalsc then
	library[language].called:= true;
      if klass = func then
	if idtype <> nil then
	  if idtype^.size > 2 then begin (* ALLOC TEMP FOR RV *)
	    retvaloffset := lc;
	    checklc (idtype^.size);
	  end;
      saveregs;
      lregc:= regc;
      if language <> pascalsy then
	regc:= parregcmax
      else
	regc:= regin
    end;
    if sy = lparent then begin
      insymbol;
      if sy <> rparent then
	loop
	  if nxt = nil then
	    error (554)
	  else if nxt^.klass in [proc,func] then begin
	  (* PASS FORMAL PROCEDURE *)
	    if nxt^.klass = proc then
	      srchid ([proc],lcp)
	    else
	      srchid ([func],lcp);
	    if lcp <> nil then begin
	      lb := compatible (nxt^.parameters,filter(lcp));
	      if not lb orif (nxt^.klass <> lcp^.klass) then
		error (503)
	      else if (nxt^.klass = func) andif
		not comptypes (nxt^.idtype,lcp^.idtype) then
		  error (503);
	      parok := true; (* KEEP THIS AWAY FROM COMPTYPES *)
	      incrementregc;
	      if lcp^.pfkind = formal then begin
		if lcp^.pflev = level then
		  mac4 (200b (*MOVE*),regc,basis,lcp^.pfaddr)
		else begin
		  mac4 (200b (*MOVE*),regc,basis,1);
		  for i := lcp^.pflev to level - 2 do
		    mac4 (200b (*MOVE*),regc,regc,1);
		  mac4 (200b (*MOVE*),regc,regc,lcp^.pfaddr)
		end
	      end
	      else begin
		if lcp^.pfaddr <> 0 then
		  mac3r (201b (*MOVEI*),regc,lcp^.pfaddr)
		else begin
		  mac3b (201b (*MOVEI*),regc,lcp^.linkchain[0]);
		  lcp^.linkchain[0] := ic - 1;
		  if lcp^.pfclass = externalsc then
		    code.information[cix] := 'E'
		  else
		    code.information[cix] := 'F';
		end;
		tolevel := lcp^.pflev;
		if tolevel > 1 then
		  if tolevel = level then (* CURRENT PROC IS PARENT OF LCP *)
		    mac4 (505b (*HRLI*),regc,basis,0)
		  else if tolevel = level - 1 then
		    mac4 (504b (*HRL*),regc,basis,1)
		  else begin
		    mac4 (200b (*MOVE*),tac,basis,1);
		    for i := tolevel to level - 3 do
		      mac4 (540b (*HRR*),tac,tac,1);
		    mac4 (504b (*HRL*),regc,tac,1)
		  end;
	      end;
	      insymbol;
	      goto 2;
	    end
	  end
	  else begin
	    saveregc := regc; (* TO FORCE STRING PARAMS INTO CORRECT REGISTER *)
	    if (nxt^.vkind=formal) andif (sy=ident) andif (not specialsw)
	      then begin
	      (* feeble attempt to prevent passing of structured constants
		 as formal parameters *)
		prterr := false;
		srchid ([vars],lcp);
		prterr := true;
		if (lcp <> nil) andif (lcp^.vconst) then
		  error (509)
	      end;
	    expression(fsys or [comma,rparent],onfixedregc);
	    parok := false;
	    if gattr.typtr <> nil then
	      if nxt <> nil then begin
		lsp := nxt^.idtype;
		if lsp <> nil then begin
		  if (nxt^.vkind = actual) then
		    if strform (lsp) then begin
		      if strform (gattr.typtr) then
			strparm (gattr, lsp, saveregc)
		      else
			error (503);
		      parok := true
		    end
		    else if lsp^.size <= 2 then begin
		      load (gattr);
		      if (gattr.typtr = intptr) andif comptypes (realptr,lsp)
			then begin
			  makereal (gattr);
			  (*$Y4                     IF DBLREAL THEN INCREMENTREGC;        *)
			end
		    end
		    else begin
		      if (not incore) and virtual then
			ldaddress
		      else loadaddress;
		      if fcp^.language <> pascalsy then
			code.instruction[cix].instr := 505b (*HRLI*)
		    end
		  else
		    with gattr do
		      if kind = varbl then
			if (not specialsw) andif ((vsclass=valparmsc)orif
			  (vsclass=spvalparmsc)) then
			    error (220)
			  else
			    if (not incore) and virtual then
			      ldaddress
			    else loadaddress
		      else
			error(463) ;
		  2:
		    if regc>parregcmax then begin
		      if toppoffset = 0 then begin
			lnxt := fcp^.next ;
			if fcp^.language = pascalsy then begin
			  toppoffset := fcp^.parmsize;
			  offset := lc;
			  checklc (toppoffset);
			end
			else begin
			  toppoffset := 1 + firstpar;
			  repeat
			    with lnxt^ do begin
			      nofpar := nofpar +1;
			      toppoffset := toppoffset + 1;
			      if (vkind = actual) and (idtype <> nil) then
				toppoffset := toppoffset + idtype^.size;
			      if lkind = actual then
				lnxt := next
			    end;
			  until lnxt = nil;
			  parlist := 1 + firstpar;
			  actualpar := parlist + nofpar;
			  mac3(271b (*ADDI*),topp,toppoffset)
			end;
		      end ;
		      with nxt^ do begin
			size2 := (vclass=valparmsc) andif (idtype^.size=2);
			if fcp^.language = pascalsy then begin
			  if size2 and dmove then begin
			    regc := regc - 1;
			    mac4(124b (*DMOVEM*),regc,basis,offset);
			    offset := offset + 2
			  end
			  else begin
			    if size2 then begin
			      mac4(202b (*MOVEM*),regc,basis,offset+1);
			      regc := regc - 1
			    end;
			    mac4(202b (*MOVEM*),regc,basis,offset);
			    offset := offset + 1;
			    if size2 then
			      offset := offset + 1
			  end
			end
			else begin
			  if vkind = actual then begin
			    if idtype^.size <= 2 then begin
			      if dmove and size2 then begin
				regc := regc - 1;
				mac4 (124b (*DMOVEM*),regc,topp,actualpar-
				  toppoffset)
			      end
			      else begin
				if size2 then begin
				  mac4(202b (*MOVEM*),regc,topp,actualpar+1-
				    toppoffset);
				  regc := regc - 1
				end;
				mac4(202b (*MOVEM*),regc,topp,actualpar-
				  toppoffset);
			      end;
			      mac4(541b (*HRRI*),regc,topp,actualpar- toppoffset
				)
			    end
			    else begin
			      if code.instruction[cix].instr=201b (*MOVEI*)then
				code.instruction[cix].instr := 505b (*HRLI*)
			      else
				mac4 (505b (*HRLI*),regc,regc,0);
			      mac4(541b (*HRRI*),regc,topp,actualpar-toppoffset)
				;
			      mac4(251b (*BLT*),regc,topp,actualpar+idtype^.size
				-1-toppoffset);
			      mac4 (201b (*MOVEI*),regc,topp,actualpar-
				toppoffset);
			    end;
			    actualpar := actualpar + idtype^.size
			  end;
			  mac4(552b (*HRRZM*),regc,topp,parlist-toppoffset);
			  parlist := parlist + 1
			end;
			regc := parregcmax
		      end
		    end;
		  if not (parok orif comptypes(lsp,gattr.typtr)) then
		    error(503)
		end
	      end
	  end;
	  if nxt <> nil then
	    nxt := nxt^.next;
	exit if sy <> comma;
	  insymbol
	end (* IF SY <> RPARENT THEN LOOP *);
      if sy = rparent then
	insymbol
      else
	error(152)
    end (*IF LPARENT*);
    for i := 0 to withix do
      with display[top-i] do
	if (cindr<>0) and (cindr<>basis) then
	  mac4(202b (*MOVEM*),cindr,basis,clc);
    with fcp^ do begin
      if (language <> pascalsy) andif (toppoffset = 0) then begin
	toppoffset:= firstpar+2;
	mac3(271b (*ADDI*),topp,toppoffset)
      end;
      if pflev > 1 then
	p := level - pflev
      else
	p:= 0;
      if nxt <> nil then
	error (554);
      if language <> pascalsy then begin
	mac3(515b (*HRLZI*),hac,-nofpar);
	mac4(202b (*MOVEM*),hac,topp,firstpar-toppoffset);
	mac4(202b (*MOVEM*),basis,topp,-toppoffset);
	mac4(201b (*MOVEI*),basis,topp,firstpar-toppoffset+1);
	if nofpar = 0 then
	  mac4(402b (*SETZM*),0,topp,firstpar-toppoffset+1)
      end
      else if retvaloffset <> 0 then
	mac4 (201b (*MOVEI*),hac,basis,retvaloffset);
      if (toppoffset>0) andif (language=pascalsy) then
	mac4(541b (*HRRI*),paroverfreg,basis,offset);
      if lkind = formal then begin
	if p = 0 then
	  mac4 (200b (*MOVE*),tac,basis,pfaddr)
	else begin
	  mac4 (200b (*MOVE*),tac,basis,1);
	  for i := 2 to p do
	    mac4 (200b (*MOVE*),tac,tac,1);
	  mac4 (200b (*MOVE*),tac,tac,pfaddr);
	end;
	support (formalcall);
      end
      else if pfaddr = 0 then begin
	mac3b(260b (*PUSHJ*),topp,linkchain[p]);
	linkchain[p]:= ic-1;
	if pfclass = externalsc then
	  code.information[cix] := 'E'
	else
	  code.information[cix] := 'F'
      end
      else
	mac3r(260b (*PUSHJ*),topp,pfaddr-p);
      if language <> pascalsy then begin
	mac3(275b (*SUBI*),topp,toppoffset);
	if (klass = func) andif (idtype <> nil) then
	  if (idtype^.size=2) and dmove then
	    mac4 (124b (*DMOVEM*),hac,topp,retlocp1)
	  else begin
	    mac4(202b (*MOVEM*),hac,topp,retlocp1);
	    if idtype^.size = 2 then
	      mac4(202b (*MOVEM*),tac,topp,retlocp2)
	  end;
	mac4(200b (*MOVE*),basis,topp,0)
      end
    end;
    for i := 0 to withix do
      with display[top-i] do
	if (cindr<>0) andif (cindr<>basis) then
	  mac4(200b (*MOVE*),cindr,basis,clc) ;
    restoreregs;
    regc := lregc;
    with gattr, fcp^ do begin
      if klass = func then begin
	typtr := idtype;
	kind := varbl;
	packfg := notpack;
	vrelbyte := no;
	vsclass := localsc;
	vlevel := 1;
	indbit := 0;
	if retvaloffset = 0 then begin
	  indexr := topp;
	  dplmt := retlocp1;
	  load (gattr)
	end
	else begin
	  indexr := basis;
	  dplmt := retvaloffset
	end
      end
    end
  end (*CALLNONSTANDARD*);
$PAGE call body

begin
(*CALL*)
  if xref then begin
    if not xref_open then
      open_xref_file;
    with fcp^ do begin
      if (pfdeckind=declared) andif (pfkind=actual) then
	writeln (symfile,' ',name)
      else if (pfdeckind=standard) andif ((klass=proc) andif (key in [1..25])
	orif (klass=func) andif (key in [1..6,13,20,36..53])) then
	  writeln (symfile,'+',name);
    end;
  end;
  if fcp^.pfdeckind = standard then begin
    lkey := fcp^.key;
    if fcp^.klass = proc then begin
      if not (lkey in [7,8,9,10,11,17,19,22,26,27] ) then
	if sy = lparent then
	  insymbol
	else
	  error(153);
      if (lkey in [5,6,25,7,8,10,11]) andif (regcmax < 11b) then
	error(317);
	(*REGISTER 11B USED BY RUNTIME SUPPORT FREE OR NOT  *)
      case lkey of
	1,2,3,4, 5,6,25:
	  getputresetrewrite;
	7, 8: begin
	  readreadln;
	  if norightparent then
	    goto 9
	end;
	9: begin
	  break ;
	  if norightparent then
	    goto 9
	end ;
	10, 11: begin
	  writewriteln;
	  if norightparent then
	    goto 9
	end;
	12, (* PACK/UNPACK NO LONGER SUPPORTED *)
	13:
	  errandskip (373,fsys or [rparent]);
	14:
	  new;
	15:
	  mark;
	16:
	  release;
	17: begin
	  getlinenr;
	  if norightparent then
	    goto 9
	end;
	(* 18 (put8bitstotty) is now a declared external procedure *)
	19: begin
	  page;
	  if norightparent then
	    goto 9
	end;
	20:
	  dispose;
	22: begin
	  close;
	  if norightparent then
	    goto 9
	end;
	21:
	  protection;
	23:
	  getchannel;
	24:
	  freechannel;
	26: begin
	  stop;
	  goto 9
	end;
	27: begin
	  return;
	  goto 9
	end;
	28:
	  set_root
      end
    end
    else begin
      if not (lkey in [1,2,11,12,13]) then begin
	if sy = lparent then
	  insymbol
	else
	  error(153);
	if not (lkey in [14,15,16,19,21..27,29,30,53]) then begin
	  expression(fsys or [rparent,comma],onregc);
	  if not (lkey in [7,8,11,12,17,18,28]) then
	    load(gattr)
	end
      end;
      case lkey of
	(* 1 (runtime) and 2 (time) are now declared external functions *)
	3:
	  abs;
	4:
	  sqr;
	5:
	  trunc;
	6:
	  odd;
	7:
	  ord;
	8:
	  chr;
	9,10:
	  predsucc;
	11,12: begin
	  eofeoln;
	  if norightparent then
	    goto 9
	end;
	13: begin
	  channel;
	  if norightparent then
	    goto 9
	end;
	14,15:
	  minimaximum;
	16:
	  size;
	17,18:
	  uplowcase;
	19:
	  address;
	20:
	  round;
	21:
	  lengthfun;
	22:
	  substrfun;
	23:
	  indexfun;
	24,25:
	  searchverify;
	26,27:
	  upperlowerbound;
	28:
	  pointr;
	29,30:
	  minmax;
	31:
	  float;
	32:
	  get_root;
	33:
	  make_pointer;
	34,35:
	  areaid_or_offset;
	36..52:
	  fortran_function;
	53:
	  get_a_file_name
      end;
      if lkey < 3 then
	goto 9
    end;
    if sy = rparent then
      insymbol
    else
      error(152);
    9:
  end (*STANDARD PROCEDURES AND FUNCTIONS*)
  else
    callnonstandard
end (*CALL*).
 #J[