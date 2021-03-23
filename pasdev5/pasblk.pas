$title block - PASCAL compiler declaration routines
$OPTIONS special, nocheck

(*	Conditional compilation switches for this module:

	X5:  If enabled, support INITPROCEDUREs
							*)
$PAGE includes
$INCLUDE pasdcl.inc

$INCLUDE paslex.inc

$INCLUDE pascmp.inc

$INCLUDE passtr.inc

$INCLUDE pasmc.inc

$INCLUDE PASEXP.INC
$PAGE declarations

external procedure cst_declaration (setofsys; storageclass);

external procedure var_list (storageclass; setofsys; ctp);

external procedure body (fsys: setofsys);

$PAGE block
public procedure block(fprocp: ctp; fsys,leaveblocksys: setofsys);

var
    i: integer;
    lsy: symbol;
    heapmark(*$X5 ,GLOBMARK *): integer;
    forwptr : ctp;				(*TEST FOR FORWORD DECLARED PROCEDURES*)
    scalar_top: disprange;

$PAGE typ
  procedure typ(fsys: setofsys; var fsp: stp; var fsize: addrrange;
    var fbitsize: bitrange; idptr: ctp);

  var
      lsp,lsp1,lsp2: stp;
      oldtop: disprange;
      lcp: ctp;
      lsize,displ: addrrange;
      i,lmin,lmax: integer;
      packflag: boolean;
      lbitsize: bitrange;
      lbtp: btp;
      bitcount:integer;

const
    offset_bitsize := 18;

    function log2(fval: integer): bitrange;

    begin
      log2 := 0;
      repeat
	log2 := log2 + 1;
	fval := fval div 2
      until fval = 0;
    end (*LOG2*);

$PAGE simpletype
    procedure simpletype(fsys: setofsys; var fsp: stp; var fsize: addrrange;
      var fbitsize: bitrange; identptr: ctp);

    var
	lsp,lsp1: stp;
	lcp,lcp1: ctp;
	ttop: disprange;
	lcnt: integer;
	lvalu: valu;
	lbitsize: bitrange;

    begin
      fsize := 1;
      skipiferr(simptypebegsys,208,fsys);
      if sy in simptypebegsys then begin
	if sy = lparent then begin
	  ttop := top;				(*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)
	  top := scalar_top;			(* Makes scalars accessible even if declared with records *)
	  new(lsp,scalar,declared);
	  lsp^.size := 1;
	  lcp1 := nil;
	  lcnt := 0;
	  repeat
	    insymbol;
	    if sy = ident then begin
	      new(lcp,konst);
	      with lcp^ do begin
		name := id;
		idtype := lsp;
		next := lcp1;
		values.ival := lcnt;
	      end;
	      enterid(lcp);
	      lcnt := lcnt + 1;
	      lcp1 := lcp;
	      insymbol
	    end
	    else
	      error(209);
	    iferrskip(166,fsys or [comma,rparent])
	  until sy <> comma;
	  top := ttop;
	  with lsp^ do begin
	    selfstp := nil;
	    typeid := identptr;
	    fconst := lcp1;
	    bitsize := log2(lcnt-1)
	  end;
	  if sy = rparent then
	    insymbol
	  else
	    error(152)
	end
	else begin
	  if sy = ident then begin
	    srchid([types,konst],lcp);
	    if lcp^.klass = konst then begin
	      insymbol;
	      new(lsp,subrange);
	      with lsp^, lcp^ do begin
		selfstp := nil;
		typeid := identptr;
		rangetype := idtype;
		if string(rangetype) then begin
		  error(303);
		  rangetype := nil
		end;
		min := values;
		size := 1
	      end;
	      if sy = elipsis then
		insymbol
	      else
		error(151);
	      constant(fsys,lsp1,lvalu);
	      with lsp^ do begin
		max := lvalu;
		if min.ival<0 then
		  bitsize := bitmax
		else
		  bitsize := log2(max.ival);
		if rangetype <> lsp1 then
		  error(304)
	      end;
	    end
	    else begin
	      insymbol;
	      lsp := lcp^.idtype;
	      if lsp <> strptr then begin
		if lsp <> nil then
		  fsize := lsp^.size;
		if lsp = intptr then
		  if not specialsw then
		    error (371);
	      end
	      else (* LSP = STRPTR *)begin
		if sy = lbrack then begin
		  insymbol;
		  lcnt := 0;
		  constant (fsys+[rbrack],lsp1,lvalu);
		  if lsp1 <> intptr then
		    error (311)
		  else
		    if lvalu.ival < 0 then
		      error (374)
		    else
		      lcnt := lvalu.ival;
		  lsp := dclstring (lcnt, varying);
		  lsp^.typeid := identptr;
		  if sy = rbrack then
		    insymbol
		  else
		    errandskip (152, fsys)
		end;
		fsize := lsp^.size
	      end
	    end
	  end					(*SY = IDENT*)
	  else begin
	    new(lsp,subrange);
	    constant(fsys or [elipsis],lsp1,lvalu);
	    if string(lsp1) then begin
	      error(303);
	      lsp1 := nil
	    end;
	    with lsp^ do begin
	      rangetype := lsp1;
	      min := lvalu;
	      size := 1
	    end;
	    if sy = elipsis then
	      insymbol
	    else
	      error(151);
	    constant(fsys,lsp1,lvalu);
	    with lsp^ do begin
	      selfstp := nil;
	      typeid := identptr;
	      max := lvalu;
	      if min.ival<0 then
		bitsize := bitmax
	      else
		bitsize := log2(max.ival);
	      if rangetype <> lsp1 then
		error(304)
	    end
	  end;
	  if lsp <> nil then
	    with lsp^ do
	      if form = subrange then
		if rangetype <> nil then
		  if rangetype = realptr then begin
		    error (320);	(* not implemented *)
		    if min.valp^.rval > max.valp^.rval then
		      error(451)
		  end
		  else if min.ival > max.ival then
		    error(451)
	end;
	fsp := lsp;
	if lsp<>nil then
	  fbitsize := lsp^.bitsize
	else
	  fbitsize := 0;
	iferrskip(166,fsys)
      end
      else begin
	fsp := nil;
	fbitsize := 0
      end
    end (*SIMPLETYPE*);

$PAGE fieldlist
    procedure fieldlist(fsys: setofsys; var frecvar: stp; var ffirstfield: ctp);

    var
	lcp,lcp1,nxt,nxt1: ctp;
	lsp,lsp1,lsp2,lsp3,llsp3,lsp4,tagsp: stp;
	minsize,maxsize,lsize: addrrange;
	lvalu,lstval: valu;
	lbitsize: bitrange;
	lbtp: btp;
	minbitcount:integer;
	lid : alfa ;

$PAGE recsection
      procedure recsection( var fcp: ctp; fsp: stp );

      begin
	if not packflag orif (lsize > 1) orif (lbitsize = 36) then begin
	  if bitcount > 0 then begin
	    displ := displ + 1;
	    bitcount := 0
	  end;
	  with fcp^ do begin
	    idtype := fsp;
	    fldaddr := displ;
	    packf := notpack;
	    fcp := next;
	    displ := displ + lsize
	  end
	end
	else					(*PACK RECORD-SECTION*)

	begin
	  if (lsp <> nil) andif (lsp^.form = pointer) andif (lsp^.v_offset) then begin
	    if bitcount > 18 then begin
	      bitcount := 0;
	      displ := displ + 1;
	    end
	    else if bitcount > 0 then
	      bitcount := 18;	(* force halfword alignment *)
	  end;
	  bitcount := bitcount + lbitsize;
	  if bitcount>bitmax then begin
	    displ := displ + 1;
	    bitcount := lbitsize
	  end;
	  if (lbitsize = 18) andif (bitcount in [18,36]) then begin
	    with fcp^ do begin
	      idtype := fsp;
	      fldaddr := displ;
	      if bitcount = 18 then
		packf := hwordl
	      else
		packf := hwordr;
	      fcp := next
	    end
	  end
	  else begin
	    new(lbtp);
	    with lbtp^.byte do begin
	      sbits := lbitsize;
	      pbits := bitmax - bitcount;
	      reladdr := displ;
	      dummybit := 0;
	      ibit := 0;
	      ireg := tac
	    end;
	    with lbtp^ do begin
	      last := lastbtp;
	      fieldcp := fcp
	    end;
	    lastbtp := lbtp;
	    with fcp^ do begin
	      idtype := fsp;
	      packf := packk;
	      fcp := next;
	      fldbp := lbtp^.byte;
	    end
	  end
	end
      end (* RECSECTION *);

$PAGE fieldlist - body
    begin
      nxt1 := nil;
      lsp := nil;
      skipiferr([ident,casesy],452,fsys);
      while sy = ident do begin
	nxt := nxt1;
	loop
	  if sy = ident then begin
	    new(lcp,field);
	    with lcp^ do begin
	      name := id;
	      idtype := nil;
	      next := nxt
	    end;
	    nxt := lcp;
	    enterid(lcp);
	    insymbol
	  end
	  else
	    error(209);
	  skipiferr([comma,colon],166,fsys or [semicolon,casesy]);
	exit if sy <> comma;
	  insymbol
	end;
	if sy = colon then
	  insymbol
	else
	  error(151);
	typ(fsys or [casesy,semicolon],lsp,lsize,lbitsize,nil);
	if lsp <> nil then
	  if lsp^.form = files then
	    error(254);
	while nxt <> nxt1 do
	  recsection(nxt,lsp);			(*RESERVES SPACE FOR ONE RECORDSECTION *)
	nxt1 := lcp;
	if sy = semicolon then begin
	  insymbol;
	  skipiferr([ident,casesy],452,fsys)
	end
      end (*WHILE*);
      nxt := nil;
      while nxt1 <> nil do
	with nxt1^ do begin
	  lcp := next;
	  next := nxt;
	  nxt := nxt1;
	  nxt1 := lcp
	end;
      ffirstfield := nxt;
      if sy = casesy then begin
	lcp:=nil;				(*POSSIBILITY OF NO TAGFIELDIDENTIFIER*)
	insymbol;
	if sy = ident then begin
	  lid := id ;
	  insymbol ;
	  if (sy<>colon) andif (sy<>ofsy) then begin
	    error(151) ;
	    errandskip(160,fsys or [lparent])
	  end
	  else begin
	    if sy = colon then begin
	      new(lsp,tagfwithid);
	      new(lcp,field) ;
	      with lcp^ do begin
		name := lid ;
		idtype := nil ;
		next := nil
	      end ;
	      enterid(lcp) ;
	      insymbol ;
	      if sy <> ident then begin
		errandskip(209,fsys or [lparent]) ;
		goto 1
	      end
	      else begin
		lid := id ;
		insymbol ;
		if sy <> ofsy then begin
		  errandskip(160,fsys or [lparent]) ;
		  goto 1
		end
	      end
	    end
	    else begin
	      new (lsp, tagfwithoutid);
	      if not specialsw then
		error (217)
	    end;
	    with lsp^ do begin
	      size:= 0 ;
	      selfstp := nil ;
	      typeid := nil;
	      fstvar := nil;
	      if form=tagfwithid then
		tagfieldp:=nil
	      else
		tagfieldtype := nil
	    end;
	    frecvar := lsp;
	    id := lid ;
	    srchid([types],lcp1) ;
	    tagsp := lcp1^.idtype;
	    if tagsp <> nil then
	      if (tagsp^.form <= subrange) orif string(tagsp) then begin
		if comptypes(realptr,tagsp) then
		  error(210)
		else if string(tagsp) then
		  error(169);
		with lsp^ do begin
		  bitsize := tagsp^.bitsize;
		  if form = tagfwithid then
		    tagfieldp := lcp
		  else
		    tagfieldtype := tagsp;
		end;
		if lcp <> nil then begin
		  lbitsize :=tagsp^.bitsize;
		  lsize := tagsp^.size;
		  recsection(lcp,tagsp);	(*RESERVES SPACE FOR THE TAGFIELD *)
		end;
		if bitcount > 0 then
		  lsp^.size:=displ + 1
		else
		  lsp^.size:= displ;
	      end
	      else
		error(402);
	    insymbol;
	  end
	end
	else
	  errandskip(209,fsys or [lparent]) ;
	1:
	  lsp1 := nil;
	minsize := displ;
	maxsize := displ;
	minbitcount:=bitcount;
	loop
	  lsp2 := nil;
	  loop
	    constant(fsys or [comma,colon,elipsis,lparent],lsp3,lvalu);
	    if not comptypes (lsp3, tagsp) then begin
	      error (305);
	      lsp3 := nil
	    end;
	    lstval := lvalu;
	    llsp3 := lsp3;
	    if sy = elipsis then begin
	      insymbol;
	      constant (fsys or [comma,colon,lparent],llsp3,lstval);
	      if not comptypes (llsp3, tagsp) then begin
		error (305);
		llsp3 := nil
	      end
	    end;
	    if (tagsp <> nil) and (llsp3 <> nil) and (lsp3 <> nil) then begin
	      if lstval.ival < lvalu.ival then
		error (451)
	      else
		while lvalu.ival <= lstval.ival do begin
		  new(lsp3,variant);
		  with lsp3^ do begin
		    nxtvar := lsp1;
		    subvar := lsp2;
		    varval := lvalu;
		    bitsize := lsp^.bitsize;
		    selfstp := nil;
		    typeid := nil;
		  end;
		  lsp1 := lsp3;
		  lsp2 := lsp3;
		  lvalu.ival := lvalu.ival + 1
		end
	    end;
	  exit if sy <> comma;
	    insymbol;
	  end;
	  if sy = colon then
	    insymbol
	  else
	    error(151);
	  if sy = lparent then
	    insymbol
	  else
	    error(153);
	  if sy = rparent then begin
	    lsp2 := nil;
	    lcp := nil
	  end
	  else
	    fieldlist(fsys or [rparent,semicolon],lsp2,lcp);
	  if bitcount = 0 then begin
	    if displ > maxsize then
	      maxsize := displ;
	  end
	  else if displ + 1 > maxsize then
	    maxsize := displ + 1;
	  while lsp3 <> nil do begin
	    lsp4 := lsp3^.subvar;
	    lsp3^.subvar := lsp2;
	    lsp3^.firstfield := lcp;
	    if bitcount > 0 then
	      lsp3^.size := displ + 1
	    else
	      lsp3^.size := displ ;
	    lsp3 := lsp4
	  end;
	  if sy = rparent then begin
	    insymbol;
	    iferrskip(166,fsys or [semicolon])
	  end
	  else
	    error(152);
	exit if sy <> semicolon;
	  displ := minsize;
	  bitcount:=minbitcount;		(*RESTAURATION *)
	  insymbol
	end;
	displ := maxsize;
	bitcount := 0;
	lsp^.fstvar := lsp1;
      end					(*IF SY = CASESY*)
      else if lsp <> nil then
	if lsp^.form = arrays then
	  frecvar := lsp
	else
	  frecvar := nil
    end;					(* FIELDLIST *)

$PAGE formaltype - in typ
    procedure formaltype;

    var
	lsy, oldsy, newsy: symbol;
	cp,lcp,lcp0,lcp1,lcp2,lcp3,nlcp2: ctp;
	lsp: stp;
	lkind: idkind;
	parwords, parlength, lc: addrrange;
	oldid: alfa;

      procedure declareparameter (id: alfa);

      var
	  lcp: ctp;

      begin
	new (lcp,vars);
	with lcp^ do begin
	  name := id;
	  selfctp := nil;
	  rlink := nil;
	  llink := nil;
	  vkind := lkind;
	  vconst := false;
	  next := lcp2;
	  if vkind = actual then
	    vclass := valparmsc
	  else
	    vclass := varparmsc
	end;
	lcp2 := lcp
      end;					(* DECLAREPARAMETER *)

    begin					(* FORMALTYPE *)
      lsy := sy;				(* REMEMBER IF PROCEDURE OR FUNCTION *)
      lcp1 := nil;
      if sy = proceduresy then begin
	lc := retloc;
	new (lcp0,proc,declared,formal)
      end
      else begin
	lc := retlocp2;				(* RESERVE TWO LOCS FOR FUNCTION VALUE *)
	new (lcp0,func,declared,formal)
      end;
      new (lsp,formalprocfunc);
      with lsp^ do begin
	selfstp := nil;
	typeid := idptr;
	size := 1;
	bitsize := 0;
	proctyptr := lcp0
      end;
      fsp := lsp;
      insymbol;
      if sy = lparent then begin
	insymbol;
	while sy in [ident,varsy] do begin
	  if sy = varsy then begin
	    lkind := formal;
	    insymbol
	  end
	  else
	    lkind := actual;
	  lcp2 := nil;
	  oldid := id;
	  oldsy := sy;
	  insymbol;
	  newsy := ident;			(* PREPARE FORM RIGHT CONTEXT CHECK *)
	  if (sy = semicolon) orif (sy = rparent) then begin
	    declareparameter ('          ');
	    newsy := sy;
	    sy := oldsy;
	    id := oldid;
	    goto 2
	  end;
	  declareparameter (oldid);
	  if sy = comma then begin
	    insymbol;
	    loop
	      if sy = ident then begin
		declareparameter (id);
		insymbol
	      end
	      else
		error (256);
	      if not (sy in [comma,colon] or fsys) then
		errandskip (256,fsys or [comma,colon,semicolon,rparent])
		exit if sy <> comma;
	      insymbol
	    end
	  end;
	  if sy = colon then begin
	    insymbol;
	    2:
	      if sy = ident then begin
		srchid ([types],lcp);
		lsp := lcp^.idtype;
		if lsp = intptr then if not specialsw then error (371);
		if lsp <> nil then
		  if (lkind = actual) andif (lsp^.form = files) then
		    error (355);
		lcp3 := lcp2;
		lcp := nil;
		while lcp2 <> nil do begin
		  with lcp2^ do begin
		    parlength := 1;
		    if (lsp <> nil) andif (lsp^.form = formalprocfunc)
		      then begin
			new (nlcp2);		(* ALLOCATE FULL SIZE TO PREVENT ILL MEM REF ON NEXT ASSIGNMENT *)
			nlcp2^ := lsp^.proctyptr^;
			nlcp2^.pfaddr := lc;
			lc := lc + 1;
			if lcp <> nil then
			  lcp^.next := nlcp2;
			if lcp3 = lcp2 then
			  lcp3 := nlcp2;
			lcp2 := nlcp2;
		      end
		    else begin
		      idtype := lsp;
		      vaddr := lc;
		      if (vkind = actual) andif (idtype <> nil) andif
			(idtype^.size > 2) then
			  vclass := spvalparmsc;
		      if (vkind = formal) or (vclass = spvalparmsc) then
			lc := lc + 1
		      else if idtype <> nil then begin
			lc := lc + idtype^.size;
			if idtype^.size = 2 then
			  parlength := 2
		      end;
		    end;
		    parwords := parwords + parlength;
		    if (parwords = parregcmax) and (parlength = 2) then
		      parwords := parwords + 1	(* SET IF REG'S 6,7*)
		  end;
		  lcp := lcp2;
		  lcp2 := lcp2^.next
		end;				(* WHILE LCP2 # NIL *)
		lcp^.next := lcp1;
		lcp1 := lcp3;
		if newsy = ident then
		  insymbol
		else
		  sy := newsy
	      end				(* IF SY = IDENT *)
	      else
		error (209);
	    iferrskip (256,fsys or [semicolon,rparent])
	  end					(* IF SY = COLON *)
	  else
	    error (151);
	  if sy = semicolon then begin
	    insymbol;
	    skipiferr (fsys or [ident,varsy],256,[rparent])
	  end;
	end;					(* WHILE SY IN [IDENT,VARSY] *)
	if sy = rparent then begin
	  insymbol;
	  iferrskip (166,fsys or [colon,semicolon])
	end
	else
	  error (152);
      end;					(* IF SY = LPARENT *)
      lcp3 := nil;
      (* REVERSE POINTERS *)
      while lcp1 <> nil do
	with lcp1^ do begin
	  lcp2 := next;
	  next := lcp3;
	  lcp3 := lcp1;
	  lcp1 := lcp2
	end;
      lcp0^.idtype := nil;
      if lsy = functionsy then
	if sy = colon then begin
	  insymbol;
	  if sy = ident then begin
	    srchid ([types],lcp1);
	    if lcp1^.idtype = intptr then if not specialsw then error (371);
	    lcp0^.idtype := lcp1^.idtype;
	    insymbol
	  end
	  else
	    errandskip (209,fsys or [semicolon])
	end
	else
	  error (455);
      with lcp0^ do begin
	parmsize := parwords - 5;		(* NUMBER OF PARAMETER WORDS IN STACK *)
	parameters := lcp3;
	selfctp := nil;
	llink := nil; rlink := nil; next := nil;
	name := '          ';
	language := pascalsy;
      end;
    end;

$PAGE type - body
  begin
  (*TYP*)
    if sy in [proceduresy,functionsy] then
      formaltype
    else begin
      skipiferr(typebegsys,170,fsys);
      packflag := false;
      if sy in typebegsys then begin
	if sy in simptypebegsys then
	  simpletype(fsys,fsp,fsize,fbitsize,idptr)
	else
	(*^*)
	if (sy = arrow) orif (sy = atsign) then begin
	  new(lsp,pointer);
	  fsp := lsp;
	  if virtual then begin
	    if sy = arrow then
	      lbitsize := bitmax
	    else begin
	      lbitsize := offset_bitsize;
	    end
	  end
	  else lbitsize := 18;
	  with lsp^ do begin
	    selfstp := nil;
	    typeid := idptr;
	    eltype := nil;
	    v_offset := virtual andif (sy = atsign);
	    size := 1;
	    bitsize := lbitsize
	  end;
	  insymbol;
	  if sy = ident then begin
	    prterr := false;			(*NO ERROR IF SEARCH NOT SUCCESSFUL*)
	    srchid([types],lcp);
	    prterr := true;
	    if lcp = nil then			(*FORWARD REFERENCED TYPE ID*)
	    begin
	      new(lcp,types);
	      with lcp^ do begin
		name := id;
		idtype := lsp;
		next := fwptr
	      end;
	      fwptr := lcp
	    end
	    else
	      lsp^.eltype := lcp^.idtype;
	    insymbol;
	    fbitsize:=lbitsize
	  	  else
	    error(209);
	end
	else begin
	  if sy = packedsy then begin
	    insymbol;
	    skipiferr(typedels,170,fsys);
	    packflag := true
	  end;
	  (*ARRAY*)
	  if sy = arraysy then begin
	    insymbol;
	    if sy = lbrack then
	      insymbol
	    else if sy <> lparent then
	      error (154)
	    else begin				(* SY = LPARENT *)
	      if not specialsw then
		error (466);
	      insymbol
	    end;
	    lsp1 := nil;
	    loop
	      new(lsp,arrays);
	      with lsp^ do begin
		aeltype := lsp1;
		inxtype := nil;
		selfstp := nil;
		typeid := nil;
		arraypf := packflag;
		size := 1
	      end;
	      lsp1 := lsp;
	      simpletype(fsys or
		[comma,rbrack,ofsy,rparent],lsp2,lsize,lbitsize,nil);
	      if lsp2 <> nil then
		if lsp2^.form <= subrange then begin
		  if lsp2 = realptr then begin
		    error(210);
		    lsp2 := nil
		  end
		  else if lsp2 = intptr then begin
		    error(306);
		    lsp2 := nil
		  end;
		  lsp^.inxtype := lsp2
		end
		else begin
		  error(403);
		  lsp2 := nil
		end;
	    exit if sy <> comma;
	      insymbol
	    end;
	    if sy = rbrack then
	      insymbol
	    else if sy <> rparent then
	      error (155)
	    else begin
	      if not specialsw then
		error (466);
	      insymbol
	    end;
	    if sy = ofsy then
	      insymbol
	    else
	      error(160);
	    typ(fsys,lsp,lsize,lbitsize,nil);
	    if lsp <> nil then
	      if lsp^.form = files then
		error(254) ;
	    repeat
	      with lsp1^ do begin
		lsp2 := aeltype;
		aeltype := lsp;
		if inxtype <> nil then begin
		  getbounds(inxtype,lmin,lmax);
		  i := lmax - lmin + 1;
		  if arraypf andif (lbitsize<=18) then begin
		  (* ARRAY BYTE PTR BUILT IN EXPLICIT CODE *)
		    lsize := (i+(bitmax div lbitsize)-1)
		      div (bitmax div lbitsize);
		  end
		  else begin
		    lsize := lsize * i;
		    arraypf := false
		  end;
		  lbitsize := bitmax;
		  bitsize := lbitsize;
		  size := lsize
		end
	      end;
	      lsp := lsp1;
	      lsp1 := lsp2
	    until lsp1 = nil
	  end
	  else
	  (*RECORD*)
	  if sy = recordsy then begin
	    insymbol;
	    oldtop := top;
	    if top < displimit then begin
	      top := top + 1;
	      display[top].fname := nil
	    end
	    else
	      error(404);
	    displ := 0;
	    bitcount:=0;
	    fieldlist(fsys-[semicolon] or [endsy],lsp1,lcp);
	    lbitsize := bitmax;
	    new(lsp,records);
	    with lsp^ do begin
	      selfstp := nil;
	      fstfld := (*LCP;*)display[top].fname;
	      fieldfg := lcp <> nil;		(* false if no fields preceded the variants *)
	      recvar := lsp1;
	      if bitcount > 0 then
		size := displ + 1
	      else
		size := displ;
	      bitsize := lbitsize;
	      recordpf := packflag
	    end;
	    top := oldtop;
	    if sy = endsy then
	      insymbol
	    else
	      error(163)
	  end
	  else
	  (*SET*)
	  if sy = setsy then begin
	    insymbol;
	    if sy = ofsy then
	      insymbol
	    else
	      error(160);
	    simpletype(fsys,lsp1,lsize,lbitsize,nil);
	    if lsp1 <> nil then
	      with lsp1^ do
		case form of
		  scalar:
		    if (lsp1=realptr) or (lsp1=intptr) then
		      error(352)
		    else if scalkind =declared then
		      if fconst^.values.ival > basemax then
			error(352);
		  subrange:
		    if ( rangetype = realptr ) or ( ( rangetype <> charptr ) and
		      ((max.ival > basemax) or (min.ival < 0) ) ) then
			error(352);
		  others: begin
		    error(353);
		    lsp1 := nil
		  end
		end;
	    lbitsize := bitmax;
	    new(lsp,power);
	    with lsp^ do begin
	      selfstp := nil;
	      elset := lsp1;
	      size:=2;
	      bitsize := lbitsize
	    end;
	  end
	  else
	  (*FILE*)
	  if sy = filesy then begin
	    insymbol;
	    if sy = ofsy then
	      insymbol
	    else
	      error(160);
	    typ(fsys,lsp1,lsize,lbitsize,nil);
	    new(lsp,files);
	    lbitsize := bitmax;
	    with lsp^ do begin
	      selfstp := nil;
	      filtype := lsp1;
	      size := lsize;
	      filepf := packflag;
	      bitsize := lbitsize
	    end;
	    if lsp1 <> nil then
	      if lsp1^.form = files then begin
		error(254);
		lsp^.filtype := nil
	      end;
	  end;
	  fsp := lsp;
	  if fsp <> nil then
	    fsp^.typeid := idptr;
	  fbitsize := lbitsize
	end;
	iferrskip(166,fsys)
      end
      else
	fsp := nil;
      if fsp = nil then begin
	fsize := 1;
	fbitsize := 0
      end
      else begin
	fsize := fsp^.size;
      end;
    end						(*IF NOT PROCEDURE/FUNCTION TYPE DECLARATION*)
  end (*TYP*);

$PAGE labeldeclaration - in block
  procedure labeldeclaration;

  var
      lab: labp;

  begin
    loop
      if sy = intconst then begin
	lab := searchlabel (val.ival, top);	(* LOOK FOR LABEL IN THIS BLOCK *)
	if lab <> nil then
	  error (211)
	else
	  lab := dcllabel (val.ival, declared);
	insymbol
      end
      else
	error(255);
      iferrskip(166,fsys or [comma,semicolon]);
    exit if sy <> comma;
      insymbol
    end;
    if sy = semicolon then
      insymbol
    else
      error(156)
  end (*LABELDECLARATION*);

$PAGE typedeclaration - in block
  procedure typedeclaration;

  var
      lcp,lcp1,lcp2: ctp;
      lsp: stp;
      lsize: addrrange;
      lbitsize: bitrange;

  begin
    skipiferr([ident],209,fsys);
    while sy = ident do begin
      new(lcp,types);
      lcp^.next := nil;
      with lcp^ do begin
	name := id;
	idtype := nil
      end;
      insymbol;
      if (sy = relop) and (op = eqop) then
	insymbol
      else
	error(157);
      typ(fsys or [semicolon],lsp,lsize,lbitsize,lcp);
      enterid(lcp);
      with lcp^ do begin
	idtype := lsp;
	(*HAS ANY FORWARD REFERENCE BEEN SATISFIED:*)
	lcp1 := fwptr;
	while lcp1 <> nil do begin
	  if lcp1^.name = name then begin
	    lcp1^.idtype^.eltype := idtype;
	    if lcp1 <> fwptr then
	      lcp2^.next := lcp1^.next
	    else
	      fwptr := lcp1^.next;
	  end;
	  lcp2 := lcp1;
	  lcp1 := lcp1^.next
	end
      end;
      if sy = semicolon then begin
	insymbol;
	iferrskip(166,fsys or [ident]);
      end
      else
	error(156)
    end;
    while fwptr <> nil do begin
      errorwithtext(405,fwptr^.name);
      fwptr := fwptr^.next
    end
  end (*TYPEDECLARATION*);

$PAGE variabledeclaration - in block
  procedure variabledeclaration (class: storageclass);

  var
      lcp,nxt, first_ctp: ctp;
      lsp: stp;
      lsize: addrrange;
      lbitsize: bitrange;
      ii: integer;
      lfileptr: ftp;

  begin
    nxt := nil;
    repeat
      loop
	if sy = ident then begin
	  new(lcp,vars);
	  with lcp^ do begin
	    name := id;
	    next := nxt;
	    idtype := nil;
	    vconst := false;
	    vkind := actual;
	    vlev := level
	  end;
	  enterid(lcp);
	  nxt := lcp;
	  insymbol;
	end
	else
	  error(209);
	skipiferr(fsys or [comma,colon] or typedels,166,[semicolon]);
      exit if sy <> comma;
	insymbol
      end;
      if sy = colon then
	insymbol
      else
	error(151);
      typ(fsys or [semicolon, becomes, relop] or
	typedels,lsp,lsize,lbitsize,nil);
      if not testpacked andif (lsp <> nil) then
	with lsp^ do begin
	  if (form = arrays) andif (aeltype <> charptr) then
	    testpacked := arraypf;
	  if form = records then
	    testpacked := recordpf
	end;
      if lsp <> nil then
	if lsp^.form = files then
	  lsize := lsize + sizeoffileblock;
      first_ctp := nxt;
      while nxt <> nil do
	with nxt^ do begin
	  idtype := lsp;
	  vclass := class;
	  case class of				(* ALLOCATE IN APPROPRIATE AREA *)
	    localsc: begin
	      vaddr := lc;
	      lc := lc + lsize
	    end;
	    staticsc, publicsc: begin
	      vaddr := statlc;
	      statlc := statlc + lsize
	    end;
	    externalsc:
	      vaddr := 0			(* END OF BACKLINK CHAIN *)
	  end;
	  if lsp <> nil then
	    if lsp^.form = files then
	      if not (class in [publicsc, staticsc, externalsc]) then
		error(454)
	      else begin
		if class in [publicsc, staticsc] then begin (* INIT FILEBLOCK *)
		  new(lfileptr);
		  with lfileptr^ do begin
		    nextftp := fileptr ;
		    fileident := nxt ;
		  end ;
		  fileptr := lfileptr;
		end
	      end (*ELSE*);
	  nxt := next ;
	end;
      if (sy=becomes) orif (sy=relop) andif (op=eqop) then begin
      (* INITIALIZE VARIABLES *)
	insymbol;
	if first_ctp <> nil then
	  if first_ctp^.next = nil then
	    var_list (class, fsys or typedels, first_ctp)
	  else
	    errandskip (559,fsys or typedels)	(* ONLY ONE VARIABLE ALLOWED *)
	else
	  while not (sy in (fsys or typedels or [semicolon])) do
	    insymbol;
      end;
      if sy = semicolon then begin
	insymbol;
	iferrskip(166,fsys or [ident])
      end
      else
	error(156)
    until (sy <> ident) andif not (sy in typedels);
    while fwptr <> nil do begin
      errorwithtext(405,fwptr^.name);
      fwptr := fwptr^.next
    end;
  end;						(* VARIABLEDECLARATION *)

$PAGE proceduredeclaration - in block
  procedure proceduredeclaration(fsy: symbol; class:storageclass);

  var
      oldlev: 0..maxlevel;
      lsy: symbol;
      lcp,lcp1: ctp;
      lsp: stp;
      forw: boolean;
      oldtop: disprange;
      lnxt: ctp;
      parwords,llc,lcm: addrrange;
      lab1: labp;

$PAGE parameterlist - in proceduredeclaration
    procedure parameterlist(fsy: setofsys; var fpar: ctp);

    var
	cp,lcp,lcp1,lcp2,lcp3,newlcp2: ctp;
	lsp: stp;
	lkind: idkind;
	parlength : addrrange;
	oldsy, newsy: symbol;
	oldid: alfa;
	extrn: boolean;

      procedure declareparameter (varid: alfa);

      var
	  lcp: ctp;

      begin
	new (lcp,vars);
	with lcp^ do begin
	  name := varid;
	  idtype := nil;
	  vkind := lkind;
	  vconst := false;
	  next := lcp2;
	  vlev := level;
	  if vkind = actual then
	    vclass := valparmsc
	  else
	    vclass := varparmsc
	end;
	lcp2 := lcp
      end;

    begin
      extrn := class = externalsc;
      lcp1 := nil;
      skipiferr(fsy or [lparent],256,fsys);
      if sy = lparent then begin
	if forw then
	  error(553);
	insymbol;
	skipiferr([ident,varsy],256,fsys or [rparent]);
	while sy in [ident,varsy] do begin
	  if sy = varsy then begin
	    lkind := formal;
	    insymbol
	  end
	  else
	    lkind := actual;
	  lcp2 := nil;
	  if extrn then begin
	    oldid := id;
	    oldsy := sy;
	    insymbol;
	    newsy := ident;
	    if (sy = semicolon) orif (sy = rparent) then begin
	      declareparameter ('          ');
	      newsy := sy;
	      sy := oldsy;
	      id := oldid;
	      goto 4
	    end;
	    declareparameter (oldid);
	    if sy = colon then
	      goto 3;
	    if sy = comma then
	      insymbol
	    else
	      error (158)
	  end;
	  loop
	    if sy = ident then begin
	      declareparameter (id);
	      (* Hold ENTERID(LCP) until it is known whether
		 it is a formal proc/func parameter, in which case
		 a different record must be entered *)
	      insymbol;
	    end
	    else
	      error(256);
	    if not (sy in [comma,colon] or fsys) then
	      errandskip(256,fsys or [comma,semicolon,rparent])
	      exit if sy <> comma;
	    insymbol
	  end;
	  if sy = colon then begin
	    3:
	      insymbol;
	    4:
	      if sy = ident then begin
		srchid([types],lcp);
		lsp := lcp^.idtype;
		if lsp = intptr then if not specialsw then error (371);
		if lsp <> nil then
		  if (lkind = actual) andif (lsp^.form = files) then
		    error(355);
		lcp3 := lcp2;
		lcp := nil;
		while lcp2 <> nil do begin
		  with lcp2^ do begin
		    parlength := 1;
		    if (lsp <> nil) andif (lsp^.form = formalprocfunc)
		      then begin
			new (newlcp2);		(* ALLOCATE FULL SIZE TO PREVENT ILL MEM REF ON NEXT ASSIGNMENT *)
			newlcp2^ := lsp^.proctyptr^;
			newlcp2^.name := name;
			newlcp2^.pflev := vlev;
			newlcp2^.pfaddr := lc;
			newlcp2^.next := next;
			lc := lc + 1;
			if lcp <> nil then
			  lcp^.next := newlcp2;
			if lcp3 = lcp2 then
			  lcp3 := newlcp2;
			lcp2 := newlcp2;
		      end
		    else begin
		      idtype := lsp;
		      vaddr := lc;
		      if (vkind = actual) andif (idtype <> nil) andif
			(idtype^.size > 2) then
			  vclass := spvalparmsc;
		      if (vkind = formal) orif (vclass = spvalparmsc) then
			lc := lc + 1
		      else if idtype <> nil then begin
			lc := lc + idtype^.size;
			if idtype^.size = 2 then
			  parlength := 2
		      end;
		    end;
		    parwords := parwords + parlength;
		    if (parwords = parregcmax) and (parlength = 2) then
		      parwords := parwords + 1;	(*SET IN REGS 6, 7*)
		  end;
		  lcp := lcp2;
		  lcp2 := lcp2^.next
		end;
		if lcp <> nil then lcp^.next := lcp1;
		lcp1 := lcp3;
		if (not extrn) orif (newsy = ident) then
		  insymbol
		else
		  sy := newsy
	      end
	      else
		error(209);
	    iferrskip(256,fsys or [semicolon,rparent])
	  end
	  else
	    error(151);
	  if sy = semicolon then begin
	    insymbol;
	    skipiferr(fsys or [ident,varsy],256,[rparent])
	  end
	end (*WHILE*);
	if sy = rparent then begin
	  insymbol;
	  iferrskip(166,fsy or fsys)
	end
	else
	  error(152);
	lcp3 := nil;
	(*REVERSE POINTERS*)
	while lcp1 <> nil do
	  with lcp1^ do begin
	    lcp2 := next;
	    next := lcp3;
	    lcp3 := lcp1;
	    lcp1 := lcp2
	  end;
	fpar := lcp3;
	(* Finally, enter the parameters in the symbol table in the order encountered *)
	lcp := lcp3;
	while lcp <> nil do begin
	  if class <> externalsc then
	    enterid (lcp)
	  else
	    with lcp^ do begin
	      selfctp := nil;
	      llink := nil;
	      rlink := nil;
	    end;
	  lcp := lcp^.next
	end
      end
      else
	fpar := nil
    end (*PARAMETERLIST*);

  begin
  (*PROCEDUREDECLARATION*)
    llc := lc;
    lc := retloc;
    if fsy = functionsy then
      lc := retlocp2;				(* RESERVE TWO WORDS FOR FUNCTION VALUE *)
    if sy = ident then begin
      srchsection(display[top].fname,lcp);	(*DECIDE WHETHER FORW.*)
      if lcp <> nil then
	with lcp^ do begin
	  if klass = proc then
	    forw := forwdecl andif (fsy = proceduresy) andif (pfkind = actual)
	  else if klass = func then
	    forw := forwdecl andif (fsy = functionsy) andif (pfkind = actual)
	  else
	    forw := false;
	  if not forw then
	    error(406)
	end
      else
	forw := false;
      if not forw then begin
	if fsy = proceduresy then
	  new(lcp,proc,declared,actual)
	else
	  new(lcp,func,declared,actual);
	with lcp^ do begin
	  name := id;
	  idtype := nil;
	  testfwdptr := nil;
	  forwdecl := false;
	  pfclass := class;
	  language := pascalsy;
	  pflev := level;
	  pfaddr := 0;
	  for i := 0 to maxlevel do
	    linkchain[i] := 0
	end;
	enterid(lcp)
      end
      else begin
	lcp1 := lcp^.next;
	while lcp1 <> nil do begin
	  with lcp1^ do
	    if klass = vars then
	      if idtype <> nil then begin
		lcm := vaddr + idtype^.size;
		if lcm > lc then
		  lc := lcm
	      end;
	  lcp1 := lcp1^.next
	end
      end;
      insymbol
    end
    else begin
      error(209);
      forw := false;
      (* Originally LCP was set to UPRCPTR or UFCTPTR to allow compilation
	 to continue. This had the result of entering a record into
	 the symbol table more than once, with undesirable side-effects.
	 Allocate a new record entirely and initialize it as appropriate. *)
      new (lcp);
      if fsy = proceduresy then
	lcp^ := uprcptr^
      else
	lcp^ := ufctptr^
    end;
    oldlev := level;
    oldtop := top;
    if level < maxlevel then
      level := level + 1
    else
      error(453);
    if top < displimit then begin
      top := top + 1;
      with display[top] do begin
	fname := nil;
	occur := blck;
	flabel := nil;
	blkid := lcp;
	if forw then
	  fname := lcp^.next
      end					(*WITH DISPLAY[TOP]*)
    end
    else
      error(404);
    parwords := 0;
    if fsy = proceduresy then begin
      parameterlist([semicolon],lcp1);
      if not forw then
	lcp^.next := lcp1
    end
    else begin
      parameterlist([semicolon,colon],lcp1);
      if not forw then
	lcp^.next := lcp1;
      if sy = colon then begin
	insymbol;
	if sy = ident then begin
	  if forw then
	    error(552);
	  srchid([types],lcp1);
	  lsp := lcp1^.idtype;
	  lcp^.idtype := lsp;
	  insymbol
	end
	else
	  errandskip(209,fsys or [semicolon])
      end
      else if not forw then
	error(455)
    end;
    if not forw then lcp^.parmsize := parwords - 5; (*NUMBER PARAM WORDS IN STACK*)
    if sy = semicolon then
      insymbol
    else
      error(156);
    if sy = forwardsy then begin
      if forw then
	error(257)
      else
	with lcp^ do begin
	  testfwdptr := forwptr;
	  forwptr := lcp;
	  forwdecl := true
	end;
      insymbol;
      if sy = semicolon then
	insymbol
      else
	error(156);
      iferrskip(166,fsys)
    end						(* SY = FORWARDSY *)
    else
      with lcp^ do begin
	if sy = externsy then begin
	  if pfclass <> codesc then
	    error (318);
	  pfclass := externalsc;
	  insymbol;
	  if sy = semicolon then
	    insymbol
	  else if not (sy in languagesys) then
	    error (156)
	end;
	if pfclass = externalsc then begin
	  if forw then
	    error(257);
	  if level <> 2 then
	    error(464);
	  if sy in languagesys then begin
	    language := sy;
	    insymbol;
	    if sy = semicolon then
	      insymbol
	    else
	      error(156);
	    iferrskip(166,fsys)
	  end;
	  if (libix = 0) or (not library[language].inorder) then begin
	    libix:= libix+1;
	    liborder[libix]:= language;
	    library[language].inorder:= true
	  end;
	  pflev := 1;
	end					(* EXTERNAL CASE *)
	else begin
	  if progresssw then begin
	    with lcp^ do begin
	      i := 1;
	      while (i<=10) andif (name[i] <> ' ') do begin
		write (tty,name[i]);
		i := i + 1
	      end
	    end;
	    write (tty,' ');
	    break (tty)
	  end;
	  pfchain := localpfptr;
	  localpfptr := lcp;
	  forwdecl := false;
	  pflink := pflist;			(* LINK TO PREVIOUS PROC/FUNC AT THIS LEVEL *)
	  pflist := nil;			(* PREPARE FOR CHAIN OF PROCS AT LOWER LEVEL *)
	  block(lcp,fsys,fsys + [period, semicolon]);
	  pflower := pflist;
	  pflist := lcp;
	  if debug then
	    next := display[top].fname;
	  if sy = semicolon then begin
	    insymbol;
	    skipiferr(blockbegsy,166,fsys);
	    lab1 := display[top].flabel;
	    if (lab1 <> nil) andif not errorflag then begin
	    (* if an include file was closed on this call to INSYMBOL
	       forward references from page and file blocks may have
	       been emitted as labels after the code was written.
	       if so they must be preserved for emission at the
	       next lower level. *)
	      while lab1^.nextlab <> nil do
		lab1 := lab1^.nextlab;
	      lab1^.nextlab := display[oldtop].flabel;
	      display[oldtop].flabel := display[top].flabel;
	    end;
	  end
	  else if main or (level > 2) or (sy <> period) then
	    error(156)
	end					(* SY # EXTERNSY *)
      end (* SY # FORWARDSY *);
    level := oldlev;
    top := oldtop;
    lc := llc;
  end (*PROCEDUREDECLARATION*);

var
    dclclass: storageclass;			(* CLASS NAME FOR DCL SECTION *)

$PAGE block - body
  procedure getstorageclass;			(* SETS ABOVE VARIABLE *)

  begin
    dclclass := defaultsc;
    if sy = publicsy then
      dclclass := publicsc
    else if sy = externalsy then
      dclclass := externalsc
    else if sy = staticsy then
      dclclass := staticsc;
    if sy in [publicsy, externalsy, staticsy] then
      insymbol;
    if (dclclass in [publicsc, externalsc]) andif (level > 1) then begin
      error (318);				(* ONLY ALLOWED AT TOP LEVEL *)
    end
  end;

var
    ltestpacked: boolean;			(*  LOCAL CLOSURE OF GLOBAL VARS *)
    llcpar: addrrange;
    lcurproc: ctp;

begin
(*BLOCK*)
  if not debug then
    mark(heapmark);
  ltestpacked := testpacked;
  llcpar := lcpar;
  lcurproc := curproc;
  dp := true;
  testpacked := false;
  forwptr := nil;
  curproc := fprocp;
  zeronest;
  lcpar := lc;
  scalar_top := top;
  repeat
    while sy in blockbegsys - [beginsy] do begin
      getstorageclass;				(* SETTING DCLCLASS *)
      if sy = labelsy then begin
	if dclclass <> defaultsc then
	  error (318);
	insymbol;
	labeldeclaration
      end
      else if sy = constsy then begin
	if not (dclclass in [defaultsc, staticsc, publicsc, externalsc]) then
	  error (318);
	if dclclass = defaultsc then
	  dclclass := localsc;
	insymbol;
	cst_declaration (fsys, dclclass)
      end
      else if sy = typesy then begin
	if dclclass <> defaultsc then
	  error (318);
	insymbol;
	typedeclaration
      end
      else if sy = varsy then begin
	if dclclass = defaultsc then
	  if level = 1 then
	    dclclass := staticsc
	  else
	    dclclass := localsc;
	insymbol;
	variabledeclaration (dclclass)
      end
      else if sy = initprocsy then begin
(*$Y5	ERROR (468);	(* NO LONGER SUPPORTED *)
	REPEAT
	  INSYMBOL;
	UNTIL SY IN [ENDSY,EOFSY];
	INSYMBOL;
	IF SY = SEMICOLON THEN 
	  INSYMBOL;	*)
(*$X5	IF DCLCLASS <> DEFAULTSC THEN
	  ERROR (318);
	INSYMBOL ;
	IF SY <> SEMICOLON THEN
	  ERRANDSKIP(156,[BEGINSY])
	ELSE
	  INSYMBOL ;
	IF SY = BEGINSY THEN BEGIN
	  MARK(GLOBMARK) ;
	  INITGLOBALS := TRUE ;
	  INSYMBOL ;
	  BODY(FSYS OR [SEMICOLON,ENDSY]) ;
	  IF SY = SEMICOLON THEN
	    INSYMBOL
	  ELSE
	    ERROR(166) ;
	  INITGLOBALS := FALSE ;
	  RELEASE(GLOBMARK) ;
	END
	ELSE
	  ERROR(201) ;
	TESTPACKED := FALSE;	*)
      end
      else if sy in [proceduresy,functionsy] then begin
	if dclclass = staticsc then begin
	  error (318);
	  dclclass := defaultsc
	end;
	if dclclass = defaultsc then
	  dclclass := codesc;
	lsy := sy;
	insymbol;
	proceduredeclaration(lsy, dclclass);
	dp := true
      end
      else
	error (318);				(* STATIC BEGIN, PERHAPS? *)
    end;
    while forwptr <> nil do
      with forwptr^ do begin
	if forwdecl then
	  errorwithtext(465,name);
	forwptr := testfwdptr
      end;
    dp := false;
    if (main or (level > 1)) then
      if sy = beginsy then
	insymbol
      else
	error (201)
    else if sy <> period then begin
      error(175);
      insymbol
    end;
    incnest;
    body(fsys or [casesy]);
    decnest;
    skipiferr(leaveblocksys,166,fsys)
  until sy in (leaveblocksys + [eofsy]);
  if lcurproc <> nil then begin
    testpacked := ltestpacked;
    lcpar := llcpar;
  end;
  curproc := lcurproc;
  if not debug then
    release(heapmark);
end						(*BLOCK*)

.
    9Æ