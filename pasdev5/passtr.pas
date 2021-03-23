$title passtr - PASCAL compiler string support routines
$OPTIONS SPECIAL, NOCHECK

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASCG.INC

$INCLUDE PASCMP.INC

$INCLUDE PASLEX.INC

$PAGE declarations

const						(* FOR CODE GENERATION *)

    p1 = 0;					(* STRING OP DESCRIPTORS *)
    l1 = 1;
    p2 = 2;
    l2 = 3;
    p3 = 4;
    move = 200b;
    movei = 201b;
    movem = 202b;
    caile = 303b;
    hrli = 505b;
    hrri = 541b;
    hrlzi = 515b;
    hrrz = 550b;
    skipe = 332b;
    skipn = 336b;
    exch = 250b;
    blt = 251b;
    ildb = 134b;
    lsh = 242b;
    setzm = 402b;
    addm = 272b;
    charbyteptr = 440700b;

$PAGE dclstring, strkind

public function dclstring (fmaxl:integer; fskind:stringkind): stp;

var
    lsp: stp;

begin
  new (lsp, strings);
  with lsp^ do begin
    skind := fskind;
    maxlength := fmaxl;
    size := (fmaxl + 4) div 5;
    if fskind = varying then
      size := size + 1;
    selfstp := nil;
    bitsize := bitmax
  end;
  dclstring := lsp
end;

public function strkind (fsp: stp): stringkind;

begin
  strkind := nonvarying;
  if fsp <> nil then
    if fsp^.form = strings then
      strkind := fsp^.skind
end;

$PAGE classify

type
    strclass =					(* PERMUTATIONS OF              *)
    ( vc, vv, ve, vss,				(*   VARYING    CST, VARBL, EXPR, SUBSTR *)
    fc, fv, fe, fss,				(*   FIXED                              *)
    chc, chv, che );				(*   CHAR                               *)

    (* ASSUME THAT TYPTR IS NOT NIL *)

function classify (var fattr: attr): strclass;

begin
  with fattr do begin
    if typtr^.form = arrays then begin
      if strform (typtr) then
	typtr := dclstring (typtr^.inxtype^.max.ival, nonvarying)
      else
	error (266)
    end;
    if comptypes (typtr, charptr) then begin
      if kind = cst then
	classify := chc
      else if kind = varbl then
	classify := chv
      else
	classify := che
    end
    else if typtr^.skind = varying		(* CAN ASSUME THAT IT IS STRING *)
    then begin
      if kind = cst then
	classify := vc
      else if kind = varbl then
	classify := vv
      else if kind = expr then
	classify := ve
      else
	classify := vss
    end
    else					(* SKIND = NONVARYING *)
    begin
      if kind = cst then
	classify := fc
      else if kind = varbl then
	classify := fv
      else if kind = expr then
	classify := fe
      else
	classify := fss
    end
  end
end;

$PAGE str1tochar, chartostr1, saveexpr

procedure str1tochar (reg: acrange);

begin
  mac3 (lsh, reg, 1000000b - 29)
end;

procedure chartostr1 (reg: acrange);

begin
  mac3 (lsh, reg, 29)
end;

public procedure saveexpr (var fattr: attr);

var
    lreg: acrange;

begin
  with fattr do begin
    if fattr.typtr <> nil then begin
      lreg := reg;
      kind := varbl;
      vrelbyte := no;
      packfg := notpack;
      vsclass := localsc;
      dplmt := lc;
      indexr := basis;
      indbit := 0;
      vlevel := 1;
      checklc (typtr^.size);
      store (lreg, fattr);
      if lreg = regc then
	regc := regc - typtr^.size
    end
  end
end;

$PAGE makestringaddressible, ldstrptr

procedure makestringaddressible (var fattr: attr);

begin
  with fattr do begin
    fetchbasis (fattr);
    if indbit = 1 then begin
      incrementregc;
      mac (vrelbyte, move, regc, 0, indexr, dplmt);
      indbit := 0;
      indexr := regc;
      dplmt := 0;
      vrelbyte := no
    end
  end
end;

procedure ldstrptr (freg: acrange; var fattr: attr);

begin
  with fattr do begin
    if kind = varbl then begin
      fetchbasis (fattr);
      if strkind (typtr) = varying then begin
	if (indbit = 1) then begin
	  mac (vrelbyte, move, freg, 0, indexr, dplmt);
	  mac4 (movei, freg, freg, 1)
	end
	else
	  mac (vrelbyte, movei, freg, indbit, indexr, dplmt+1)
      end
      else					(* NONVARYING *)
      begin
	mac (vrelbyte, movei, freg, indbit, indexr, dplmt)
      end
    end
    else					(* KIND = CST *)
    begin
      mac3 (movei, freg, 0);
      depcst (strg, fattr);
      if typtr^.skind = varying then
	mac4 (movei, freg, freg, 1)
    end
  end
end;

$PAGE alterstringcst

public procedure alterstringcst (var fattr: attr; tsp: stp);

var
    lcp: csp;
    copylen: 0..strglgth;
    i: integer;

begin
  with fattr do begin
    if not comptypes (typtr, tsp) then begin
      if comptypes (tsp, charptr)		(* TARGET IS CHAR *)
      then begin
	if cval.valp^.slgth = 0 then
	  cval.ival := ord (' ')
	else
	  cval.ival := ord (cval.valp^.sval[1])
      end
      else					(* TARGET IS STRING *)
      begin
	if tsp^.form = arrays then
	  tsp := dclstring (tsp^.inxtype^.max.ival, nonvarying);
	new (lcp, strg: tsp^.maxlength);
	with lcp^ do begin
	  slgth := tsp^.maxlength;
	  if comptypes (typtr, charptr) then begin
	    copylen := 1;
	    sval[1] := chr (cval.ival)
	  end
	  else begin
	    if slgth > cval.valp^.slgth then
	      copylen := cval.valp^.slgth
	    else
	      copylen := slgth;
	    for i := 1 to copylen do
	      sval[i] := cval.valp^.sval[i];
	  end;
	  for i := copylen+1 to slgth do
	    sval[i] := ' ';
	end;
	cval.valp := lcp
      end;
      typtr := tsp
    end
  end
end;

$PAGE strop

public procedure strop (op: supports; reg: acrange);

var
    regcount: acrange;

begin
  if (op = ctss) or (op = ctssr) then
    regcount := p3
  else
    regcount := l2;
  if (reg+regcount) > regcmax then
    error (310);
  mac3 (movei, tac, reg);			(* PUT PTR TO DESCRIPTOR IN REG1 *)
  support (op)
end;

$PAGE maxlen
public function maxlen (var fattr: attr): integer;


begin
  with fattr do begin
    if typtr = nil then
      maxlen := 0
    else if typtr^.form = strings then
      maxlen := typtr^.maxlength
    else if typtr^.form = arrays then begin
      maxlen := typtr^.inxtype^.max.ival
    end
    else if comptypes (typtr, charptr) then
      maxlen := 1
    else
      maxlen := 0
  end
end;

$PAGE strdesc

public procedure strdesc (freg: acrange; var fattr: attr);

begin
  with fattr do begin
    if comptypes (typtr, charptr) then begin	(* MAKE IT A STRING *)
      if kind = cst then
	alterstringcst (fattr, dclstring (1, nonvarying))
      else if (kind = varbl) and (packfg = packr) then begin	(* BYTE PTR LOADED, GET LEN *)
	mac3 (movei, freg+l1, 1);
	kind := substrng;
	reg := bpaddr
      end
      else begin
	if kind = varbl then
	  makecode (hrrz, freg, fattr);
	chartostr1 (reg);
	typtr := dclstring (1, nonvarying)
      end
    end;
    if kind = expr then begin			(* MAKE IT A VARBL *)
      saveexpr (fattr)
    end;
    if kind in [varbl, cst] then begin		(* LOAD POINTER AND LENGTH *)
      ldstrptr (freg, fattr);
      mac3 (hrli, freg, charbyteptr);		(* MAKE IT A BYTE PTR *)
      if strkind (typtr) = varying then
	mac4 (move, freg+l1, freg, -1)
      else
	mac3 (movei, freg+l1, maxlen (fattr));
      kind := substrng;
      reg := freg
    end;
    if reg <> freg then
      error (171)				(* COMPILER ERROR *)
  end
end;

$PAGE movewords

procedure movewords (var sattr: attr; soff: addrrange; var tattr: attr;
  toff: addrrange; movetype: stp);

var
    source, target: attr;
    lreg: acrange;

begin
  source := sattr;
  source.typtr := movetype;
  target := tattr;
  target.typtr := movetype;
  if movetype^.size <= 2 then begin
    if soff <> 0 then begin			(* ASSUME NOT EXPR OR CST *)
      makestringaddressible (source);
      source.dplmt := source.dplmt + soff
    end;
    load (source);
    if toff <> 0 then begin
      makestringaddressible (target);
      target.dplmt := target.dplmt + toff
    end;
    store (source.reg, target)
  end
  else begin
    ;						(* MOVE WITH A BLT *)
    incrementregc;
    lreg := regc;
    with source do begin
      if kind = cst then begin
	mac3 (hrli, lreg, 0);
	depcst (strg, source)
      end
      else begin
	makestringaddressible (source);
	dplmt := dplmt + soff;
	mac5 (vrelbyte, hrli, lreg, indexr, dplmt)
      end
    end;
    with target do begin
      makestringaddressible (target);
      dplmt := target.dplmt + toff;
      mac5 (vrelbyte, hrri, lreg, indexr, dplmt);
      mac5 (vrelbyte, blt, lreg, indexr, dplmt + movetype^.size - 1)
    end
  end
end;

$PAGE strparm

public procedure mvstring (var sattr, tattr: attr);

forward;

public procedure strparm (var sattr: attr; tsp: stp; onreg: acrange);

var
    tattr: attr;
    sclass: strclass;
    lreg: acrange;

begin
  if (sattr.typtr <> nil) and strform (sattr.typtr) then begin
    with sattr do begin
      sclass := classify (sattr);
      if comptypes (tsp, charptr) then begin
	if sclass in [chc, chv, che] then
	  load (sattr)
	else if sclass in [fss, vss] then begin
	  mac3 (ildb, reg, reg);		(* LOAD THROUGH BYTE POINTER *)
	  mac3 (skipn, 0, reg+1);
	  mac3 (movei, reg, ord (' '));		(* BLANK IF LENGTH = 0 *)
	  regc := reg;
	  kind := expr
	end
	else if sclass in [vc, fc] then begin
	  alterstringcst (sattr, charptr);
	  load (sattr)
	end
	else if sclass = ve then begin
	  mac3 (exch, reg-1, reg);		(* GET BYTE IN LOWER REG *)
	  mac3 (skipn, 0, reg);			(* IF LENGTH ZERO, RESULT IS ' '*)
	  mac3 (hrlzi, reg-1, ord (' ') * 4000b);
	  str1tochar (reg-1);
	  regc := reg - 1;
	  kind := expr
	end
	else if sclass = fe then begin
	  str1tochar (reg);
	  regc := reg;
	  if typtr^.size > 1 then
	    regc := regc - 1;
	  kind := expr
	end
	else if sclass in [vv, fv] then begin
	  incrementregc;
	  lreg := regc;
	  makestringaddressible (sattr);
	  if sclass = vv then
	    dplmt := dplmt + 1;
	  mac (vrelbyte, move, lreg, indbit, indexr, dplmt);
	  str1tochar (lreg);
	  if sclass = vv then begin
	    mac5 (vrelbyte, skipn, 0, indexr, dplmt-1);
	    mac3 (movei, lreg, ord (' '))
	  end;
	  kind := expr;
	  reg := lreg;
	  regc := lreg;
	end;
	typtr := tsp;
      end					(* OF CONVERSION TO CHAR *)

      else					(* TARGET IS STRING *)
      begin
	if (sclass in [vss,fss,ve,fe]) orif not comptypes (typtr, tsp)
	  then begin
	    with tattr do begin
	      kind := varbl;
	      vrelbyte := no;
	      vsclass := localsc;
	      indexr := basis;
	      dplmt := lc;
	      indbit := 0;
	      vlevel := 1;
	      packfg := notpack;
	      checklc (tsp^.size);
	      typtr := tsp
	    end;
	    mvstring (sattr, tattr);
	    sattr := tattr;
	    if (onreg <> 0) then
	      regc := onreg
	      (* FORCE PARAMETER INTO CORRECT REGISTER *)
	  end;
	if typtr^.size <= 2 then
	  load (sattr)
	else
	  ldaddress
      end
    end
  end
end;

$PAGE mvstring

public procedure mvstring (* VAR SATTR, TATTR: ATTR *);

var
    lreg: acrange;
    maxl: integer;
    saveregc: acrange;
    sclass, tclass: strclass;

begin
  saveregc := regc;
  if (sattr.typtr <> nil) and (tattr.typtr <> nil) then begin
    sclass := classify (sattr);
    tclass := classify (tattr);
    if comptypes (sattr.typtr, tattr.typtr) and not ((sclass in [vss, fss]) or
      (tclass in [vss, fss])) then begin
	movewords (sattr, 0, tattr, 0, sattr.typtr)
      end
      else if sclass in [ve, fe] then begin	(* STORE BEFORE MOVING *)
	saveexpr (sattr);
	mvstring (sattr, tattr)
      end
      else if (sclass in [che, chv]) and (tclass = vv) then begin
	load (sattr);
	makestringaddressible (tattr);
	with tattr do begin
	  chartostr1 (sattr.reg);
	  mac (vrelbyte, movem, sattr.reg, indbit, indexr, dplmt + 1);
	  mac3 (movei, sattr.reg, 1);		(* LENGTH IS ONE *)
	  mac (vrelbyte, movem, sattr.reg, indbit, indexr, dplmt);
	  saveregc := sattr.reg - 1
	end
      end
      else if (sclass = vc) and (tclass = vv) then begin
	if maxlen (sattr) = 0 then begin
	  fetchbasis (tattr);
	  with tattr do
	    mac (vrelbyte, setzm, 0, indbit, indexr, dplmt)
	end
	else if maxlen (tattr) >= maxlen (sattr) then	(* TARGET IS LONGER, NEED NOT EXTEND CONST *)
	  movewords (sattr, 0, tattr, 0, sattr.typtr)
	else begin
	  alterstringcst (sattr, tattr.typtr);
	  movewords (sattr, 0, tattr, 0, sattr.typtr)
	end
      end
      else if (sclass = vv) and (tclass = vv) then begin
	incrementregc;				(* GET FOUR REGS FOR DESC *)
	ldstrptr (regc+p1, sattr);
	mac4 (move, regc+l1, regc+p1, 1000000b-1);
	ldstrptr (regc+p2, tattr);
	mac3 (move, regc+l2, regc+l1);		(* COPY LENGTH *)
	if maxlen (sattr) > maxlen (tattr) then begin
	  mac3 (caile, regc+l2, maxlen (tattr));
	  mac3 (movei, regc+l2, maxlen (tattr));
	end;
	mac4 (movem, regc+l2, regc+p2, 10000000b-1);	(* STORE RESULT LENGTH *)
	strop (cmff, regc)
      end
      else if (sclass in [vss, fss]) and (tclass = vv) then begin
	lreg := sattr.reg;
	saveregc := lreg - 1;
	ldstrptr (lreg+p2, tattr);
	mac3 (move, lreg+3, lreg+l1);
	if maxlen (sattr) > maxlen (tattr) then begin
	  mac3 (caile, lreg+3, maxlen (tattr));
	  mac3 (movei, lreg+3, maxlen (tattr));
	end;
	mac4 (movem, lreg+l2, lreg+p2, 1000000b-1);
	strop (cmsf, lreg)
      end
      else if (sclass = fc) and (tclass = vv) then begin
	alterstringcst (sattr, dclstring (maxlen (sattr), varying));
	mvstring (sattr, tattr)
      end
      else if (sclass = fv) and (tclass = vv) then begin
	if maxlen (tattr) >= maxlen (sattr) then
	  maxl := maxlen (sattr)
	else
	  maxl := maxlen (tattr);
	makestringaddressible (tattr);
	incrementregc;
	mac3 (movei, regc, maxl);
	with tattr do begin
	  mac (vrelbyte, movem, regc, indbit, indexr, dplmt);
	  regc := regc - 1;
	  if indexr = tac then begin
	    incrementregc;
	    mac3 (move, regc, tac)		(* SAVE IN CASE USED AGAIN *)
	  end
	end;
	movewords (sattr, 0, tattr, 1, dclstring (maxl, nonvarying))
      end
      else if (sclass in [che, chv]) and (tclass in [fv, vss, fss]) then begin
	load (sattr);
	chartostr1 (sattr.reg);
	sattr.typtr := dclstring (1, nonvarying);
	mvstring (sattr, tattr)
      end
      else if (sclass = chc) and (tclass in [fv, vv, vss, fss]) then begin
	alterstringcst (sattr, dclstring (1, tattr.typtr^.skind));
	mvstring (sattr, tattr)
      end
      else if (sclass in [vc, fc]) and (tclass = fv) then begin
	if maxlen (sattr) >= maxlen (tattr) then begin	(* TOO LONG FC'S, >= VC'S *)
	  alterstringcst (sattr, tattr.typtr);
	  mvstring (sattr, tattr)
	end
	else if (maxlen (tattr) - maxlen (sattr)) <= 30 then begin  (* SHORTER, OKAY TO PAD OUT *)
	  alterstringcst (sattr, tattr.typtr);
	  mvstring (sattr, tattr)
	end
	else begin				(* PAD IN CODE, NOT IN STORAGE *)
	  incrementregc;
	  ldstrptr (regc+p1, sattr);
	  mac3 (movei, regc+l1, maxlen (sattr));
	  ldstrptr (regc+p2, tattr);
	  mac3 (movei, regc+l2, maxlen (tattr));
	  strop (cmff, regc)
	end
      end
      else if (sclass in [fv, vv]) and (tclass = fv) then begin
	incrementregc;
	ldstrptr (regc+p1, sattr);
	if sattr.typtr^.skind = varying then
	  mac4 (move, regc+l1, regc+p1, 1000000b-1)
	else
	  mac3 (movei, regc+l1, maxlen (sattr));
	ldstrptr (regc+p2, tattr);
	mac3 (movei, regc+l2, maxlen (tattr));
	strop (cmff, regc)
      end
      else if (sclass in [vss, fss]) and (tclass = fv) then begin
	lreg := sattr.reg;			(* CONTAINS BYTE PTR, LEN *)
	saveregc := lreg - 1;
	ldstrptr (lreg+p2, tattr);
	mac3 (movei, lreg+l2, maxlen (tattr));
	strop (cmsf, lreg)
      end
      else if (sclass in [fc, vv, vc, fv]) and (tclass in [vss, fss]) then begin
      (* ONLY OCCURS WHEN SUBSTR IS TARGET OF ASSIGNMENT, BYTE
	 PTR AND LENGTH ARE LOADED *)
	lreg := tattr.reg;
	saveregc := lreg - 1;
	strdesc (lreg+p2, sattr);		(* DO MOVE IN REVERSE *)
	strop (cmssr, lreg)
      end
      else if (sclass in [vss, fss]) and (tclass in [vss, fss]) then begin
      (* ASSIGNMENT OF SUBSTRING TO SUBSTRING, TARGET WAS PUSHED
	 FIRST IN COURSE OF PROCESSING *)
	saveregc := tattr.reg-1;
	strop (cmssr, tattr.reg)
      end
      else if (tclass = chv) then begin
	strparm (sattr, charptr, 0);		(* COERCES AND LOADS CHAR *)
	store (sattr.reg, tattr)
      end
  end;
  regc := saveregc
end;

$PAGE strcat

public procedure strcat (reg:acrange; var lop, rop: attr);
(*** SETS GATTR ON RETURN ***)

var
    rattr: attr;				(* RESULT ATTR, GATTR MAY BE LOP OR ROP *)
    lreg: acrange;
    maxl: integer;

begin
  strdesc (reg+p1, lop);			(* PUSH DESCRIPTORS *)
  strdesc (reg+p2, rop);
  maxl := maxlen (lop) + maxlen (rop);		(* GIVES MAX LENGTH EVEN FOR VARYING RESULT *)
  if (strkind (lop.typtr) = varying) or (strkind (rop.typtr) = varying) then
    rattr.typtr := dclstring (maxl, varying)
  else
    rattr.typtr := dclstring (maxl, nonvarying);
  with rattr do begin				(* ALLOC TEMP FOR RESULT *)
    kind := varbl;
    vrelbyte := no;
    packfg := notpack;
    vsclass := localsc;
    dplmt := lc;
    indexr := basis;
    indbit := 0;
    vlevel := 1;
    checklc (typtr^.size);
    if typtr^.skind = varying then begin	(* STORE LENGTH TOO *)
      mac4 (movem, lop.reg+l1, basis, dplmt);
      mac4 (addm, rop.reg+l1, basis, dplmt)
    end
  end;
  ldstrptr (reg+p3, rattr);			(* 3RD ARG IS PTR TO RESULT *)
  strop (ctss, reg);
  regc := reg - 1;
  gattr := rattr
end.
  