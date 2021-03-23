$TITLE pascg - PASCAL compiler code generation



$PAGE includes
type alfa = packed array[1..10] of char;
$SYSTEM PASDCL.INC

$SYSTEM PASLEX.INC

$SYSTEM PASOPT.INC


	EXTERNAL FUNCTION COMPTYPES (STP; STP) : BOOLEAN;
	EXTERNAL FUNCTION STRFORM (STP): BOOLEAN;


$SYSTEM pasmc.inc
$PAGE declarations

external procedure mac (relbyte; instrange; acrange; ibrange; acrange;
  addrrange);

external procedure mac5 (relbyte; instrange; acrange; acrange; addrrange);

external procedure mac4 (instrange; acrange; acrange; addrrange);

external procedure mac4r (instrange; acrange; acrange; addrrange);

external procedure mac3r (instrange; acrange; addrrange);

external procedure mac3b (instrange; acrange; addrrange);

external procedure mac3 (instrange; acrange; addrrange);

external procedure incrementregc;

$PAGE fullword
public procedure fullword(frelbyte: relbyte; flefth: addrrange;
  frighth: addrrange);

begin
(*FULLWORD*)
  cix := cix + 1;
  if cix > cixmax then begin
    if curproc = nil then
      errorwithtext(356,'MAIN      ')
    else
      errorwithtext(356, curproc^.name);
    cix := 0
  end;
  with code, halfword[cix] do begin
    lefthalf := flefth;
    righthalf := frighth;
    information[cix] := 'W';
    relocation[cix] := frelbyte
  end;
  ic := ic + 1
end (*FULLWORD*);

$PAGE insertaddr
public procedure insertaddr(frelbyte: relbyte; fcix:coderange;fic:addrrange);

begin
  if not errorflag then
    with code do begin
      instruction[fcix].address := fic;
      relocation[fcix] := frelbyte
    end
end;

$PAGE depcst
public procedure depcst (konsttyp: cstclass; var fattr:attr) options dump(ifm,final);

var
    ii:integer;
    lksp,llksp: ksp;
    lcsp: csp;
    offset: 0..2;
    notdmove: boolean;
    neuekonstante,gleich:boolean;
    lcix: coderange;

begin
  offset := 0;
  notdmove := code.instruction[cix].instr <> 120b (*DMOVE*);
  if konsttyp in [strg, strd] then
    if fattr.typtr^.skind = nonvarying then
      offset := 1;
      (* WE ASSUME THAT STRING CONSTANTS WILL BE VARYING. THAT
	 IS HAVE A LENGTH WORD.  WHEN A FIXED STRING IS REF'ED
	 WE ACCESS THE STRING PART AT OFFSET ONE.  IF NO BACK
	 LINK IS CREATED FOR THE LENGTH WORD (I.E NO VARYING
	 REFERENCES OCCUR), THEN NO LENGTH WORD IS EMITTED     *)
  neuekonstante:=true;
  lksp := firstkonst;
  while (lksp<>nil) andif neuekonstante do
    with lksp^,constptr^ do begin
      if (cclass = konsttyp) orif ((konsttyp in [strg,strd]) andif
	(cclass in [strg,strd])) then
	  case konsttyp of
	    reel:
	      if rval = fattr.cval.valp^.rval then
		neuekonstante := false;
	    int:
	      if intval = fattr.cval.ival then
		neuekonstante := false;
	    pset:
	      if pval = fattr.cval.valp^.pval then
		neuekonstante := false;
	    strd, strg:
	      if fattr.cval.valp^.slgth = slgth then begin
		gleich := true;
		ii := 1;
		while (ii <= slgth) andif (ii <= fattr.cval.valp^.slgth) andif
		  gleich do begin
		    if fattr.cval.valp^.sval[ii] <> sval[ii] then
		      gleich := false;
		    ii:=ii+1
		  end;
		if gleich then
		  neuekonstante := false
	      end
	  end (*CASE*);
      llksp := lksp;
      lksp := nextkonst
    end (*WHILE*);
  if not neuekonstante then
    with llksp^ do begin
      if addr[offset] = 0 then
	insertaddr (no,cix,0)
      else
	insertaddr(right,cix,addr[offset]);
      addr[offset] := ic-1;
      code.information[cix] := 'C';
      if notdmove andif (konsttyp in [pset, strd]) then begin
	if addr[offset+1] = 0 then
	  insertaddr(no,cix-1,0)
	else
	  insertaddr(right,cix-1,addr[offset+1]);
	addr[offset+1] := ic-2;
	code.information [cix-1] := 'C';
      end
    end
  else begin
    if konsttyp = int then begin
      new(lcsp,int);
      lcsp^.intval := fattr.cval.ival
    end
    else
      lcsp := fattr.cval.valp;
    code.information[cix] := 'C';
    insertaddr(no,cix,0);			(*JUST IN CASE *)
    if (konsttyp in [pset, strd]) and notdmove then
      code.information[cix-1] := 'C';
    new(lksp);
    with lksp^ do begin
      for ii := 0 to 2 do
	addr[ii] := 0;
      addr[offset] := ic - 1;
      if notdmove andif (konsttyp in [pset,strd]) then
	addr[offset+1] := ic - 2;
      constptr := lcsp;
      nextkonst := nil
    end;
    if firstkonst = nil then
      firstkonst := lksp
    else
      llksp^.nextkonst := lksp
  end
end (*DEPCST*);

$PAGE support
public procedure support(f_support: supports);
var fsupport: supports;

var
    inst: instrange;
    register: acrange;

begin
  fsupport := f_support;
  case fsupport of
    pointererror, errorinassignment, indexerror, offsetoverflow, v_nil_word : begin
      register := hac;
      inst := 265b (*jsp*);
    end;
    v_nil_offset: begin
      inst := 316b (*camn*);
      register := hac;
    end;
    stackoverflow : begin
      inst := 327b (*JUMPG*);
      register := topp;
    end;
    stmtblock : begin
      register := tac;
      inst := 265b (*jsp*);
    end;
    procentry : begin
      register := tac;
      inst := 265b (*jsp*);
      if not dmove then
	fsupport := ka10entry
      else if not kl10sw then
	fsupport := ki10entry
    end;
    procreturn, exitprogram : begin
      register := hac;
      inst := 254b (*JRST*);
      if fsupport = procreturn then
	if not dmove then
	  fsupport := ka10exit
	else if not kl10sw then
	  fsupport := ki10exit
    end;
    initprogram : begin
      register := paroverfreg;
      inst := 265b (*jsp*);
    end;
    others : begin
      inst := 260b (*pushj*);
      register := topp
    end
  end;
  mac3b(inst,register,rnts.link[fsupport]);
  code.information[cix]:= 'E';
  rnts.link[fsupport]:= ic-1
end;

$PAGE fwdfileref, fwdpageref
public procedure fwdfileref;

var
    lab: labp;

begin
  with filedata[inclnest] do
    if fwdfile <> fwdpage then begin
      lab := dcllabel (-1, standard);
      with lab^ do begin
	defined := true;
	labchain := fwdfile;
	labaddr := fwdpage
      end;
      fwdpage := fwdfile;
    end;
end;

public procedure fwdpageref;

var
    lab: labp;

begin
  with filedata[inclnest] do
    if fwdpage <> laststmt then begin
      lab := dcllabel (-1, standard);
      with lab^ do begin
	defined := true;
	labchain := fwdpage;
	labaddr := laststmt
      end;
      laststmt := fwdpage;
    end;
end;

$PAGE emitstring, newfiler
public procedure emitstring (str: stringtype);

var
    i: integer;
    string10: packed record
      case boolean of
	true: (
	  left1: addrrange;
	  right1: addrrange;
	  left2: addrrange;
	  right2: addrrange);
	false: (
	  alf: packed array[1..10] of char)
    end;

begin
  with string10 do begin
    if length(str)>10 then
      alf := str[1:10]
    else
      alf[1:10] := str;
    fullword (no,left1,right1);
    fullword (no,left2,right2)
  end
end;

public procedure newfiler options dump (ifm,final);

var
    tempstring: string60;

begin
  mac3r (254b (*JRST*),0,ic+9);			(* branch around file and page info *)
  with filedata[inclnest] do begin
    if fileic = 0 then
      fullword (no,0,0)
    else
      fullword (left,fileic,0);
      (* file block backlink,,forward ptr to last page block *)
    fileic := ic - 1;
    fwdfile := fileic;
    fwdpage := fileic;
    fullword (left,400000b,fileid);		(* ptr to prog_block ptr,,file number *)
    if source = reading then
      tempstring := srcnam[filenum-1]
    else
      tempstring := inclname[inclnest];
      (* REMOVE DEVICE, IF ANY *)
    tempstring := substr (tempstring,index(tempstring,':')+1);
    (* STRIP OFF EXTENSION AND/OR PPN *)
    emitstring (substr(tempstring,1,search(tempstring,['.','[','('], length(
      tempstring)+1)-1));
  end
end;

$PAGE newpager, newliner
public procedure newpager;

var
    i : integer;
    ch : char;
    done : boolean;

begin
  with filedata[inclnest] do begin
    if fwdfile = 0 then
      newfiler
    else begin
      fwdpageref;
      mac3r (254b (*JRST*),0,ic+5);		(* branch around page info *)
    end;
    fullword (left,fwdpage,0);
    (* page block backlink,,forward ptr to last stmt block *)
    fwdpage := ic - 1;
    laststmt := fwdpage;
    fullword (no,0,pagecnt);
    lastpage := pagecnt;
    lastline := 0;
    i := 0;
    if lensubtitle > 0 then
      i := verify (subtitle[1:lensubtitle],lettersdigitsorleftarrow,lensubtitle+1)-1;
    if i = 0 then
      emitstring(' ')
    else
      emitstring (uppercase(subtitle[1:i]))
  end
end;

public procedure newliner;

begin
  with filedata[inclnest] do begin
    if pagecnt <> lastpage then
      newpager;
    if lastline <> localline then begin
      support (stmtblock);
      fullword (left,laststmt,localline);
      (* stmt block backlink,,line # *)
      laststmt := ic - 1;
      lastline := localline;
      stmt_indicator := '*';
    end
  end
end;

$PAGE adjsp
public procedure adjsp;				(*ADJUST STACK POINTER*)

begin
  if kl10sw then begin
    mac3(105b (*ADJSP*),topp,0);
    patchsize := cix
  end
  else begin
    mac3(201b (*MOVEI*),tac,0);
    patchsize := cix;
    mac3(507b (*HRLS*),0,tac);
    mac3(270b (*ADD*),topp,tac);
    support(stackoverflow)
  end
end;

$PAGE enterbody
public procedure enterbody;

var
    i: integer;
    lcp : ctp;
    lbtp: btp;
    offset: addrrange;

begin
  if xref andif (sy <> period) then begin
    if curproc <> nil then
      write (symfile,'*',curproc^.name)
    else
      write (symfile,'*',entry);
    if source = reading then
      writeln (symfile,' ',srcnam[filenum-1])
    else
      writeln (symfile,' ',inclname[inclnest]);
  end;
  lbtp := lastbtp;
  while lbtp <> nil do begin
    with lbtp^ do begin
      fieldcp^.fldaddr := ic;
      lbtp := last;
    end;
    ic := ic + 1
  end;
  if (debug or tracer) andif (main or (curproc <> nil)) then begin
    pfblock := ic;
    if curproc <> nil then
      emitstring (curproc^.name)
    else
      emitstring (entry);
    fullword (no,level-1,0);			(* LEVEL,,INDEX TO SYMBOL TABLE *)
    if progchain = 0 then
      fullword (no,0,0)
      (* ptr to "program block",,version # *)
    else
      fullword (left,progchain,0);
    progchain := ic - 1;
  end
  else
    pfblock := 0;
  if curproc <> nil then begin
    with curproc^ do begin
      if pflev > 1 then
	for i := maxlevel downto pflev+1 do
	  mac4(540b (*HRR*),basis,basis,1)
    end;
    pfstart := ic;
    (* SAVE CALLER'S BASIS IN 1(TOPP) *)
    (* TOPP-UPON-ENTRY BECOMES CALLEE'S BASIS *)
    support(procentry);
    if pfblock <> 0 then
      fullword (right,0,pfblock)
    else
      fullword (no,0,377777b);
      (* ALLOCATE ROOM ON STACK FOR CALLEE'S FRAME *)
    adjsp;
    if testpacked then begin
      mac4(541b (*HRRI*),tac,basis,lcpar);
      mac3(505b (*HRLI*),tac,-lc+lcpar);
      mac4(402b (*SETZM*),0,tac,0);
      mac3r(253b (*AOBJN*),tac,ic-1);
    end;
    with curproc^ do begin
      if klass = func then
	if idtype <> nil then
	  if idtype^.size > 2 then
	    mac4 (202b (*MOVEM*), hac, basis, retloc)
    end;
    regc := reginp1;
    offset := - curproc^.parmsize;
    lcp := curproc^.next;
    while lcp <> nil do
      with lcp^ do begin
	if klass in [vars,proc,func] then
	  if (idtype <> nil) orif (klass <> vars) then
	    if (klass <> vars) orif ((vkind=formal) orif (idtype^.size=1) orif
	      (vclass=spvalparmsc)) then	(*COPY PARAMETERS FROM REGISTERS INTO LOCAL CELLS*)
	      begin
		if regc <= parregcmax then begin
		  if klass = vars then
		    mac4 (202b (*MOVEM*),regc,basis,vaddr)
		  else
		    mac4(202b (*MOVEM*),regc,basis,pfaddr);
		  regc := regc + 1
		end
		else begin
		  mac4(200b (*MOVE*),tac,paroverfreg,offset);
		  offset := offset + 1;
		  if klass = vars then
		    mac4 (202b (*MOVEM*),tac,basis,vaddr)
		  else
		    mac4(202b (*MOVEM*),tac,basis,pfaddr)
		end
	      end
	      else
	      (* check IF IDTYPE^.SIZE=2 removed since any value parameter
		 of size > 2 is an SPVALPARMSC and is handled above *)
	      if regc < parregcmax then begin
		if dmove then
		  mac4(124b (*DMOVEM*),regc,basis,vaddr)
		else begin
		  mac4(202b (*MOVEM*),regc,basis,vaddr);
		  mac4(202b (*MOVEM*),regc+1,basis,vaddr+1)
		end;
		regc := regc+2
	      end
	      else begin
		regc := paroverfreg;		(* PATHOLOGICAL *)
		if dmove then begin
		  mac4(120b (*DMOVE*),hac,paroverfreg,offset);
		  mac4(124b (*DMOVEM*),hac,basis,vaddr)
		end
		else begin
		  mac4(505b (*HRLI*),tac,paroverfreg,offset);
		  mac4(541b (*HRRI*),tac,basis,vaddr);
		  mac4(251b (*BLT*),tac,basis,vaddr+1)
		end;
		offset := offset + 2
	      end;
	lcp := lcp^.next;
      end
  end
  else
    mainstart := ic
end (*ENTERBODY*);

$PAGE leavebody
public procedure leavebody;

var
    j,k : addrrange ;
    lksp: ksp ;

  procedure loadfileaddr (fid: ctp);

  begin
    with fid^ do begin
      mac3b (201b (*MOVEI*), regc, vaddr);
      vaddr := ic - 1;
      code.information[cix] := 'E'
    end
  end;

begin
  if debug then
    newliner;
  if curproc <> nil then begin
    support(procreturn)
  end
  else begin
    if main then begin
      support(exitprogram);
      startaddr := ic;
      mac3(255b (*JFCL*),0,0);			(*NO-OP TO ALLOW FOR CCL ENTRY*)
      (* INIT STACK, CORE, AND ENVIRONMENT *)
      mac3(201b (*MOVEI*),basis,corsiz);
      support(initprogram);
      if pfblock = 0 then
	fullword (no,0,377777b)
      else
	fullword (right,0,pfblock);
      adjsp;

      (* OPEN STANDARD FILES IF NECESSARY *)
      regc := reginp1;				(* MAY BE REQUIRED ELSEWHERE *)
      if ttyinuse or ininuse or outinuse then begin
	mac3 (403b (*SETZB*),reginp2,reginp3);
	mac3 (403b (*SETZB*),reginp4,reginp5);
	(* DEFAULT OPEN *)
	if outinuse then begin
	  loadfileaddr (outfptr);
	  support (rewritefile)
	end;
	if ininuse then begin
	  loadfileaddr (inputfptr);
	  support (resetfile);
	end;
	if ttyinuse then begin
	  loadfileaddr (toutfptr);
	  support (rewritefile);
	  support (opentty);			(* STAR PROMPT *)
	  loadfileaddr (ttyfptr);
	  support (resetfile)
	end
      end;
      mac3r(254b (*JRST*),0,mainstart);
    end;
  end;
  codeend := ic;
  lksp:= firstkonst;
  while lksp <> nil do
    with lksp^,constptr^ do begin
      kaddr:= ic;
      if addr[0] = 0 then
	kaddr := kaddr-1;			(* OMIT FIRST(LENGTH) WORD IF NOT USED *)
      case cclass of
	int (* $X4 , REEL *):
	  ic := ic + 1 ;
	REEL:
	  IC := IC + 1 + ORD(DBLREAL);	
	pset:
	  ic := ic + 2 ;
	strd, strg: begin
	  ic := ic + (slgth + 4) div 5;
	  if addr[0] <> 0 then
	    ic := ic + 1			(* LENGTH WORD *)
	end
      end ;
      (*CASE*)
      lksp := nextkonst
    end (*WITH , WHILE*);
  if level = 1 then
    highestcode := ic
end (*LEAVEBODY*);

$PAGE fetchbasis
public procedure fetchbasis(var fattr: attr);

const
    ac_field := 40b;	(* shift "regn" parameter to VTRNS./FTRNS. into ac field *)
var
    p,q: integer;
    convert: record
	case boolean of
	true: (int: integer);
	false: (supp: supports)
    end;

begin
  with fattr do
    if (vsclass = externalsc) and (vrelbyte = right)
    (* 2ND CHECK, KLUDGE TO AVOID RELOCING LOCALS *)
    then begin
    (* ASSUME THAT NEXT INSTRUCTION EMITTED WILL REFERENCE VALUE *)
      with vid^ do begin
	if vaddr = 0 then
	  vrelbyte := no
	else
	  vrelbyte := right;
	if (dplmt <> 0) orif (typtr <> nil) andif (typtr^.size > 1) then begin	(* CANNOT REF DIRECTLY, OFFSET INVOLVED *)
	  if indexr <> 0 then
	    mac5 (vrelbyte,201b (*MOVEI*),indexr,indexr,vaddr)
	  else begin
	    mac5 (vrelbyte,201b (*MOVEI*), tac, 0, vaddr);
	    indexr := tac
	  end;
	  vrelbyte := no;
	  vaddr := ic-1;
	end
	else begin				(* CAN REFERENCE STRAIGHT *)
	  dplmt := vaddr;			(* TO EMIT BACKLINK *)
	  vaddr := ic;				(* NEXT INST MAKES REF *)
	  indbit := ord (overlaysw);
	end
      end
    end
    else if vlevel>1 then begin
      p := level - vlevel;
      if p=0 then
	if indexr=0 then
	  indexr := basis
	else
	  mac4 (271b (*ADDI*),indexr,basis,0)
      else begin
	mac4(550b (*HRRZ*),tac,basis,1);
	for q := p downto 2 do
	  mac4(540b (*HRR*),tac,tac,1);
	if indexr=0 then
	  indexr := tac
	else
	  mac3(270b (*ADD*),indexr,tac)
      end;
      vlevel:=1					(*DA IN WITHSTATEMENT DIE MOEGLICHKEIT BESTEHT,
						 DASS ES 2-MAL DURCH FETCHBASIS LAEUFT*)
    end
    else if (vlevel = 0) then begin	(* have virtual ptr to translate *)
      if indexr > regcmax then begin	(* under WITH *)
	support (f_trans);
	if packfg = packk then
	  fullword (no,indexr*ac_field,vid^.fldbp.reladdr+dplmt)
	else fullword (no,indexr*ac_field,dplmt);
	indexr := tac;
      end
      else begin
	if dplmt <> 0 then begin
	  if dplmt > 0 then
	    mac5 (vrelbyte,271b(*addi*),indexr,0,dplmt)
	  else mac5 (vrelbyte,275b(*subi*),indexr,0,-dplmt);
	  dplmt := 0;
	  vrelbyte := no;
	end;
	if incore then begin
	  convert.int := ord (vtrans2) + indexr - 2;
	  support (convert.supp);
	end
	else begin
          support (v_trans);
          if typtr <> nil then
	    fullword (no,indexr*ac_field,typtr^.size);
	end;
      end;
      vlevel := 1;
    end
end;
(*FETCHBASIS*)

$PAGE getparaddr
public procedure getparaddr;

begin
  fetchbasis(gattr);
  with gattr do begin
    incrementregc;
    mac5(vrelbyte,200b (*MOVE*),regc,indexr,dplmt);
    indexr := regc;
    vrelbyte:= no;
    indbit := 0;
    vlevel := 1;
    dplmt := 0;
  end
end;

$PAGE makecode
public procedure makecode(finstr_: instrange; fac_: acrange; var fattr: attr);
var finstr: instrange; fac: acrange;

var
    linstr: instrange;
    lregc: acrange;
    ichars: addrrange;

begin
  finstr := finstr_; fac := fac_;
  with fattr do
    if typtr<>nil then begin
      case kind of
	cst:
	  if typtr=realptr then begin
	    IF NOT DBLREAL THEN	
	      mac3(finstr,fac,0)
	    ELSE IF FINSTR = 200B (*MOVE*) THEN
	      MAC3 (120B(*DMOVE*),FAC-1,0)
	    ELSE MAC3 (FINSTR,FAC-1,0) ;
	    depcst(reel,fattr)
	  end
	  else if typtr^.form=scalar then with cval do
	      if ((ival >= 0) andif (ival <= maxaddr)) orif (((abs(ival)
		<= hwcstmax+1) andif (ival <> min8)) andif
		  ((finstr = 200b (*MOVE*)) orif (ival >= 0))) then begin
		    if finstr=200b		(*MOVE*)
		    then
		      if ival < 0 then
			finstr := 571b		(*HRREI*)
		      else
			finstr := 551b		(*HRRZI*)
		    else if (finstr>=311b) andif (finstr <= 317b) then
		      finstr := finstr - 10b	(*E.G. CAML --> CAIL*)
		    else
		      finstr := finstr+1;
		    mac3(finstr,fac,ival);
		  end
		  else begin
		    mac3(finstr,fac,0);
		    depcst(int,fattr)
		  end
	  else if typtr=nilptr then begin
	   if virtual then begin
	    if finstr = 200b(*move*) then
	      mac3 (474b(*seto*),fac,0)
	    else begin
	      mac3b (finstr,fac,v_nil);
	      v_nil := ic - 1;
	    end
	   end
	   else begin
	    if finstr=200b			(*MOVE*)
	    then
	      finstr := 571b			(*HRREI*)
	    else if (finstr>=311b) and (finstr<=317b) then
	      finstr := finstr-10b
	    else
	      finstr := finstr+1;
	    mac3(finstr,fac,377777b);
	   end;
	  end
	  else if typtr^.form=power then begin
	    if (finstr=200b (*MOVE*)) andif dmove then
	      mac3(120b (*DMOVE*),fac-1,0)
	    else begin
	      mac3(finstr,fac,0);
	      mac3(finstr,fac-1,0)
	    end;
	    depcst (pset,fattr)
	  end
	  else if typtr^.form=strings then
	    if (finstr = 200b) andif (cval.valp^.slgth <= 2) then begin
	      ichars := 0;			(* BUILD HWORD WITH IMMEDIATE CHARS *)
	      with cval.valp^ do begin
		if slgth >= 1 then
		  ichars := ord (sval[1]) * 4000b;
		if slgth = 2 then
		  ichars := ord (sval[2]) * 20b + ichars;
		if typtr^.skind = varying then begin
		  if slgth = 0 then
		    mac3 (400b (*SETZ*), fac-1, 0)
		  else
		    mac3 (551b (*HRRZI*), fac-1, slgth);
		end;
		mac3 (515b (* HRLZI *), fac, ichars)
	      end
	    end
	    else if typtr^.size = 1 then begin
	      mac3(finstr,fac,0);
	      depcst(strg,fattr)
	    end
	    else if typtr^.size = 2 then begin
	      if (finstr=200b (*MOVE*)) andif dmove then
		mac3(120b (*DMOVE*),fac-1,0)
	      else begin
		mac3(finstr,fac,0);
		mac3(finstr,fac-1,0)
	      end;
	      depcst (strd,fattr)
	    end;
	varbl: begin
	  fetchbasis(fattr);
	  lregc := fac;
	  if (indexr>regin) andif (indexr<=regcmax) andif ((packfg<>notpack)
	    orif (finstr=200b (*MOVE*))) then
	      if (typtr^.size = 2) andif loadnoptr then
		lregc := indexr+1
	      else
		lregc := indexr
	    else if (packfg<>notpack) andif (finstr<>200b (*MOVE*)) then begin
	      incrementregc;
	      lregc := regc
	    end;
	  case packfg of
	    notpack: begin
	      if (typtr^.size = 2) andif loadnoptr then
		if (finstr=200b (*MOVE*)) andif dmove then
		  mac5(vrelbyte,120b (*DMOVE*),lregc-1,indexr,dplmt)
		else begin
		  if (finstr>113b(*DFDV*)) orif (finstr<110b(*DFAD*)) then
		    mac5(vrelbyte,finstr,lregc,indexr,dplmt+1);
		  mac5(vrelbyte,finstr,lregc-1,indexr,dplmt)
		end
	      else
		mac(vrelbyte,finstr,lregc,indbit,indexr,dplmt);
	    end;
	    packk, packr: begin
	      if packfg = packk then begin
		if (indexr <> tac) orif (dplmt <> 0) then
		  mac5(vrelbyte,201b (*MOVEI*),tac,indexr,dplmt);
	      end;
	      if (bpaddr>regin) andif (bpaddr<=regcmax) then
		if (indexr<=regin) orif (bpaddr<indexr) then
		  lregc := bpaddr
		else
		  lregc := indexr;
	      if packfg = packk then
		mac3b(135b (*LDB*),lregc,bpaddr)
	      else
		mac3b (134b (*ILDB*), lregc, bpaddr);
	    end;
	    hwordl:
	      mac5(vrelbyte,554b (*HLRZ*),lregc,indexr,dplmt);
	    hwordr:
	      mac5(vrelbyte,550b (*HRRZ*),lregc,indexr,dplmt)
	  end (*CASE*);
	  if (finstr<>200b (*MOVE*)) andif (packfg<>notpack) then
	    mac3(finstr,fac,lregc)
	  else
	    fac := lregc
	end;
	expr:
	  if finstr<>200b			(*MOVE*)
	  then
	    if typtr^.size = 2 then begin
	      if (finstr>113b(*DFDV*)) orif (finstr<110b(*DFAD*)) then mac3(finstr,fac,reg);
	      mac3(finstr,fac-1,reg-1)
	    end
	    else
	      mac3(finstr,fac,reg)
      end (*CASE*);
      kind := expr;
      reg := fac;
    end;
end;

$PAGE vload

public procedure vload (var fattr: attr);

var instr: instrange;
    propogate_areaid: boolean;

begin
  with fattr do begin
    incrementregc;
    propogate_areaid := indexr <> 0;
    fetchbasis (fattr);
    if packfg = notpack then instr := 200b(*move*)
    else if packfg = hwordl then instr := 554b(*hlrz*)
    else instr := 550b(*hrrz*);
    mac (vrelbyte,instr,hac,indbit,indexr,dplmt);
    if (indexr > regin) andif (indexr <= regcmax) then
      regc := indexr
    else if propogate_areaid then
      mac3 (200b(*move*),regc,indexr);
    if propogate_areaid then begin
      mac3b (630b(*tdz*),regc,mask_offset);
      mask_offset := ic-1;
    end;
    if packfg = notpack then
      support (v_nil_offset)
    else mac3 (306b(*cain*),hac,777777b);
    support (v_nil_word);
    if propogate_areaid then
      mac3 (270b(*add*),regc,hac)
    else mac3 (200b(*move*),regc,hac);
    kind := expr;
    reg := regc;
  end;
end;
$PAGE load
public procedure load(var fattr: attr);

begin
  with fattr do
    if typtr<>nil then
      if kind<>expr then begin
	if (typtr^.form = pointer) andif (typtr^.v_offset) then begin
	  vload (fattr);
	end
	else begin
	  incrementregc;
	  if loadnoptr andif (typtr^.size = 2) then
	    incrementregc ;
	  makecode(200b (*MOVE*),regc,fattr);
	regc := reg
	end;
      end;
end;
(*LOAD*)

$PAGE store
public procedure store(fac_: acrange; var fattr: attr);
var fac: acrange;

var
    lattr: attr;

begin
  fac := fac_;
  lattr := fattr;
  with lattr do
    if typtr <> nil then begin
      fetchbasis(lattr);
      case packfg of
	notpack: begin
	  if (typtr^.size = 2) andif dmove then
	    mac(vrelbyte,124b (*DMOVEM*),fac-1,indbit,indexr,dplmt)
	  else begin
	    if typtr^.size = 2 then begin
	      mac5(vrelbyte,202b (*MOVEM*),fac,indexr,dplmt+1);
	      fac := fac-1
	    end;
	    mac(vrelbyte,202b (*MOVEM*),fac,indbit,indexr,dplmt)
	  end
	end;
	packk, packr: begin
	  if packfg = packk then begin
	    if (indexr <> tac) orif (dplmt <> 0) then
	      mac5(vrelbyte,201b (*MOVEI*),tac,indexr,dplmt);
	    mac3b(137b (*DPB*),fac,bpaddr)
	  end
	  else
	    mac3b (136b (*IDPB*),fac,bpaddr);
	end;
	hwordl:
	  mac5(vrelbyte,506b (*HRLM*),fac,indexr,dplmt);
	hwordr:
	  mac5(vrelbyte,542b (*HRRM*),fac,indexr,dplmt)
      end (*CASE*);
    end (*WITH*);
end (*STORE*);

$PAGE loadaddress
public procedure loadaddress;

begin
  incrementregc ;
  begin
    with gattr do
      if typtr <> nil then begin
	case kind of
	  cst:
	    if strform(typtr) then begin
	      mac3(201b (*MOVEI*),regc,0);
	      depcst(strg,gattr)
	    end
	    else
	      error(171);
	  varbl: begin
	    if (indexr>regin) andif (indexr <= regcmax) then
	      regc := indexr;
	    fetchbasis(gattr);
	    case packfg of
	      notpack: begin
		mac(vrelbyte,201b (*MOVEI*),regc,indbit,indexr,dplmt);
	      end;
	      packk,packr,hwordl,hwordr:
		error(357)
	    end;
	  end;
	  expr:
(*	    error(171) *)
	end;
	kind := varbl;
	dplmt := 0;
	indexr:=regc;
	indbit:=0;
	vrelbyte := no;
	vlevel := 1
      end
  end
end (*LOADADDRESS*);
$PAGE ldaddress

public  procedure ldaddress;			(* LOADS POSSIBLY VIRTUAL POINTER *)
  begin
    with gattr do begin
      if (kind = varbl) andif (vlevel = 0) then begin
	incrementregc;
	  if (indexr > regin) andif (indexr <= regcmax)
	    then regc := indexr
	    else mac3 (200b (*MOVE*), regc, indexr);
	  if dplmt <> 0 then mac3 (271b (*ADDI*), regc, dplmt);
	dplmt := 0;
	indexr := regc;
	indbit := 0;
	vrelbyte := no;
	vlevel := 1;
      end
      else loadaddress;
    end
  end.
    ST3hm