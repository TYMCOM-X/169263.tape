$title pascsn - PASCAL compiler structure constant/variable initialization
$OPTIONS TRACE SPECIAL NOCHECK

(*	Conditional compilations switches used:

	4:  Allow double precision reals unless disabled *)

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASLEX.INC

$INCLUDE PASEXP.INC

$INCLUDE PASCMP.INC

$INCLUDE PASMC.INC
$PAGE declarations

external function left_shift (integer; bitrange): integer;

static var
    base_address: addrrange;
    max_ix: -1..cixmax;
    cst_value: valu;
    cst_ptr: stp;
    fsys: set of symbol;

$PAGE cst_list
procedure cst_list (ix: coderange;		(* index into CODE table *)
c_type: stp;					(* type pointer *)
packing: packkind;				(* determines packing of fields and arrays *)
bits: bitrange);				(* if packing=PACKK bits is the rightmost bit of the field *)

var
    smax, i, mini, maxi: integer;
    variant_ptr: stp;
    var_ptr, field_ptr: ctp;
    reloc: no..both;
    cst2_value: valu;
    oldic: addrrange;
    ptrfunction: boolean;

  procedure set_field (inx: coderange; field: ctp);
  (* simplifies recursion for record fields *)

  label
      1;

  begin
    1:
      if field <> nil then
	with field^ do begin
	  if packf=packk then
	    cst_list (inx+fldbp.reladdr,idtype,packk,fldbp.pbits)
	  else
	    cst_list (inx+fldaddr,idtype,packf,bitmax);
	  if next <> nil then begin
	    if sy = comma then
	      insymbol
	    else
	      error (158);
	    field := next;
	    goto 1;
	  end;
	end;
  end;

$PAGE put_cst - in cst_list
  procedure put_cst (inx: coderange; val: valu; cst_type: cstclass; str_len: integer);
  (* places constants in the CODE table according to their type *)

(*$y4 label 1;	*)

  var
      cst_rec: packed record
	case boolean of
	  true: (
	    cst_int: integer);	(* overlays 5 characters *)
	  false: (
	    cst_str: packed array[0..4] of char)
      end;
      offset, start: integer;
      i, j, k: integer;

  begin						(* put_cst *)
    if errorflag then
      offset := 0
    else with code do
      if cst_type = int then begin
	  offset := 0;
	  case packing of
	    notpack:
	      word[inx] := val.ival;
	    hwordr:
	      halfword[inx].righthalf := val.ival;
	    hwordl:
	      halfword[inx].lefthalf := val.ival;
	    packk:
	      word[inx] := left_shift (val.ival,bits) + word[inx]
	  end;					(* case packing *)
      end
      else with val.valp^ do
      case cst_type of
	reel: begin
	  word[inx] := intval;			(* actually rval *)
(*$y4	  if dblreal then goto 1;	*)
	  offset := 0;
	end;
	pset: begin
	  word[inx] := intval;
(*$y4	1:	*)
	  offset := 1;
	  word[inx+1] := intval1;
	end;
	strd, strg: begin
	(* if cst_type = strd no length word is emitted *)
	  offset := (slgth + 4) div 5;
	  if cst_type = strg then begin
	    word[inx] := str_len;
	    start := inx;
	  end
	  else begin
	    start := inx - 1;
	    offset := offset - 1;
	  end;
	  for i := 1 to (str_len + 4) div 5 do begin
	    j := (i-1)*5+1;	(* string index at start of word *)
	    for k := 0 to 4 do begin
	      if j+k > slgth then	(* if no string left *)
		cst_rec.cst_str[k] := ' '	(* pad with blanks *)
	      else cst_rec.cst_str[k] := sval[j+k];
	    end;
	    word[start+i] := cst_rec.cst_int;
	  end;
	end
      end;					(* case cst_type *)
    if inx + offset > max_ix then
      max_ix := inx + offset;
  end;						(* put_cst *)

$PAGE multi_array - in cst_list
  procedure multi_array (index: coderange; array_type: stp);
  (* recurses through itself for multiply dimensioned arrays
     and through cst_list for each element in an array *)

  var
      i, imax, mini, maxi: integer;
      cst_value: valu;
      noparent: boolean;

  begin
    if array_type <> nil then
      with array_type^ do
	if (aeltype <> nil) andif (inxtype <> nil) then begin
	  if cix+size > max_ix then
	    max_ix := cix + size;
	  getbounds (inxtype,mini,maxi);
	  imax := maxi - mini;			(* number of elements - 1 *)
	  if string (array_type) and arraypf andif (sy=stringconstant)
	    then begin
	    (* packed arrays of char can be initialize as strings *)
	      constant (fsys or [comma,rparent,semicolon],cst_ptr,cst_value);
	      if cst_ptr = charptr then begin
		(* convert single character to string *)
		new (cst2_value.valp,strd:1);
		with cst2_value.valp^ do begin
		    sval[1] := chr (cst_value.ival);
		    slgth := 1;
		  end;
		cst_value.valp := cst2_value.valp;
	      end;
	      put_cst (index, cst_value, strd, imax+1);	(* will not emit a length word *)
	    end					(* if string *)
	  else begin
	    if sy = lparent then begin
	      insymbol;
	      noparent := false;
	    end
	    else
	      noparent := true;
	    for i := 0 to imax do begin
	      if (aeltype^.form = arrays) then
	      (* recurse to initialize elements in row-major order *)
		multi_array (index+i*aeltype^.size,aeltype)
	      else if arraypf then
		cst_list (index+(i div (bitmax div aeltype^.bitsize))
		  ,aeltype,packk, bitmax-(1+ (i mod (bitmax div aeltype^.bitsize
		    ))) *aeltype^.bitsize)
	      else
		cst_list (index+i*aeltype^.size,aeltype,notpack,bitmax);
	    exit if not (sy in [comma,rparent,semicolon]) do
		if i <> imax then
		  error (414);			(* missing or too many array elements *)
	      if i < imax then
		if sy=comma then
		  insymbol
		else exit if true do
		  errandskip (158,fsys or [comma,rparent,semicolon]);
	    end;				(* for each element *)
	    if not noparent then
	      if sy = rparent then
		insymbol
	      else
		error (152);
	  end					(* if not string *)
	end					(* if array_type <> nil *)
  end;						(* multi_array *)

$PAGE cst_list - body
begin						(* cst_list *)
  if c_type <> nil then
    with c_type^ do begin
      case form of
	scalar: begin
	  constant (fsys or [comma,rparent,semicolon],cst_ptr,cst_value);
	  if scalkind = standard then begin
	    if cst_ptr <> nil then begin
	      if c_type = intptr then begin
		if comptypes (intptr, cst_ptr) then
		  put_cst (ix,cst_value,int,0)
		else
		  error (255)			(* integer expected *)
	      end
	      else if c_type = charptr then begin
		if comptypes (cst_ptr,charptr) then
		  put_cst (ix,cst_value,int,0)
		else
		  error (177)			(* char expected *)
	      end
	      else begin			(* must be realptr *)
	      (* convert integer constants to real if necessary *)
		if comptypes (cst_ptr,intptr) then begin
		  new (cst2_value.valp,reel);
		  cst2_value.valp^.rval := cst_value.ival;
		  (* Assume rval2 zeroed by NEW *)
		  cst_value := cst2_value;
		end;
		put_cst (ix,cst_value,reel,0);
	      end
	    end
	  end
	  else if fconst <> nil then
	    if comptypes (cst_ptr, fconst^.idtype) andif (cst_value.ival >=0)
	      andif (cst_value.ival <= fconst^.values.ival) then
		put_cst (ix,cst_value,int,0)
	      else
		error (260)			(* type conflict *)
	end;					(* case form = scalar *)
	subrange: begin
	  constant (fsys or [comma,rparent,semicolon],cst_ptr,cst_value);
	  if (cst_ptr <> nil) andif (rangetype <> nil) then
	    if comptypes (cst_ptr,rangetype) then begin
	      getbounds (c_type, mini, maxi);
	      if (cst_value.ival >= mini) andif (cst_value.ival <= maxi) then
		put_cst (ix,cst_value,int,0)
	      else
		error (367)			(* out of subrange *)
	    end
	    else
	      error (260)			(* type conflict *)
	end;					(* case form = subrange *)
	pointer: begin
	  if sy = arrow then begin
	  (* eventually should allow full pointer initialization here
	     for the moment allow pointers only to already defined variables
	     and error if external and packing is in left halfword ==>
	     can't presently emit left backlink chains *)
	    if not specialsw then
	      error (416);
	    insymbol;
	    if sy <> ident then
	      error (209)
	    else begin
	      if not (packing in [notpack,hwordl,hwordr]) then
		error (561);
	      srchid ([vars],var_ptr);
	      with var_ptr^ do
		if idtype <> nil then begin
		  if not comptypes (eltype,idtype) then
		    error (260);
		  case vclass of
		    publicsc, staticsc: begin
		      cst_value.ival := vaddr;
		      if packing = hwordl then
			reloc := left
		      else
			reloc := right;
		    end;
		    externalsc: begin
		      if packing = hwordl then
			error (563);
		      cst_value.ival := vaddr;
		      if vaddr = 0 then
			reloc := no
		      else
			reloc := right;
		      vaddr := base_address + ix;
		    end;
		    others:
		      error (562)
		  end;
		  code.relocation[ix] := code.relocation[ix] + reloc;
		  put_cst (ix,cst_value,int,0);
		end;				(* with *)
	      insymbol;
	    end;				(* if ident *)
	  end					(* if arrow *)
	  else begin
	    ptrfunction := id = 'PTR       ';
	    constant (fsys or [comma,rparent,semicolon],cst_ptr,cst_value);
	    if cst_ptr <> nil then
	      if (cst_ptr = nilptr) or ptrfunction then
		put_cst (ix,cst_value,int,0)
	      else
		error (416)			(* must be NIL *)
	  end					(* if sy # arrow *)
	end;
	(* case form = pointer *)
	power: begin
	  oldic := ic;				(* to determine if code emitted by expression *)
	  regc := regin;
	  regcmax := within;			(* these may not have been initialized *)
	  expression (fsys or [comma,rparent,semicolon],onregc);
	  if oldic = ic then begin
	    if gattr.typtr <> nil then
	      if (gattr.kind=cst) andif (gattr.typtr^.form=power) then
		put_cst (ix,gattr.cval,pset,0)
	      else
		error (178)			(* set expected *)
	  end
	  else
	    error (178)				(* constant set only *)
	end;					(* case form = power *)
	arrays: begin
	  multi_array (ix,c_type);
	end;					(* case form = arrays *)
	records: begin
	  if sy = lparent then
	    insymbol
	  else
	    error (153);
	  if fieldfg then
	    set_field (ix,fstfld);
	  if (recvar <> nil) andif (recvar^.form in [tagfwithid, tagfwithoutid])
	    then begin
	      if (fstfld <> nil) and fieldfg then
		if sy = comma then
		  insymbol
		else
		  error (158);
	      cst_list (ix,recvar,notpack,bitmax);
	      (* gets tagfield and variants *)
	    end;
	  if sy = rparent then
	    insymbol
	  else
	    error (152);
	end;					(* case form = records *)
	files:
	  errandskip (224,fsys or [semicolon]);
	tagfwithid, tagfwithoutid: begin
	  if form = tagfwithid then
	    set_field (ix,tagfieldp)
	  else begin
	  (* compiler may want to do this  *)
	    if not specialsw then
	      error (415);			(* cannot initialize undescriminated unions *)
	    constant (fsys or [comma,rparent,semicolon],cst_ptr,cst_value);
	  end;
	  if form = tagfwithid then begin
	    if not comptypes (tagfieldp^.idtype,cst_ptr) then
	      error (305)
	  end
	  else if not comptypes (tagfieldtype,cst_ptr) then
	    error (305);
	  variant_ptr := fstvar;
	  while variant_ptr <> nil do begin
	  exit if variant_ptr^.varval.ival = cst_value.ival;
	    variant_ptr := variant_ptr^.nxtvar;
	  end;
	  if (variant_ptr <> nil) andif ((variant_ptr^.firstfield <> nil) orif (variant_ptr^.subvar <> nil)) then begin
	    if sy = comma then
	      insymbol
	    else
	      error (158);
	    set_field (ix,variant_ptr^.firstfield);
	    if (variant_ptr^.subvar <> nil) andif
	      (variant_ptr^.subvar^.form in [tagfwithid, tagfwithoutid])
		then begin
		  if variant_ptr^.firstfield <> nil then
		    if sy = comma then
		      insymbol
		    else
		      error (158);
		  cst_list (ix,variant_ptr^.subvar,notpack,bitmax);
		end;
	  end;
	end;					(* case form = tagf.. *)
	strings: begin
	  constant (fsys or [comma, rparent, semicolon], cst_ptr, cst_value);
	  if cst_ptr <> nil then
	    if strform (cst_ptr) then begin
	      if cst_ptr = charptr then begin
	      (* char read, make it a string *)
		new (cst2_value.valp,strg:1);
		cst2_value.valp^.slgth := 1;
		cst2_value.valp^.sval[1] := chr (cst_value.ival);
		cst_value := cst2_value;
	      end;
	      if skind = varying then
		put_cst (ix,cst_value,strg,cst_value.valp^.slgth)
	      else
		put_cst (ix,cst_value,strd,maxlength);
	    end
	    else
	      error (267);			(* string expected *)
	end;					(* case form = strings *)
	formalprocfunc:
	  errandskip (224,fsys or [semicolon])	(* may not be initialized *)
      end					(* case form *)
    end;
end;						(* cst_list *)

$PAGE var_list
public procedure var_list (class: storageclass; fsy: setofsys; var_ptr: ctp);

var
    ix: -1..cixmax;

begin
  if var_ptr^.idtype <> nil then
    with var_ptr^ do begin
      fsys := fsy;
      base_address := vaddr;
      if class in [staticsc,publicsc] then begin
	if cix+idtype^.size > cixmax then begin
	  error (356);
	  cix := -1;
	end
	else with code do
	  for ix := cix+1 to idtype^.size+cix do begin
	    word[ix] := 0;
	    relocation[ix] := 0;
	    information[ix] := 'W';
	  end;
	cst_list (cix+1,idtype,notpack,bitmax);
	cix := cix + idtype^.size;
	(* for variables reserve full size even if variants selected were smaller *)
	codeend := statlc;
	writemc (writeconstants);
      end
      else
	errandskip (560,fsy or [semicolon]);
    end
end;						(* var_list *)

$PAGE cst_declaration
public procedure cst_declaration (fsy: setofsys; class: storageclass);

var
    ix: -1..cixmax;
    type_ptr: stp;
    type_ctp: ctp;
    var_ptr: ctp;
    cst_value: valu;
    next_ctp: ctp;
    save_id: alfa;

begin
  fsys := fsy;					(* temporary *)
  if not (class in [staticsc,localsc,publicsc,externalsc]) then
    error (318);
  skipiferr ([ident],209,fsy);
  while sy = ident do begin
    next_ctp := nil;
    if class = externalsc then begin
      loop
	if sy = ident then begin
	  new (var_ptr,vars);
	  var_ptr^.name := id;
	  var_ptr^.next := next_ctp;
	  next_ctp := var_ptr;
	  insymbol;
	end
	else
	  error (209);				(* identifier expected *)
	skipiferr (fsys or [comma,colon],166,[semicolon]);
      exit if sy <> comma;
	insymbol;
      end;
    end						(* if class = externalsc *)
    else begin
    (* right context check to determine if structured constant
       ==> default to static in high segment *)
      save_id := id;
      insymbol;
    end;
    if (sy = becomes) orif ((sy = relop) andif (op = eqop)) then begin
      if class <> localsc then
	error (558);				(* structured constants must be declared with type *)
      new (var_ptr,konst);
      insymbol;
      constant (fsys or [semicolon],type_ptr,cst_value);
      with var_ptr^ do begin
	name := save_id;
	idtype := type_ptr;
	next := nil;
	selfctp := nil;
	nocode := false;
	values := cst_value;
      end;
      enterid (var_ptr);
    end
    else if sy = colon then begin
      if class <> externalsc then begin
	new (next_ctp,vars);
	next_ctp^.name := save_id;
	next_ctp^.next := nil;
      end;
      insymbol;
      srchid ([types],type_ctp);
      type_ptr := type_ctp^.idtype;
      while next_ctp <> nil do
	with next_ctp^ do begin
	  idtype := type_ptr;
	  selfctp := nil;
	  nocode := false;
	  vconst := true;
	  vkind := actual;
	  if class = localsc then
	    vclass := staticsc
	  else
	    vclass := class;
	  vlev := level;
	  if type_ptr <> nil then
	    case class of
	      externalsc:
		vaddr := 0;
	      staticsc: begin
		vaddr := statlc;
		base_address := statlc;
	      end;
	      localsc, publicsc: begin
		vaddr := ic;
		base_address := ic;
	      end
	    end;				(* case *)
	  enterid (next_ctp);
	  next_ctp := next;
	end;					(* with *)
      insymbol;
      if class <> externalsc then begin
	if (sy=becomes) orif ((sy=relop) andif (op=eqop)) then begin
	  insymbol;
	  if type_ptr = nil then
	    while not (sy in (fsys or [semicolon])) do
	      insymbol
	  else
	    with code do begin
	      if cix+type_ptr^.size > cixmax then begin
		error (356);
		cix := -1;
	      end
	      else for ix := cix+1 to cix+type_ptr^.size do begin
		word[ix] := 0;
		relocation[ix] := no;
		(* may be changed in cst_list for pointers *)
		information[ix] := 'W';
	      end;
	    end;
	  max_ix := -1;
	  cst_list (cix+1,type_ptr,notpack,bitmax);
	  (* normally cix := cix + typtr^.size, however for constants
	     allocate only storage required --> variants may be smaller *)
	  if class = staticsc then
	    statlc := statlc + max_ix - cix
	  else
	    ic := ic + max_ix - cix;
	  cix := max_ix;
	end
	else
	  errandskip (564,fsys or [semicolon]);	(* must be initialized *)
      end					(* if not externalsc *)
    end						(* if sy = colon *)
    else
      errandskip (166,fsys or [semicolon]);
    if sy = semicolon then begin
      insymbol;
      iferrskip (166,fsys or [ident]);
    end
    else
      error (156)				(* semicolon expected *)
  end;						(* while sy = ident *)
  if cix >= 0 then begin
  (* structured constants emitted, dump them *)
    if class = staticsc then
      codeend := statlc
    else
      codeend := ic;
    writemc (writeconstants);
  end;
end.						(* constantdeclaration *)
