$TITLE pascmp - PASCAL compiler type comparison routines
$OPTIONS SPECIAL, NOCHECK

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASLEX.INC

$PAGE comptypes
public function comptypes(fsp1,fsp2: stp) : boolean;
(*DECIDE WHETHER STRUCTURES POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE*)

var
    nxt1,nxt2: ctp;
    comp: boolean;
    lmin,lmax,i: integer;
    ltestp1,ltestp2: testp;

begin
  if fsp1 = fsp2 then
    comptypes := true
  else if (fsp1 <> nil) andif (fsp2 <> nil) then
    if fsp1^.form = fsp2^.form then
      case fsp1^.form of
	scalar:
	  comptypes := false;
	  (* IDENTICAL SCALARS DECLARED ON DIFFERENT LEVELS ARE
	   NOT RECOGNIZED TO BE COMPATIBLE*)
	subrange:
	  comptypes := comptypes(fsp1^.rangetype,fsp2^.rangetype);
	pointer:
	  if (fsp1=arbptrtyp)orif(fsp2=arbptrtyp) then
	    comptypes:= true
	  else begin
	    comp := false;
	    ltestp1 := globtestp;
	    ltestp2 := globtestp;
	    while ltestp1 <> nil do
	      with ltestp1^ do begin
		if (elt1 = fsp1^.eltype) andif (elt2 = fsp2^.eltype) then
		  comp := true;
		ltestp1 := lasttestp
	      end;
	    if not comp then begin
	      new(ltestp1);
	      with ltestp1^ do begin
		elt1 := fsp1^.eltype;
		elt2 := fsp2^.eltype;
		lasttestp := globtestp
	      end;
	      globtestp := ltestp1;
	      comp := comptypes(fsp1^.eltype,fsp2^.eltype)
	    end;
	    comptypes := comp;
	    globtestp := ltestp2
	  end;
	power:
	  comptypes := comptypes(fsp1^.elset,fsp2^.elset);
	arrays: begin
	  getbounds (fsp1^.inxtype,lmin,lmax);
	  i := lmax-lmin;
	  getbounds (fsp2^.inxtype,lmin,lmax);
	  comptypes := (i=lmax-lmin) andif (fsp1^.arraypf = fsp2^.arraypf) andif
	    comptypes(fsp1^.aeltype,fsp2^.aeltype);
	end;
	(*ALTERNATIVES: -- ADD A THIRD BOOLEAN TERM: INDEXTYPE MUST
	 BE COMPATIBLE. MAY GIVE TROUBLE FOR ASSIGNMENT OF STRINGCONSTANTS
	 -- ADD A FOURTH BOOLEAN TERM: LOWBOUNDS MUST
	 BE THE SAME*)
	records: begin
	  nxt1 := fsp1^.fstfld;
	  nxt2 := fsp2^.fstfld;
	  comp := true;
	  while (nxt1 <> nil) andif (nxt2 <> nil) do begin
	    comp := comp andif comptypes(nxt1^.idtype,nxt2^.idtype);
	    nxt1 := nxt1^.next;
	    nxt2 := nxt2^.next
	  end;
	  comptypes := comp andif (nxt1 = nil) andif (nxt2 = nil) andif
	    (fsp1^.recvar = nil) andif (fsp2^.recvar = nil)
	end;
	(*IDENTICAL RECORDS ARE RECOGNIZED TO BE COMPATIBLE
	 IFF NO VARIANTS OCCUR*)
	strings:
	  comptypes := (fsp1^.maxlength = fsp2^.maxlength) andif
	    (fsp1^.skind = fsp2^.skind);
	files:
	  comptypes := comptypes(fsp1^.filtype,fsp2^.filtype);
	formalprocfunc:
	  comptypes := false
	  (* DETERMINED IN CALLNONSTANDARD *)
      end					(*CASE*)
    else					(*FSP1^.FORM # FSP2^.FORM*)
    if fsp1^.form = subrange then
      comptypes := comptypes(fsp1^.rangetype,fsp2)
    else if fsp2^.form = subrange then
      comptypes := comptypes(fsp1,fsp2^.rangetype)
    else
      comptypes := false
  else
    comptypes := true
end (*COMPTYPES*);

$PAGE string, strform
public function string (fsp: stp): boolean;

begin
  string := false;
  if fsp <> nil then
    with fsp^ do
      if form = arrays then
	if comptypes(aeltype,charptr) then
	  string := true
end (*STRING*);

public function strform (fsp: stp): boolean;

begin
  strform := false;
  if comptypes (fsp, charptr) then
    strform := true
  else
    with fsp^ do
      if form = strings then
	strform := true
      else if form = arrays then
	if arraypf then
	  if comptypes (aeltype, charptr) then
	    if inxtype^.min.ival = 1 then 
	      strform := true
end .
 