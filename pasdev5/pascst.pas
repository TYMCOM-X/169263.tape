$title pascst - PASCAL compiler constant routine
$OPTIONS SPECIAL, NOCHECK

(*	Conditional compilation switches used:

	Y4: Unless disabled, support double precision reals *)

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASLEX.INC

$INCLUDE PASSTR.INC

external procedure call (setofsys; ctp);

$PAGE constant
public procedure constant(fsys: setofsys; var fsp: stp; var fvalu: valu);

var
    lsp,lsp1: stp;
    lcp: ctp;
    sign: (none,pos,neg);

begin
  lsp := nil;
  fvalu.ival := 0;
  skipiferr(constbegsys,207,fsys);
  if sy in constbegsys then begin
    if sy = stringconst then begin
      if lgth = 1 then
	lsp := charptr
      else
	lsp := dclstring (lgth, nonvarying);
      fvalu := val;
      insymbol
    end
    else begin
      sign := none;
      if (sy = addop) andif (op in [plus,minus]) then begin
	if op = plus then
	  sign := pos
	else
	  sign := neg;
	insymbol
      end;
      if sy = ident then begin
	srchid([konst,func],lcp);
	if lcp^.klass = func then begin
	  with lcp^ do begin
	    if (pfdeckind = standard) then begin
	      if key in [7 (* ord *), 8 (* chr *)] then begin
		insymbol;
		if sy = lparent then
		  insymbol
		else
		  error (153);
		constant (fsys + [rparent],lsp,fvalu);
		if key=7 then
		  lsp := intptr
		else begin
		  if lsp <> intptr then
		    error (459);
		  lsp := charptr;
		end;
		if sy = rparent then
		  insymbol
		else
		  error (152);
	      end				(* ord, chr *)
	      else if key in [14 (* mini *), 15 (* maxi *), 16 (* size *), 26	(* lwb *)
	      , 27 (* upb *), 28 (* ptr *)] then begin
		insymbol;
		call (fsys, lcp);		(* should leave constant value in gattr.cval *)
		fvalu.ival := gattr.cval.ival;
		lsp := gattr.typtr;
	      end
	      else
		errandskip (207,fsys);
	    end					(* if standard function *)
	    else
	      errandskip (207, fsys);
	  end;					(* with *)
	end					(* klass = func *)
	else begin				(* KLASS = KONST *)
	  with lcp^ do begin
	    lsp := idtype;
	    if lsp = realptr then begin
	      new (fvalu.valp,reel);
	      fvalu.valp^.rval := values.valp^.rval;
	      (* Assume RVAL2=0 *)
	    end
	    else
	      fvalu := values
	  end;
	  if sign <> none then
	    if lsp = intptr then begin
	      if sign = neg then
		fvalu.ival := -fvalu.ival
	    end
	    else if lsp = realptr then begin
	      if sign = neg then
		fvalu.valp^.rval := -fvalu.valp^.rval
	    end
	    else
	      error(167);
	  insymbol;
	end;					(* KONST ID *)
      end
      else if sy = intconst then begin
	if sign = neg then
	  val.ival := -val.ival;
	lsp := intptr;
	fvalu := val;
	insymbol
      end
      else if sy = realconst then begin
	if sign = neg then
	  val.valp^.rval := -val.valp^.rval;
	lsp := realptr;
	fvalu := val;
	insymbol
      end
      else
	errandskip(168,fsys)
    end;
    iferrskip(166,fsys);
  end;
  fsp := lsp
end (*CONSTANT*).
    