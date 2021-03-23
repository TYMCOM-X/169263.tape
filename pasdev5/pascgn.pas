$TITLE pascgn - PASCAL compiler code emission routines
$OPTIONS SPECIAL, NOCHECK

$INCLUDE PASDCL.INC

external procedure error (integer);

external procedure errorwithtext (integer; alfa);

$PAGE mac
public procedure mac(frelbyte : relbyte; finstr : instrange; fac : acrange;
  findbit : ibrange; finxreg : acrange; faddress : addrrange);

begin
(*$X5  IF NOT INITGLOBALS THEN BEGIN	*)
    cix := cix + 1;
    if cix > cixmax then begin
      if curproc = nil then
	errorwithtext(356,'MAIN      ')
      else
	errorwithtext(356, curproc^.name);
      cix := 0
    end;
    with code, instruction[cix] do begin
      instr :=finstr;
      ac :=fac;
      indbit :=findbit;
      inxreg :=finxreg;
      address :=faddress;
      information[cix]:= ' ';
      relocation[cix] := frelbyte
    end;
    ic := ic + 1
(*$X5  END
  ELSE
    ERROR(507)	*)
end (*MAC*);

$PAGE mac5, mac4, mac3
public procedure mac5(frelbyte: relbyte; finstr : instrange;
  fac,finxreg : acrange; faddress : addrrange);

begin
  mac(frelbyte,finstr,fac,0,finxreg,faddress)
end;

public procedure mac4(finstr: instrange;fac, finxreg: acrange;
  faddress: addrrange);

begin
  mac(no,finstr,fac,0,finxreg,faddress)
end;

public procedure mac3(finstr : instrange; fac:acrange; faddress: addrrange);

begin
  mac(no,finstr,fac,0,0,faddress)
end;

$PAGE mac4r, mac3r, mac3b
public procedure mac4r(finstr : instrange; fac,finxreg : acrange;
  faddress : addrrange);

begin
  mac(right,finstr,fac,0,finxreg,faddress)
end;

public procedure mac3r(finstr : instrange; fac:acrange; faddress: addrrange);

begin
  mac(right,finstr,fac,0,0,faddress)
end;

public procedure mac3b(finstr:instrange; fac:acrange; faddress:addrrange);
(*CALLED WHEN FADDRESS MIGHT BE REGISTER, E.G., WHEN ACCESSING
  THROUGH BYTE POINTERS*)

begin
  if faddress<16 then
    mac3(finstr,fac,faddress)
  else
    mac3r(finstr,fac,faddress)
end;

$PAGE checklc, incrementregc
public procedure checklc (i: integer);

begin
  lc := lc + i;
  if lc > lcmax then
    lcmax := lc;
end;

public procedure incrementregc;

begin
  regc := regc + 1 ;
  if regc > regcmax then begin
    error(310) ;
    regc := regin
  end
end.
