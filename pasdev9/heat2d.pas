program heat2d;

$IF double type dreal = minimum (real) .. maximum (real) prec 16;
$IFNOT double type dreal = real;

external function hexreal (dreal): string;

 const
   dt : dreal = 0.1;
   d2dt : dreal = 20.0;
   pm4 : dreal = 0.0001;
   one : dreal = 1.0;
   two : dreal = 2.0;
   jmax = 16;
   kmax = 16;

 var
  rtime: integer;
  nmax, nyt, nxt, nym, nxm, jx, kx, j, k, nc, ncl, itb, ite, itrun: 0..16000;
  tlow, thih, deltt, delt, time, ta, edtime, eddt: dreal;

 type matrix = array [1..jmax, 1..kmax] of dreal;

 var
  dens, temp, cap, ts, c, cbb, al, dbb, a, b, s, cb, dum, db: matrix;

begin
  rewrite (output,'heat.ls');
  rewrite (tty);
  (* Generate problem. *)

  for k := 1 to kmax do begin
    for j := 1 to jmax do begin
      dens [j,k] := j+k;
      temp [j,k] := j+k;
      cap [j,k] := j+k;
      ts [j,k] := j+k;
      c [j,k] := j+k;
      cbb [j,k] := j+k;
      al [j,k] := j+k;
      dbb [j,k] := j+k;
      a [j,k] := j+k;
      b [j,k] := j+k;
      s [j,k] := j+k;
      cb [j,k] := j+k;
      dum [j,k] := j+k;
      db [j,k] := j+k;
    end;
  end;

  nc := 0;
  nyt := (jmax+1) div 2;
  nxt := kmax-1;
$IF short nmax := 10;
$IFNOT short nmax := 100;
  edtime := 1.0000000;
  eddt := 0.5000000;
  nym := nyt - 1;
  nxm := nxt - 1;
  jx := nym - 1;
  kx := nxm - 1;

  for k := 1 to nxt do begin
    for j := 1 to nyt do begin
      dens [j,k] := 1.0000000;
      temp [j,k] := 10.0000000;
      ts [j,k] := 1.0000000;
      cap [j,k] := 0.1000000;
    end;
  end;

  ncl := nc;
  time := 0.0000000;
  ta := max (pm4, dt);

  (* Computation begins. *)

  while (nc < nmax) do begin

    nc := nc + 1;
    time := time + dt;
    for k := 1 to nxt do begin
      for j := 1 to nyt do begin;
	ta := sqrt (temp [j,k]);
	al [j,k] := dens [j,k] * ta * temp [j,k];
	c [j,k] := pm4 * ta * temp [j,k];
      end;
    end;

    (* Couplings *)

    for k := 1 to nxm do begin
      for j := 1 to nym do begin
	cb [j,k] := two * sqr (max (c [j,k], c [j,k+1])) / (c [j,k] + c [j,k+1]);
	ta := cb [j,k] * abs (temp [j,k] - temp [j,k+1]) / max (al [j,k], al [j,k+1]);
	cbb [j,k] := cb [j,k] / (one + ta);
	db [j,k] := two * sqr (max (c [j,k], c [j+1,k])) / (c [j,k] + c [j+1,k]);
	ta := s [j,k] * abs (temp [j,k] - temp [j+1,k]) / max (al [j,k], al [j+1,k]);
	dbb [j,k] := db [j,k] / (one + ta);
	s [j,k] := dens [j,k] * cap [j,k] * d2dt;
	ts [j,k] := temp [j,k];
      end;
    end;

    (* Boundary conditions for free boundary *)

    for j := 1 to nyt do begin;
      a [j,1] := 0.0000000;
      b [j,1] := 1.0000000;
      temp [j,1] := 1.0000000;
      temp [j,nxm] := 1.0000000;
      ts [j,1] := 1.0000000;
      ts [j,nxm] := 1.0000000;
    end;
    for k := 1 to nxt do begin
      a [1,k] := 0.0000000;
      b [1,k] := 1.0000000;
      temp [1,k] := 1.0000000;
      temp [nym,k] := 1.0000000;
      ts [1,k] := 1.0000000;
      ts [nym,k] := 1.0000000;
    end;

    (* Z sweep *)

    for j := 2 to jx do begin
      for k := 2 to kx do begin
	dum [j,k] := s [j,k] + cbb [j,k] + cbb [j,k-1] * (one - a [j,k-1]);
	a [j,k] := cbb [j,k] / dum [j,k];
	b [j,k] := ( s [j,k] * temp [j,k] + cbb [j,k-1] * b [j,k-1]
		  + dbb [j,k] * (temp [j+1,k] - temp [j,k])
		  - dbb [j-1,k] * (temp [j,k] - temp [j-1,k])       ) / dum [j,k];
      end;
      for k := kx downto 2 do
	ts [j,k] := a [j,k] * ts [j,k+1] + b [j,k];
    end;

    (* R sweep *)

    for k := 2 to kx do begin
      for j := 2 to jx do begin
	dum [j,k] := s [j,k] + dbb [j,k] + dbb [j-1,k] * (one - a [j-1,k]);
	a [j,k] := dbb [j,k] / dum [j,k];
	b [j,k] :=
	      ( s [j,k] * ts [j,k] + dbb [j-1,k] * b [j-1,k]
		  + cbb [j,k] * (ts [j,k+1] - ts [j,k])
		  - cbb [j,k-1] * (ts [j,k] - ts [j,k-1])	) / dum [j,k];
      end;
      for j := jx downto 2 do
	temp [j,k] := a [j,k] * temp [j+1,k] + b [j,k];
    end;

    (* Back substitution R direction *)

    if time > edtime then begin
      writeln (nc, hexreal (time));

      for k := 2 to kx do begin
	for j := 2 to jx do begin
	  deltt := abs (temp [j,k] - ts [j,k]);
	  if delt < deltt then delt := deltt;
	  if tlow > temp [j,k] then tlow := temp [j,k];
	  if thih < temp [j,k] then thih := temp [j,k];
	end;
      end;

      edtime := edtime + eddt;
      ncl := nc;
      writeln (hexreal (delt), hexreal (tlow), hexreal (thih));  
    end (* if *) ;

  end (* while *) ;

  for j := 2 to jx do begin
   writeln (j, hexreal (temp[j,2]), hexreal (temp[j,3]), hexreal (temp[j,kx-1]), hexreal (temp[j,kx]));  
  end;
  rtime := runtime - rtime;
  writeln (tty);
  writeln (tty,'Execution time: ',rtime/1000.0:8:2,' CPU seconds.');

end.
