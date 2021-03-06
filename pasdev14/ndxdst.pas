program NDXDST;

$include opstat.typ

$include spnams.con

external procedure readinstr(var f:text; var p:p_instr);
external procedure killinstr(var p:p_instr);

var
	inf,outf: text;
	infilename,outfilename: string;
	p_ins: p_instr;
	p_op: p_opspec;
	p_case: p_casedis;

	(* counting variables *)

	i,j: u4bits;
	distrib: array[u4bits,u4bits] of integer;
	total,wtotal,linesubtotal: integer;
	colsubtotal: array[u4bits] of integer;
	index_pending: boolean;

begin
  open(tty);
  rewrite(tty);
  writeln(tty,'NDXDST');
  writeln(tty);
  write(tty,'Input file (default .OPS): ');
  break;
  readln(tty);
  read(tty,infilename);
  write(tty,'Output file (default .LST): ');
  break;
  readln(tty);
  read(tty,outfilename);

  reset(inf,'.OPS ' || infilename);
  rewrite(outf,'.LST ' || outfilename);

(* initialize *)

  for i := minimum(i) to maximum(i) do
    for j := minimum(j) to maximum(j) do distrib[i,j] := 0;
  total := 0;	wtotal := 0;

  while not eof(inf) do begin
    readinstr(inf,p_ins);

(* count statistics *)

    p_op := p_ins^.o_list;
    index_pending := false;
    while p_op <> nil do begin
      if index_pending then begin
	distrib[p_op^.mode,p_op^.reg] := distrib[p_op^.mode,p_op^.reg] + 1;
	total := total + 1;	wtotal := wtotal + specsizes[p_op^.mode]
	end;
      index_pending := p_op^.mode = 4;
      p_op := p_op^.o_next
      end;
    killinstr(p_ins)
    end;

(* output summary *)

  writeln(outf,'Indexed operand specifier frequency: file ',filename(inf));
  writeln(outf,'Total indexed operand specifiers counted: ',total);
  writeln(outf,'Weighted total: ',wtotal);
  writeln(outf);
  writeln(outf,'Register:   R0   R1   R2   R3   R4   R5   R6   R7 ' ||
    '  R8   R9  R10  R11   AP   FP   SP   PC             Wtd.');
  writeln(outf,'ModeWeight<---><---><---><---><---><---><---><--->' ||
    '<---><---><---><---><---><---><---><--->Total Freq  Freq');
  for j := minimum(j) to maximum(j) do colsubtotal[j] := 0;
  for i := minimum(i) to maximum(i) do begin
    write(outf,specnames[i],specsizes[i]:0);
    linesubtotal := 0;
    for j := minimum(j) to maximum(j) do begin
      write(outf,distrib[i,j]:5);
      linesubtotal := linesubtotal + distrib[i,j];
      colsubtotal[j] := colsubtotal[j] + distrib[i,j]
      end;
    writeln(outf,linesubtotal:5,linesubtotal*100.0/total:5:1,'%',
      linesubtotal*specsizes[i]*100.0/wtotal:5:1,'%')
    end;
  write(outf,'Total     ');
  for j := minimum(j) to maximum(j) do write(outf,colsubtotal[j]:5);
  writeln(outf,total:5);
  write(outf,'Freq      ');
  for j := minimum(j) to maximum(j) do write(outf,colsubtotal[j]*100.0/total:5:1);
  writeln(outf);
  writeln(outf,'              %    %    %    %    %    %    %    %' ||
    '    %    %    %    %    %    %    %    %')
  end.
  