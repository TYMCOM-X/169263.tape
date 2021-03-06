program pfmhis;

(*     Print histogram of page fault data produced by PFMCNV.  *)

type      (* An object module name *)
  modname = packed array [1..32] of char;

const
  blankname: modname := '                                ';

procedure lookup_module(pageaddr: integer; var result: modname);

  (*       Given the first address of a page, find the last object module
	  making a contribution to it by scanning the VAX Link map.
       Many pathological data references.      *)

  static var
    mapfile: text;  (* the link map file *)
    baseaddr: integer := -1;        (* the base address of the current module in the scan *)
    lastmod, modulename: modname := blankname;      (* the last and current module names *)

  var
    mapfilename: string;    (* the link map file name *)
    mapline: string[254];   (* a line read from the map file *)

begin
  if baseaddr = -1 then begin       (* if first call then ... *)
    write(tty, 'Map file: '); 
    break;        (* open the map file and find psect synopsis *)
    readln(tty); 
    read(tty, mapfilename);
    reset(mapfile, '.MAP ' || mapfilename);
    repeat
      readln(mapfile, mapline)
    until index(mapline, 'Program Section Synopsis') <> 0
  end;

  while baseaddr < pageaddr + 512 do begin        (* scan until first module past this page *)
    lastmod := modulename;
    loop    (* scan for a module line *)
      readln(mapfile, mapline);
    exit if index(mapline, 'Symbols By Name') <> 0 do begin
							modulename := blankname;        (* nothing more, give up *)
							baseaddr := maximum(integer);
							close(mapfile)
						      end;
    exit if (length(mapline) >= 33) andif (mapline[33] = '0') do
      getstring(mapline, modulename:32, baseaddr:8:h)
    end
  end;
  result := lastmod (* the result is the last one, not current *)
end;

const
  hist_width := 60;       (* maximum histogram width *)
  starstring :=
    '************************************************************';

type
  datindex = (fault_pc, fault_addr);
  datitem = array [datindex] of integer;
  datfile = file of datitem;      (* file of 2-longword records from PFMCNV *)
  histarray = array [0..*] of integer;    (* a histogram array *)

var
  outfile: text;  (* where to print the histogram *)
  outfilename: string;    (* its file name *)
  infile: datfile;        (* the file from PFMCNV *)
  hipc, loaddr, hiaddr, lopage, hipage, pagenumber: integer;

  (* PC limit, address limits, histogram page slot limits, current page number *)

  hist: ^histarray;       (* the histogram *)
  item: datitem;  (* an item from infile *)
  hist_max, hist_delta: integer;  (* size of highest spike, size of each asterisk *)
  answer: string; (* yes/no *)
  totfaults, nfaults: integer;    (* total faults in infile, number counted in histogram *)
  pname: string;  (* name of program causing the faults *)
  wssize: integer;        (* size of its working set *)
  histindex: datindex;    (* whether to do histogram by PC or address *)
  mname: modname; (* object module name for this line of histogram *)

begin

  open(tty); 
  rewrite(tty);

  (* get answers from user *)

  write(tty, 'Histogram by PC? '); 
  break;
  readln(tty); 
  read(tty, answer);
  if (answer = '') orif (uppercase(answer[1]) = 'Y')
  then histindex := fault_pc
  else histindex := fault_addr;

  write(tty, 'Highest PC (hex): '); 
  break;
  readln(tty); 
  read(tty, hipc:8:h);
  write(tty, 'Low faulting address (hex): '); 
  break;
  readln(tty); 
  read(tty, loaddr:8:h);
  write(tty, 'High faulting address (hex): '); 
  break;
  readln(tty); 
  read(tty, hiaddr:8:h);

  (* initialize the histogram array *)

  if histindex = fault_pc then begin
    lopage := 0;
    hipage := hipc div 512
  end
  else begin
    lopage := loaddr div 512;
    hipage := hiaddr div 512
  end;
  new(hist, hipage - lopage + 1);
  for pagenumber := lopage to hipage do hist^[pagenumber - lopage] := 0;

  (* initialize the input file *)

  reset(infile, 'PFM.DAT');

  totfaults := extent(infile);

  (* accumulate the histogram *)

  while not eof(infile) do begin
    read(infile, item);

    (* only count it if PC and address fall within limits *)

    if (item[fault_pc] in [0..hipc]) andif
	       (item[fault_addr] in [loaddr..hiaddr]) then begin
      pagenumber := item[histindex] div 512;
      hist^[pagenumber - lopage] := succ(hist^[pagenumber - lopage])
    end
  end;

  close(infile);

  (* get summary data *)

  hist_max := 0;
  nfaults := 0;
  for pagenumber := lopage to hipage do begin
    nfaults := nfaults + hist^[pagenumber - lopage];
    if hist^[pagenumber - lopage] > hist_max then
      hist_max := hist^[pagenumber - lopage]
  end;
  hist_delta := (hist_max + hist_width - 1) div hist_width;
  if hist_delta <= 0 then hist_delta := 1;

  (* establish output file *)

  loop
    write(tty, 'Output to: '); 
    break;
    readln(tty); 
    read(tty, outfilename);
    if outfilename = '' then outfile := ttyoutput
    else rewrite(outfile, '.LIS ' || outfilename);
  exit if eof(outfile);
    writeln(tty, 'Error.')
  end;

  (* get documentary info from user *)

  write(tty, 'Program name: '); 
  break;
  readln(tty); 
  read(tty, pname);
  write(tty, 'Working set size: '); 
  break;
  readln(tty); 
  read(tty, wssize);

  (* write out heading of histogram *)

  write(outfile, 'Histogram of page faults for ', pname, ' at w.s.', wssize,
    ' pages by ');
  if histindex = fault_pc
  then writeln(outfile, 'program counter:')
  else writeln(outfile, 'faulting address:');
  writeln(outfile, 'PC ranges from 00000000 to ', hipc:8:h);
  writeln(outfile, 'Faulting address ranges from ', loaddr:8:h,
      ' to ', hiaddr:8:h);
  writeln(outfile, 'Total page faults for run =', totfaults);
  writeln(outfile, 'Page faults counted =', nfaults);
  writeln(outfile, '    One unit =', hist_delta, ' page faults.');
  writeln(outfile);

  (* write out the histogram itself *)

  for pagenumber := lopage to hipage do begin
    lookup_module(pagenumber*512, mname);
    writeln(outfile, mname, pagenumber*512:8:h, '  ',
	    starstring:
	      ((hist^[pagenumber - lopage] + hist_delta - 1) div hist_delta) )
  end
end.
   