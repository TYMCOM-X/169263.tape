$TITLE dmpobj - dump vax rel file
program dmpobj;
  
type
   relf_byte = 0..255;
   hex_array = array [0..15] of char;
   word = packed array [1..4] of relf_byte;
  
const
   hex: hex_array = ('0', '1', '2', '3', '4', '5', '6', '7',
		     '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  
var
   infilename, outfilename: string[30];
   infile: file of word; 
   outfile: text;
   input_word: word;
   count, last, i, j, line_index: 0..513;  (* count of bytes in current record *)
   low_byte, high_byte: relf_byte;  (* for reading two-byte record length *)
   record_num: 0..10000;  (* current record in object file *)
   line: array [1..512] of relf_byte;  (* bytes to be printed on current line *)
   descr: array [1..512] of string[4];
   rec_type: array [0..6] of string[4] := 
			  (' hdr', ' gsd', ' tir', ' eom', ' dbg', ' tbr', ' lnk');
   hdr_type: array [0..6] of string[4] :=
			  (' mhd', ' lnm', ' src', ' ttl', ' cpr', ' mtc', ' gtx');
   gsd_type: array [0..3] of string[4] :=
			  (' psc', ' sym', ' epm', ' pro');
   line_num: 1..32;  (* line with current record *)
   ptr: 1..5;  (* byte within current dec 10 word *)
$PAGE read_byte
procedure read_byte (var input_byte: relf_byte);
  
   var i: 1..4;

   begin
      ptr := ptr + 1;
      if ptr = 5 then begin
	ptr := 1;
	if not eof (infile) then
	  read (infile, input_word)
	else
	  for i := 1 to 4 do input_word[i] := 0;
      end;
      input_byte := input_word[ptr]
   end;
$PAGE hdr_rec
procedure hdr_rec;
  
   begin
      descr[2] := hdr_type[line[2]];
      case line[2] of

	 0: begin  (* module header record *)
	    descr[3] := ' lvl';
	    descr[5] := ' max'; descr[4] := 'size';
	    descr[6] := ':nam';
	    for j := 7 to 6 + line[6] do
	       descr[j] := '  ' || chr (line[j]) || ' ';
	    i := line[6] + 7;
	    descr[i] := ':ver';
	    for j := i + 1 to i + line[i] do
	       descr[j] := '  ' || chr (line[j]) || ' ';
	    i := i + line[i] + 1;

	    for j := i to i + 16 do
	       descr[j] := '  ' || chr (line[j]) || ' ';
	    descr[i,4] := ']'; descr[i+16,1] := '[';

	    i := i + 17;
	    for j := i to i + 16 do
	       descr[j] := '  ' || chr (line[j]) || ' ';
	    descr[i,4] := ']'; descr[i+16,1] := '['
	 end;

	 1:  (* lang. processor name & vers. *)
	    for j := 3 to last do
	       descr[j] := '  ' || chr (line[j]) || ' '

      end (* case *);
   end;
$PAGE gsd_rec
procedure gsd_rec;
  
   begin
      i := 2;
      while i <= last do begin
	 descr[i] := gsd_type[line[i]];
	 case line[i] of
  
	    0: begin (* psect *)
	       descr[i+1] := 'algn';
	       i := i + 2; (* skip over alignment *)
	       descr[i+1] := ' fla'; descr[i] := 'gs  ';
	       i := i + 2;
	       descr[i+3] := ' --a'; descr[i+2] := 'lloc'; descr[i+1] := 'atio'; descr[i] := 'n-- ';
	       i := i + 4
	    end;

	    1: begin (* global symbol spec. *)
	       descr[i+1] := 'dtyp';
	       i := i + 2;  (* skip over data type *)
	       descr[i+1] := ' fla'; descr[i] := 'gs  ';
	       i := i + 2;
	       if odd (line[i-2] div 2) then begin (* label address bytes if its a definition *)
		  descr[i] := '--- ';
		  descr[i+1] := 'ss--';
		  descr[i+2] := 'ddre';
		  descr[i+3] := '---a';
		  descr[i+4] := ' ---';
		  i := i + 5 
	       end
	    end;

	    2: begin (* entry point *)
	       descr[i+1] := 'dtyp';
	       i := i + 2; (* skip over data type *)
	       descr [i+1] := ' fla'; descr[i] := 'gs  ';
	       i := i + 2;
	       descr [i] := '--- ';
	       descr [i+1] := 'ss--';
	       descr [i+2] := 'ddre';
	       descr [i+3] := '---a';
	       descr [i+4] := ' ---';
	       i := i + 5;
	       descr [i] := 'sk- ';
	       descr [i+1] := ' -ma';
	       i := i + 2
	    end
	 end (* case *);

	 descr[i] := ':nam';
	 for j := i + 1 to i + line[i] do
	    descr[j] := '  ' || chr (line[j]) || ' ';
	 i := i + line[i] + 1
      end (* while *);
   end;
$PAGE tir_rec
procedure tir_rec;
 
   begin
      i := 2;
      while i <= last do

	 case line[i] of

	    128..255: begin  (* stoim *)
	       descr [i] := 'stoi';
	       descr [i+1][4] := ']';
	       i := i + 256 - line[i];
	       descr [i][1] := '[';
	       i := i + 1
	    end;

	    0: begin
	       descr [i] := 'gbl ';
	       for j := i+2 to i+1+line[i+1] do
		  descr [j] := '  ' || chr (line[j]) || ' ';
	       i := i + line[i+1] + 2
	    end;

	    1: begin
	       descr[i] := ':sta';
	       descr[i+1] := 'byte';
	       i := i + 2
	    end;

	    2: begin
	       descr[i] := ':sta';
	       descr[i+1] := 'word';
	       descr[i+2] := ' ---';
	       i := i + 3
	    end;

	    3: begin
	       descr[i] := ':sta';
	       descr[i+1] := 'word';
	       descr[i+2] := 'long';
	       descr[i+3] := '----';
	       descr[i+4] := ' ---';
	       i := i + 5
	    end;

	    4: begin
	       descr[i] := ':sta';
	       descr[i+1] := 'sect';
	       descr[i+2] := '  +p';
	       i := i + 3
	    end;

	    5: begin
	       descr[i] := ':sta';
	       descr[i+1] := 'sect';
	       descr[i+2] := '- +p';
	       descr[i+3] := ' ---';
	       i := i + 4
	    end;

	    6: begin
	       descr[i] := ':sta';
	       descr[i+1] := 'sect';
	       descr[i+2] := '- +p';
	       descr[i+3] := '----';
	       descr[i+4] := '----';
	      descr[i+5] := ' ---';
	       i := i + 6
	    end;

	    25: begin
	       descr[i] := 'lwds';
	       i := i + 1
	    end;
  
	    28: begin
	       descr[i] := 'picr';
	       i := i + 1
	    end;
  
	    50: begin
	       descr[i] := ' nop';
	       i := i + 1
	    end;
  
	    51: begin
	       descr[i] := ' add';
	       i := i + 1
	    end;
  
	    80: begin
	       descr[i] := 'relb';
	       i := i + 1
	    end;
  
	    others: begin
	      descr[i] := '????';
	      i := i + 1
	    end
  
	 end (* case *);
   end;
$PAGE eom_rec
procedure eom_rec;
  
   begin
       descr[2] := 'elvl';
       if last > 2 then begin (* transfer address? *)
	  descr[3] := 'ss- ';
	  descr[4] := 'ddre';
	  descr[5] := 'er-a';
	  descr[6] := 'ansf';
	  descr[7] := ' -tr'
       end
   end;
$PAGE mainline
begin
  
   rewrite (ttyoutput, 'tty:');
   open (tty,'tty:');
   write (ttyoutput, 'rel file to be dumped-- ');
   break (ttyoutput);
   readln (tty);
   read (tty,infilename);
   write (ttyoutput, 'output dump file-- ');
   break (ttyoutput);
   readln (tty);
   read (tty, outfilename);

   reset (infile, infilename);
   rewrite (outfile, outfilename);
   if not eof(outfile) then writeln (ttyoutput, 'error from rewrite');

   record_num := 0;
   ptr := 4;  (* force read of first word *)

   while not eof(infile) do begin
      read_byte (low_byte);
      read_byte (high_byte);
      count := low_byte + 256 * high_byte;
   exit if count = 0;
      writeln (outfile);
      writeln (outfile);
      writeln (outfile, '     record number ',record_num:4,'   size ', count:3, ' bytes');
      writeln (outfile);
      record_num := record_num + 1;
      last := count;
      if odd(count) then count := count + 1;
  
      for i := 1 to count do begin
        read_byte (line[i]);
        descr[i] := '    '
      end;
  
      descr[1] := rec_type [line[1]];
  
      if line[1] = 0 (* hdr *) then
        hdr_rec
  
      else if line[1] = 1 (* gsd *) then
        gsd_rec
  
      else if line[1] = 2 (* tir *) then
	tir_rec
  
      else if line[1] = 3 (* eom *) then
	eom_rec;
  
      for line_num := 1 to (count+15) div 16 do begin
	 for line_index := line_num*16 downto (line_num-1)*16 + 1 do
	    if line_index > count then
	       write (outfile, '    ')
	    else
	       write (outfile, descr[line_index]);
	 writeln (outfile);
	 for line_index := line_num*16 downto (line_num-1)*16 + 1 do
	    if line_index > count then
	       write (outfile, '    ')
	    else
	       write (outfile, ' ', hex[line[line_index] div 16]:1,
				    hex[line[line_index] mod 16]:1, ' ');
	 writeln (outfile, ' ':10, hex[(line_num-1) div 16]:1,
				   hex[(line_num-1) mod 16]:1, '0')
       end
   end (* while *);

   close (outfile)
  
end.
 