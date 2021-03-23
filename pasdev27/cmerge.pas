$title cmerge -- file copy and merge utility
$length 43

program cmerge;

const
    version_number = '0.1';

    line_max = 255;

    max_keys = 16;


type
    line_index = 0 .. line_max;

    text_line = string [line_max];

    char_set = set of char;

    key_type = ( alpha, numeric, upperalpha );
    key_value = record
	kind: key_type;
	reversed: boolean;
	posn: line_index;
	len : line_index
    end (* key_value *);

    key_list = record
	count: 0 .. max_keys;
	value: array [1..max_keys] of key_value
    end (* key_list *);

    key_set = record
	clist: key_list;
	dlist: key_list;
	klist: key_list
    end (* key_set *);
$PAGE

    op_codes = ( no_op, switches_only, copy, union, difference, intersection );

    option_list = record
	rec_option: boolean;
	rec_len: line_index;
	sum_option:  boolean;
	exit_option:  boolean
    end (* option_list *);


var
    in_file_1,
    in_file_2,
    out_file : text;

    key_set_1,
    key_set_2: key_set;

    options: option_list;

    function_code: op_codes;
$PAGE get_command

procedure get_command ( var in_file_1: text; var key_set_1: key_set;
			var in_file_2: text; var key_set_2: key_set;
			var out_file : text; var options: option_list;
			var function_code: op_codes );


const
    eol = ' ';					(* Ok, because all blanks are compressed
						   out of command lines during input. *)
    file_name_len = 40;
    max_switches = 20;
    switch_len = 20;


type
    file_name = string [file_name_len];
    switch_index = 0 .. switch_len;
    switch_text = string [switch_len];
    switch_list = record
	count: 0 .. max_switches;
	text: array [1..max_switches] of switch_text
    end;


var
    command_line: text_line;

    in_name_1,
    in_name_2,
    out_name : file_name;

    in_1_switches,
    in_2_switches,
    out_switches : switch_list;
$PAGE get_command - get_index

function get_index ( switch: switch_text; var loc: switch_index;
		     var index: line_index; default: line_index;
		     delimiters: char_set ): boolean;

var
    lim: line_index;
    rem: line_index;
    digit: 0 .. 9;

begin
  get_index := true;
  if switch [loc] <> eol then
    loc := loc + 1;
  if switch [loc] in ['0'..'9'] then begin
    lim := line_max div 10;
    rem := line_max mod 10;
    index := 0;
    while switch [loc] in ['0'..'9'] do begin
      digit := ord(switch[loc]) - ord('0');
      if (index > lim) or ((index = lim) andif (digit > rem)) then begin
	writeln (tty,'% INDEX TOO LARGE IN /',switch);
	get_index := false;
	loc := length (switch);
      end
      else begin
	index := index*10 + digit;
	loc := loc + 1;
      end;
    end (* while switch [loc] in ['0'..'9'] *);
  end
  else
    index := default;
  if not (switch [loc] in delimiters + [eol]) then begin
    writeln (tty,'% BAD SWITCH /',switch);
    get_index := false;
  end;
end (* get_index *);
$PAGE get_command - read_command_line

procedure read_command_line ( var command_line: text_line );

begin
  command_line := '';
  write (tty,'*');
  break;
  readln (tty);
  while not eoln (tty) do begin
    if tty^ <> ' ' then
       command_line := command_line || uppercase(tty^);
    get (tty);
  end;
  if command_line = '' then
    command_line := '/E';
  if length(command_line) = line_max then begin
    writeln (tty,'% COMMAND LINE TOO LONG');
    command_line := '';
  end;
  command_line := command_line || eol;
end (* read_command_line *);
$PAGE get_command - parse_command_line

procedure parse_command_line ( command_line: text_line;
			       var in_name_1, in_name_2, out_name: file_name;
			       var in_1_switches, in_2_switches, out_switches: switch_list;
			       var function_code: op_codes );


type
    char_set = set of char;
$PAGE get_command - parse_command_line - parse_file_name

function parse_file_name ( command_line: text_line;
			   last_index: line_index;
			   var next_index: line_index;
			   var name: file_name;
			   var switches: switch_list;
			   delimiters: char_set
			 ): boolean;

var
    ind: line_index;

begin
  next_index := search ( substr(command_line,last_index+1), delimiters+['/',eol] );
  if next_index <> 0
    then name := substr(command_line,last_index+1,next_index-1)
    else name := '';
  next_index := last_index + next_index;
  switches.count := 0;
  while (next_index <> 0) andif (command_line [next_index] = '/') do begin
    ind := next_index;
    next_index := search ( substr(command_line,ind+1), delimiters+['/',eol] );
    if next_index = 0 then
      writeln (tty,'% USAGE: output/switches = input1/switches [(+|-|*) input2/switches]')
    else if next_index > switch_len then begin
      writeln (tty,'% SWITCH TOO LONG: ',substr(command_line,ind,next_index));
      next_index := 0;
    end
    else if switches.count = max_switches then begin
      writeln (tty,'% TOO MANY SWITCHES');
      next_index := 0;
    end
    else begin
      switches.count := switches.count + 1;
      switches.text [switches.count] := substr(command_line,ind+1,next_index-1) || eol;
      next_index := ind + next_index;
    end;
  end (* while (next_index <> 0) andif (command_line [next_index] <> eol) *);
  parse_file_name := (next_index <> 0);
end (* parse_file_name *);
$PAGE get_command - parse_command_line - main routine

var
    ind, ind1: line_index;

begin
     if parse_file_name ( command_line, 0, ind, out_name, out_switches, ['='] )
  andif parse_file_name ( command_line, ind, ind, in_name_1, in_1_switches, ['+','-','*'] )
  andif parse_file_name ( command_line, ind, ind1, in_name_2, in_2_switches, [] )
    then
      if out_name = '' then
	function_code := switches_only
      else if command_line [ind] = '+' then
	function_code := union
      else if command_line [ind] = '-' then
	function_code := difference
      else if command_line [ind] = '*' then
	function_code := intersection
      else
	function_code := copy
    else
      function_code := no_op;
end (* parse_command_line *);
$PAGE get_command - open_file

type
    io_mode = ( input_mode, output_mode );


function open_file ( var io_file: text; name: file_name; mode: io_mode ): boolean;

begin
  if mode = input_mode then begin
    reset (io_file,name);
    open_file := not eof(io_file);
    if not open_file then
      writeln (tty,'% BAD INPUT FILE "',name,'"');
  end
  else begin
    rewrite (io_file,name);
    open_file := eof(io_file);
    if not open_file then
      writeln (tty,'% BAD OUTPUT FILE "',name,'"');
  end;
end (* open_file *);
$PAGE get_command - build_key_lists - parse_key

function build_key_lists ( switches: switch_list; var keys: key_set ): boolean;

    function parse_key ( switch: switch_text; var keys: key_set ): boolean;

	function append_key ( var list: key_list; key: key_value; code: char ): boolean;
	begin
	  with list do begin
	    append_key := (count <> max_keys);
	    if append_key then begin
	      count := count + 1;
	      value [count] := key;
	    end
	    else
	      writeln (tty,'% TOO MANY ',code,'-KEYS');
	  end (* with list *);
	end (* append_key *);

	function evaluate_key ( text: switch_text; tind: switch_index;
				var key: key_value ): boolean;
	var ind: 0 .. switch_len;
	begin
	  with key do begin
	    if text [tind+1] = 'N' then begin
	      kind := numeric;
	      ind := tind+1;
	    end
	    else if text [ind+1] = 'U' then begin
	      kind := upperalpha;
	      ind := tind + 1;
	    end
	    else begin
	      kind := alpha;
	      ind := tind;
	    end;
	    if text [ind+1] = 'R' then begin
	      reversed := true;
	      ind := ind + 1;
	    end
	    else
	      reversed := false;
	    evaluate_key :=
	      get_index ( text, ind, posn, 1, ['.'] ) andif
	      get_index ( text, ind, len, line_max, [] );
	    if evaluate_key and (posn = 0) then begin
	      writeln (tty,'% KEY LOCATION 0 NOT ALLOWED IN /',text);
	      evaluate_key := false;
	    end;
	  end (* with key *);
	end (* evaluate_key *);
$PAGE get_command - build_key_lists - main routine

    var
	lists: char_set;
	ind: 0 .. switch_len;
	key: key_value;

    begin (* parse_key *);
      if [switch[1],switch[2]] = ['D','K'] then begin
	lists := ['D','K'];
	ind := 2;
      end
      else begin
	lists := [switch[1]];
	ind := 1;
      end;
      parse_key :=
	evaluate_key ( switch, ind, key ) andif
	( not ('C' in lists) orif append_key (keys.clist,key,'C') ) andif
	( not ('D' in lists) orif append_key (keys.dlist,key,'D') ) andif
	( not ('K' in lists) orif append_key (keys.klist,key,'K') );
    end (* parse_key *);


var
    ind: 0 .. max_switches;

begin (* build_key_lists *);
  keys.clist.count := 0;
  keys.dlist.count := 0;
  keys.klist.count := 0;
  build_key_lists := true;
  ind := 0;
  with switches do
    while build_key_lists and (ind <> count) do begin
      ind := ind + 1;
      if text [ind] [1] in ['C','D','K'] then
	build_key_lists := parse_key ( text [ind], keys );
    end;
end;
$PAGE get_command - check_key_lists

function check_key_lists ( var set1, set2: key_set ): boolean;

    function match_lists ( code: char; var list1, list2: key_list;
			   force_one_key: boolean ): boolean;

    const standard_key: key_value = ( alpha, false, 1, line_max );

    var ind: 1 .. max_keys;

    begin
      if (list1.count = 0) and (list2.count = 0) and force_one_key then begin
	list1.count := 1;
	list1.value [1] := standard_key;
	list2.count := 1;
	list2.value [1] := standard_key;
      end
      else if (list1.count = 0) and (list2.count <> 0) then
	list1 := list2
      else if (list1.count <> 0) and (list2.count = 0) then
	list2 := list1;
      match_lists := (list1.count = list2.count);
      if not match_lists then
	writeln (tty, '% THE ', code, '-KEY LISTS ARE DIFFERENT LENGTHS')
      else begin
	for ind := 1 to list1.count do begin
	  match_lists := (list1.value[ind].reversed = list2.value[ind].reversed) and
			 ( (list1.value[ind].kind = numeric) =
			   (list2.value[ind].kind = numeric) );
	exit if not match_lists do
	  writeln (tty, '% KEY ', ind, ' IS INCOMPATIBLE IN THE ', code, '-KEY LISTS');
	end;
      end;
    end (* match_lists *);

begin
  check_key_lists :=
    match_lists ( 'C', set1.clist, set2.clist, false ) andif
    match_lists ( 'D', set1.dlist, set2.dlist, false ) andif
    match_lists ( 'K', set1.klist, set2.klist, true );
end (* check_key_lists *);
$PAGE get_command - collect_options

function collect_options ( in_1_switches, in_2_switches, out_switches: switch_list;
			   var options: option_list ): boolean;

    function get_options_from ( switches: switch_list; var options: option_list;
				keys_allowed: boolean ): boolean;
    var ind: 1 .. max_switches;
	ich: switch_index;
    begin
      get_options_from := true;
      with switches, options do
	for ind := 1 to count do
	  if text [ind] [1] in ['C','D','K'] then
	    if keys_allowed then
	      (* ignore it *)
	    else begin
	      writeln (tty,'% /',text[ind],'IS NOT AN OUTPUT SWITCH');
	      get_options_from := false;
	    end
	  else if text [ind] [1] = 'E' then
	    exit_option := true
	  else if text [ind] [1] = 'R' then
	    if rec_option then begin
	      writeln (tty,'% MULTIPLE /R SWITCHES');
	      get_options_from := false;
	    end
	    else begin
	      rec_option := true;
	      ich := 1;
	      get_options_from := get_index ( text[ind], ich, rec_len, 80, [] );
	    end
	  else if text [ind] [1] = 'S' then
	    sum_option := true
	  else begin
	    writeln (tty,'% UNKNOWN SWITCH /',text[ind]);
	    get_options_from := false;
	  end;
    end (* get_options_from *);

begin
  with options do begin
    num_option := false;
    rec_option := false;
    sum_option := false;
    exit_option := false;
  end;
  collect_options :=
    get_options_from ( out_switches, options, false ) andif
    get_options_from ( in_1_switches, options, true ) andif
    get_options_from ( in_2_switches, options, true );
end (* collect_options *);
$PAGE get_command - main routine

begin
  read_command_line ( command_line );

  parse_command_line ( command_line, in_name_1, in_name_2, out_name,
		       in_1_switches, in_2_switches, out_switches,
		       function_code );

  if function_code = switches_only then begin
    if collect_options ( in_1_switches, in_2_switches, out_switches, options ) then;
    function_code := no_op;
  end
  else if (function_code <> no_op) andif
    open_file ( in_file_1, in_name_1, input_mode ) andif
    ((function_code = copy) orif open_file ( in_file_2, in_name_2, input_mode )) andif
    build_key_lists ( in_1_switches, key_set_1 ) andif
    build_key_lists ( in_2_switches, key_set_2 ) andif
    check_key_lists ( key_set_1, key_set_2 ) andif
    open_file ( out_file, out_name, output_mode ) andif
    collect_options ( in_1_switches, in_2_switches, out_switches, options )
      then
	(* ok *)
      else
	function_code := no_op;
end (* get_command *);
$PAGE merge_files

procedure merge_files ( var in_file_1: text; key_set_1: key_set;
			var in_file_2: text; key_set_2: key_set;
			var out_file : text; options: option_list;
			function_code: op_codes );

type
    file_number = 1 .. 2;

var
    line: array [file_number] of text_line;
    clist, dlist, klist: array [file_number] of key_list;
    line_num: array [file_number] of 0..999999;
    first_line: boolean;
    last_line: text_line;
    last_file: file_number;

const
    end_file = chr(177b);
$PAGE merge_files - warning

procedure warning ( n: file_number; code: char );

var
    num: 0 .. 999999;
    txt: string [6];

begin
  num := line_num [n];
  txt := '';
  while num <> 0 do begin
    txt := chr(ord('0')+(num mod 10)) || txt;
    num := num div 10;
  end;
  writeln (tty,'[',code,' ',n:1,'/',txt,']');
end (* warning *);
$PAGE merge_files - get_line

procedure get_line ( var f: text; n: file_number );

var
    buf: packed array [1..line_max] of char;
    ind: line_index;

begin
  if eof(f) then
    line [n] := end_file
  else begin
    line_num [n] := line_num [n] + 1;
    ind := 0;
    while not eoln(f) and (ind <> line_max) do begin
      ind := ind + 1;
      buf [ind] := f^;
      get (f);
    end;
    if not eoln(f) then
      warning (n,'T');
    readln (f);
    line [n] := substr (buf,1,ind);
  end;
end (* get_line *);
$PAGE merge_files - compare - eget_field

type rel_codes = ( lss, eql, gtr );

function compare ( line1, line2: text_line; keys1, keys2: key_list ): rel_codes;

    procedure get_field ( line: text_line; key: key_value; var cline: text_line );
    var pos: line_index;
	i:  line_index;
	c: char;
	signed: boolean;
    begin
      with key do begin
	pos := min(posn,length(line)+1);
	cline := substr(line,pos,min(len,length(line)-pos+1));
	if kind = upperalpha then
	  cline := uppercase (cline)
	else if kind = numeric then begin
	  signed := false;
	  for i := 1 to length(cline) do begin
	    c := cline [i];
	    if c = '-' then
	      signed := true;
	    if c in ['0'..'9'] then
	      if signed then
		cline [i] := chr(2*ord('0')-ord(c))
	      else
		(* ok *)
	    else
	      cline [i] := ' ';
	  end;
	end;
      end;
  if (compare <> eql) andif keys1.value[ind].reversed then
    if compare = lss
      then compare := gtr
      else compare := lss;
    end (* get_field *);
$PAGE merge_files - compare - main routine

var
    ind: 1 .. max_keys;
    cline1, cline2: text_line;

const
    std_key: key_value = ( alpha, 1, line_max );

begin
  if (keys1.count = 1) andif (keys1.value[1] = std_key) andif
     (keys2.count = 1) andif (keys2.value[1] = std_key) then
      if line1 < line2 then
	compare := lss
      else if line1 > line2 then
	compare := gtr
      else
	compare := eql
  else
    if line1 = end_file then
      compare := gtr
    else if line2 = end_file then
      compare := lss
    else begin
      compare := eql;
      for ind := 1 to keys1.count do begin
	get_field ( line1, keys1.value[ind], cline1 );
	get_field ( line2, keys2.value[ind], cline2 );
	exit if cline1 < cline2  do  compare := lss;
	exit if cline1 > cline2  do  compare := gtr;
      end;
    end;
end (* compare *);
$PAGE merge_files - put_line

procedure put_line ( n: file_number );

begin
  if first_line then begin
    writeln (out_file,line[n]);
    first_line := false;
  end
  else if (dlist[n].count = 0) orif
    (compare(line[n],last_line,dlist[n],dlist[last_file]) = eql) then
      writeln (out_file,line[n])
  else if compare(line[n],last_line,clist[n],clist[last_file]) <> eql then
    warning (n,'C');
  last_file := n;
  last_line := line[n];
end (* put_line *);
$PAGE merge_files - main routine

var
    rel: rel_codes;
    comp_line: text_line;

begin
  clist [1] := key_set_1.clist;
  dlist [1] := key_set_1.dlist;
  klist [1] := key_set_1.klist;
  clist [2] := key_set_2.clist;
  dlist [2] := key_set_2.dlist;
  klist [2] := key_set_2.klist;
  line_num [1] := 0;
  line_num [2] := 0;
  first_line := true;
  case function_code of

    copy:
      repeat
	get_line (in_file_1,1);
	put_line (1);
      until line [1] = end_file;

    union:
      begin
	get_line (in_file_1,1);
	get_line (in_file_2,2);
	repeat
	  rel := compare (line[1],line[2],klist[1],klist[2]);
	  case rel of
	    lss:
	      begin
		put_line (1);
		get_line (in_file_1,1);
	      end;
	    gtr:
	      begin
		put_line (2);
		get_line (in_file_2,2);
	      end;
	    eql:
	      begin
		comp_line := line [1];
		repeat
		  put_line (1);
		  get_line (in_file_1,1);
		until compare(line[1],comp_line,klist[1],klist[1]) <> eql;
		repeat
		  put_line (2);
		  get_line (in_file_2,2);
		until compare(line[2],comp_line,klist[2],klist[1]) <> eql;
	      end
	  end (* case rel *);
	until (line [1] = end_file) and (line [2] = end_file);
      end (* union *);

    others:


  end (* case function_code *);
end (* merge_files *);
$PAGE main program

begin
  open (tty);
  rewrite (
  writeln (tty,'CMERGE, Version ',version_number);
  writeln (tty);

  repeat

    get_command ( in_file_1, key_set_1, in_file_2, key_set_2,
		  out_file, options, function_code );

    if function_code <> no_op then
      merge_files ( in_file_1, key_set_1, in_file_2, key_set_2,
		    out_file, options, function_code );

  until options.exit_option;
end (* cmerge *).
   #Co7