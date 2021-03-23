program logger options special (word), nocheck, noqbl;
$PAGE includes
$system (pasdev2)dtime.typ
$system (pasdev2)dtime.inc
$system (pasdev1)paslog.typ
$system (pasdev2)cmdutl.typ
$system (pasdev2)query.inc
$PAGE declarations
type
  alfa = packed array [1..10] of char;
  halfword =0..#o777777;
const
    page_len := 45;
    user_header :=
      '   User        runs    %   runtime    %   errors hiseg  lowseg lines included';
    total_head :=
      '                     total          total';
    sample_head :=
      '                    sample         sample';
    project_header :=
      '   Project     runs    %   runtime    %   errors hiseg  lowseg lines included'
	;
    file_header :=
      ' Filename   users ppn    errors runtime lines  incl  hiseg lowseg     date      time      options'
	;
    sfile_header :=
      '   File        runs    %   runtime    %   errors hiseg  lowseg lines included'
	;
    blanks := '                              ';
    wild_char := '?';
    wild_string := '??????';
    inst_file_name = 'LOG.TXT[52250,222]';
type
    string_6 = string[6];
    string30 = string[30];
    opts = (opt_standard, opt_summary, opt_detail, opt_hex, opt_decimal, opt_octal);
    optlist = record
      name: packed array[1..10] of char;
      abbrev: 0..10
    end;
    opt_list = array[opts] of optlist;
    comp_options = (op_ka10,op_check,op_debug,op_terse,op_ki10,op_alloc,op_main,
      op_incore,op_overlay,op_progress,op_tmpcor,op_source,op_trace,op_virtual,
	op_xref,op_double,op_special,op_autorun,op_hash);
    log_file_type= file of log_file_record;
    user_ptr = ^user_record;
    day_ptr = ^day_rec;
    comp_ptr = ^comp_rec;
    stat_ptr = ^stat_rec;
    day_rec = packed record
      next_day: day_ptr;
      comp_tree: comp_ptr;
      comp_first, comp_last: stat_ptr
    end;
    comp_rec = packed record
      left, right: comp_ptr;
      first_comp, last_comp: stat_ptr;
      f_name: alfa
    end;
    stat_rec = packed record
      next_stat: stat_ptr;
      errors: halfword;
      of_file: comp_ptr;
      chron_next: stat_ptr;
      runtim: real;
      ctime: dtime_int
    end;
    user_record = record
      left, right: user_ptr;
      errors, runs, runs_err_free, lines, incl_lines, hiseg, lowseg: halfword;
      runtim: real;
      case boolean of
	true: (
	  next: user_ptr;
	  u_ppn: integer;
	  first_day, current_day: day_ptr);
	false: (
	  file_name: alfa)
    end;
    mask = packed array[1..6] of char; (* mask for ppns and filenames *)
    sixbit = packed array[1..6] of 0..77b;
    ppn_ptr = ^ppn_filter;
    ppn_filter = record
      proj: mask;
      prog: mask;
      next: ppn_ptr (* chained list of ppn filters *)
    end;
    file_ptr = ^file_filter; (* chained list of filename masks *)
    file_filter = record
      file_mask: mask;
      next: ^file_filter
    end;
var
    log_file: log_file_type;
    current_record: log_file_record;
    out2, inst_file: text;
    output_format: opts;
    first_date: dtime_int; (* record runs only between these limits *)
    first_ppn_mask: ppn_ptr;
    first_file_mask: file_ptr;
    current_ppn_mask: ppn_ptr;
    current_file_mask: file_ptr;
    current_user, head_user, first_user,head_file, cur_file: user_ptr;
    convert: packed record
      case boolean of
	true: (
	  fb: filblock);
	false: (
	  device: sixbit;
	  name: sixbit;
	  extension: sixbit;
	  case boolean of
	    true: (
	      ppn: integer);
	    false: (
	      ppn_chars: packed array[1..12] of 0..7b))
    end;
    record_count, global_count, global_err_free, net_err_free: integer;
    log_header: string;
    lines_out: integer;
    char_: char;
    str, src_file_name: string;
    not_first: boolean;
    specific: boolean;
    net_errors, net_lines, net_incl_lines, net_hiseg, net_lowseg, total_errors,
      total_lines, total_incl_lines, total_hiseg, total_lowseg: integer;
    total_runtime, net_runtime: real;
    count_alloc, count_ka10, count_ki10, count_debug, count_trace, count_double,
      count_check, count_main, count_overlay, count_progress, count_source,
	count_auto_run, count_tmpcor, count_hash,
	  count_special, count_terse, count_xref, count_virtual: integer;
    net_alloc, net_ka10, net_ki10, net_debug, net_trace, net_double, net_check,
      net_main, net_overlay, net_progress, net_source, net_special, net_terse,
	net_tmpcor, net_hash, net_auto_run,
	  net_xref, net_virtual: integer;
    err_count: array[0..21] of integer;
    net_err_count: array[0..21] of integer;
    err_ix: integer;
    listing: boolean;
    i, ix: integer;
    max_errors, max_hiseg, max_lowseg, max_lines, max_incl_lines: integer;
    max_runtime: real;
    n_max_errors, n_max_hiseg, n_max_lowseg, n_max_lines, n_max_incl_lines:
      integer;
    n_max_runtime: real;
    all_ppns, all_files: boolean;
    this_proj, this_prog, this_file: mask;
    last_proj: mask := '      ';
    last_prog: mask := '      ';
    day_names: array[sunday..saturday] of packed array[1..4] of char := ( 'Sun '
      ,'Mon ','Tue ','Wed ','Thu ','Fri ','Sat ');
    last_date_reported: date_int;
    last_date: dtime_int;
    option_name: opt_list := (
      ('STANDARD  ',3),
      ('SUMMARY   ',3),
      ('DETAIL    ',3),
      ('HEX       ',3),
      ('DECIMAL   ',3),
      ('OCTAL     ',3));
    opt: opts;
    comp_op: comp_options;
    option_used: array[comp_options] of boolean;
    summarize: boolean;

type
  option_detail = array[comp_options] of record
    option_char: char;
    option_expl: string[30]
  end;

const
  option_summary: option_detail := (
	('A','KA10'),
	('C','Check'),
	('D','Debug'),
	('E','Terse'),
	('I','KI10'),
	('L','Alloc'),
	('M','Main'),
	('N','Incore'),
	('O','Overlay'),
	('P','Progress'),
	('R','Tmpcor'),
	('S','Source'),
	('T','Trace (and not Debug)'),
	('V','Virtual'),
	('X','Xref'),
	('2','Double'),
	('&','Special'),
	('@','Auto-run'),
	('#','###nnn.tmp command file'));

external function lookup_opt (
	cmdline; var cmdlineidx; var opt_list; opts; var opts): boolean;
$PAGE init_everything
procedure init_everything;
var
  d_err: dtime_err;
begin
  last_date_reported.d := minimum (last_date_reported.d);
  first_file_mask := nil;
  first_ppn_mask := nil;
  first_file_mask := nil;
  max_errors := 0;
  max_hiseg := 0;
  max_lowseg := 0;
  max_lines := 0;
  max_incl_lines := 0;
  max_runtime := 0;
  global_count := 0;
  record_count := 0;
  global_err_free := 0;
  net_err_free := 0;
  for ix := lowerbound(err_count) to upperbound(err_count) do begin
    net_err_count[ix] := 0;
    err_count[ix] := 0;
  end;
  for comp_op := minimum (comp_options) to maximum (comp_options) do
    option_used[comp_op] := false;
  head_user := nil;
  first_user := nil;
  head_file := nil;
  total_errors := 0;
  total_lines := 0;
  total_incl_lines := 0;
  total_runtime := 0;
  total_hiseg := 0;
  total_lowseg := 0;
  count_alloc := 0;
  count_ka10 := 0;
  count_ki10 := 0;
  count_debug := 0;
  count_trace := 0;
  count_double := 0;
  count_check := 0;
  count_main := 0;
  count_overlay := 0;
  count_progress := 0;
  count_source := 0;
  count_special := 0;
  count_terse := 0;
  count_xref := 0;
  count_auto_run := 0;
  count_tmpcor := 0;
  count_hash := 0;
end;
$PAGE width, width8
function width (val: integer): integer;
var i: integer;
begin
  i := val;
  width := 0;
  repeat
    width := width + 1;
    i := i div 10
  until i = 0;
end;
function width8 (val: integer): integer;
var i: integer;
begin
  i := val;
  width8 := 0;
  repeat
    width8 := width8 + 1;
    i := i div 10b;
  until i = 0;
end;
$PAGE num_string, hex, octal, decimal

function num_string (ival: integer; radix: integer): string_6;
var
  val: integer;
  i, ix: integer;
  str1, str2: packed array[1..6] of char;
type
  conversion = packed array[1..16] of char;
const
  numbers: conversion := '0123456789ABCDEF';
begin
  val := ival;
  ix := 0;
  repeat
    ix := ix + 1;
    str1[ix] := numbers[1 + val mod radix];
    val := val div radix;
  until val = 0;
  for i := 1 to ix do
    str2[ix+1-i] := str1[i];
  num_string := substr (str2, 1, ix);
end;
function octal (val: integer): string_6;
begin
  octal := num_string (val, 10b);
end;
function decimal (val: integer): string_6;
begin
  decimal := num_string (val, 10);
end;
function hex (val: integer): string_6;
begin
  hex := num_string (val, #h10);
end;
$PAGE sval, both_sval, both_str, bothln_str, both_blank

procedure sval (
	var f: text;
	s: string30;
	field_width: integer;
	trailing_blanks: integer);
begin
  if field_width <> 0 then
    write (f,' ':max(0,field_width-length(s)));
  write (f,s,' ':trailing_blanks);
end;
procedure both_sval (
	var f: text;
	s: string30;
	field_width: integer;
	trailing_blanks: integer);
begin
  sval (f, s, field_width, trailing_blanks);
  sval (output, s, field_width, trailing_blanks);
end;
procedure both_blank (var f: text; width: integer);
begin
  write (f,' ':width);
  write (output,' ':width);
end;
procedure both_str (var f: text; str: string);
begin
  write (f,str);
  write (output,str);
end;
procedure bothln_str (var f: text; str: string);
begin
  writeln (f,str);
  writeln (output,str);
end;
$PAGE both_line, both_char

procedure both_line (var f: text);
begin
  writeln (f);
  writeln (output);
end;
procedure both_char (var f: text; ch: char);
begin
  write (f,ch);
  write (output,ch);
end;
$PAGE ival, both_ival
type radix_base = array[opt_hex..opt_octal] of integer;
const radix: radix_base := (#h10, 10, 10b);
procedure ival (
	var f: text;
	must_print: boolean;
	value: integer;
	field_width: integer;
	trailing_blanks: integer;
	format: opts);
begin
  if must_print orif (value <> 0) then
      sval (f, num_string (value, radix[format]), field_width, trailing_blanks)
  else
    write (f, ' ':field_width + trailing_blanks);
end;
procedure both_ival (
	var f: text;
	must_print: boolean;
	value: integer;
	field_width: integer;
	trailing_blanks: integer;
	format: opts);
begin
  ival (output, must_print, value, field_width, trailing_blanks, format);
  ival (f, must_print, value, field_width, trailing_blanks, format);
end;
$PAGE rval, both_rval

procedure rval (
	var f: text;
	value: real;
	field_width: integer;
	precision: integer;
	trailing_blanks: integer);
begin
  write (f, value:field_width:precision,' ':trailing_blanks);
end;
procedure both_rval (
	var f: text;
	value: real;
	field_width: integer;
	precision: integer;
	trailing_blanks: integer);
begin
  rval (f, value, field_width, precision, trailing_blanks);
  rval (output, value, field_width, precision, trailing_blanks);
end;
$PAGE check_len, high_size
procedure check_len;
begin
  if listing then begin
    if lines_out >= page_len then begin
      page (output);
      lines_out := 0;
    end;
    lines_out := lines_out + 1;
  end;
end;

function high_size (size: integer): integer;
begin
  if output_format = opt_octal
    then high_size := size + 400000b
    else high_size := size;
end;
$PAGE set_mask
function set_mask (str: string_6): mask;
var
    i: integer;
begin
  set_mask := '      ';
  if search (str,['*','?']) = 0 then
    set_mask[1:length(str)] := str
  else if str[1] = '*' then
    if length(str) = 1 then
      set_mask := wild_string
    else begin
      set_mask[6-length(str)+2:length(str)-1] := substr (str,2);
      set_mask[1:6-length(str)+1] := substr (wild_string,6-length(str));
    end
  else begin
    set_mask[1:length (str)-1] := substr (str,1,length(str)-1);
    set_mask[length(str):6-length(str)+1] := substr (wild_string,1,6-length(str)
      +1) ;
  end;
end;
$PAGE filter_ppn
function filter_ppn: boolean;
label 1;
begin
  current_ppn_mask := first_ppn_mask;
  while current_ppn_mask <> nil do
    with current_ppn_mask^ do begin
      for i := 1 to 6 do
	if (proj[i] <> wild_char) andif (proj[i] <> this_proj[i]) orif
	  (prog[i] <> wild_char) andif (prog[i] <> this_prog[i]) then
	    goto 1;
      filter_ppn := true;
      return;
      1:
	current_ppn_mask := next;
    end;
  filter_ppn := false;
end;
$PAGE filter_file
function filter_file: boolean;
label 1;
begin
  current_file_mask := first_file_mask;
  while current_file_mask <> nil do
    with current_file_mask^ do begin
      for i := 1 to 6 do
	if (file_mask[i] <> wild_char) andif (file_mask[i] <> this_file[i]) then
	  goto 1;
      filter_file := true;
      return;
      1:
	current_file_mask := next;
    end;
  filter_file := false;
end;
$PAGE check_option

procedure check_option (option_flag: boolean; var counter: integer);

begin
  if option_flag then
    counter := counter + 1;
end;
$PAGE check_err
procedure check_err (w: integer);
const
    space := 36;
    net_space := 52;
begin
  while (err_ix < upperbound (err_count)) andif (err_count[err_ix] = 0) do
    err_ix := err_ix + 1;
  if (err_ix <= upperbound (err_count)) andif (err_count[err_ix] <> 0)
    then begin
      if specific then begin
	if w < net_space then
	  both_blank (ttyoutput,net_space-w);
      end
      else if w < space then
	both_blank (ttyoutput,space-w);
      if err_ix = 0 then
	both_str (ttyoutput,'None')
      else begin
	both_ival (ttyoutput,true,err_ix,3,0,opt_decimal);
	if err_ix = upperbound (err_count)
	  then both_char (ttyoutput,'+')
	  else both_char (ttyoutput,' ');
      end;
      both_rval (ttyoutput,err_count[err_ix]*100/global_count,7,2,0);
      both_char (ttyoutput,'%');
      if specific andif (net_err_count[err_ix] <> 0) then begin
	both_blank (ttyoutput,2);
	both_rval (ttyoutput,net_err_count[err_ix]*100/record_count,7,2,0);
	both_char (ttyoutput,'%');
      end;
    end;
  both_line (ttyoutput);
  err_ix := err_ix + 1;
end;
$PAGE report_option
type
    string20 = string[20];
procedure report_option (str: string20; count, n_count: integer);
const
    blanks := '                 ';
begin
  if count <> 0 then begin
    both_blank (ttyoutput,4);
    both_sval (ttyoutput,str,length(str),9-length(str));
    both_rval (ttyoutput,count*100/global_count,6,2,0);
    both_char (ttyoutput,'%');
    if specific andif (n_count <> 0) then begin
      both_char (ttyoutput,' ');
      both_rval (ttyoutput,n_count*100/record_count,6,2,0);
      both_char (ttyoutput,'%');
      check_err (28);
    end
    else
      check_err (20);
  end;
end;
$PAGE find_user
procedure find_user (ppn: integer);
var
    last_user, prev_user: user_ptr;
begin
  if head_user <> nil then begin
    current_user := head_user;
    while current_user <> nil do
      with current_user^ do begin
	last_user := current_user;
	if ppn = u_ppn then
	  return;
	if ppn < u_ppn then
	  current_user := left
	else
	  current_user := right;
      end;
  end;
  new (current_user);
  if head_user = nil then
    head_user := current_user
  else if ppn < last_user^.u_ppn then
    last_user^.left := current_user
  else
    last_user^.right := current_user;
  with current_user ^ do begin
    left := nil;
    right := nil;
    u_ppn := ppn; (* assume rest zeroed by new *)
    first_day := nil;
    current_day := nil;
  end;
  prev_user := nil;
  last_user := first_user;
  while (last_user <> nil) andif (last_user^.u_ppn < ppn) do begin
    prev_user := last_user;
    last_user := last_user^.next;
  end;
  if prev_user = nil then
    first_user := current_user
  else
    prev_user^.next := current_user;
  current_user^.next := last_user;
end;
$PAGE update_user
procedure update_user;
begin
  with current_record do begin
    find_user (users_ppn);
    with current_user^ do begin
      i := min (no_errors,upperbound(err_count));
      net_err_count[i] := net_err_count[i] + 1;
      errors := errors + no_errors;
      runs := runs + 1;
      if no_errors = 0 then
	runs_err_free := runs_err_free + 1;
      lines := lines + no_lines;
      incl_lines := incl_lines + no_incl_lines;
      hiseg := hiseg + highseg_size;
      lowseg := lowseg + lowseg_size;
      runtim := runtim + run_time / 1000;
      net_errors := net_errors + no_errors;
      net_lines := net_lines + no_lines;
      net_incl_lines := net_incl_lines + no_incl_lines;
      net_hiseg := net_hiseg + highseg_size;
      net_lowseg := net_lowseg + lowseg_size;
      net_runtime := net_runtime + run_time/1000;
      n_max_errors := max (n_max_errors,no_errors);
      n_max_lines := max (n_max_lines,no_lines);
      n_max_incl_lines := max (n_max_incl_lines,no_incl_lines);
      n_max_hiseg := max (n_max_hiseg,highseg_size);
      n_max_lowseg := max (n_max_lowseg,lowseg_size);
      n_max_runtime := max (n_max_runtime,run_time/1000);
    end;
    check_option (alloc_strategy > 0, net_alloc);
    check_option (not kl10 and ka10_ki10, net_ki10);
    check_option (not (kl10 or ka10_ki10), net_ka10);
    check_option (opt_debug, net_debug);
    check_option (not opt_debug and opt_trace, net_trace);
    check_option (opt_double, net_double);
    check_option (opt_check, net_check);
    check_option (opt_main, net_main);
    check_option (opt_overlay, net_overlay);
    check_option (opt_progress, net_progress);
    check_option (opt_source, net_source);
    check_option (opt_special, net_special);
    check_option (opt_terse, net_terse);
    check_option (opt_xref, net_xref);
    (* check_option (opt_incore, net_incore); *)
    check_option (opt_virtual (* and not opt_incore *), net_virtual);
    check_option (opt_auto_run, net_auto_run);
    check_option (opt_tmpcor, net_tmpcor);
    check_option (opt_hash, net_hash);
  end;
end;
$PAGE find_file
procedure find_file;
var
    prev_file: user_ptr;
begin
  cur_file := head_file;
  while cur_file <> nil do
    with cur_file^ do begin
      if file_name[1:6] = this_file then
	return;
      prev_file := cur_file;
      if file_name[1:6] < this_file then
	cur_file := right
      else
	cur_file := left;
    end;
  new (cur_file);
  with cur_file^ do begin
    left := nil;
    right :;
    if head_file = nil then
      head_file := cur_file
    else if prev_file^.file_name[1:6] < this_file then
      prev_file^.right := cur_file
    else
      prev_file^.left := cur_file;
    file_name[1:6] := this_file;
    file_name[7:4] := '    ';
  end;
end;
$PAGE update_file
procedure update_file;
begin
  with current_record do begin
    find_file;
    with cur_file^ do begin
      errors := errors + no_errors;
      runs := runs + 1;
      if no_errors = 0 then
	runs_err_free := runs_err_free + 1;
      lines := lines + no_lines;
      incl_lines := incl_lines + no_incl_lines;
      hiseg := hiseg + highseg_size;
      lowseg := lowseg + lowseg_size;
      runtim := runtim + run_time / 1000;
    end;
  end;
end;
$PAGE report_stats
type
    record_type = (full_ppn, project_only, file_only);
procedure report_stats (u: user_ptr; which: record_type);
var
    i: integer;
    s: string;
    str: string;
    r: real;
begin
  with u^ do begin
    if lines_out >= page_len then begin
      if which <> project_only
	then page (output)
	else writeln;
      if specific then
	if which = project_only
	  then bothln_str (ttyoutput,total_head)
	  else bothln_str (ttyoutput,sample_head);
      case which of
	full_ppn:
	  bothln_str (ttyoutput,user_header);
	project_only:
	  bothln_str (ttyoutput,project_header);
	file_only:
	  bothln_str (ttyoutput,sfile_header)
      end;
      writeln;
      lines_out := 2 + ord (specific);
    end;
    lines_out := lines_out + 1;
    if which = file_only then
      str := file_name
    else begin
      str := octal (u_ppn div 1000000b);
      if which = full_ppn then
	str := str || ',' || octal (u_ppn mod 1000000b);
    end;
    both_sval (ttyoutput,str,0,14-length(str));
    if specific andif (which <> project_only)
      then r := runs*100/record_count
      else r := runs*100/global_count;
    both_ival (ttyoutput,true,runs,4,0,opt_decimal);
    both_rval (ttyoutput,r,8,2,1);
    if specific andif (which <> project_only)
      then r := runtim*100/net_runtime
      else r := runtim*100/total_runtime;
    both_rval (ttyoutput,runtim/runs,6,2,2);
    both_rval (ttyoutput,r,6,2,1);
    if errors = 0
      then both_str (ttyoutput,' none  ')
      else both_rval (ttyoutput,errors/runs,5,2,2);
    both_ival (ttyoutput,true,high_size(hiseg div runs_err_free),6,1,output_format);
    both_ival (ttyoutput,false,lowseg div runs_err_free,6,1,output_format);
    both_ival (ttyoutput,true,lines div runs,5,1,opt_decimal);
    if incl_lines <> 0 then begin
      both_char (ttyoutput,' ');
      both_ival (ttyoutput,true,incl_lines div runs,6,0,opt_decimal);
    end;
    both_line (ttyoutput);
  end;
end;
$PAGE report_users
procedure report_users;
var
    u: user_ptr;
begin
  lines_out := maximum (lines_out);
  u := first_user;
  while u <> nil do begin
    report_stats (u, full_ppn);
    u := u^.next;
  end;
  writeln (tty);
end;
$PAGE report_projects
procedure report_projects;
var
    p, next_ppn: user_ptr;
    proj: integer;
begin
  lines_out := maximum (lines_out);
  p := first_user;
  while p <> nil do
    with p^ do begin
      proj := u_ppn div 1000000b;
      next_ppn := next;
      while (next_ppn <> nil) andif (next_ppn^.u_ppn div 1000000b = proj)
	do begin
	  errors := errors + next_ppn^.errors;
	  runs := runs + next_ppn^.runs;
	  runs_err_free := runs_err_free + next_ppn^.runs_err_free;
	  lines := lines + next_ppn^.lines;
	  incl_lines := incl_lines + next_ppn^.incl_lines;
	  hiseg := hiseg + next_ppn^.hiseg;
	  lowseg := lowseg + next_ppn^.lowseg;
	  runtim := runtim + next_ppn^.runtim;
	  next_ppn := next_ppn^.next;
	end;
      report_stats (p,project_only);
      p := next_ppn;
    end;
  writeln (tty);
end;
$PAGE report_files
procedure report_files;
var
    f: user_ptr;
  procedure walk_files (f: user_ptr);
  begin
    if f <> nil then
      with f^ do begin
	walk_files (left);
	report_stats (f,file_only);
	walk_files (right);
      end;
  end;
begin
  if head_file <> nil then begin
    lines_out := maximum (lines_out);
    walk_files (head_file);
  end;
  writeln (tty);
end;
$PAGE summary
procedure summary;
const
    header1 :=
      '                  Average             Maximum         Compilation errors'
	;
    header2 :=
      '              total   sample      total   sample           total    sample'
	;
    header3 := '            Average    Maximum    Compilation errors';
var
    indent: integer;
    t_lines,t_incl_lines, t_hiseg, t_lowseg: integer;
    t_runtime: real;
    n_lines,n_incl_lines, n_hiseg, n_lowseg: integer;
    n_runtime: real;
begin
  if specific then
    indent := 52
  else
    indent := 34;
  if lines_out > 0 then
    page (output);
  writeln (tty);
  bothln_str (ttyoutput,log_header);
  if specific then begin
    both_ival (ttyoutput,true,record_count,0,0,opt_decimal);
    both_str (ttyoutput,' of ');
    both_ival (ttyoutput,true,global_count,0,1,opt_decimal);
    bothln_str (ttyoutput,'records in sample.');
  end
  else begin
    both_ival (ttyoutput,true,record_count,0,1,opt_decimal);
    bothln_str (ttyoutput,'records.');
  end;
  both_line (ttyoutput);
  t_lines := total_lines div global_count;
  t_incl_lines := total_incl_lines div global_count;
  t_runtime := total_runtime / global_count;
  t_hiseg := total_hiseg div global_err_free;
  t_lowseg := total_lowseg div global_err_free;
  n_lines := net_lines div record_count;
  n_incl_lines := net_incl_lines div record_count;
  n_runtime := net_runtime / record_count;
  n_hiseg := net_hiseg div net_err_free;
  n_lowseg := net_lowseg div net_err_free;
  if specific then begin
    bothln_str (ttyoutput,header1);
    bothln_str (ttyoutput,header2);
  end
  else bothln_str (ttyoutput,header3);
  both_str (ttyoutput,'Errors      ');
  both_rval (ttyoutput,total_errors/global_count,7,3,2);
  if specific then
    if net_errors = 0
      then both_str (ttyoutput,'  None   ')
      else both_rval (ttyoutput,net_errors/record_count,7,3,2);
  both_blank (ttyoutput,2);
  both_ival (ttyoutput,true,max_errors,7,2,opt_decimal);
  if specific then
    both_ival (ttyoutput,true,n_max_errors,7,2,opt_decimal);
  both_blank (ttyoutput,2);
  check_err (indent);
  both_str (ttyoutput,'Runtime     ');
  both_rval (ttyoutput,total_runtime/global_count,7,3,2);
  if specific then
    both_rval (ttyoutput,net_runtime/record_count,7,3,2);
  both_blank (ttyoutput,2);
  both_rval (ttyoutput,max_runtime,7,3,2);
  if specific then
    both_rval (ttyoutput,n_max_runtime,7,3,2);
  both_blank (ttyoutput,2);
  check_err (indent);
  both_str (ttyoutput,'Lines       ');
  both_ival (ttyoutput,true,t_lines,7,2,opt_decimal);
  if specific then
    both_ival (ttyoutput,true,n_lines,7,2,opt_decimal);
  both_blank (ttyoutput, 2);
  both_ival (ttyoutput,true,max_lines,7,2,opt_decimal);
  if specific then
    both_ival (ttyoutput,true,n_max_lines,7,2,opt_decimal);
  both_blank (ttyoutput,2);
  check_err (indent);
  both_str (ttyoutput,'(included)  ');
  both_ival (ttyoutput,true,t_incl_lines,7,2,opt_decimal);
  if specific then
    both_ival (ttyoutput,true,n_incl_lines,7,2,opt_decimal);
  both_blank (ttyoutput,2);
  both_ival (ttyoutput,true,max_incl_lines,7,2,opt_decimal);
  if specific then
    both_ival (ttyoutput,true,n_max_incl_lines,7,2,opt_decimal);
  both_blank (ttyoutput,2);
  check_err (indent);
  both_str (ttyoutput,'Hiseg       ');
  both_ival (ttyoutput,true,high_size(t_hiseg),7,2,output_format);
  if specific then
    both_ival (ttyoutput,true,high_size (n_hiseg),7,2,output_format);
  both_blank (ttyoutput,2);
  both_ival (ttyoutput,true,high_size (max_hiseg),7,2,output_format);
  if specific then
    both_ival (ttyoutput,true,high_size(n_max_hiseg),7,2,output_format);
  both_blank (ttyoutput,2);
  check_err (indent);
  both_str (ttyoutput,'Lowseg      ');
  both_ival (ttyoutput,true,t_lowseg,7,2,output_format);
  if specific then
    both_ival (ttyoutput,true,n_lowseg,7,2,output_format);
  both_blank (ttyoutput,2);
  both_ival (ttyoutput,true,max_lowseg,7,2,output_format);
  if specific then
    both_ival (ttyoutput,true,n_max_lowseg,7,2,output_format);
  both_blank (ttyoutput,2);
  check_err (indent);
  check_err (0);
  both_str (ttyoutput,'Options:');
  if specific then begin
    both_str (ttyoutput,'      total   sample');
    check_err (28)
  end
  else check_err (8);
  report_option ('###PAS',count_hash,net_hash);
  report_option ('ALLOC',count_alloc,net_alloc);
  report_option ('AUTORUN',count_auto_run,net_auto_run);
  report_option ('CHECK',count_check,net_check);
  report_option ('DEBUG',count_debug,net_debug);
  report_option ('DOUBLE',count_double,net_double);
  (*report_option ('INCORE',count_incore,net_incore);     *)
  report_option ('KA10',count_ka10,net_ka10);
  report_option ('KI10',count_ki10,net_ki10);
  report_option ('MAIN',count_main,net_main);
  report_option ('OVERLAY',count_overlay,net_overlay);
  report_option ('PROGRESS',count_progress,net_progress);
  report_option ('SOURCE',count_source,net_source);
  report_option ('SPECIAL',count_special,net_special);
  report_option ('TERSE',count_terse,net_terse);
  report_option ('TMPCOR',count_tmpcor,net_tmpcor);
  report_option ('TRACE',count_trace,net_trace);
  report_option ('VIRTUAL',count_virtual,net_virtual);
  report_option ('XREF',count_xref,net_xref);
  while err_ix <= upperbound (err_count) do
    check_err (0);
  both_line (ttyoutput);
end;
$PAGE sum_users

procedure sum_users;

var
  u: user_ptr;
  d: day_ptr;
  c: comp_ptr;
  s: stat_ptr;
  user_id, file_no, user_compiles, gross_compiles, user_errors, gross_errors: integer;
  seq_no: integer;
  seq_list: stat_ptr;
   proj_compiles, proj_errors, day_compiles, day_errors: integer;
  proj_runtime, day_runtime: real;
  user_runtime, gross_runtime: real;

procedure drop_totals (skip: integer; header: string; compiles: integer; rtime: real);
var i: integer;
    r: real;
begin
 if compiles > 1 then begin
  for i := 1 to skip do
    both_line (out2);
  both_str (out2,header);
  both_ival (out2,true,compiles,width(compiles),1,opt_decimal);
  both_str (out2,'compilations + ');
  i := 5 + max (0,trunc(log(rtime)));
  both_rval (out2,rtime,i,3,1);
  both_str (out2,'seconds or ');
  r := rtime / compiles;
  i := 5 + max (0,trunc(log(r)));
  both_rval (out2,r,i,3,1);
  bothln_str (out2,'seconds average.');
 end;
end;

function time_set (d: dtime_int): string;
var i: integer;
begin
  time_set := lowercase (ns_t1 (extr_time (d)));
  if time_set[1] = '0' then
    time_set[1] := ' '
  else if time_set[1:2] = '12' then begin
    i := index (time_set,'m');
    time_set[i-1] := 'p';
  end;
end;

procedure list_files (c: comp_ptr);
var s: stat_ptr;
begin
  if c^.left <> nil then list_files (c^.left);
  file_no := file_no + 1;
  s := c^.first_comp;
  while s <> nil do with s^ do begin
    if s = c^.first_comp
      then write (file_no: 6,c^.f_name:11)
      else write (' ':17);
    seq_no := seq_no + 1;
    write (out2,seq_no:6,seq_list^.of_file^.f_name:11);
    write (out2,seq_list^.runtim:10:3);
    write (runtim:10:3);
    both_str (out2,' sec');
    if errors = 0 then write (' ':12)
    else if errors = 1 then write ('    1 error ')
    else write (errors:5,' errors');
    if seq_list^.errors = 0 then write (out2,' ':12)
    else if seq_list^.errors = 1 then write (out2,'    1 error ')
    else write (out2,seq_list^.errors:5,' errors');
    both_blank (out2,3);
    writeln (out2, time_set (seq_list^.ctime));
    writeln (time_set (ctime));
    day_compiles := day_compiles + 1;
    day_runtime := day_runtime + runtim;
    day_errors := day_errors + errors;
    s := next_stat;
    seq_list := seq_list^.chron_next;
  end;
  if c^.right <> nil then list_files (c^.right);
end;

begin
  user_id := 0;
  gross_compiles := 0;
  gross_errors := 0;
  gross_runtime := 0;
  proj_compiles := 0;
  proj_errors := 0;
  proj_runtime := 0;
  u := first_user;
  while u <> nil do begin
    d := u^.first_day;
    user_errors := 0;
    user_compiles := 0;
    user_runtime := 0;
    if d <> nil then begin
      user_id := user_id + 1;
      both_str (out2,'User ID ');
      both_ival (out2,true,user_id,0,0,opt_decimal);
      both_str (out2,': ');
      both_ival (out2,true,u^.u_ppn div 1000000b,0,0,opt_octal);
      both_char (out2,',');
      both_ival (out2,true,u^.u_ppn mod 1000000b,0,0,opt_octal);
      both_line (out2);
      while d <> nil do begin
	both_str (out2,'   Date: ');
	bothln_str (out2,ns_d2 (extr_date (d^.comp_tree^.first_comp^.ctime)));
	file_no := 0;
	seq_no := 0;
	seq_list := d^.comp_first;
	day_compiles := 0;
	day_errors := 0;
	day_runtime := 0;
	list_files (d^.comp_tree);
	user_compiles := user_compiles + day_compiles;
	user_runtime := user_runtime + day_runtime;
	user_errors := user_errors + day_errors;
	drop_totals (0,'   Day total: ',day_compiles,day_runtime);
	d := d^.next_day;
      end;
    end;
    drop_totals (1,'   User total: ',user_compiles,user_runtime);
    proj_errors := proj_errors + user_errors;
    proj_compiles := proj_compiles + user_compiles;
    proj_runtime := proj_runtime + user_runtime;
    if (u^.next = nil) orif (u^.u_ppn div 1000000b <> u^.next^.u_ppn div 1000000b) then begin
      drop_totals (1,'Project total: ',proj_compiles,proj_runtime);
      gross_compiles := gross_compiles + proj_compiles;
      gross_runtime := gross_runtime + proj_runtime;
      gross_errors := gross_errors + proj_errors;
      proj_compiles := 0;
      proj_errors := 0;
      proj_runtime := 0;
    end;
    u := u^.next;
    if u <> nil then begin
      page (output);
      page (out2);
    end;
  end;
  if user_id = 0 then writeln (tty,'No compilations match specifications.')
  else begin
    drop_totals (2,'Grand total: ',gross_compiles,gross_runtime);
    both_line (out2);
    both_str (out2,'Average compilations per user: ');
    both_rval (out2,gross_compiles/user_id,7,2,0);
    both_line (out2);
    both_line (out2);
  end;
end;
$PAGE add_to_users

procedure add_to_users;
var
  d: day_ptr;
  c, prev_comp: comp_ptr;
  s: stat_ptr;

begin
  with current_record do begin
    find_user (users_ppn);
    with current_user^ do begin
      if (current_day = nil) orif (extr_date (current_day^.comp_first^.ctime).d <> extr_date (date_and_time).d) then begin
	new (d);
	d^.next_day := nil;
	d^.comp_tree := nil;
	d^.comp_first := nil;
	d^.comp_last := nil;
	if current_day = nil
	  then first_day := d
	  else current_day^.next_day := d;
	current_day := d;
      end
      else d := current_day;
    end;
    c := d^.comp_tree;
    prev_comp := nil;
    while c <> nil do begin
    exit if c^.f_name = this_file;
      prev_comp := c;
      if c^.f_name < this_file 
	then c := c^.right
	else c := c^.left;
    end;
    if c = nil then begin
      new (c);
      c^.f_name := this_file;
      c^.left := nil;
      c^.right := nil;
      c^.first_comp := nil;
      c^.last_comp := nil;
      if prev_comp = nil then d^.comp_tree := c
      else if this_file < prev_comp^.f_name then prev_comp^.left := c
      else prev_comp^.right := c;
    end;
    new (s);
    s^.errors := no_errors;
    s^.next_stat := nil;
    s^.runtim := run_time / 1000;
    s^.ctime := date_and_time;
    s^.of_file := c;
    s^.chron_next := nil;
    if d^.comp_last = nil
      then d^.comp_first := s
      else d^.comp_last^.chron_next := s;
    d^.comp_last := s;
    if c^.last_comp = nil
      then c^.first_comp := s
      else c^.last_comp^.next_stat := s;
    c^.last_comp := s;
  end;
end;
$PAGE extract
procedure extract;
var
    i: integer;
    proj_found, prog_found: boolean;
    char_: char;
begin
  with current_record, convert do begin
    fb := file_name;
    ppn := users_ppn;
    proj_found := false;
    prog_found := false;
    for i := 1 to 6 do begin
      this_file[i] := chr(40b+name[i]);
      if proj_found orif (ppn_chars[i] <> 0) then begin
	char_ := chr (ord('0')+ppn_chars[i]);
	proj_found := true;
      end
      else
	char_ := ' ';
      this_proj[i] := char_;
      if prog_found orif (ppn_chars[i+6] <> 0) then begin
	char_ := chr (ord('0')+ppn_chars[i+6]);
	prog_found := true;
      end
      else
	char_ := ' ';
      this_prog[i] := char_;
    end;
  end;
end;
$PAGE list_record
procedure list_record;
var
    this_date: date_int;
    s: string;
    i: integer;

  procedure signal_option (option_flag: boolean; op: comp_options);
  begin
    if option_flag then begin
      write (option_summary[op].option_char);
      option_used[op] := true;
    end;
  end;

begin
  with current_record, convert do begin
    if lines_out >= page_len then begin
      page(output);
      writeln (file_header);
      writeln;
      lines_out := 2;
      last_proj := '      ';
      last_prog := '      ';
      last_date_reported.d := minimum (last_date_reported.d);
    end;
    check_len;
    i := search (this_file,[' '],length(this_file)+1)-1;
    write (substr (this_file,1,i));
    if (extension[1] = 0) orif (extension[1]=ord('P')-40b) andif
      (extension[2]=ord('A')-40b)andif (extension[3]=ord('S')-40b) then
	write (' ':10-i)
      else begin
	write ('.');
	for ix := 1 to 3 do
	  write (chr(extension[ix]+40b));
	write (' ':6-i);
      end;
    write (' ');
    i := verify (this_prog,[' '],1);
    if last_proj = this_proj then begin
      if this_prog = last_prog then
	write (' ':13)
      else begin
	write (' ':6,',',substr (this_prog,i,7-i),' ':i-1);
	last_prog := this_prog;
      end;
    end
    else begin
      write (this_proj,',',substr(this_prog,i,7-i),' ':i-1);
      last_proj := this_proj;
      last_prog := this_prog;
    end;
    write (' ');
    ival (output,false,no_errors,5,2,opt_decimal);
    rval (output,run_time/1000,6,2,2);
    ival (output,true,no_lines,5,1,opt_decimal);
    ival (output,false,no_incl_lines,5,1,opt_decimal);
    if no_errors = 0 then begin
      ival (output,false,high_size(highseg_size),6,1,output_format);
      ival (output,false,lowseg_size,6,2,output_format);
    end
    else write (output,' ':15);
    this_date := extr_date (date_and_time);
    if this_date.d = last_date_reported.d then
      write (' ':11)
    else begin
      s := substr (ns_d2 (this_date),1,6);
      s[2:2] := lowercase (s[2:2]);
      if s[6] = ',' then s[6] := ' ';
      write (day_names[day_of_week(this_date)],s,' ');
      last_date_reported := this_date;
    end;
    s := lowercase (ns_t1 (extr_time (date_and_time)));
    if s[1] = '0' then
      s[1] := ' ';
    s := substr (s,1,5) || substr (s,9,3);
    if (s[1:2] = '12') then s[7] := 'p';
    write (s);
    write (' -- ');
    signal_option (not (kl10 or ka10_ki10), op_ka10);
    signal_option (opt_check, op_check);
    signal_option (opt_debug, op_debug);
    signal_option (opt_terse, op_terse);
    signal_option (ka10_ki10 and not kl10, op_ki10);
    signal_option (alloc_strategy > 0, op_alloc);
    signal_option (opt_main, op_main);
    (* signal_option (opt_incore, op_incore); *)
    signal_option (opt_overlay, op_overlay);
    signal_option (opt_progress, op_progress);
    signal_option (opt_tmpcor, op_tmpcor);
    signal_option (opt_source, op_source);
    signal_option (opt_trace and not opt_debug, op_trace);
    signal_option (opt_virtual (* and not opt_incore *), op_virtual);
    signal_option (opt_xref, op_xref);
    signal_option (opt_double, op_double);
    signal_option (opt_special, op_special);
    signal_option (opt_auto_run, op_autorun);
    signal_option (opt_hash, op_hash);
  end;
  writeln;
end;
$PAGE explain_options

procedure explain_options;

begin
  check_len;
  writeln;
  check_len;
  writeln ('Option abbreviation summary:');
  check_len;
  writeln;
  check_len;
  writeln ('Letter  Option');
  for comp_op := minimum (comp_options) to maximum (comp_options) do
    if option_used[comp_op] then with option_summary[comp_op] do begin
      check_len;
      writeln (option_char:4,' ':4,option_expl);
    end;
end;
$PAGE get_options
function get_options (str: string): boolean;
var i,j: integer;
  s: string;
begin
  s := str;
  i := 1;
  summarize := false;
  listing := false;
  output_format := opt_octal;
  while (i <= length (s)) andif (substr (s, i) <> '') do begin
    if lookup_opt (s,i,option_name,maximum(opts),opt) then begin
      case opt of
	opt_standard: begin
	  summarize := false;
	  listing := false;
	end;
	opt_summary:
	  summarize := true;
	opt_detail:
	  listing := true;
	others:
	  output_format := opt
      end;
      if (i <= length (s)) andif (s[i] = ',') then
	i := i + 1;
    end
    else begin
      get_options := false;
      return;
    end;
  end;
  get_options := true;
end;
$PAGE get_parameters
procedure get_parameters;
var
    error: dtime_err;
    j: integer;
    first_given, last_given: boolean;
    logfname: file_name;
begin
  if query (' Do you require instructions') then begin
    open (inst_file,inst_file_name);
    while not eof (inst_file) do begin
      readln (inst_file);
      while not eoln (inst_file) do begin
	read (inst_file,char_);
	write (tty,char_);
      end;
      writeln (tty);
    end;
    close (inst_file);
  end;
  loop
    write (tty,' Options: ');
    break (tty);
    readln (tty);
    str := '';
    while not eoln (tty) do begin
      read (tty,char_);
      str := str || uppercase (char_);
    end;
  exit if get_options (str);
    write (tty,'?Legal options are ');
    for opt := minimum (opts) to maximum (opts) do begin
      if opt > minimum (opts) then write (tty,', ');
      if opt = maximum (opts) then write (tty,'and ');
      write (tty,substr (option_name[opt].name,1,search(option_name[opt].name,[' '],11)-1));
    end;
    writeln (tty,'.');
  end;
  loop
    write (tty,' Directory: ');
    break;
    readln (tty);
    src_file_name := '';
    while not eoln (tty) do begin
      read (tty,char_);
      src_file_name := src_file_name || uppercase (char_);
    end;
    if length (src_file_name) > 0 then
      if src_file_name[1] = '[' then
	if src_file_name[length(src_file_name)] <> ']' then
	  src_file_name := src_file_name || ']';
    reset (log_file,'PASCAL.LOG ' || src_file_name);
  exit if not eof (log_file) ;
    writeln (tty,'Can''t open file PASCAL.LOG',src_file_name);
  end;
  logfname := filename (log_file);
  rewrite (output,'paslog.lst');
  if summarize then begin
    rewrite (out2,'paslog.tmp');
    log_header := '*** Scan of ' || logfname;
    write (tty,' System: '); break (tty);
    readln (tty);
    if not eoln (tty) then begin
      log_header := log_header || ' on system ';
      while not eoln (tty) do begin
	read (tty,char_);
	log_header := log_header || char_;
      end;
    end;
    if not summarize then log_header := log_header || ' ***';
    bothln_str (out2, log_header);
    both_line (out2);
  end;
  init_everything;
  log_header := '*** Scan of ' || logfname || ' on ';
  str := ns_d2 (extr_date (daytime));
  str[2:2] := lowercase (str[2:2]);
  log_header := log_header || str || ' at ';
  str := lowercase (ns_t1 (extr_time(daytime)));
  if str[1] = '0' then
    str := substr (str,2);
  log_header := log_header || str;
  if not summarize then
    log_header := log_header || ' ***';
  if summarize then begin
    bothln_str (out2, log_header);
  end;
  write (tty,' Projects: ');
  break;
  readln (tty);
  str := '';
  while not eoln (tty) do begin
    read (tty,char_);
    if char_ <> ' ' then
      str := str || char_;
  end;
  all_ppns := str = '';
  first_ppn_mask := nil;
  if not all_ppns then
    repeat
      new (current_ppn_mask);
      with current_ppn_mask^ do begin
	next := first_ppn_mask;
	first_ppn_mask := current_ppn_mask;
	i := verify (str,['0'..'7','*','?'],length(str)+1);
	if search(substr(str,1,i-1),['*']) <> 0 then
	  proj := set_mask (substr(str,1,i-1))
	else proj := set_mask (substr(blanks,1,6-(i-1))||substr(str,1,i-1));
	if (i < length(str)) andif (str[i] = ',') then begin
	  str := substr (str,i+1);
	  i := verify (str,['0'..'7','*','?'],length(str)+1);
	  if search (substr(str,1,i-1),['*']) <> 0 then
	    prog := set_mask (substr(str,1,i-1))
	  else prog := set_mask (substr(blanks,1,6-(i-1))||substr(str,1,i-1));
	  if prog = '      ' then
	    prog := wild_string;
	end
	else
	  prog := wild_string;
	if i <= length(str) then
	  str := substr (str,i+1);
      end;
    until (i > length (str)) orif (str = '');
  write (tty,' Files: ');
  break;
  readln (tty);
  str := '';
  while not eoln (tty) do begin
    read (tty,char_);
    if char_ <> ' ' then
      str := str || uppercase (char_);
  end;
  all_files := str = '';
  if not all_files then
    repeat
      new (current_file_mask);
      with current_file_mask^ do begin
	next := first_file_mask;
	first_file_mask := current_file_mask;
	i := verify (str,['A'..'Z','0'..'9','?','*'],length(str)+1);
	file_mask := set_mask (substr (str,1,i-1));
	if i <= length (str) then
	  if str[i] = '.' then
	    repeat
	      i := i + 1
	    until (i > length(str)) orif (str[i] = ';');
	if i <= length (str) then
	  str := substr (str,i+1);
      end;
    until (i > length(str)) orif (str = '');
  loop
    first_given := false;
    last_given := false;
    write (tty,' Dates: ');
    break;
    readln (tty);
    str := '';
    while not eoln (tty) do begin
      read (tty,char_);
      str := str || uppercase (char_);
    end;
  exit if str = '' do begin
      ec_ext (error,day_zero,first_date);
      last_date := add_days (daytime,1);
    end;
    i := search (str,['-'],length(str)+1);
    ec_ext (error,substr(str,1,i-1),first_date);
    if error <> dt_err then begin
      first_given := true;
      if i >= length (str) then
	last_date := add_days (daytime,1)
      else begin
	ec_ext (error,substr (str,i+1),last_date);
	last_given := true;
      end
    end;
  exit if error = dt_noerr;
    writeln (tty,'?Invalid date/time.');
  end;
  if summarize then begin
    if first_given then begin
      str := ns_d2 (extr_date (first_date));
      str[2:2] := lowercase (str[2:2]);
      both_line (out2);
      both_str (out2,'*** Scan of ' || logfname || ' from ' || str || ' to ');
      if last_given then begin
	str := ns_d2 (extr_date (last_date));
	str[2:2] := lowercase (str[2:2]);
	bothln_str (out2, str);
      end
      else bothln_str (out2,'present');
    end;
    page (output);
    page (out2);
  end;
  if listing then begin
    writeln (log_header);
    writeln;
    writeln ('Specified hiseg/lowseg limits are ',option_name[output_format].name);
    writeln;
    writeln (file_header);
    writeln;
    lines_out := 6;
  end
  else
    lines_out := 0;
end;
$PAGE report_totals
procedure report_totals;
var
  hours, mins, secs, time: integer;
begin
  time := round (total_runtime);
  writeln (tty);
  secs := time mod 60;
  time := time div 60;
  mins := time mod 60;
  time := time div 60;
  hours := time;
  write (tty,'Total CPU time:');
  if hours > 0 then write (tty,hours,' hours,');
  if mins > 0 then write (tty,mins,' minutes,');
  writeln (tty,secs,' seconds.');
  writeln (tty);
end;
$PAGE log - body
begin
  rewrite (tty);
  writeln (tty);
  open (tty);
  get_parameters;
  repeat
    current_record := log_file^;
    with current_record do begin
(*    exit if summarize andif (secs_diff (date_and_time, last_date) > 0); *)
      if highseg_size >= 400000b
	then highseg_size := highseg_size - 400000b
        else highseg_size := max (0, highseg_size);
      if lowseg_size >= 400000b
	then lowseg_size := lowseg_size - 400000b
        else lowseg_size := max (0, lowseg_size);
      extract;
      global_count := global_count + 1;
      if no_errors = 0 then
	global_err_free := global_err_free + 1;
      total_errors := total_errors + no_errors;
      total_lines := total_lines + no_lines;
      total_incl_lines := total_incl_lines + no_incl_lines;
      total_hiseg := total_hiseg + highseg_size;
      total_lowseg := total_lowseg + lowseg_size;
      total_runtime := total_runtime + run_time / 1000;
      i := min (no_errors, upperbound(err_count));
      err_count[i] := err_count[i] + 1;
      max_errors := max (max_errors, no_errors);
      max_lines := max (max_lines, no_lines);
      max_incl_lines := max (max_incl_lines, no_incl_lines);
      max_hiseg := max (max_hiseg, highseg_size);
      max_lowseg := max (max_lowseg, lowseg_size);
      max_runtime := max (max_runtime, run_time/1000);
      check_option (alloc_strategy > 0, count_alloc);
      check_option (ka10_ki10 and not kl10, count_ki10);
      check_option (not (ka10_ki10 or kl10), count_ka10);
      check_option (opt_debug, count_debug);
      check_option (opt_trace and not opt_debug, count_trace);
      check_option (opt_double, count_double);
      check_option (opt_check, count_check);
      check_option (opt_main, count_main);
      check_option (opt_overlay, count_overlay);
      check_option (opt_progress, count_progress);
      check_option (opt_source, count_source);
      check_option (opt_special, count_special);
      check_option (opt_terse, count_terse);
      check_option (opt_xref, count_xref);
      (* check_option (opt_incore, count_incore); *)
      check_option (opt_virtual (* and not opt_incore *), count_virtual);
      check_option (opt_auto_run, count_auto_run);
      check_option (opt_hash, count_hash);
      check_option (opt_tmpcor, count_tmpcor);
      if (all_ppns orif filter_ppn) andif (all_files orif filter_file) andif
	(secs_diff (date_and_time,first_date) >= 0) andif
	  (secs_diff (last_date,date_and_time) >= 0) then begin
	    record_count := record_count + 1;
	    if no_errors = 0 then
	      net_err_free := net_err_free + 1;
	    if summarize then add_to_users
	    else begin
	      if listing then
		list_record;
	      update_user;
	      if not all_files then
		update_file;
	    end;
	  end;
    end;
    get (log_file);
  until eof (log_file);
  close (log_file);
  if listing then
    explain_options;
  if record_count = 0 then
    writeln (tty,'All records rejected by specified conditions.')
  else if record_count = 1 then
    writeln (tty,'One record.')
  else if summarize then sum_users
  else begin
    specific := record_count <> global_count;
    summary;
    report_users;
    report_projects;
    report_files;
    report_totals;
  end;
  break;
  close (output);
end.
L@
pP