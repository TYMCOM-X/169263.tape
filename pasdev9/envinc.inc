type
unit_range = integer; (* addressing units in memory *)
bit_range = unit_range; (* bits in memory *)
char_range = unit_range; (* chars in memory *)
align_range = 0..128; (* possible bit alignments *)
prec_type = 1..16; (* possible real precisions *)
set_range = 0 .. #o777777;
id_range = 0 .. #o777777; (* for numbering symbols, tuples, etc. *)
index_range = 0 .. 4095; (* for numbering scope and control flow blocks *)
parm_range = 0 .. 1023; (* for counting subroutine parameters *)
parm_index = 1 .. maximum (parm_range);
deb_file_index = 0 .. #o377777; (* index into debugger symbol table file *)
(* Options and related declarations *)
type
optionlist = (
chk_ass_opt, (* check assertions *)
chk_cas_opt, (* check case statements *)
chk_com_opt, (* check array compatibility *)
chk_fld_opt, (* check field variant tags *)
chk_fil_opt, (* check file references and dereferences *)
chk_inp_opt, (* check input integers and reals *)
chk_poi_opt, (* check nil pointer dereferences *)
chk_stk_opt, (* check stack overflow *)
chk_str_opt, (* check substring bounds *)
chk_sub_opt, (* check array subscripts *)
chk_val_opt, (* check scalar assignments *)
sp_coe_opt, (* allow ADDRESS and pointer coercion functions *)
sp_ptr_opt, (* allow use of type PTR *)
sp_wor_opt, (* allow use of type MACHINE_WORD *)
map_opt, (* generate line/object code mapping *)
symbols_opt, (* list declared symbols *)
calls_opt, (* list called procedures *)
assembly_opt, (* list generated code *)
xref_opt, (* generate cross-reference *)
trace_opt, (* emit trace information *)
qblocks_opt, (* allow quick block analysis *)
optimize_opt); (* optimize code, overridden by debug *)
set_of_options = set of optionlist;
switch_ptr = ^ switch_node;
search_ptr = ^ search_node;
option_mode = ( opt_is_off, opt_is_auto, opt_is_on );
command_options = 
record
semantic_options: set_of_options;
banner_opt: boolean; (* if banner pages desired *)
code_opt: boolean; (* if code generation *)
debug_opt: boolean; (* compile for use with debugger *)
errors_opt: boolean; (* print an errors listing *)
finish_opt: boolean; (* finish compilations after warnings *)
global_opt: boolean; (* if .XRF global cross reference desired *)
lsys_opt: boolean; (* list $SYSTEM files *)
mainseg_opt: boolean; (* code is for main sharable overlay *)
masking_opt: boolean; (* allow MASK(ATTENTION) to work *)
names_opt: boolean; (* print file name at start of compilation *)
overlay_opt: boolean; (* code will be overlaid *)
quick_opt: option_mode; (* use checkout code generator *)
source_opt: option_mode; (* if listing desired *)
standard_opt: boolean; (* enforce standard Pascal *)
statistics_opt: boolean; (* if stats desired after compilation *)
terse_opt: boolean; (* if short error messages *)
underflow_opt: boolean; (* signal MATHERROR on floating underflows *)
page_length: 0..255; (* zero => pagination *)
page_width: 0..255; (* columns per listing line *)
external_unique_length: 0..255; (* zero => uniqueness checked to full length *)
alloc_mode: 0..99; (* dynamic allocation strategy code *)
switches: switch_ptr; (* from enable/disable *)
dump_switches: switch_ptr; (* from dump ( ) *)
search_list: search_ptr; (* from search ( ) *)
storage: 0..#O777777 (* stack size *)
end;
switch_string = string[32];
switch_node = 
packed record
next_switch: switch_ptr; (* singly linked *)
enabled: boolean;
name: packed array [1..*] of char
end;
search_node = 
packed record
next: search_ptr; (* singly linked *)
name: packed array [1..*] of char
end;
(* Source line description. *)
const line_length = 254;
type line_index = 0..255 (* line_length + 1 *);
line_string = string [line_length];
(* Source id record gives file, page, and line no of a statement *)
const
max_include_level = 3; (* permits 4 levels of files *)
max_file_no = 255; (* source_id parameters *)
max_page_no = 4095;
max_line_no = 65535;
type
file_range = 0..max_file_no;
page_range = 0..max_page_no;
line_range = 0..max_line_no;
source_id =
packed record
file_no: file_range; (* 0 is main source file *)
page_no: page_range;
line_no: line_range
end;
const
null_source: source_id := (0, 0, 0);
last_source: source_id := (max_file_no, max_page_no, max_line_no);
(* Error/warning message severity levels. *)
type
severity_level = 0..3; (* no error, warning, error, fatal error *)
(* Generic types used by utilities *)
type
real_type = minimum (real) .. maximum (real) prec 16;
pos_int = 0 .. maximum (integer);
parm_string = string[32]; (* large enough for most things *)
parm_number = 0..262144;
(* Global data *)
external var
cur_source: source_id; (* current file/page/line *)
linect: integer; (* total source lines read *)
inclct: integer; (* source lines read from include files *)
max_severity: severity_level; (* highest severity error detected *)
err_count: integer; (* number of errors in compilation *)
warnings: integer; (* number of warnings in compilation *)
finish: boolean; (* go on and generate code? *)
quick: boolean; (* use quick code generator? *)
default_options: command_options; (* default global options *)
prog_options: command_options; (* global options--initialized to default_options,
augmented with program/module statement options *)
all_opts: set_of_options; (* all options specified on any block *)
opts_listing: boolean; (* true if any listing options specified *)
allc_required: boolean; (* true if ALLCONDITIONS ever specified *)
const
max_width = 254; (* maximum line width *)
max_plength = 254; (* maximum page length *)
type
fio_width = 0..max_width;
fio_string = string[max_width];
file_block =
record
file_var: text; (* file variable to operate on *)
file_title: file_name; (* true name of file, set on open *)
pageno: 0..999999; (* page number of current page *)
lineno: 0..999999; (* line number within page (local) *)
column: 1..max_width; (* current printing column position. *)
width: 0..max_width; (* maximum width of output line *)
c_column: 0..max_width; (* continuation column; 0 => truncate *)
plength: 0..max_plength; (* maximum length of output page; 0 => no limit *)
new_page: procedure (var file_block); (* subroutine to call to perform a "page eject" *)
page_header: procedure (var file_block) (* subroutine to call to write a page header *)
end;
external procedure fio_open (var file_block; packed array [1..*] of char);
external procedure fio_attach (var file_block; text);
external procedure fio_close (var file_block);
external procedure fio_page (var file_block);
external procedure fio_skip (var file_block);
external procedure fio_nskip ( var file_block; integer; integer );
external procedure fio_tab (var file_block; fio_width);
external procedure fio_write (var file_block; packed array [1..*] of char);
external procedure fio_nop (var file_block);
external procedure fio_eject (var file_block);
external procedure fio_reopen (var file_block);
external procedure fio_line (var file_block; packed array [1..*] of char);
external procedure fio_space (var file_block; fio_width);
type title_string = string[80];
(* Lists of files used in compilation and logical sections within those files. *)
type
source_ptr = ^src_id;
page_ptr = ^page_id;
src_id =
packed record
file_no: file_range; (* number assigned to file *)
pages: page_ptr; (* links pages in file *)
next_file: source_ptr; (* sequential list *)
incl_level: 0..max_include_level; (* inclusion level *)
incl_page: page_range; (* physical page on which included *)
system_file: boolean;
file_name: packed array [1..*] of char (* from filename *)
end;
page_id =
packed record
left_page, right_page: page_ptr; (* for xref after compilation *)
next_page: page_ptr; (* sequential list of pages in a file *)
following_page: page_ptr; (* next page in listing *)
in_file: source_ptr; (* link to enclosing file *)
page_number: page_range; (* "section" number *)
incl_page: page_range; (* physical page no for xref *)
subtitle: packed array [1..*] of char (* page subtitle if any *)
end;
(* Defines states of output files which are "open" across passes. *)
type
file_status =
( unopened, (* has never been opened *)
prev_opened, (* opened in a previous pass, but not in this one *)
now_open ); (* file is now open *)
external var
main_file: file_name; (* names of important files *)
list_file: file_name;
rel_file: file_name;
list_explicit: boolean; (* list file name from ",list=source" *)
file_list: source_ptr; (* start of list of files referenced *)
no_files: file_range; (* number of current file *)
fin_source: source_id; (* last line read prior to abort *)
line: line_string; (* uppercased input line + 2 blanks *)
literal_line: line_string; (* input line as read *)
src_on: boolean; (* line should be listed *)
ln_enabled: boolean; (* line is enabled for compilation *)
end_of_file: boolean; (* true => last line has been read *)
src_selected: boolean; (* true => source on somewhere in program *)
main_title: title_string; (* first title which appears in input *)
global_title: title_string; (* current title *)
page_title: title_string; (* currently applicable $PAGE title *)
df_status: file_status; (* indicates current state of dump file *)
lf_status: file_status; (* ... of list file *)
elf_status: file_status; (* ... of error log file *)
dumpfb: file_block; (* FIO control blocks *)
listfb: file_block; (* .width is current page_width
.plength is current page_length *)
(*---------------------------------------------------------------------------*)
(*
* INCLUDE FILE FOR PASCAL DAY/TIME ROUTINES PACKAGE.
*)
CONST
MAX_YEAR = 2217;
DAY_ZERO = 'NOV 17, 1858';
TYPE
DTIME_INT = PACKED RECORD D: 0..1 END; (* INTERNAL DAY/TIME *)
DTIME_EXT = PACKED ARRAY [1..18] OF CHAR; (* STANDARD EXTERNAL DAY/TIME *)
NS_EXT = STRING[24]; (* FOR NON-STANDARD EXTERNAL DAY/TIMES *)
DATE_INT = PACKED RECORD D: 0..1 END; (* INTERNAL DATE *)
TIME_INT = PACKED RECORD D: 0..1 END; (* INTERNAL TIME *)
DEC_DATE = 0..#O77777; (* 15 BIT DEC DATE *)
TYM_DATE = 0..#O77777; (* 15 BIT TYMSHARE DATE *)
DEC_TIME = 0..86400000; (* MILLISECONDS SINCE MIDNIGHT TIME *)
DTIME_ERR = (DT_NOERR,DT_ERR); (* ERROR CODES *)
DAYS = INTEGER;
SECONDS = INTEGER;
WEEK_DAY = (SUNDAY,MONDAY,TUESDAY,WEDNESDAY,THURSDAY,
FRIDAY,SATURDAY);
NS_DATE1 = PACKED ARRAY [1..8] OF CHAR; (* 'MM/DD/YY' *) 
NS_DATE2 = PACKED ARRAY [1..12] OF CHAR; (* 'MMM DD, YYYY' *)
NS_TIME1 = PACKED ARRAY [1..11] OF CHAR; (* 'HH:MM:SS PM' *)
DTIMEREC = RECORD (* BINARY DAY/TIME RECORD *)
YEAR: 1858..MAX_YEAR;
MONTH: 1..12;
DAY: 1..31;
HOURS: 0..23;
MINS: 0..59;
SECS: 0..59
END;
DATEREC = RECORD (* BINARY DATE RECORD *)
YEAR: 1858..MAX_YEAR;
MONTH: 1..12;
DAY: 1..31
END;
TIMEREC = RECORD (* BINARY TIME RECORD *)
HOURS: 0..23;
MINS: 0..59;
SECS: 0..59
END;
(*---------------------------------------------------------------------------*)
(* Standard Procedures and Functions *)
type
std_pr_fun =
( propen, prreset, prrewrite, prupdate,
prget, prput, prpage, prclear,
prbreak, prempty, prclose, prscratch,
prread8, prwrite8, prseek, 
prnew, prallocate, prdispose, prassert,
prsignal, prmask, prunmask,
fnabs, fnsqr, fnsqrt,
fnln, fnlog, fnexp,
fnsin, fnarcsin, fnsinh,
fncos, fnarccos, fncosh,
fntan, fnarctan, fntanh, fncotan,
fnrandom, fnmin, fnmax, fnodd,
fnround, fntrunc, fnord, fnchr,
fnsucc, fnpred, fnminimum, fnmaximum,
fneoln, fneopage, fneof, fncursor,
fniostatus, fnextstatus,
fnmathstatus, fnprogramstatus,
fnspecialstatus, fnexiostatus,
fndate, fntime, fnruntime,
fnmasked, fnpending,
fnlength, fnlowercase, fnuppercase,
fnsubstr, fnindex, fnverify, fnsearch,
fnlowerbound, fnupperbound, fndimension,
fnsize, fnextent, fnaddress, fnxfer );
(* Note: XFER is not a user function. It is the function code
representing a type name used as a transfer function. *)
std_proc = propen .. prunmask;
std_func = fnabs .. fnxfer;
(* The compiler's INTERNAL SYMBOL TABLE is pointer linked structure consisting
of the name table, the lexically order tree of block nodes, symbol nodes
within (sic) the block, type nodes, and value (constant) nodes. *)
type
nam = ^name_node;
blk = ^block_node;
sym = ^symbol_node;
typ = ^type_node;
$PAGE storage_class
(* STORAGE CLASS is an attribute of a data symbol (i.e., var, value parm, or
constant), or, in Pass 4, of an address. *)
storage_class = 
( local_sc, (* local to a block *)
parameter_sc, (* parameter to a block *)
static_sc, (* static to a single compiled module *)
constant_sc, (* constant storage of a compilation *)
external_sc, (* relative to an external name *)
(* The following storage classes are used to distinguish the dummy
"pseudo-symbols" which are created to represent certain information
for optimization. *)
dynamic_sc, (* represents the targets of a class of pointers *)
fileblk_sc, (* represents the file statuses of a class of files *)
opt_sc, (* miscellaneous optimization symbols *)
(* The following storage classes are introduced to represent relocation
attributes of addresses during code generation. *)
code_sc, (* within code area of a compilation *)
absolute_sc, (* constant address *)
temp_sc, (* relative to temporary area for a block *)
register_sc, (* item is a register *)
runtime_sc, (* runtime symbol *)
def_sc, (* internal label *)
self_rel_sc, (* self relocating *)
unallocated ); (* data not allocated *)
$PAGE name_node
(* The NAME TABLE contains the text of all <identifier>s seen during a
compilation. It is organized as an unbalanced binary tree. *)
name_node =
packed record
alink, zlink: nam; (* branches of tree; name < current is on alink *)
scopechain: sym; (* points to 1st symbol in scope with this name;
chains through sym^.scopechain *)
visited: boolean; (* for debugger file creation walk *)
file_loc: deb_file_index; (* relocation into debugger file *)
text: packed array [1..*] of char (* text of name *)
end;
$PAGE value_node
(* VALUE NODES are used to represent constant values. In order to save storage,
simple scalar values are stored in a VAL record instead of a pointer to a 
value node. The val record is used in all referencing structures. *)
value_kind = ( scalar_cst, real_cst, string_cst, set_cst, ptr_cst, array_cst,
record_cst, subr_cst, no_value (* indicates no assigned value *),
alloc_cst (* indicates a value which has been allocated in pass 4 *) );
val_ptr = ^ value_node;
val =
packed record
case kind: value_kind of
scalar_cst:
( ival: machine_word ); (* ordinal values of a scalar value *)
real_cst,
string_cst,
set_cst,
array_cst,
record_cst:
( valp: val_ptr ); (* more complex value, described below *)
ptr_cst:
( ); (* the only pointer constant is NIL *)
subr_cst:
( blkp: blk ); (* ptr to block node of subr name *)
alloc_cst:
( defp: val_ptr ) (* really a definition pointer, set in pass 4 *)
end;
value_node =
packed record
def_addr: val_ptr; (* really a DEF pointer in pass 4 *)
case kind: value_kind of
scalar_cst: (* for full word consts *)
( scalar_val: machine_word );
real_cst:
( real_prec: prec_type; (* apparent precision *)
real_val: real_type ); (* converted value *)
string_cst:
( str_varying_ref: boolean; (* initially false, set if length field is needed *)
str_val: packed array [1..*] of char ); (* text of string *)
set_cst:
( set_origin: set_range; (* implicit offset of 1st bit *)
set_val: packed array [0..*] of boolean ); (* set element values *)
array_cst, record_cst:
( struc_type: typ; (* array or record type; nil until known *)
elem_vals: array [1..*] of val ) (* ptrs to element values *)
end;
$PAGE symbol_node
(* SYMBOL NODES contain information about all objects declared in the
program or created by the compiler. *)
sym_kind = ( labels, fields, types, consts, vars, values, for_inds,
std_procs, std_funcs, conditions, blocks );
fld_range = 1..2047;
symbol_node =
packed record
name: nam; (* if user symbol, ptr to name in name table; if
compiler symbol, nil. *)
block: blk; (* block name appears in; nil for fields *)
next: sym; (* next symbol of similar kind: for fields, linked
off record/variant; for others: next parm,
var, etc. linked off block *)
scopechain: sym; (* next symbol in scope with same name *)
type_desc: typ; (* object data type, nil for labels, standard subrs *)
id_number: id_range; (* among vars/labels or other symbols *)
visited: boolean; (* for debugger file creation walk *)
file_loc: deb_file_index; (* relocation into debugger file *)
case kind: sym_kind of
types: (); (* no further information *)
labels:
( lab_defined: boolean; (* implies seen in body of block *)
lab_nonlocal_use: boolean; (* target of nonlocal goto *)
lab_declaration: source_id ); (* source line of label declaration *)
fields:
( fld_number: fld_range; (* logical offset of field, for handling record consts *)
fld_record: typ; (* record containing field *)
fld_variant: typ; (* record/first variant of which field is a
member of the fixed part *)
fld_offset: bit_range; (* offset of field from start of record *)
fld_width: bit_range ); (* allocated width in bits *)
consts, vars, values, for_inds, conditions:
( dcl_class: storage_class; (* storage class as declared, e.g. static, parm, local *)
public_dcl: boolean; (* item declared as public *)
abnormal_use: boolean; (* item not candidate for CSE *)
non_local_use: boolean; (* item accessed non locally *)
parm_use: boolean; (* item used as a parameter *)
allocated: boolean; (* item referenced in memory *)
standard: boolean; (* language-defined condition *)
maskable: boolean; (* may be masked (standard conditions only) *)
explicit_type: boolean; (* to detect e.g. const s = 'ABCDE' *)
item_addr: unit_range; (* offset from start of class *)
init_value: val ); (* initial value for static/public vars; value of
consts; unused in all other cases. *)
std_procs, std_funcs:
( std_pf_code: std_pr_fun ); (* code to dispatch to p/f handler *)
blocks:
( ) (* used as a cross-reference code only *)
end;
sym_list = (* chain of symbol nodes *)
packed record
first: sym;
last: sym
end;
(* VL nodes are used to construct a simple linked list of all the variable and
label symbols in the program. *)
vl_link = ^ vl_node;
vl_node = packed re
last: vl_link;
symbol: sym
end;
$PAGE type_node
(* TYPE NODES contain information described user declared and builtin
data types. *)
type_kind = ( scalars, bools, chars, ints, reals, sets, pointers,
files, strings, arrays, records, variants, tags,
procs, funcs, unknown_type, indirect_type );
string_kind = ( varying, nonvarying );
file_modes = (* kinds of files, for OPEN and family *)
( textfile, (* TEXT *)
typedfile, (* FILE OF <some type> *)
binaryfile, (* FILE OF * *)
anyfile ); (* invalid file type; compatible with all *)
param_desc =
packed record
parm_type: typ;
parm_kind: sym_kind (* vars or values *)
end;
type_node =
packed record
base_size: bit_range; (* minimal size of data item in bits *)
type_id: sym; (* type identifier, if named type; nil otherwise *)
visited: boolean; (* for debugger file creation walk *)
file_loc: deb_file_index; (* relocation into debugger file *)
packable: boolean; (* data may be minimally packed *)
flexible: boolean; (* type has '*' bound *)
case kind: type_kind of
bools, ints, chars, scalars: (* basic types + subranges of enumerated types *)
( base_type: typ; (* type over which interval is defined, may loop *)
minval, maxval: machine_word; (* ordinal value of range limits *)
case type_kind of (* below only non-nil for maximal subrange *)
bools, scalars:
( cst_list: sym_list ) ); (* chain thru sym^.next of enumerated values,
sublist of blk^.id_list *)
reals: (* subranges of real *)
( precision: prec_type; (* precision, in decimal digits *)
rminval, rmaxval: real_type ); (* limits of range *)
sets:
( set_element_type: typ ); (* type of elements of set *)
pointers:
( target_type: typ; (* type of data pointed to by target *)
heap_class: sym ); (* represents vars accessible to this type *)
arrays:
( element_alignment: align_range; (* required element alignment *)
element_type: typ; (* type of element of the array *)
index_type: typ; (* type of subscripts *)
element_size: bit_range ); (* size of elements of the array *)
files:
( file_kind: file_modes; (* text, typed, binary, any *)
component_type: typ; (* data in file, if not binary or any *)
file_class: sym; (* represents all files with this type *)
comp_size: bit_range ); (* in bits *)
strings:
( str_kind: string_kind; (* varying or non-varying *)
str_length: char_range ); (* maximum length of varying string or complete
length of non-varying string *)
records, variants: (* size gives max size including all inferior variants *)
( field_list: sym; (* first field in fixed part of record or variant
(fixed part ends with change of fld_variant ptr);
for records, this is start of chain of all fields *)
variant_tag: typ; (* variable part tag *)
case type_kind of
records:
( record_alignment: align_range ); (* minimal alignment for record *)
variants:
( tag: typ; (* superior tag of which this is a variant *)
next_variant: typ; (* chain of variants for a given tag *)
others_var: boolean; (* true => others variant *)
minlab, maxlab: machine_word ) ); (* case label range, min = max for non-range *)
tags: (* size is for variants with () field lists *)
( tag_field: sym; (* field holding value of tag; if nil then 
this is an undiscrimated union *)
tag_type: typ; (* data type of tag *)
tag_recvar: typ; (* record or variant containing tag *)
first_variant: typ ); (* chain thru typ^.next_variant of variants *)
procs, funcs:
( fortran_call: boolean; (* use fortran calling sequence *)
class_block: blk; (* subr class for subr vars/parms of this type *)
return_type: typ; (* type of return value, if function; else nil *)
parmlist_size: unit_range; (* aggregate size of parameter list including descriptors *)
params: packed array [1..*] of param_desc );
unknown_type, indirect_type: (* forward referenced types, before/after definition *)
( actual_type: typ; (* type node forward referenced; this is
nil, if referenced before defined. *)
declaration: source_id ) (* line where type is first referred to *)
end;
$PAGE block_node
(* BLOCK NODES represent levels of lexical scope and contain information about
all symbols declared in that level. *)
block_kind =
( root_blk, program_blk, module_blk, subr_blk, class_blk, extern_blk );
call_link = ^ call_link_node;
level_index = 0 .. 63;
(* Subroutine classes are chained as siblings of the root block, and are
accessible through links in the subroutine type nodes. If there is
an <extern> block, it will be chained as a level 2 procedure (under
the program or module), and will be accessible through the global link
'ext_block'. *)
block_node =
packed record
parent: blk; (* containing block *)
peer: blk; (* next block within same parent *)
children: blk; (* chain thru blk^.peer of contained blocks *)
visited: boolean; (* for debugger file creation walk *)
file_loc: deb_file_index; (* relocation into debugger file *)
number: index_range; (* unique block number *)
level: level_index; (* level 0 contains standard names *)
apparent_level: level_index; (* level accounting for quick blocks *)
max_call_level: level_index; (* deepest apparent_level from which block is called *)
hndlr_depth: level_index; (* max nesting of handled compound statements *)
recursive: boolean; (* subroutine can call itself *)
class_bound: boolean; (* subroutine is bound to a class *)
return_sym: sym; (* if function, dummy symbol addressing return value *)
parm_list: sym_list; (* chain thru sym^.next of parameters *)
label_list: sym_list; (* chain thru sym^.next of labels within this block *)
type_list: sym_list; (* chain thru sym^.next of all type ids *)
id_list: sym_list; (* chain thru sym^.next of all other names except fields *)
semantic_options: set_of_options; (* check, debug, map, symbols, dump, special,
and optimize permitted *)
dump_switches: switch_ptr; (* from dump ( ) *)
owner: blk; (* block containing this blk's frame *)
calls: call_link; (* chain thru call_link^.next_call of blocks called *)
downward_call_thread: blk; (* DFST order threads for call tree *)
upward_call_thread: blk;
lex_thread: blk; (* block ordering by decreasing level numbers *)
pos_local_size: unit_range; (* size of pos offset local storage *)
neg_local_size: unit_range; (* size of neg offset local storage *)
pos_stk_begin: unit_range; (* owner frame offset for pos storage *)
neg_stk_begin: unit_range; (* owner frame offset for neg storage *)
pos_stk_end: unit_range; (* owner frame last word + 1 for pos storage *)
neg_stk_end: unit_range; (* owner frame last word + 1 for neg storage *)
parm_list_base: unit_range; (* offset of parm list pointer if > 6 words *)
case kind: block_kind of
root_blk: (); (* imaginary block containing standard defs *)
program_blk, module_blk: (* level 1 or main program *)
( id: nam; (* compilation id *)
start_addr: unit_range; (* start of mainline, for program_blk only *)
comp_dtime: dtime_int ); (* day/time of compilation *)
subr_blk: (* actual procedures and functions *)
( subr_sym: sym; (* procedure or function id *)
forward_dcl: boolean; (* indicates body not yet given *)
declaration: source_id ); (* location of the declaration *)
class_blk: (* block representing a subr var/parm type *)
( class_type: typ ); (* the subr type of the class *)
extern_blk: () (* block represents all external procedures *)
end;
(* The blocks which are called from a given block are represented by a
right-threaded binary tree structure (see Knuth, volume 1, pp 319-325).
The ordering on the tree is: p precedes q if level(p) > level(q), or
if level(p) = level(q) and number(p) < number(q). *)
call_link_node =
packed record
called_subr: blk; (* block node for a called subr *)
r_terminal: boolean; (* true if rlink is a thread; false for a link *)
llink, rlink: call_link (* the tree structure links *)
end;
external var
root_name: nam; (* base of unbalanced binary name tree *)
env_name: nam; (* set in envmodule compilation *)
env_dtime: dtime_int; (* date/time of envmodule compilation *)
root_block: blk; (* imaginary outermost block containing standard names *)
cur_block: blk; (* innermost block in scope *)
ext_block: blk; (* block to represent all external routines *)
lex_block: blk; (* start of the lex_thread chain, ordered by level *)
blk_number: 0..2047; (* value given to cur_block^ *)
max_level: level_index; (* highest block level in the program *)
heap_chain: sym; (* chain of heap class var symbols *)
file_chain: sym; (* chain of file class var symbols *)
io_opsym: sym; (* input/output optimizer symbol *)
cdatesym: sym; (* COMPDATE predefined symbol *)
ctimesym: sym; (* COMPTIME predifined symbol *)
sym_vl_number: id_range; (* numbering for var and label symbols *)
vl_base: id_range; (* first free after symbol table initialization *)
sym_nvl_number: id_range; (* numbering for other kinds of symbols *)
nvl_base: id_range; (* first free after symbol table initialization *)
vl_list, (* chain of var and label symbols *)
vll_base: vl_link; (* chain after symbol table initialization *)
(* Pointers to Standard Types *)
external var
type_int: typ; (* INTEGER *)
type_fullword: typ; (* MACHINE_WORD *)
type_non_neg: typ; (* non-negative integers *)
type_bool: typ; (* BOOLEAN *)
type_char: typ; (* CHAR *)
type_real: typ; (* REAL *)
type_ptr: typ; (* PTR *)
type_text: typ; (* TEXT *)
type_options: typ; (* IO_OPTIONS *)
stat_io: typ; (* IO_STATUS *)
stat_program: typ; (* PROGRAM_STATUS *)
stat_math: typ; (* MATH_STATUS *)
stat_special: typ; (* SPECIAL_STATUS *)
(* Pointers to Standard File Symbols *)
external var
file_input: sym; (* INPUT *)
file_output: sym; (* OUTPUT *)
file_tty: sym; (* TTY *)
filettyoutput: sym; (* TTYOUTPUT *)
(* Address Information for Code Generation *)
external var
size_init: unit_range; (* initialized static variable area *)
size_uninit: unit_range; (* uninitialized static variable area *)
size_cond: unit_range; (* condition cell area *)
(********* Target Machine Customization *********)
type
fstr_3 = packed array [1..3] of char;
vstr_40 = string [40];
radix_type = ( hex_radix, octal_radix );
external var
(* TMPREFIX is a three-letter target machine prefix which is used in
constructing the names of the individual passes of the compiler. *)
tmprefix: fstr_3;
(* TTYINAME and TTYONAME are the external file names to be associated with the
terminal input and terminal output files. *)
ttyiname: vstr_40;
ttyoname: vstr_40;
(* REL EXTENSION is the default extension for the compiler's relocatable
binary output file. *)
rel_extension: fstr_3;
(* HAVE CHECKOUT and HAVE OPTIMIZER indicate whether checkout and optimizing
code generators are available for this compiler. *)
have_checkout: boolean;
have_optimizer: boolean;
(* RADIX is HexRadix or OctalRadix to indicate the output mode for target
machine numeric constants. *)
radix: radix_type;
(* ADR WIDTH is the number of digits (in the target machine radix) to be
printed for a target machine address value. *)
adr_width: integer;
(* SREALPREC is the number of significant digits which the target machine
will make available in a variable of type "Real" (i.e., a "single-
precision" real number. *)
srealprec: prec_type;
(* ARR LEN SIZE and STR LEN SIZE are the number of bits required by the
length words for a flexible array and a flexible string, respectively,
in a dynamically allocated variable. *)
arr_len_size: integer;
str_len_size: integer;
(* SET LWB LIMIT and SET UPB LIMIT are the smallest and largest possible
set elements. SET SIZE LIMIT is the largest allowable base size for a
set type (the base size of a set type is computed by AllocType). *)
set_lwb_limit: integer;
set_upb_limit: integer;
set_size_limit: integer;
(* Extra elements may be allocated at the beginning or end of a set, if
this will improve the efficiency of set operations. When allocating
a set, its first element will be rounded down to a multiple of
SET LQUANTUM, or to SET LBASE if that is smaller. The last element
will be rounded up to one less than a multiple of SET UQUANTUM, or to
SET UBASE if that is larger. *)
set_lquantum: integer;
set_uquantum: integer;
set_lbase: integer;
set_ubase: integer;
(* BYTE SIZE is the number of bits in the smallest addressable unit on the
target machine. *)
byte_size: integer;
(* INT PREC LIMIT is the maximum number of bits which can be used in an
integer computation. (Generally, this will be the same as the word
size. It needn't be larger than the number of bits required to
represent a MachineWord variable. *)
int_prec_limit: bit_range;
(* QBL ALLOWED is a flag which indicates whether quick blocks may be
constructed for this target machine. *)
qbl_allowed: boolean;
external function ngm ( a, b: integer ): integer;
external function nlm ( a, b: integer ): integer;
external function gcd ( a, b: integer ): integer;
external function int_bits ( x: integer ): integer;
(*---------------------------------------------------------------------------*)
(*
* INCLUDE FILE FOR PASCAL DAY/TIME ROUTINES PACKAGE.
*)
EXTERNAL FUNCTION DAYTIME: DTIME_INT;
EXTERNAL FUNCTION DC_EXT(DTIME: DTIME_INT): DTIME_EXT;
EXTERNAL PROCEDURE EC_EXT(VAR ERR_CODE: DTIME_ERR;
DT_EXT: NS_EXT; VAR TIME_: DTIME_INT);
EXTERNAL FUNCTION EXTR_DATE(DTIME: DTIME_INT): DATE_INT;
EXTERNAL FUNCTION EXTR_TIME(DTIME: DTIME_INT): TIME_INT;
EXTERNAL FUNCTION DT_COMBINE(DATE: DATE_INT; TIME: TIME_INT): DTIME_INT;
EXTERNAL FUNCTION DAYS_DIFF(DTIME1,DTIME2: DTIME_INT): DAYS;
EXTERNAL FUNCTION SECS_DIFF(DTIME1,DTIME2: DTIME_INT): SECONDS;
EXTERNAL FUNCTION ADD_DAYS(DTIME: DTIME_INT;
NUM_DAYS: DAYS): DTIME_INT;
EXTERNAL FUNCTION ADD_SECS(DTIME: DTIME_INT;
NUM_SECS: SECONDS): DTIME_INT;
EXTERNAL FUNCTION DAY_OF_WEEK(DATE: DATE_INT): WEEK_DAY;
EXTERNAL FUNCTION NS_D1(DATE: DATE_INT): NS_DATE1;
EXTERNAL FUNCTION NS_D2(DATE: DATE_INT): NS_DATE2;
EXTERNAL FUNCTION NS_T1(TIME: TIME_INT): NS_TIME1;
EXTERNAL FUNCTION GMDTIME: DTIME_INT;
EXTERNAL FUNCTION DC_DTIME(DTIME: DTIME_INT): DTIMEREC;
EXTERNAL PROCEDURE EC_DTIME(VAR ERR_CODE: DTIME_ERR; DTIMEBIN: DTIMEREC;
VAR DTIME: DTIME_INT);
EXTERNAL PROCEDURE EC_DATE(VAR ERR_CODE: DTIME_ERR;
DATE_BIN: DATEREC; VAR DATE: DATE_INT);
EXTERNAL FUNCTION DC_DATE(DATE: DATE_INT): DATEREC;
EXTERNAL PROCEDURE EC_TIME(VAR ERR_CODE: DTIME_ERR;
TIME_BIN: TIMEREC; VAR TIME: TIME_INT);
EXTERNAL FUNCTION DC_TIME(TIME: TIME_INT): TIMEREC;
EXTERNAL FUNCTION EC_DCDATE(D_DATE: DEC_DATE): DATE_INT;
EXTERNAL FUNCTION EC_TSDATE(T_DATE: TYM_DATE): DATE_INT;
EXTERNAL FUNCTION EC_DCTIME(D_TIME: DEC_TIME): TIME_INT;
(*---------------------------------------------------------------------------*)
   (É