
$INCLUDE pasadr.typ

(* Options and related declarations *)

type
  optionlist = (
        chk_ass_opt,            (* check assertions *)
        chk_cas_opt,            (* check case statements *)
        chk_com_opt,            (* check array compatibility *)
        chk_fld_opt,            (* check field variant tags *)
        chk_fil_opt,            (* check file references and dereferences *)
        chk_inp_opt,            (* check input integers and reals *)
        chk_poi_opt,            (* check nil pointer dereferences *)
        chk_stk_opt,            (* check stack overflow *)
        chk_str_opt,            (* check substring bounds *)
        chk_sub_opt,            (* check array subscripts *)
        chk_val_opt,            (* check scalar assignments *)
        sp_coe_opt,             (* allow ADDRESS and pointer coercion functions *)
        sp_ptr_opt,             (* allow use of type PTR *)
        sp_wor_opt,             (* allow use of type MACHINE_WORD *)
        map_opt,                (* generate line/object code mapping *)
        symbols_opt,            (* list declared symbols *)
        calls_opt,              (* list called procedures *)
        assembly_opt,           (* list generated code *)
        xref_opt,               (* generate cross-reference *)
        trace_opt,              (* emit trace information *)
        qblocks_opt,            (* allow quick block analysis *)
        optimize_opt);          (* optimize code, overridden by debug *)

  set_of_options = set of optionlist;

  switch_ptr = ^ switch_node;
  search_ptr = ^ search_node;

  option_mode = ( opt_is_off, opt_is_auto, opt_is_on );

  command_options = 
    record
      semantic_options: set_of_options;
      banner_opt: boolean;                      (* if banner pages desired *)
      code_opt: boolean;                   (* if code generation *)
      debug_opt: boolean;              (* compile for use with debugger *)
      errors_opt: boolean;              (* print an errors listing *)
      finish_opt: boolean;                 (* finish compilations after warnings *)
      global_opt: boolean;                      (* if .XRF global cross reference desired *)
      lsys_opt: boolean;                        (* list $SYSTEM files *)
      mainseg_opt: boolean;                     (* code is for main sharable overlay *)
      masking_opt: boolean;             (* allow MASK(ATTENTION) to work *)
      names_opt: boolean; (* print file name at start of compilation *)
      overlay_opt: boolean;                 (* code will be overlaid *)
      quick_opt: option_mode;               (* use checkout code generator *)
      source_opt: option_mode;                  (* if listing desired *)
      standard_opt: boolean;               (* enforce standard Pascal *)
      statistics_opt: boolean;             (* if stats desired after compilation *)
      terse_opt: boolean;                  (* if short error messages *)
      underflow_opt: boolean;           (* signal MATHERROR on floating underflows *)
      page_length: 0..255;        (* zero => pagination *)
      page_width: 0..255;         (* columns per listing line *)
      external_unique_length: 0..255; (* zero => uniqueness checked to full length *)
      alloc_mode: 0..99;        (* dynamic allocation strategy code *)
      switches: switch_ptr;       (* from enable/disable *)
      dump_switches: switch_ptr;  (* from dump ( ) *)
      search_list: search_ptr; (* from search ( ) *)
      storage: 0..#O777777         (* stack size *)
    end;

  switch_string = string[32];
  switch_node = 
    packed record
      next_switch: switch_ptr;    (* singly linked *)
      enabled: boolean;
      name: packed array [1..*] of char
    end;

  search_node = 
    packed record
      next: search_ptr;    (* singly linked *)
      name: packed array [1..*] of char
    end;

(* Source line description. *)

const line_length = 254;
type  line_index = 0..255 (* line_length + 1 *);
      line_string = string [line_length];


(* Source id record gives file, page, and line no of a statement *)

const
  max_include_level = 3;        (* permits 4 levels of files *)
  max_file_no = 255;                    (* source_id parameters *)
  max_page_no = 4095;
  max_line_no = 65535;

type
  file_range = 0..max_file_no;
  page_range = 0..max_page_no;
  line_range = 0..max_line_no;
  source_id =
      packed record
        file_no: file_range;    (* 0 is main source file *)
        page_no: page_range;
        line_no: line_range
      end;

const
   null_source: source_id := (0, 0, 0);
   last_source: source_id := (max_file_no, max_page_no, max_line_no);

(* Error/warning message severity levels. *)

type
  severity_level = 0..3;                (* no error, warning, error, fatal error *)


(* Generic types used by utilities *)

type
  real_type = minimum (real) .. maximum (real) prec 16;
  pos_int = 0 .. maximum (integer);
  parm_string = string[32];             (* large enough for most things *)
  parm_number = 0..262144;


(* Global data *)

external var
  cur_source: source_id;                (* current file/page/line *)
  linect: integer; (* total source lines read *)
  inclct: integer; (* source lines read from include files *)

  max_severity: severity_level;         (* highest severity error detected *)
  err_count: integer;                   (* number of errors in compilation *)
  warnings: integer; (* number of warnings in compilation *)
  finish: boolean;                      (* go on and generate code? *)
  quick: boolean;                       (* use quick code generator? *)

  default_options: command_options;     (* default global options *)

  prog_options: command_options; (* global options--initialized to default_options,
                                    augmented with program/module statement options *)
  all_opts: set_of_options;             (* all options specified on any block *)

  opts_listing: boolean; (* true if any listing options specified *)

  allc_required: boolean; (* true if ALLCONDITIONS ever specified *)
   