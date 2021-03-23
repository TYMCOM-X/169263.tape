$TITLE PASOPD -- MDSI PASCAL Option Definitions Data Module

module pasopd;

$INCLUDE pascal
$INCLUDE cmdutl[31024,320156]
$INCLUDE pasopd.typ
$PAGE options
public const

    opdotb_option_table: op_list =
    ( ( 'TERSE     ', 1, ord (opt_terse) ),
      ( 'VERBOSE   ', 1, ord (opt_verbose) ),
      ( 'LENGTH    ', 3, ord (opt_length) ),
      ( 'WIDTH     ', 3, ord (opt_width) ),
      ( 'ENABLE    ', 2, ord (opt_enable) ),
      ( 'DISABLE   ', 3, ord (opt_disable) ),
      ( 'QUICK     ', 1, ord (opt_quick) ),
      ( 'STATISTICS', 4, ord (opt_statistics) ),
      ( 'NAMES     ', 4, ord (opt_names) ),
      ( 'CODE      ', 3, ord (opt_code) ),
      ( 'FINISH    ', 3, ord (opt_finish) ),
      ( 'SOURCE    ', 1, ord (opt_source) ),
      ( 'SEARCH    ', 3, ord (opt_search) ),
      ( 'LSYSTEM   ', 4, ord (opt_lsystem) ),
      ( 'ERRORS    ', 3, ord (opt_errors) ),
      ( 'BANNER    ', 3, ord (opt_banner) ),
      ( 'CHECK     ', 2, ord (opt_check) ),
      ( 'TRACE     ', 2, ord (opt_trace) ),
      ( 'QBLOCKS   ', 3, ord (opt_qblocks) ),
      ( 'MAP       ', 3, ord (opt_map) ),
      ( 'SYMBOLS   ', 3, ord (opt_symbols) ),
      ( 'CALLS     ', 2, ord (opt_calls) ),
      ( 'ASSEMBLY  ', 1, ord (opt_assembly) ),
      ( 'XREF      ', 1, ord (opt_xref) ),
      ( 'OPTIMIZE  ', 3, ord (opt_optimize) ),
      ( 'SPECIAL   ', 2, ord (opt_special) ),
      ( 'OVERLAY   ', 2, ord (opt_overlay) ),
      ( 'MAINSEG   ', 4, ord (opt_mainseg) ),
      ( 'DEBUG     ', 3, ord (opt_debug) ),
      ( 'UNDERFLOW ', 5, ord (opt_underflow) ),
      ( 'MASKING   ', 4, ord (opt_masking) ),
      ( 'GLOBAL    ', 4, ord (opt_global) ),
      ( 'STANDARD  ', 4, ord (opt_standard) ),
      ( 'EXTLENGTH ', 6, ord (opt_ext_unique_length) ),
      ( 'DUMP      ', 4, ord (opt_dump) ),
      ( 'ALLOC     ', 2, ord (opt_alloc) ),
      ( 'STORAGE   ', 3, ord (opt_storage) ),
      ( 'FORTRAN   ', 7, ord (opt_fortran) ),
      ( 'RUN       ', 3, ord (opt_run) ),
      ( 'RUNOFFSET ', 6, ord (opt_runoffset) ),
      ( 'HELP      ', 1, ord (opt_help) ),
      ( 'EXIT      ', 4, ord (opt_exit) )  );


    opdclo_cmdline_options: options_set = [opt_terse..opt_storage];
    opdblo_block_options: options_set = [opt_check..opt_fortran];
    opdnoo_no_options : options_set = [opt_quick..opt_dump];
    opdauo_auto_options: options_set = [opt_source, opt_quick];


    opdmto_map_to_optionlist: in_options_set =
        ( trace_opt, qblocks_opt, map_opt, symbols_opt, calls_opt,
          assembly_opt, xref_opt, optimize_opt ) ;


    opdmfo_map_from_optionlist: to_options_set =
        ( opt_check, opt_check, opt_check, opt_check, opt_check, opt_check,
          opt_check, opt_check, opt_check, opt_check, opt_check, opt_special,
          opt_special, opt_special, opt_map, opt_symbols, opt_calls,
          opt_assembly, opt_xref, opt_trace, opt_qblocks, opt_optimize );
$PAGE CHECK suboptions
public const

    opdcot_chk_opt_table: chk_op_list =
    ( ( 'ASSERTIONS', 3, ord (opt_chk_ass) ),
      ( 'CASES     ', 3, ord (opt_chk_cas) ),
      ( 'COMPATIBIL', 3, ord (opt_chk_com) ),
      ( 'FIELDS    ', 3, ord (opt_chk_fld) ),
      ( 'FILES     ', 3, ord (opt_chk_fil) ),
      ( 'INPUT     ', 3, ord (opt_chk_inp) ),
      ( 'POINTERS  ', 3, ord (opt_chk_poi) ),
      ( 'STACK     ', 3, ord (opt_chk_stk) ),
      ( 'STRINGS   ', 3, ord (opt_chk_str) ),
      ( 'SUBSCRIPTS', 3, ord (opt_chk_sub) ),
      ( 'VALUES    ', 3, ord (opt_chk_val) ) );


    opdmtc_map_to_chk_opt: in_chk_opt_set =
        ( chk_ass_opt, chk_cas_opt, chk_com_opt, chk_fld_opt, chk_fil_opt,
	  chk_inp_opt, chk_poi_opt, chk_stk_opt, chk_str_opt, chk_sub_opt,
	  chk_val_opt );


    opdmfc_map_from_chk_opt: to_chk_opt_set =
        ( opt_chk_ass, opt_chk_cas, opt_chk_com, opt_chk_fld, opt_chk_fil,
	  opt_chk_inp, opt_chk_poi, opt_chk_stk, opt_chk_str, opt_chk_sub,
	  opt_chk_val );
$PAGE SPECIAL suboptions
public const

    opdsot_sp_opt_table: sp_op_list =
    ( ( 'COERCIONS ', 3, ord (opt_sp_coe) ),
      ( 'PTR       ', 3, ord (opt_sp_ptr) ),
      ( 'WORD      ', 3, ord (opt_sp_wor) ) );


    opdmts_map_to_sp_opt: in_sp_opt_set =
        ( sp_coe_opt, sp_ptr_opt, sp_wor_opt );


    opdmfs_map_from_sp_opt: to_sp_opt_set =
        ( opt_sp_coe, opt_sp_ptr, opt_sp_wor );
$PAGE immediate commands
public const

    opdict_imd_cmd_table: imd_list =
    ( ( 'ENVIRON   ', 3, ord (imd_environment) ),
      ( 'TARGET    ', 3, ord (imd_target) ) );
$PAGE opd_dummy
public procedure opd_dummy;
begin
end.
