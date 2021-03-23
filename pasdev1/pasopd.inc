$SYSTEM cmdutl[31024,320156]

$INCLUDE pasopd.typ

external const
    opdclo_cmdline_options: options_set;
    opdblo_block_options: options_set;
    opdnoo_no_options : options_set;
    opdauo_auto_options: options_set;

    opdotb_option_table: op_list;
    opdmto_map_to_optionlist: in_options_set;
    opdmfo_map_from_optionlist: to_options_set;

    opdcot_chk_opt_table: chk_op_list;
    opdmtc_map_to_chk_opt: in_chk_opt_set;
    opdmfc_map_from_chk_opt: to_chk_opt_set;

    opdsot_sp_opt_table: sp_op_list;
    opdmts_map_to_sp_opt: in_sp_opt_set;
    opdmfs_map_from_sp_opt: to_sp_opt_set;

    opdict_imd_cmd_table: imd_list;
