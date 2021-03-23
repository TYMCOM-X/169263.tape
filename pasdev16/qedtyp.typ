(* QEDTYP.TYP - created 04/29/82 by djm *)

const
  token_chars : set of char = ['A' .. 'Z'];

type
  rangetypes = (one, dollar, dot, dotp1, lb, lbp1);

  rangelist = record
      lbound, hbound1, hbound2: rangetypes;
      required, permitted: ldcount
  end;

  cmd_range = 1 .. ord (maximum (qedcmds)) + 1;
  sub_opt_range = 1 .. ord (maximum (sub_options)) + 1;
  set_param_range = 1 .. ord (maximum (set_params)) + 1;
  split_opt_range = 1 .. ord (maximum (split_options)) + 1;
  caller_range = 1 .. ord (maximum (toktyp)) + 1;

  qcmdlist = array [cmd_range] of cmd_lookup_record;
  sub_opt_list = array [sub_opt_range] of cmd_lookup_record;
  set_par_list = array [set_param_range] of cmd_lookup_record;
  split_op_list = array [split_opt_range] of cmd_lookup_record;
  caller_list = array [caller_range] of cmd_lookup_record;
  defrangelist = array [qedcmds] of rangelist;
