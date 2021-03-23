(* QED.TYP - modified 9/16/81 by djm to add case_param *)

type
  qedcmds =
      ( append, change, delete, insert, edit, modify, load, print, substitute,
        after, before, writecmd, save, find, gotocmd, resetcmd, join, copy,
        move, transfer, bound, list, eqcmd, number, opencmd, outputcmd, closecmd,
        setcmd, split, quit, exitcmd, uparrow, why, indent, underbar, readcmd);

  qed_cmd_set =
    set of qedcmds;

  set_params =
      ( del_param, lcnt_param, mark_param, tab_param, wild_param, case_param );

  set_param_set = set of set_params;
