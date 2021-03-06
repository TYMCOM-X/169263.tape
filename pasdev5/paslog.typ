type
    halfword = 0..777777b;
    log_file_stats = packed record
      file_name: filblock;
      version: real;		(* compiler version *)
      run_time: integer;      (* in milliseconds *)
      no_lines: integer;      (* no source lines read *)
      no_incl_lines: integer;      (* no lines read from include files *)
      no_errors: integer;      (* no errors detected in compilation *)
      users_ppn: integer;	(* from getppn uuo *)
      date_and_time: dtime_int;      (* from daytime *)
      lowseg_size: halfword;
      hiseg_size: halfword;
      alloc_strategy: min_allocation..max_allocation;
      ka10_ki10: boolean;
      kl10: boolean;
      opt_debug: boolean;	(* if double precision reals *)
      opt_double: boolean;	(* if in debug mode *)
      opt_check: boolean;	(* if check option in effect *)
      opt_main: boolean;	(* if main/external compilation *)
      opt_overlay: boolean;	(* if overlaid *)
      opt_progress: boolean;
      opt_source: boolean;	(* if listing produced *)
      opt_special: boolean;	(* if non-portable *)
      opt_terse: boolean;
      opt_trace: boolean;
      opt_xref: boolean;
      opt_virtual: boolean;
      opt_auto_run: boolean;	(* if run at offset 1 *)
      opt_tmpcor: boolean;	(* if run from tmpcor file *)
      opt_hash: boolean;	(* if run from ###PAS.TMP file *)
      opt_incore: boolean;
      filler1: integer;	(* even out record to 16 (decimal) words *)
      filler2: integer;
      filler3: integer
    end;
    