type
  qiomode = (qinput_mode, qoutput_mode);
  qiooptions = (qio_append, qio_confirm, qio_ascii);
  qiooption_set = set of qiooptions;

external procedure qopenfile ( var f: text;		(* file to open *)
			     fid: file_name;		(* name of file *)
			     ext: string[3];		(* default extension *)
			     mode: qiomode;		(* I/O mode *)
			     opts: qiooption_set;	(* options *)
			     var err: qerrcode );	(* error code *)
  