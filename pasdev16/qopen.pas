$PAGE qopenfile
module qopen
  options special;
(* 
     This module contains the routine QOPENFILE, modelled after RDLIB's
  OPEN_FILE.
*)


type
  qiomode = (qinput_mode, qoutput_mode);
  qiooptions = (qio_append, qio_confirm, qio_ascii);
  qiooption_set = set of qiooptions;

(* QOPENFILE opens a text file for input or output. The mode is specified by
   the caller. For an output file the user can also request append mode and
   old/new file prompting.  The caller may supply a default extension for the
   file name. ERR is returned indicating if the open was successful. *)

public procedure qopenfile
	    (	var f: text;
		fid: file_name; ext: string[3];
		mode: qiomode;
		option_set: qiooption_set;
		var err: qerrcode );

 var question: query_string;
     lext: packed array[1..5] of char;
 begin
  err := qok;
  lext := '     ';
  lext := '.' || ext;
  case mode of 

    qinput_mode:
      begin
	if (option_set - [qio_ascii]) <> [] then begin
	  err := qnoinfile;
	  return
	  end;
	if qio_ascii in option_set
	  then open (f, lext || fid, [ascii])
	  else open (f, lext || fid);
	if iostatus <> io_ok then err := qnoinfile;
      end;

    qoutput_mode:
      begin
	if qio_confirm in option_set then begin
	  open (f, lext || fid);
	  if eof (f)
	    then question := 'New file: ' || fid
	    else begin
		question := 'Old file: ' || filename (f);   (* used full file_id of that found *)
		if f <> tty then close (f)
	    end;
	  if not query (question) then begin
	    err := qnofile;
	    return
	  end
	end;
	if qio_append in option_set
	  then rewrite (f, lext || fid, [preserve])
	  else rewrite (f, lext || fid);
	if iostatus <> io_ok then err := qnooutfile
      end

  end;
 end.
 