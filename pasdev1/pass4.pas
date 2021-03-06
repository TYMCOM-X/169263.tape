$TITLE PASS4 -- Pascal Compiler Code Generation Pass

program pass4
  options storage(8000);
$PAGE includes
$SYSTEM pascal
$SYSTEM ptmcon
$SYSTEM pasist
$SYSTEM pasfil
$SYSTEM paslog
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasifu
$SYSTEM pasdat
$SYSTEM pastal
$SYSTEM ptmgen
$SYSTEM pascv
$SYSTEM passw
$SYSTEM prgdir[31024,320156]
$SYSTEM infpac[31024,320156]
$SYSTEM run[31024,320156]
$SYSTEM tmpnam

external var
    auto_run: boolean;
$PAGE pass4 - main program
var start_time: integer;
    segstuff: segrecd;
    code_size, const_size, static_size: unit_range;
    space_units: string[6];
    excmsg: string;

begin

  begin (* so we can have exception control *)

    start_time := runtime;
    if not dat_get (tempname ('PAS'), true) then begin
      rewrite (tty);
      writeln (tty, '?Compiler temporary file PAS lost');
      stop;
    end;

    rewrite (tty);

    if finish and prog_options.code_opt then begin
      ch_open (true, false);
      tal_init;
      gen_code (code_size, const_size, static_size);
      ch_close;
    end;

    if prog_options.statistics_opt then begin
      seginfo (segstuff);
      writeln (tty, '[Pass 4: ', (runtime - start_time) / 1000.0:8:3, ' seconds, ',
		    (segstuff.lowlen+511) div 512:3, '+',
		    (segstuff.highlen+511) div 512:3, 'P]');
      if finish and prog_options.code_opt then begin
	if byte_size = 8 then
	  space_units := ' bytes'
	else
	  space_units := ' words';
	writeln (tty, '[Code area:      ', cv_radix (code_size, adr_width), space_units, ' (',
		      cv_int (code_size), ' decimal)]');
	writeln (tty, '[Constant area:  ', cv_radix (const_size, adr_width), space_units, ' (',
		      cv_int (const_size), ' decimal)]');
	writeln (tty, '[Static area:    ', cv_radix (static_size, adr_width), space_units, ' (',
		      cv_int (static_size), ' decimal)]');
	writeln (tty);
      end;
    end;

    close (ttyoutput);

    if auto_run then begin
      if prog_options.overlay_opt then begin
	log_record.lowseg_size := const_size + static_size;
	log_record.highseg_size := code_size;
      end
      else begin
	log_record.lowseg_size := static_size;
	log_record.highseg_size := code_size + const_size;
      end;
      log_write;
      run ('PASCMD' || prgm_dir (), true);
      rewrite (tty);
      writeln (tty, '?Unable to run PASCMD');
    end

  exception

    stack_overflow: begin
      rewrite (tty);
      writeln (tty, '?The code generation pass of the compiler has exceeded its available');
      writeln (tty, ' stack space.  Please inform the compiler maintenance personnel.')
    end;

    storage_overflow: begin
      rewrite (tty);
      writeln (tty, '?The code generation pass of the compiler has exceeded its available');
      writeln (tty, ' heap-storage space.  Please inform the compiler maintenance personnel.')
    end;

    io_error: begin
      rewrite (tty);
      writeln (tty, '?The code generation pass of the compiler has encountered an I/O problem.');
      case exiostatus of
	io_rewr: begin
	  writeln (tty, ' An attempt was made to write to a file not open for output.  One');
	  writeln (tty, ' possibility is that you don''t have sufficient disk space.')
	end;

	io_eof: begin
	  writeln (tty, ' An attempt was made to read beyond the end of a file, or from');
	  writeln (tty, ' a file not open for input.')
	end;

	io_opnf: begin
	  writeln (tty, ' An attempt was made to read from, or write to, a file that');
	  writeln (tty, ' had not been successfully opened.')
	end;

	others: begin
	  excmsg := exception_message;
	  writeln (tty);
	  writeln (tty, '%', char (9), excmsg)
	end

      end;
      if exiostatus <> io_rewr then
	writeln (tty, ' Please inform the compiler maintenance personnel.')
    end

  end; (* of handled mainline *)

  (* Now, if we fall out here we must have come out through the exception
     handler.  Try to get back to PASCMD to pick up with the next compilation. *)

  run ('PASCMD' || prgm_dir (), true);
  rewrite (tty);
  writeln (tty, '?Unable to run PASCMD')

end.
 