  program pte080;

	type	sng_type = string[80];

	var	sw: boolean;
		sng_vr: string[256];
		inptx,outtx: text;
		dt_fl: file of sng_type;
		inptx_flnm,outtx_flnm,dt_flnm: file_name;
		inptx_vr,outtx_vr,data_fl_vr: io_status;
                bug : boolean;

	exception excp_1,excp_2,excp_3;

    procedure print(numb: integer; msg: string[50]);
      begin writeln(outtx); 
	writeln(outtx,' point =',numb:3,'     ',msg); break(outtx) 
      end;

    procedure iosts_print(numbp: integer; iostsp: io_status);
      begin writeln(outtx);
	write(outtx,' io_point =',numbp:2,'     iostatus = ');
	case iostsp of
	  io_ok  : writeln(outtx,'IO_OK  ');
	  io_intr: writeln(outtx,'IO_INTR');
	  io_rewr: writeln(outtx,'IO_REWR');
	  io_eof : writeln(outtx,'IO_EOF ');
	  io_outf: writeln(outtx,'IO_OUTF');
	  io_inpf: writeln(outtx,'IO_INPF');
	  io_opnf: writeln(outtx,'IO_OPNF');
	  others : writeln(outtx,'OTHERS ')
	end; break(outtx)
      end;

    procedure rd_sng(var sngp:sng_type);

	var	sng_hdr: packed array[1..3] of char;

      begin readln(inptx); 
	read(inptx,sng_hdr);
	assert(sng_hdr='***'); read(inptx,sng_hdr);
	if sng_hdr='end' then signal(excp_1) else signal(excp_2);
	exception
	  program_error:
	    if programstatus=program_assertion then
	      begin print(1,' PROGRAM_ASSERTION signalled');
	        if sng_hdr='   ' then read(inptx,sngp)
	           else signal(excp_3)
	      end
	    else begin print(2,' Unexpected PROGRAM_ERROR signalled');
		   exception_message; signal()
		 end;
	  others: begin print(3,' OTHERS signalled');
		    exception_message; signal()
		  end;
      end;

    procedure wr_file(var data_flnm: file_name);

	label	10;

	var	sng: sng_type;
		data_fl: file of sng_type;
		bl_vr: boolean;

      begin bl_vr:=true;
    10: begin if bl_vr then reset(data_fl,data_flnm);
	  data_fl^:='   new data file   '; put(data_fl);
	  loop begin
	    rd_sng(sng); data_fl^:=sng; put(data_fl);
	    exception
	      excp_2:
		begin print(10,' EXCP_2 signalled; wrong command');
		  data_fl^:=' ??? wrong command'; put(data_fl) 
		end;
	      io_error:
		begin inptx_vr:=iostatus(inptx);
			iosts_print(1,inptx_vr);
		      outtx_vr:=iostatus(outtx);
			iosts_print(2,outtx_vr);
		      data_fl_vr:=iostatus(data_fl);
			iosts_print(3,data_fl_vr);
                if (inptx_vr=io_eof) or ((inptx_vr=io_opnf) and bug) then
	          begin print(11,' IO_EOF signalled for INPTX');
		    close(inptx); open(inptx,inptx_flnm); bug := false;
		  end 
		else begin print(13,' Unexpected IO_ERROR signalled');
			 exception_message; signal()
		     end 
		end;
	      others: begin print(12,' Unexpected ERROR signalled');
			exception_message; signal()
		     end
	  end end;
	  exception
	    program_error:
	      if programstatus=program_file then
		begin print(17,' PROGRAM_FILES for DATA_FL');
		  rewrite(data_fl,data_flnm,[preserve]); bl_vr:=false
		end 
	      else begin print(18,' Unexpected PROGRAM_ERROR');
			exception_message; signal()
		   end;
	    special_error:
	      if specialstatus=special_ill_mem_ref then
		begin print(19,' SPECIAL_ILL_MEM_REF for DATA_FL');
		  rewrite(data_fl,data_flnm,[preserve]); bl_vr:=false
		end 
	      else begin print(20,' Unexpected SPECIAL_ERROR');
			exception_message; signal()
		   end;
	    io_error:
	      begin 
		inptx_vr:=iostatus(inptx);
			iosts_print(4,inptx_vr);
		outtx_vr:=iostatus(outtx);
			iosts_print(5,outtx_vr);
		data_fl_vr:=iostatus(data_fl);
			iosts_print(6,data_fl_vr);
		case data_fl_vr of
		  io_opnf: 
	            print(13,' file association failure for DATA_FL');
		  io_rewr:
		    begin
		      print(14,' IO_REWR signalled for DATA_FL');
		      close(data_fl)
		    end;
		  others: 
		    begin print(15,' Unexpected IO_ERROR for DATA_FL');
		      exception_message; signal()
		    end
		end;
		rewrite(data_fl,data_flnm,[preserve]); bl_vr:=false
	      end
	end; goto 10;
	exception others: begin print(16,' OTHERS signalled');
			    close(data_fl); signal()
			  end
      end;

    begin open(tty); rewrite(tty); writeln(tty); 
	writeln(tty,' give an input file name'); break(tty);
      	readln(tty); read(tty,inptx_flnm);
      rewrite(inptx,inptx_flnm,[preserve]); writeln(tty);
	writeln(tty,' give an output file name'); break(tty);
	readln(tty); read(tty,outtx_flnm);
      rewrite(outtx,outtx_flnm); dt_flnm:='pte000.tmp'; sw:=true;
      bug := true;
      while sw do
	begin sw:=false;
	  wr_file(dt_flnm);
	  exception
	    excp_3: begin print(20,' EXCP_3 signalled; wrong line');
		      sw:=true
		    end
	end;
      exception
	excp_1: begin print(21,' EXCP_1 signalled; ---finish---');
		  writeln(outtx); writeln(outtx,' data file contents');
		  reset(dt_fl,dt_flnm); 
		  while not eof(dt_fl) do
		    begin sng_vr:=dt_fl^; writeln(outtx,sng_vr);
		      get(dt_fl)
		    end; close
		end;
	allconditions:
	  begin print(22,' Unexpected ALLCONDITIONS signalled');
		exception_message; close
	  end
    end.
