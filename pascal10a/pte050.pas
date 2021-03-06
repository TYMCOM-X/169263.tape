  program pte050;

	const	int1_cst=8;
		int2_cst=12;
		rl1_cst=481.035;
		rl2_cst=893.12;

	var	subint: -20..20;
		int,int1,int2: integer;
		rl,rl1,rl2: -1..2.0**120 prec 7;
		math_vr: math_status;
		outtx: text;
		outtx_flnm: file_name;

    procedure print(numb: integer; mesgp: string[50]);
	begin writeln(outtx);
	  writeln(outtx,' point =',numb:3,'     ',mesgp);
	  break(outtx) 
	end;

    begin open(tty); rewrite(tty); writeln(tty);
	write(tty,' give an output file name : '); break(tty);
	readln(tty); read(tty,outtx_flnm);
      rewrite(outtx,outtx_flnm);
	math_vr:=math_flt_und;
	int1:=int1_cst; int2:=int2_cst; rl1:=rl1_cst; rl2:=rl2_cst;
      loop begin
	case math_vr of
	  math_flt_und: begin subint:=0; rl2:=rl2**int2;
			  rl:=rl1**(-int1); rl:=rl/rl2
			end;
	  math_flt_ovf: begin rl1:=rl1**int1; rl:=rl1*rl2 end;
	  math_int_ovf: int:=int1**12;
	  math_zero_divide: rl:=int1/subint;
	  math_arg_arcsin: rl:=arcsin(int1);
	  math_arg_arccos: rl:=arccos(int1)
	end;
	exception
	  math_error:
	    case mathstatus of
	      math_flt_und: 
		begin print(12,' MATH_FLT_UND signalled');
		  math_vr:=math_flt_ovf
		end;
	      math_flt_ovf:
		begin print(13,' MATH_FLT_OVF signalled');
		  math_vr:=math_int_ovf
		end;
	      math_int_ovf:
		begin print(14,' MATH_INT_OVF signalled');
		  math_vr:=math_zero_divide
		end;
	      math_zero_divide:
		begin print(15,' MATH_ZERO_DIVIDE signalled');
		  math_vr:=math_arg_arcsin
		end;
	      math_arg_arcsin:
		begin print(16,' MATH_ARG_ARCSIN signalled');
		  math_vr:=math_arg_arccos
		end;
	      others: begin print(17,' MATH_ERROR signalled');
			signal()
		      end
	    end;
	  others: begin print(18,' OTHERS signalled');
		    exception_message; signal()
		  end
      end end;
      exception
	math_error:
	  if mathstatus=math_arg_arccos then
	    print(20,' MATH_ARG_ARCCOS signalled') 
	  else begin print(21,' MATH_ERROR signalled');
		 exception_message
	       end;
	allconditions: begin print(22,' ALLCONDITIONS signalled');
			 exception_message
		       end
    end.
   