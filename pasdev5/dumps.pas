$LENGTH 43
$OPTIONS special, notrace, nocheck

module dumps$;

$INCLUDE debtyp.inc
$INCLUDE debrun.inc
$INCLUDE debio.inc
external var scope$stack:scope_type;
external function find$module(pb:progblkptr): alfa;
external function deref$ctp(ptr:integer):ctp;
external function first$module:linkentryptr;
external function next$module(curmodule:linkentryptr):linkentryptr;
external function r50$asc(r50name:r50word):alfa;
external procedure info$stmt(sb: stmtblkptr; var info: source_id);
external function sym$lookup(name: alfa; var mname: alfa): linkentryptr;
external function call$basis(sf: stkframeptr): stkframeptr;
$PAGE dmp$files  prints file numbers, file names

public procedure dmp$files(pb:progblkptr);

(* Prints files in order chained, i.e., backwards *)

  var fb: fileblkptr;

   procedure print_file(fb:fileblkptr);
      begin
	 writ$int(fb^.file_number,decimal);
	 if fb^.file_name <> ''
	    then writ$str(' '!!fb^.file_name);
	 writ$nl('');
      end;	    (* print_file *)

   begin	    (* dmp$files *)
      fb:= pb^.last_file;
      repeat
	print_file(fb);
	fb:= fb^.previous_file
      until ord(fb)=0
   end;		    (* dmp$files *)
$PAGE dmp$pages  prints page numbers and names

public procedure dmp$pages(fb:fileblkptr);

(* Prints pages in order chained, i.e., backwards.  Previous page pointer
   of first page points to the file block. *)

  var pgb: pageblkptr;

   procedure print_page(pgb:pageblkptr);
      begin
	 writ$int(pgb^.page_number,decimal);
	 if pgb^.subtitle <> ''
	    then writ$str(' '!!pgb^.subtitle);
	 writ$nl('');
      end;	    (* print_page *)

   begin	    (* dmp$pages *)
      pgb:= fb^.last_page;
      repeat
	print_page(pgb);
	pgb:= pgb^.previous_page
      until ord(fb)=ord(pgb)
   end;		    (* dmp$pages *)
$PAGE dmp$scope prints out current scope$stack

public procedure dmp$scope;

   var
      proc_ctp:ctp;
      modname:alfa;
      level:displaylevel;

   begin	    (* dmp$scope *)
      with scope$stack.displays[1] do begin
	 modname := find$module(prog_blk);
	 writ$str(modname);
	 writ$str('(HISEG=');
	 writ$int(prog_blk^.hiseg_base,octal);
	 writ$str(',LOWSEG=');
	 writ$int(prog_blk^.lowseg_base,octal);
	 writ$str(')');
	 writ$nl('');
      end;	    (* with *)
      for level := 2 to scope$stack.display_levels do
	 with scope$stack.displays[level] do begin
	    proc_ctp := deref$ctp(proc_ptr);
	    writ$str(proc_ctp^.name);
	    writ$str(' (BASIS=');
	    if stackbase = nil then writ$str('UNDEFINED')
		else writ$int(ord(stackbase),octal);
	    writ$str(')');
	    writ$nl('');
	 end;	    (* with *)
   end;		    (* dmp$scope *)
$PAGE dmp$modules  prints out names of all pascal modules

public procedure dmp$modules;

   var lnkptr:linkentryptr;
       modulename:alfa;

   begin	    (* dmp$modules *)
      lnkptr := first$module;
      while lnkptr <> nil do begin
	 if ( (lnkptr^.modname mod 1000000b) = lnkptr^.firstword^.right_r50)
	    then begin
	       modulename := r50$asc(lnkptr^.modname);
	       if index(modulename,'$')=0 then begin (*ignore debugger modules*)
		 writ$str(modulename);
		 if lnkptr^.firstword^.prog_blk = nil
		    then writ$str('(NOT DEBUG)');
		 writ$nl('');
	       end;
	    end;    (* if *)
	 lnkptr := next$module(lnkptr);
      end;	    (* while *)
   end;		    (* dmp$modules *)
$PAGE dmp_source -- dump a source program location

procedure dmp_source(info: source_id);
begin
  with info do begin
    writ$str(substr(mname,1,index(mname,' ',11)-1) || '@');
    if fnumber<>0 then begin
      writ$int(fnumber,decimal);
      writ$ch('-')
    end;
    if pnumber<>0 then begin
      writ$int(pnumber,decimal);
      writ$ch('/')
    end;
    writ$int(lnumber,decimal)
  end
end (*dmp_source*);
$PAGE dmp$breaks -- dump currently set breakpoints

type brknumtype = 0..maxbrkpt;

external var brk$strings: array[0..maxbrkpt] of string;

public procedure dmp$breaks;
  var i: brknumtype; info: source_id;
	 nobreaks: boolean;
begin
  nobreaks:= true;
  for i:= minimum(brknumtype) to maximum(brknumtype) do
    if brk$table[i]<>nil then begin
      nobreaks:= false;
      writ$int(i,decimal); writ$ch(' ');
      info$stmt(brk$table[i],info); dmp_source(info);
      writ$nl('  ' || brk$strings[i])
    end;
  if nobreaks then writ$nl('No breakpoints set')
end (*dmp$breaks*);
$PAGE dmp$location -- dump current program location

public procedure dmp$location;
  var info: source_id; mainlink: ^procblklink; procblk: procblkptr;
begin
  case rea$on of

  init: writ$nl('First statement of main program');

  badreason: writ$nl('Erroneous entry to debugger -- please report');

  trap: writ$str('Debugger entered after trap');

  rterr: writ$str('Debugger entered after run-time error');

  step: ;

  brkpt: begin
    writ$str('Breakpoint #'); writ$int(break$,decimal)
  end;

  intrp: writ$str('Debugger entered after break hit')

  end (*case*);

  if rea$on in [trap..intrp] then begin
    if ord(c$tmt)<>0 then begin (*if defined*)
      writ$str(' at ');
      info$stmt(c$tmt,info); dmp_source(info);
    end;
    if ba$i$.mainflag=0 then begin
      mainlink:= ptr(ba$i$.basis^.rtn_addr);
      procblk:= mainlink^.proc_blk
    end
    else procblk:= ba$i$.basis^.link_addr^.proc_blk;
    if procblk<>nil then begin
      writ$str(' in '); writ$str(procblk^.proc_name)
    end;
    writ$nl('')
  end
end (*dmp$location*);
$PAGE dmp$stack -- dump current contents of stack

public procedure dmp$stack(n: stklevel);


label 1;	    (*emergency exit location*)

  (*routine to search for a jsp 1,stmt. either forwards or backwards*)

  type direction = (forwards,backwards);

  function find_jsp(startloc: addrrange; which_way: direction): stmtblkptr;
    var mname: alfa; lnkptr: linkentryptr; intptr: ^integer; jsp_stmt: integer;
    const jsp1_0 = 265040000000b;   (* jsp 1,0 *)
    var increment: -1..1;
  begin
    mname:= '';	    (*sym$lookup insists*)
    lnkptr:= sym$lookup('STMT.',mname);
    if lnkptr=nil then begin
      writ$nl('internal error in dmp$stack -- please report');
      goto 1
    end;
    jsp_stmt:= jsp1_0 + lnkptr^.symaddr;    (*construct word to match*)
    if which_way = forwards then increment:= 1
    else increment:= -1;
    intptr:= ptr(startloc);
    while intptr^<>jsp_stmt do	(*loop until found, forever, or trap*)
      intptr:= ptr(ord(intptr)+increment);
    find_jsp:= ptr(ord(intptr)+1)   (*point to word after jsp*)
  end (*find_jsp*);


  (*routine to dump information for main stack frame*)

  procedure dmp_main(sf: stkframeptr);
    var mainlink: ^procblklink;
  begin
    writ$str('1 '); (*main frame is always number 1*)
    mainlink:= ptr(sf^.rtn_addr);   (*main frame is different than normal*)
    if mainlink^.proc_blk = nil then writ$str('Mainline  ')
    else writ$str(mainlink^.proc_blk^.proc_name);
    writ$str(' BASIS='); writ$int(ord(sf),octal);
    writ$str(' LVL=0 LOC=');

    (*not straightforward to find first stmtblk in main, as the call to
      initp. is at end of main code, followed by jrst to actual code.
      hence, always print address of initp. call for location.*)

    writ$int(ord(sf^.rtn_addr)-1,octal);
    writ$nl('')
  end (*dmp_main*);


  (*routine to dump information for normal stack frame*)

  procedure dmp_frame(framenum: stklevel; sf: stkframeptr);
    var procblk: procblkptr; stmtblk: stmtblkptr;
	info: source_id; caller_sf: stkframeptr;
	mainlink: ^procblklink;
  begin
    writ$int(framenum,decimal); writ$ch(' ');
    procblk:= sf^.link_addr^.proc_blk;
    if procblk=nil then writ$str('Unknown   ')
    else writ$str(procblk^.proc_name);
    writ$str(' BASIS='); writ$int(ord(sf),octal);
    if procblk<>nil then begin
      writ$str(' LVL='); writ$int(procblk^.pflev,decimal)
    end;	    (*cannot determine level otherwise*)
    writ$str(' LOC=');
    if (procblk=nil) orif (procblk^.prog_blk=nil) then
      writ$int(ord(sf^.link_addr)-1,octal)  (*write pntry. address*)
    else begin	    (*find first stmt block in routine*)
      stmtblk:= find_jsp(ord(sf^.link_addr),forwards);
      info$stmt(stmtblk,info);
      dmp_source(info)
    end;
    writ$str(' CALL=');
    caller_sf:= call$basis(sf);
    if caller_sf=nil then begin	(*caller was mainline*)
      mainlink:= ptr(sf^.parent_basis^.rtn_addr);
      procblk:= mainlink^.proc_blk
    end
    else procblk:= caller_sf^.link_addr^.proc_blk;
    if (procblk=nil) orif (procblk^.prog_blk=nil) then
      writ$int(sf^.rtn_addr-1,octal)	(*caller not debug, write addr(pushj)*)
    else begin
      stmtblk:= find_jsp(sf^.rtn_addr-1,backwards); (*find 1st stmt before pushj*)
      info$stmt(stmtblk,info);
      dmp_source(info)
    end;
    writ$nl('')
  end (*dmp_frame*);


  var cursf, nextsf: stkframeptr; i,maxi: stklevel;

begin		    (*dmp$stack*)
  if ba$i$.mainflag=0 then dmp_main(ba$i$.basis)
  else begin
    maxi:= 1; (*determine number of frames on stack*)
    cursf:= ba$i$.basis; (*so we can number while printing backwards*)
    repeat
      maxi:= maxi+1;
      cursf:= call$basis(cursf)
    until cursf=nil;
    nextsf:= ba$i$.basis;
    i:= 0;
    repeat
      cursf:= nextsf;
      dmp_frame(maxi,cursf);
      i:= i+1; maxi:= maxi-1;	    (*maintain count of frames dumped*)
     exit if (i>=n) and (n>0);
      nextsf:= call$basis(cursf)
    until nextsf=nil;	(*caller of last routine was main*)
    if i<>n then dmp_main(cursf^.parent_basis);	(*if not exit on count*)
  end;

1:		    (*for emergency exit from find_jsp*)

end (*dmp$stack*).
    