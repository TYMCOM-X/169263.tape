$PAGE includes, externals, and globals
$INCLUDE QEDLN.TYP
$INCLUDE QEDLN.INC
$INCLUDE QED.TYP
$INCLUDE QEDERR.INC
$INCLUDE QLD.TYP
$INCLUDE QED.INC
$INCLUDE QUERY.INC
$INCLUDE EDITRE.INC
$INCLUDE IORESE.INC
$INCLUDE FILUTL.INC

external procedure do_scribe (line: cmdline);
external function setlib (ppn: file_id): boolean;
external procedure runprg (prog: file_id);
external procedure runcll (prog: file_id);
external procedure dotcl;
external function onescape: boolean;
external procedure passcc;


var
  lasterr: qerrcode;                    (* last error reported *)
  lasterrlevel: qederrlevel;            (* level of message last printed *)
$PAGE command definitions;
type
  cl_cmds = ( compilecmd, pascalcmd, linkcmd, runcmd, librarycmd, systatcmd,
              directorycmd, queuecmd, verifycmd, typecmd, protectcmd, renamecmd,
              deletecmd, copycmd, scribecmd, formatcmd, prettycmd, subdoccmd,
              hellocmd, byecmd, quitcmd, exit_cmd, lptcmd, whycmd, helpcmd       );

$include lookup.typ

type clcmdlist = array [cl_cmds] of cmdlist;
static var cllist: clcmdlist;

initprocedure;
 begin
  cllist [COMPILECMD    ].name := 'COMPILE   ';  cllist [COMPILECMD     ].abbrev := 3;
  cllist [PASCALCMD     ].name := 'PASCAL    ';  cllist [PASCALCMD      ].abbrev := 3;
  cllist [LINKCMD       ].name := 'LINK      ';  cllist [LINKCMD        ].abbrev := 4;
  cllist [RUNCMD        ].name := 'RUN       ';  cllist [RUNCMD         ].abbrev := 1;
  cllist [LIBRARYCMD    ].name := 'LIBRARY   ';  cllist [LIBRARYCMD     ].abbrev := 3;
  cllist [SYSTATCMD     ].name := 'SYSTAT    ';  cllist [SYSTATCMD      ].abbrev := 3;
  cllist [DIRECTORYCMD  ].name := 'DIRECTORY ';  cllist [DIRECTORYCMD   ].abbrev := 3;
  cllist [LPTCMD        ].name := 'LPT       ';  cllist [LPTCMD         ].abbrev := 3;
  cllist [QUEUECMD      ].name := 'QUEUE     ';  cllist [QUEUECMD       ].abbrev := 1;
  cllist [VERIFYCMD     ].name := 'VERIFY    ';  cllist [VERIFYCMD      ].abbrev := 1;
  cllist [TYPECMD       ].name := 'TYPE      ';  cllist [TYPECMD        ].abbrev := 2;
  cllist [PROTECTCMD    ].name := 'PROTECT   ';  cllist [PROTECTCMD     ].abbrev := 3;
  cllist [RENAMECMD     ].name := 'RENAME    ';  cllist [RENAMECMD      ].abbrev := 3;
  cllist [DELETECMD     ].name := 'DELETE    ';  cllist [DELETECMD      ].abbrev := 1;
  cllist [COPYCMD       ].name := 'COPY      ';  cllist [COPYCMD        ].abbrev := 2;
  cllist [SCRIBECMD     ].name := 'SCRIBE    ';  cllist [SCRIBECMD      ].abbrev := 2;
  cllist [FORMATCMD     ].name := 'FORMAT    ';  cllist [FORMATCMD      ].abbrev := 6;
  cllist [PRETTYCMD     ].name := 'PRETTY    ';  cllist [PRETTYCMD      ].abbrev := 6;
  cllist [SUBDOCCMD     ].name := 'SUBDOC    ';  cllist [SUBDOCCMD      ].abbrev := 6;
  cllist [HELLOCMD      ].name := 'HELLO     ';  cllist [HELLOCMD       ].abbrev := 5;
  cllist [BYECMD        ].name := 'BYE       ';  cllist [BYECMD         ].abbrev := 3;
  cllist [QUITCMD       ].name := 'QUIT      ';  cllist [QUITCMD        ].abbrev := 2;
  cllist [EXIT_CMD      ].name := 'EXIT      ';  cllist [EXIT_CMD       ].abbrev := 2;
  cllist [WHYCMD        ].name := 'WHY       ';  cllist [WHYCMD         ].abbrev := 3;
  cllist [HELPCMD       ].name := 'HELP      ';  cllist [HELPCMD        ].abbrev := 4;
 end;

(* procedure to lookup command names *)

external function lookup_clcmds
    (   line: cmdline; var idx: cmdlineidx;
        var list: clcmdlist; maxscalar: cl_cmds;
        var cmd: cl_cmds                   ): boolean;
$PAGE cl_execute
static var cmdfile: text;                               (* file to pass commands to programs *)

procedure cl_execute
     (  line: cmdline;                           (* line to parse *)
        var buffer: qbuffer                    (* buffer in use *)
        var err: qerrcode       );              (* QED error code. Value passed in is error code
                                                   from qexecute. If no cl command is found, it is
                                                   left unchanged; else, it is set to a code denoting
                                                   an error in the command or QOK. *)

 var
   lindex: cmdlineidx;                          (* parsing cursor *)
   cmd: cl_cmds;                                (* command to execute *)
   progname, fid1, fid2: file_id;               (* file names *)
   dummyline: cmdline;                          (* for building commands for pip, etc. *)
   temp: packed array[1..10] of char;


  (* simple parsing utilities *)

  procedure skipblanks;
   begin
    while (lindex <= length (line)) andif (ord (line[lindex]) <= ord (' '))
      do lindex := lindex + 1
   end;

  function checkeol: boolean;
   begin
    skipblanks;
    checkeol := (lindex > length (line))
   end;

  function checkpunct (ch: char): boolean;
   begin
    skipblanks;
    if (lindex <= length (line)) andif (ch = line[lindex])
      then begin
        checkpunct := true;
        lindex := lindex + 1
      end
    else checkpunct := false
   end;

  function file_parameter (var fid: file_id): boolean;
   var l: cmdlineidx;
   begin
    fid := '';
    file_parameter := false;
    skipblanks;
    if lindex > length (line) then return;      (* nothing on line - don't check for semicolon since
                                                   on some systems, semi is part of filename *)
    l := lindex;
    if pr_file_id (line, lindex, fid)           (* see if file here *)
      then file_parameter := true
      else if l = lindex                        (* bad file, if cursor ... *)
        then file_parameter := false            (* ... didn't move, no filename there *)
        else begin                              (* ... did move, error *)
          file_parameter := false;
          err := qbadfile
        end
   end;

  function filelist (var cmd: cmdline): boolean;
   begin
    filelist := false;                          (* assume failure *)
    loop                                            (* any number of files permissible *)
      if not file_parameter (fid1) then return;
      cmd := cmd || fid1;
    exit if checkeol;
      if not checkpunct (',') then return;          (* files must be separated by ','s *)
      cmd := cmd || ','
    end;
    filelist := true;
   end;

(* routine to pass a command to a program *)

  procedure run (prog: file_id; command: cmdline);
   var prg: packed array[1..3] of char;         (* to get first 3 chars of filename *)
   begin
    prg := substr (prog, 5);                    (* assume name in dev:progrm format *)
    rewrite (cmdfile, '###' || prg || '.tmp');  (* file is to contain command line *)
    writeln (cmdfile, command);                 (* put in command line *)
    writeln (cmdfile);                          (* for those which require a blank to terminate *)
    close (cmdfile);
    runcll (prog);                              (* run program at CL entry point *)
   end;

  (* routine to check if okay to discard buffer contents *)

  function discard_changes: boolean;
   begin
    if buffer.changes
      then discard_changes := query ('Unwritten changes, OK')
      else discard_changes := true              (* nothing to lose, proceed *)
   end;

  (* routine to insure that buffer contents have been written *)

  function output_buffer: boolean;
   begin
    if buffer.changes then begin
      if buffer.curfileok
        then qfilewrite (buffer, buffer.curfile, 1, buffer.lastlineno, err)
        else err := qbadfile;
      if err <> qok then begin
        output_buffer := false;
        return
      end
    end;
    output_buffer := true;
   end;

  (* procedure to insure that their is a default file, and that it has been
     written.  For use with commands that apply to the default. *)

  function use_buffer: boolean;
   begin
    if buffer.curfileok
      then use_buffer := output_buffer          (* use if okay to output to it *)
      else begin                                (* nothing to use *)
        err := qbadfile;
        use_buffer := false
      end
   end;


begin (* cl_execute *) ;
  lindex := 1;                                  (* get command name *)
  if not lookup_clcmds (line, lindex, cllist, maximum (cl_cmds), cmd)
    then return;                                (* no command on line *)

  case cmd of

    runcmd:
      begin
        if file_parameter (progname) andif checkeol
          then runprg (progname)                (* run the specified program *)
          else return
      end;

    compilecmd:
      begin
        if checkeol then dummyline := ''                (* no options *)
        else if checkpunct ('/') then dummyline := substr (line, lindex-1)
        else return;
        if not use_buffer then return;
        run ('sys:pascal', buffer.curfile || dummyline)
     end;

    pascalcmd:
      begin
        if not checkeol then return;
        if not output_buffer then return;
        rewrite (cmdfile, '###pas.tmp'); close (cmdfile);       (* delete any command file lying around *)
        runprg ('sys:pascal')
      end;

    linkcmd:
      begin
        if checkeol
          then runprg ('sys:link')                      (* enter link command level *)
          else begin                                    (* pass command line to  link *)
            rewrite (cmdfile, '###lnk.tmp');            (* done here, because link is non-standard *)
            writeln (cmdfile, substr (line, lindex));
            writeln (cmdfile, '/g');
            close (cmdfile);
            runcll ('sys:link')
          end
      end;

    librarycmd:
      begin
        if not setlib (substr (line, lindex)) then return
      end;

    systatcmd:
      runprg ('sys:systat');

    directorycmd:
      runprg ('sys:direct');

    lptcmd:
      begin
        if checkeol
          then run ('sys:queue', 'lpt:/loc:97/l')       (* lpt <cr> => list queue *)
          else run ('sys:queue', 'lpt:/loc:97=' || substr (line, lindex))       (* queue files (with options) *)
      end;

    queuecmd:
      runprg ('sys:queue');

    verifycmd:
      runprg ('sys:filcom');

    scribecmd:
      begin
        if checkeol then begin                  (* scribe default file *)
          if not use_buffer then return;
          do_scribe (buffer.curfile);
        end
        else if checkpunct ('/') then begin             (* default file with args *)
          if not use_buffer then return;
          do_scribe (buffer.curfile || substr (line, lindex-1))
        end
        else do_scribe (substr (line, lindex))  (* full invocation with args *)
     end;

    formatcmd:
      runprg ('rnd:format');

    prettycmd:
      runprg ('rnd:pretty');

    typecmd:
      begin
        if file_parameter (fid1) andif checkeol
          then run ('sys:pip', 'tty:=' || fid1)
          else return
      end;

    renamecmd:
      begin
        if file_parameter (fid1) andif checkpunct ('=')
             andif file_parameter (fid2) andif checkeol
          then run ('sys:pip', fid1 || '/r=' || fid2)
          else return
      end;

    copycmd:
      begin
        if file_parameter (fid1) andif checkpunct ('=')
             andif file_parameter (fid2) andif checkeol
          then run ('sys:pip', fid1 || '/x=' || fid2)
          else return
      end;

    deletecmd:
      begin
        dummyline := 'dsk:/d=';                           (* pip deletion command *)
        if not filelist (dummyline) then return;
        run ('sys:pip', dummyline)
      end;

    protectcmd:
      begin                                     (* protect <nnn> files *)
        if not checkpunct ('<') then return;
        lindex := lindex - 1;                   (* position at the '<' *)
        if (lindex+4) > length (line) then return;      (* too few characters to have protection code *)
        if verify (substr (line, lindex+1, 3), ['0'..'7']) <> 0 then return;
        if line[lindex+4] <> '>' then return;
        dummyline := substr (line, lindex, 5) || '/r=';          (* extract proctection code *)
        lindex := lindex+5;                             (* move past protection code *)
        if not filelist (dummyline) then return;        (* add list of files to rename *)
        run ('sys:pip', dummyline);
      end;

    whycmd:
      begin
        qederr (ttyoutput, lasterr, lasterrlevel + 1);
        lasterrlevel := lasterrlevel + 1
      end;

    helpcmd:
      begin
        for cmd := minimum (cl_cmds) to maximum (cl_cmds) do begin
	  if (ord (cmd) mod 7) = 0 then writeln (tty);	(* put 7 on a line *)
	  temp := lowercase (cllist[cmd].name);
	  write (tty, temp);
        end;
        writeln (tty, '+ QED commands.');
	writeln (tty); break;
      end;

    hellocmd:
      begin
        if not checkeol then return;
        if discard_changes then runprg ('sys:hello')
      end;

    byecmd:
      begin
        if not checkeol then return;
        if discard_changes then runprg ('sys:logout')
      end;

    exit_cmd, quitcmd:
      begin
        if not checkeol then return;
        dotcl
      end;

    others:
      begin
        writeln (tty, 'Command not yet implemented.');
        break
      end

  end;

  err := qok                                    (* if we get here, no error has been found in the command *)
end;
$PAGE command loop declarations
public procedure pasclp;

var
  buffer:       qbuffer;                        (* working buffer *)
  line:         cmdline;                        (* command to process *)
  lindex:       cmdlineidx;
  execrng:      ldrange;                        (* temps for calling qexecute *)
  ble:          qlineno;
  err:          qerrcode;                       (* qed error code *)


begin
  open (tty); rewrite (tty);
  passcc;                               (* read control characters *)
  qinitbuf (buffer);                            (* initialize the buffer *)
  qinit (buffer);                               (* setup editor parameters *)
  line := '';                                   (* init previous command line *)

  if onescape then begin                        (* set up escape handler *)
    ioreset;                                    (* get I/O in definable state *)
    open (tty); rewrite (tty);
    writeln (tty);
  end;

  writeln (tty);
  loop
    write (tty, '>'); break;                    (* issue prompt *)
    line := editread (line);                    (* read command line *)

    lindex := 1;        (* see if the command can be processed by qed *)
    execrng.lbound := 0; execrng.hbound := qbuflength (buffer);
    qexecute (buffer, line, lindex, execrng, ble, false,
         [minimum (qedcmds)..maximum (qedcmds)] - [why], err);

    if err <> qok then                     (* not QED cmd, try for exec command *)
      cl_execute (line, buffer, err);      (* many cases will not return *)

    (* If cl_execute finds a valid command (and returns), err will be qok or a
       code indicating the appropriate error for the command. Otherwise it is
       left unchanged. *)

    if err <> qok then begin
      ireset;                                   (* flush type-ahead *)
      qederr (ttyoutput, err, 1);                (* print error message *)
      lasterr := err;
      lasterrlevel := 1;
    end
  end (* loop *) ;

end.                                            (* start-up *)
    