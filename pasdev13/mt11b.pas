(* MT11B - module of Pascal overlay manager test program.
   Prompts for the number of the next procedure to call (enter
   an invalid procedure number to return to the caller.  *)

module MT11B options overlay;

external var
  local_pub: integer;

public procedure MT112;

var
  cmd_line: cmd_string;
  proc_index: procedure_index;
  local_var: integer;

begin
  local_var := local_pub;
  local_pub := 888;

  loop
    write ( ttyoutput, 'MT112 - enter proc number: ' );
    break ( ttyoutput );

    if eoln ( tty ) then readln ( tty );
    read ( tty, cmd_line );

    proc_index := get_proc_index ( cmd_line );
  exit if not valid_index ( proc_index );
    procs[proc_index];
  end (* loop *) ;
  writeln ( ttyoutput, 'Exit MT112' );
  break ( ttyoutput );
end.
