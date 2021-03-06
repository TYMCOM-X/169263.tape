(* MTENV - environment module for Pascal overlay system test program.  *)

envmodule mtenv;

$SYSTEM mtest.mod
$SYSTEM dtime.typ[,320156]
$SYSTEM dtime.inc[,320156]
$SYSTEM ovldef.typ
$SYSTEM ovldef.inc

type
  overlay_procedure = procedure;
  procedure_index = 0..11;
  map_index = 1..999;
  map_array = packed array [map_index] of procedure_index;
  proc_array = array [procedure_index] of overlay_procedure;
  cmd_string = string[80];
  cmd_line_index = 0..81;
  mod_index = 1..9;
  mod_name_string = string[ 6 ];

external var
  proc_map: map_array;
  procs: proc_array;

external function get_proc_index ( cmd_string ): procedure_index;
external function valid_index ( integer ): boolean;

external procedure mt021;
external procedure mt031;
external procedure mtinit;
external procedure mt011;
external procedure mt012;
external procedure mt111;
external procedure mt112;
external procedure mt121;
external procedure mt131;
external procedure mt211;
external procedure mt221;
external procedure mt231;

end.

  