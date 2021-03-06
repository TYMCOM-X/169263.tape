(* MTINIT - initialization module for Pascal overlay system test program. *)

module mtinit options overlay;


public procedure mtinit;

var
  i: map_index;

begin
  for i := minimum(map_index) to maximum(map_index) do
    proc_map[ i ] := 0;

  proc_map[ 111 ] := 1;
  procs[ 1 ] := mt111;

  proc_map[ 121 ] := 2;
  procs[ 2 ] := mt121;

  proc_map[ 131 ] := 3;
  procs[ 3 ] := mt131;

  proc_map[ 211 ] := 4;
  procs[ 4 ] := mt211;

  proc_map[ 221 ] := 5;
  procs[ 5 ] := mt221;

  proc_map[ 231 ] := 6;
  procs[ 6 ] := mt231;

  proc_map[ 011 ] := 7;
  procs[ 7 ] := mt011;

  proc_map[ 012 ] := 8;
  procs[ 8 ] := mt012;

  proc_map[ 112 ] := 9;
  procs[ 9 ] := mt112;

  proc_map[ 021 ] := 10;
  procs[ 10 ] := mt021;

  proc_map[ 031 ] := 11;
  procs[ 11 ] := mt031;
end.
   