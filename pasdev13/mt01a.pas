(* MT01A - main line for Pascal overlay system test program.  Allows
   a user to interactively exercise the overlay manager.

   All files in this test system begin with the letters 'MT'.  All of
   the modules should be compiled with the environment file created
   from file MTENV.PAS.

   The main routine loops, reading user commands.  The permitted
   commands are:
    	1. 'Q' - exit the program,
	2. 'C <procedure number>' - call the public procedure with
	   the given number,
	3. 'F <file name> [TRUE ! FALSE]' - enter a new file,
	4. 'A <module name> <file index>' - associate a previously entered
	   file,
	5. 'D <file index>' - delete a file,
	6. 'V <module name> <version number>' - change module version. 
   
   The public procedures which the user may call interactively
   all have names of the form 'MTxyz', where x, y and z are digits
   with the following interpretation:
	x - the area number, or 0 for a sharable overlay,
	y - the overlay module within area x (y is 1 based),
	z - the entry point number within module y (z is 1 based).

   Source files are numbered similarly with the one exception that
   'z' is a letter indicating the Pascal module within the overlay
   module (i.e., 'A' is the first Pascal module composing overlay
   module 'xy', 'B' is the second, etc.).  *)

program mt01a options mainseg;

public exception
  user_condition;

public var
  proc_map: map_array;
  procs: proc_array;

var
  cmd_line: cmd_string;
  proc_index: procedure_index;
  cursor: cmd_line_index;
  file_name_text: file_name;
  is_database: boolean;
  file_index: fileindex;
  code: ovl_code;
  mod_name: mod_name_string;
  mod_id: ovl_id;
  found: boolean;
  version_num: version;
  time_stamp: dtime_int;

label
  100;

$PAGE valid_index, get_proc_index

(* VALID_INDEX is passed an integer.  It returns TRUE if the integer
   is a valid index into the PROCS array.  *)

public function valid_index ( value: integer ): boolean;

begin
  valid_index := (value > minimum(procedure_index)) and
		 (value <= maximum(procedure_index));
end;


(* GET_PROC_INDEX is passed a string which should contain the text of
   an integer.  If the integer is a valid procedure number then the
   index in array PROCS of the procedure is returned; otherwise 0
   is returned.  *)

public function get_proc_index ( cmd_line: cmd_string ): procedure_index;

var
  i: integer;

begin
  getstring ( cmd_line, i );
  if (iostatus = io_ok) andif (i >= minimum(map_index)) andif
     (i <= maximum(map_index)) andif valid_index(proc_map[i])
    then get_proc_index := proc_map[ i ]
    else get_proc_index := 0;
end  (* proc get_proc_index *);
$PAGE get_token
(* GET_TOKEN is a parsing utility.  It is passed a string (parameter
   CMD_LINE) and an index into the string (parameter CURSOR).  It
   returns the text of the first substring at or beyond the index
   which is delimited by blanks or tabs.  If the index is beyond the 
   end of the string or only blanks and tabs remain, then the null 
   string is returned.  Parameter CURSOR is updated to point beyond the
   end of the token, if a token is found.  *)

function get_token ( cmd_line: cmd_string; var cursor: cmd_line_index ): cmd_string;

const
  tab: char:= chr( 9 );
  white_space: set of char := [' ', tab];

var
  start: cmd_line_index;

begin
  start := verify ( substr(cmd_line, cursor), white_space, 
    length(cmd_line) - cursor + 2) + cursor - 1;
  cursor := search ( substr(cmd_line, start), white_space, 
    length(cmd_line) - start + 2) + start - 1;
  get_token := substr ( cmd_line, start, cursor - start );
end  (* proc get_token *);
$PAGE get_mod_id
(* GET_MOD_ID is given a module name string (parameter MOD_NAME).  It
    looks the name up in a tabe of module names.  If the name is found
   then parameter found is set to true and parameter MOD_ID is set
   to the corresponding element of scalar type OVL_ID.  If the name is
   not found then FOUND is set to false.  *)

procedure get_mod_id ( mod_name: mod_name_string; var mod_id: ovl_id;
    var found: boolean );

type 
  mod_record = packed record
    name: mod_name_string;
    id: ovl_id;
  end;
  mod_array = packed array [mod_index] of mod_record;

static var
  mod_table: mod_array := (
    ( 'MTINIT', mtinit_ov ),
    ( 'MT11',   mt11_ov ),
    ( 'MT12',   mt12_ov ),
    ( 'MT13',   mt13_ov ),
    ( 'MT21',   mt21_ov ),
    ( 'MT22',   mt22_ov ),
    ( 'MT23',   mt23_ov ),
    ( 'MT02',   mt02_ov ),
    ( 'MT03',   mt03_ov ) );

var
  i: 0..maximum(integer);

begin
  found := false;
  i := minimum( mod_index );
  while not found and (I <= maximum(mod_index)) do begin
    found := uppercase(mod_name) = mod_table[ i ].name;
    if found
      then mod_id := mod_table[ i ].id
      else i := i + 1;
  end;
end  (* get_mod_id *);
$PAGE mt011

(* MT011 simply does a non-local GOTO to the main routine
   whenever called.  *)

public procedure mt011;

begin
 goto 100;
end (* proc mt011 *) ;
$PAGE mt012

(* MT012 - permits the user to call another public procedure
   or to return (if an invalid procedure number is entered).  *)


public procedure mt012;

var
  cmd_line: cmd_string;
  proc_index: procedure_index;

begin
  loop
    write ( ttyoutput, 'MT012 - enter proc number: ' );
    break ( ttyoutput );

    if eoln ( tty ) then readln ( tty );
    read ( tty, cmd_line );

    proc_index := get_proc_index ( cmd_line );
  exit if not valid_index ( proc_index );
    procs[proc_index];
  end (* loop *) ;
  writeln ( ttyoutput, 'Exit MT012' );
  break ( ttyoutput );
end;
$PAGE mt01a - main routine

begin
  open ( tty );
  rewrite ( ttyoutput );

  mtinit;	(* init array of procs *)

100:

  loop
    write ( ttyoutput, 'Main routine - enter command: ' );
    break ( ttyoutput );

    if eoln ( tty ) then readln ( tty );
    read ( tty, cmd_line );

    if length ( cmd_line ) >= 1 then
    case uppercase ( cmd_line[ 1 ] ) of

      'Q':	stop;			(* 'Q' - stop *)

      'C', '0'..'9':
	begin				(* 'C <proc num>' - call a proc *)
	  if uppercase( cmd_line[ 1 ] ) = 'C' 
	    then cmd_line := substr ( cmd_line, 2 );
	  proc_index := get_proc_index ( cmd_line );
	  if valid_index ( proc_index ) then
	    procs[ proc_index ];
	end;

      'F':				(* Enter a new file *)
	begin				(* 'F <file name> [TRUE ! FALSE]' *)
	  cursor := 2;
	  file_name_text := get_token ( cmd_line, cursor );
	  is_database := uppercase(get_token(cmd_line,cursor)) = 'TRUE';
	
	  ovextfile ( file_name_text, is_database, file_index, code );

	  writeln ( ttyoutput, 'Ovl code: ', ord(code), '    File index: ',
	    file_index );
	end;

      'V':				(* Change versions *)
	begin				(* 'V <module name> <Version number>' *)
	  cursor := 2;
	  mod_name := get_token ( cmd_line, cursor );
	  get_mod_id ( mod_name, mod_id, found );

	  if not found then begin
	    writeln ( ttyoutput, '?Unknown module name' );
	  end
	  else begin
	    getstring ( substr(cmd_line, cursor), version_num );
	    ovlversion ( mod_id, version_num, time_stamp, code );
	    writeln ( ttyoutput, 'Ovl code: ', ord(code), '    Update time: ',
	      dc_ext ( time_stamp ) );
	  end;
	end;

      'A':				(* Associate a file *)
	begin				(* 'A <module name> <file index>' *)
	  cursor := 2;
	  mod_name := get_token ( cmd_line, cursor );
	  get_mod_id ( mod_name, mod_id, found );
	  if not found then begin
	    writeln ( ttyoutput, '?Unknown module name' );
	  end
	  else begin
$IFNOT AUTO_FILE_INDEX
	    getstring ( substr(cmd_line,cursor), file_index);
	    if iostatus <> io_ok then file_index := 0;
$ENDIF
	    ovmodfile ( mod_id, file_index, code );
	    writeln ( ttyoutput, 'Ovl code: ', ord(code) );
	  end;
	end;

      'D':				(* Delete a file from OVL MGR's tables *)
	begin				(* 'D <file index>' *)
$IFNOT AUTO_FILE_INDEX
	  getstring ( substr ( cmd_line, 2 ), file_index );
$ENDIF
	  ovdelfile ( file_index, code );
	  writeln ( ttyoutput, 'Ovl code: ', ord(code) );
	end;


      others: ;

    end  (* case *) ;
  end  (* while *) ;
end.
