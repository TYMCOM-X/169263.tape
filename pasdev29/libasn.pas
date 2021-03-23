$PAGE library association module
module libasn
  options special(word);

$include lib.typ
$include dirio.inc
$PAGE open_library
public procedure open_library ( var f: library;  (* library to open *)
				lib_name: name_type;
				var new: boolean;
				var err: errcode );

(* This procedure opens or creates a library and initializes the library
   record *)

var entry : directory_entry;
(* Note -- all lines with a 'XXX' should be deleted later. *)
static var f_str: string[10]; (* XXX *)

begin
  new := false;

  with lib_name do
(* XXX
    update ( f.iofile, name||'.'||ext, [retry] );
XXX *)
    f_str := name || '.' || ext; (* XXX *)
    update ( f.iofile, f_str, [retry] ); (* XXX *)

  if iostatus (f.iofile) <> io_ok then err := lib_openfail

  else if eof(f.iofile) then begin  (* new library opened *)
	 new := true;
         f.last_dir := 0;
	 f.segout_address := num_dir_blks + 1
	 end
       else begin  (* previously existing library opened *)
	f.last_dir := num_dir_blks*dirs_per_blk;
	rd_dir ( f, f.last_dir, entry, err );

	while (not(entry.in_use) and (err = lib_ok)) do begin  (* find the last dir entry *)
	  f.last_dir := f.last_dir - 1;
	  rd_dir ( f, f.last_dir, entry, err )
	end;

	f.segout_address := entry.address + (entry.length-1)div chars_per_blk + 1
  end;
  f.dir_buf_loc := 0;
  f.segin_address := 0;
  f.segout_ptr := 1;
  f.open_seg := false

end;  (* open_library *)
$PAGE close_library
public procedure close_library ( var f: library );

(* This procedure closes a library file *)

begin
  close (f.iofile)
end. (* close_library *)
