external procedure rd_dir ( var f: library;
			  index: dir_idx;  (* directory pointer *)
			  var entry: directory_entry; (* entry record *)
			  var err: errcode );
(* This reads the indexed directory entry and returns it. Error
   if the index is past the last directory entry  *)

external procedure wr_dir ( var f: library;
			  index: dir_idx;  (* directory pointer *)
			  entry: directory_entry; (* entry to write *)
			  var err: errcode );
(* This procedure writes over the indexed directory entry and flushes
   the dir buffer. If index > f.last_dir then the last_dir is 
   updated    *)

external function find_dir ( var f: library;
			   seg_name: name_type; (* entry to find *)
			   var index: dir_idx  (* index of find *)
				): boolean; (* true if successful find *)
(* This searches for the segname directory entry and returns its index.
   The boolean function value is an indicator of the success of the
   search. The search begins at index+1     *)
 