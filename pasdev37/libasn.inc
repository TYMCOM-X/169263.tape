external procedure open_library ( var f: library;  (* library to open *)
				lib_name: name_type;
				var new: boolean;
				var err: errcode );
(* This procedure opens or creates a library and initializes the library
   record *)

external procedure close_library ( var f: library );
   