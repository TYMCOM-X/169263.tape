external procedure cls_segment ( var f: library );
(* This procedure closes the currently open segment. *)

external procedure opn_segment ( var f: library;
				seg_name: name_type; (* segment to open *)
				var err: errcode );
(* This procedure creates a new directory entry open for writing,
   and closes any previously opened segment.  *)
    