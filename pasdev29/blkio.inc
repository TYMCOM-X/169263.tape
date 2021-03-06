external procedure rd_segblk ( var f: library;
			     addr: seg_address; (* physical address *)
			     var err: errcode );
(* This procedure reads the physical addressed block into the
   segin buffer and sets the segin address pointer.  *)

external procedure wr_segblk ( var f: library;
			     var err: errcode );
(* This procedure flushes the segout buffer to the segout address *)

external procedure rddirblk ( var f: library;
			     addr: dir_address; (* physical address *)
			     var err: errcode );
(* This procedure reads the physical addressed directory block into
   the directory buffer and sets the dir address pointer *)

external procedure wrdirblk ( var f: library;
			     var err: errcode );
(* This procedure flushes the directory buffer to the dir address *)

    