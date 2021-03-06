$PAGE segment association module
module segasn
  options special(word);

$include lib.typ
$include blkio.inc
$include dirio.inc
$include dtime.inc[31024,320156]
$PAGE cls_segment
public procedure cls_segment ( var f: library );

(* This procedure closes the currently open segment. *)

var err : errcode;

begin
  if f.open_seg then begin
  (* first, flush the current segout buffer *)
    f.segout_buffer.words.data[(f.segout_ptr-1) div 5 + 1]:= 0;
    while ((f.segout_ptr + 4) < chars_per_blk) do begin
      f.segout_ptr := f.segout_ptr + 5;
      f.segout_buffer.words.data[(f.segout_ptr-1) div 5 + 1] := 0
    end;
    f.open_seg := false;
    wr_segblk (f, err );
    f.segout_address := f.segout_address + 1;
    f.segout_ptr := 1
  end
end; (* cls_segment *)
$PAGE opn_segment
public procedure opn_segment ( var f: library;
                                seg_name: name_type; (* segment to open *)
                            var err: errcode );

(* This procedure creates a new directory entry open for writing,
   and closes any previously opened segment.  *)

var entry : directory_entry;


  procedure set_entry_ptr;
  (* This procedure attempts to find a not in_use directory entry
     in the currently read block and sets the open_dir_ptr there.
     If one can't be found in the current block, then succeeding
     blocks are searched.    *)

  var idx, index : dir_idx;

  begin  (* set_entry_ptr *)
    index := 0;

    (* first, find if it already exists. If so, write over the entry *)
    if find_dir (f, seg_name, index ) then f.open_dir_ptr := index

    else begin  (* to find an free directory entry *)
      if f.dir_buf_loc = 0 then idx := 1
      else begin
       idx := (f.dir_buf_loc-1) * dirs_per_blk + 1;
  rd_dir ( f, idx, entry, err );

      while ((entry.in_use) and (idx <= f.last_dir) and (err = lib_ok)) do begin
      rd_dir (f, idx, entry, err );
         idx := idx + 1
      end
      end;

      if err = lib_ok then f.open_dir_ptr := idx
    end
  end;  (* set_entry_ptr *)


$PAGE opn_seg main
begin  (* body of opn_segment *)
  if f.open_seg then cls_segment (f);
  set_entry_ptr;

  with entry do begin
    file_seg.name := seg_name.name;
    file_seg.ext := seg_name.ext;
    address := f.segout_address;
    length := 0;
    write_date := daytime;
    access_date := daytime;
    in_use := true
  end;

  f.open_seg := true;
  f.segout_ptr:=1;
  wr_dir ( f, f.open_dir_ptr, entry, err );
end. (* opn_segment *)
 
   