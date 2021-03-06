$PAGE module segout
module segout
  options special(word);

$include lib.typ
$include blkio.inc
$include dirio.inc
$PAGE wr_word
public procedure wr_word ( var f: library;
			   var num_to_write: word;	(* number to write,written *)
			   word_array: array[1..*] of word;(* array to write *)
			   var err: errcode );

(* This procedure writes num_to_write words from the array to the end of
   the currently open segment   *)

var
  i: word;  (* loop counter *)
  entry: directory_entry;

begin
  if not f.open_seg then err := seg_notopen
  else begin
    if num_to_write > upperbound (word_array) then
      num_to_write := upperbound (word_array);

    rd_dir (f, f.open_dir_ptr, entry, err);
    i := 1;
    while i <= num_to_write do begin  (* write to the segout buffer *)
      f.segout_buffer.words.data[(f.segout_ptr-1) div 5 + 1] :=
	word_array[i];

(*    if the buffer is full, flush it to the library file *)
      if ((f.segout_ptr+4) = chars_per_blk) then begin
	f.segout_ptr := 1;
	wr_segblk (f, err);
	entry.length := entry.length + chars_per_blk;
	wr_dir (f, f.open_dir_ptr, entry, err);

	f.segout_address := f.segout_address + 1
      end
      else f.segout_ptr := f.segout_ptr + 5;
    exit if err <> lib_ok;
      i := i + 1
    end;  (* while *)

    entry.length := entry.length + f.segout_ptr - 1;
    wr_dir (f, f.open_dir_ptr, entry, err);

    num_to_write := i - 1
  end
end;  (* wr_word *)
$PAGE wr_char
public procedure wr_char ( var f: library;
			   var num_to_write: char_idx;	(* number to write,written *)
			   char_array: array[1..*] of char;(* array to write *)
			   var err: errcode );

(* This procedure writes num_to_write chars from the array to the end of
   the currently open segment   *)

var
  i: char_idx;  (* loop counter *)
  entry: directory_entry;

begin
  if not f.open_seg then err := seg_notopen
  else begin
    if num_to_write > upperbound (char_array) then
      num_to_write := upperbound (char_array);

    rd_dir (f, f.open_dir_ptr, entry, err);
    i := 1;
    while i <= num_to_write do begin  (* write chars to the segout buffer *)
      f.segout_buffer.chars.data[f.segout_ptr] := char_array[i];

(*    if the buffer is full, flush it to the library *)
      if f.segout_ptr = chars_per_blk then begin
	f.segout_ptr := 1;
	wr_segblk (f, err);
	entry.length := entry.length + chars_per_blk;
	wr_dir (f, f.open_dir_ptr, entry, err);

	f.segout_address := f.segout_address + 1
      end
      else f.segout_ptr := f.segout_ptr + 1;
    exit if err <> lib_ok;
      i := i + 1
    end;  (* while *)

    entry.length := entry.length + f.segout_ptr - 1;
    wr_dir (f, f.open_dir_ptr, entry, err);

    num_to_write := i - 1
  end
end;  (* wr_char *)
$PAGE wr_line
public procedure wr_line ( var f: library;
			   line: string[*];		(* string to write *)
			   var err: errcode );

(* This procedure writes the given string to the end of
   the currently open segment   *)

var
  i: word;  (* loop counter *)
  entry: directory_entry;
  tline: string[255];

begin
  if not f.open_seg then err := seg_notopen
  else begin

    rd_dir (f, f.open_dir_ptr, entry, err);
    i := 1;
    tline := line || chr(13) || chr(10);  (* add EOLN chars *)
    while i <= length (tline) do begin  (* write the line to the segout buffer *)
      f.segout_buffer.chars.data[f.segout_ptr] := tline[i];

(*    if the buffer is full, flush it to the library *)
      if f.segout_ptr = chars_per_blk then begin
	f.segout_ptr := 1;
	wr_segblk (f, err);
	entry.length := entry.length + chars_per_blk;
	wr_dir (f, f.open_dir_ptr, entry, err);

	f.segout_address := f.segout_address + 1
      end
      else f.segout_ptr := f.segout_ptr + 1;
    exit if err <> lib_ok;
      i := i + 1
    end;  (* while *)

    entry.length := entry.length + f.segout_ptr - 1;
    wr_dir (f, f.open_dir_ptr, entry, err)

  end
end.  (* wr_line *)
  