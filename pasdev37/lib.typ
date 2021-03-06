$include dtime.typ[31024,320156]
const

  chars_per_blk := 635;      
  words_per_blk := 127;
  dirs_per_blk  := 16;

  num_dir_blks  := 4;              (* the extent of the directory *)
  max_blocks    := 377777;


type


  char_ptr = 1..chars_per_blk;
  word_ptr = 1..words_per_blk;
  dir_ptr  = 1..dirs_per_blk;

  seg_address = 0 .. max_blocks;
  dir_address = 0..num_dir_blks;

  address = 0..max_blocks;
  word = -377777777777b..377777777777b;
  dir_idx = 0..num_dir_blks*dirs_per_blk;      (* ptr to entire directory *)
  char_idx = 1..maximum(machine_word);           (* character index *)

  errcode =                    (* library error codes *)
    (        lib_ok,                 (* no error *)
        lib_notopen,            (* attempt to access a closed library *)
      lib_openfail,           (* file association failure *)
        lib_iofatal,            (* fatal I/O error *)
 seg_notopen,            (* attempt to write to unopened segment *)
    seg_notfind,            (* directory access failure *)
        bad_diridx,             (* non-existent directory entry *)
    file_notfind,           (* external file not found *)
 intr_fatal  );          (* internal addressing error *)

  name_type = record
    name : packed array[1..6] of char;
    ext  : packed array[1..3] of char
  end;

  directory_entry = record    (* an 8-word directory entry *)
    file_seg : name_type;
    address : 0..max_blocks;        (* start address of the segment *)
    length : 0..maximum(word);      (* length of the segment in characters *)
    write_date : dtime_int;  (* DTIME internal representations *)
    access_date : dtime_int;
    in_use : boolean                (* "in use" segment flag *)
  end;

  (* three one-block data types for I/O transfers *)
  dir_block = array[ dir_ptr ] of directory_entry;
  word_block = record
    checksum : word;
    data : packed array[ word_ptr ] of word
  end;
  char_block = record
    checksum : word;
    data : packed array[ char_ptr ] of char
  end;

  blocktype = (dir_type, word_type, char_type);

  library_rec = record        (* a one-block library record *)
    case blocktype of (* undiscriminated union *)
      dir_type : (entry : dir_block);
      word_type : (words : word_block);
      char_type : (chars : char_block)
  end;

$PAGE library record

  library = record               (* the internal library record.
                               This contains the I/O file,
                           buffers, and pointers needed for the
                          file management system.   *)

    iofile : file of library_rec;               (* I/O file *)

    dir_buffer,                               (* I/O buffers *)
    segin_buffer,
    segout_buffer : library_rec;

    dir_buf_loc : dir_address;                (* physical addresses of the *)
    segin_address,                     (* buffers. On input buffers, 0 *)
    segout_address : seg_address;   (* indicates the nil, or empty
                                        buffer (meaningless on output) *)

    segout_ptr : char_ptr;         (* points to the first
                                        unwritten char in the segout
                                  buffer. Whenever this increments
                                      past chars_per_blk and back to
                                        1, the segout buffer is
                                       flushed and the segout_address
                                        incremented.      *)

    open_seg : boolean;                 (* flag for open segment.
                                     When this is true, segout_
                                    address is assumed to have the
                                        address(physical) of the current
                                      output block, and segout_ptr
                                  is assumed to point to the first
                                      unwritten char in the buffer. *)

    open_dir_ptr : dir_idx;         (* an index to the directory
                                  entry of the currently open
                                   segment. Meaningless if 
                                      open_seg is false   *)

    last_dir : 0..num_dir_blks*dirs_per_blk           (* index to the last
                                  directory entry.  *)
  end;
 
  