external procedure wr_word ( var f: library;
			   var num_to_write: word;	(* number to write,written *)
			   word_array: array[1..*] of word;(* array to write *)
			   var err: errcode );
(* This procedure writes num_to_write words from the array to the end of
   the currently open segment   *)

external procedure wr_char ( var f: library;
			   var num_to_write: char_idx;	(* number to write,written *)
			   char_array: array[1..*] of char;(* array to write *)
			   var err: errcode );
(* This procedure writes num_to_write chars from the array to the end of
   the currently open segment   *)

external procedure wr_line ( var f: library;
			   line: string[*];		(* string to write *)
			   var err: errcode );
(* This procedure writes the given string to the end of the currently
   open segment *)
   