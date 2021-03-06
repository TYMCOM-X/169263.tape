(* DEBASM.INC - external declarations for public routines of Pascal Debugger
   module of assembly language helpers DEBASM.MAC.  *)
  
external procedure ld$byte  (word: unit_range; bit, bitsize: elem_sizes; var value: machine_word);
external procedure st$byte  (word: unit_range; bit, bitsize: elem_sizes;     value: machine_word);
  
external procedure abort$;
  
external procedure tt$in    (var ch: char);
external procedure tt$out   (    ch: char);
  
external procedure deb$call (start: half_word);
   