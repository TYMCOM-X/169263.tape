type
  prgdir_word = 0..377777777777B;
  prgdir_string = string[15];

external procedure prgm_ppn (var ppn: prgdir_word);

external function prgm_dir: prgdir_string;
  