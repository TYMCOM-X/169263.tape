external procedure fatal_error (message: packed array [1..*] of char);
external function get_temp (size: op_desc; len_in_words: boolean): op_desc;
external function dftemp () : op_desc;
external procedure dfpush_dummy;
external procedure dfpop;
external function bst_mv_seq (    n: integer;
                                  source_indirect, target_indirect: boolean;
                              var bests_size, bests_cycles: integer;
                              var result_loc_after_and_indirect: boolean): integer;
external procedure blk_move (    len_desc: op_desc;
                                 len_in_words: boolean;
                                 source_loc, dest_loc: op_desc;
                             var result_loc: op_desc;
                             var after_last_word: boolean);
external procedure do_check (exp: expr);
external procedure gen_code (var code_size, const_size, static_size: unit_range);
    