

external var
  line_text: packed array [1..256] of char;
  line_no: integer;
  line_len: 0 .. 256;


external procedure inpinit;

external function  get_char ( var c: char ): char;

external procedure put_back ( c: char );

external procedure str_back ( s: str_pointer; ind, len: string_index );

external procedure blk_back ( t: eval_text; ind, len: eval_index );

external procedure tsave ( s: string_parm );

external procedure tscratch;

external procedure tkeep;
 