const

    line_size = 255;

type

    number = 0 .. 32767;

    reg_exp = ^ reg_exp_node;

    line_index = 0 .. line_size + 1;

    parm_string = packed array [1..*] of char;

    relation = ( lss, eql, gtr );
   