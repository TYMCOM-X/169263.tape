type name_code = packed array [1..3] of char;
external function tempname ( name_code ): file_name;
    