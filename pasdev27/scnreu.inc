
(*********   S C N R E U  - -  SCANNR Regular Expression Utilities   *********)

external var       lambda: reg_exp;
external var       phi: reg_exp;
external procedure initre;
external procedure clearre;
external function  literal ( n: integer ): reg_exp;
external function  lit_range ( min, max: integer ): reg_exp;
external function  iterate ( op: reg_exp ): reg_exp;
external function  negate ( op: reg_exp ): reg_exp;
external function  conjoin ( left, right: reg_exp ): reg_exp;
external function  alternate ( left, right: reg_exp ): reg_exp;
external function  catenate ( left, right: reg_exp ): reg_exp;
external function  derivative ( re: reg_exp; a: number ): reg_exp;
external function  re_compare ( reg_exp; reg_exp ): relation;
external function  re_null ( reg_exp ): boolean;
external function  litval ( re: reg_exp; var result: integer ): boolean;
external procedure prt_re ( re: reg_exp );
    