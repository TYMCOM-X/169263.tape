
(*********   S C N N A M - - SCANNR Name Table Utilities   *********)

type input_modes = ( ascii_mode, numeric_mode, symbolic_mode, undefined_mode );

external var       input_mode: input_modes;
		   min_symbol, max_symbol: number;

external procedure initnames;
external procedure clearnames;
external procedure entername ( sym_value; reg_exp );
external procedure entersymbol ( sym_value; number );
external function  lookupname ( sym_value ): reg_exp;
external procedure set_mode ( sym_value; input_modes );
external procedure prt_symbol ( number; parm_string );
  