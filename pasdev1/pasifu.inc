external var       t_chain: tuple;
		   t_end: tuple;
		   if_cursor: tuple;
external procedure ch_init;
external procedure ch_open ( old_file, new_file: boolean );
external procedure ch_close;
external procedure ch_end;
external procedure new_chain;
external procedure end_chain;
external procedure emit ( t: tuple );
external procedure t_set ( t: tuple );
external procedure ap_chain ( after, t: tuple );
external procedure dechain ( t: tuple );
external procedure reclaim;
external procedure clr_rslt;
external procedure del_tuples;
external procedure wr_tuples;
external procedure rd_tuples;
external function  is_expr ( t: tuple ): boolean;
