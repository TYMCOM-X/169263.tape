external procedure deb_stable;	(* dump symbol table for debugger *)
external procedure deb_init;	(* initialize debug dump structures *)
external procedure stmt_block (stmt: source_id; stmt_kind: symbols);	(* dumps stmt block *)
external procedure fp_blocks;	(* dumps file and page blocks *)
external function trace_block (block: blk): def;	(* dumps trace block *)
external procedure prog_block;	(* dumps program block *)
external procedure blk_end;	(* end of block stmt check *)
    