external procedure deb_init;	(* initialize debug dump structures *)
external procedure stmt_block (stmt: source_id; stmt_kind: symbols);	(* dumps stmt block *)
external procedure fp_blocks;	(* dumps file and page blocks *)
external function trace_block: def;	(* dumps trace block *)
external procedure blk_end;
    