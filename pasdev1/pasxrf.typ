type

  xrf_class =
     (  value_ctxt, mod_ctxt, var_parm_ctxt, ref_ctxt, file_xrf, page_xrf,
        line_xrf, block_xrf, end_xrf, index_xrf, deref_xrf, call_xrf,
        field_xrf, wfield_xrf, decl_xrf, fileblk_xrf, baserec_xrf  );

  xparm_val = 0 .. #O777777;

  xrf_record = packed record
    code: xrf_class;
    var_lab_parm: boolean;
    parameter: xparm_val
  end;

(*  xst_record = packed record
        id_number: xparm_val;
        name: xparm_val;
        parent: xparm_val;
        type_name:  xparm_val;
        type_class: type_kind;
        case class: sym_kind of
          blocks:
            ( blk_class: block_kind );
          vars,
          consts,
          values:
            ( var_class: storage_class )
    end;  *)
