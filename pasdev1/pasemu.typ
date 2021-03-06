type
  wl_type = ^ wl_node;

  wl_node = record
    with_ref: expr; (* The record reference expression. *)
    last: wl_type (* The with list link. *)
  end;

  tag_value_list = ^ tag_val_node;

  tag_val_node = record
    tag: sym;
    labval: val;
    next: tag_value_list
  end;
   