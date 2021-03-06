(*  The IfFileIndex type is used only in the PASIF module.  However, a
    variable of this type must be declared in PASDAT so that it can be
    preserved between passes, so the type is declared here.  *)


type
  if_file_index = ^ if_file_node;

  if_file_node = packed record
    next: if_file_index;
    block: blk;
    cursor: integer;
  end;
    