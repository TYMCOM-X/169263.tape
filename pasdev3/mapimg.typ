(* MAPIMG.TYP - type definitions required by the VAX link management
   routine MAP_IMAGE_FILE.  *)

type
  mif_status = (mif_ok, mif_open_failure, mif_mapping_failure, mif_close_failure);
 