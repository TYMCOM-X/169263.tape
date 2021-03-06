(* DEBLIB.INC - external declarations of runtime routines required by the
   Pascal Debugger.  These routines come from three different places:
  
  1.  routines from the Debugger support module DBSUP (from DBSUP.MAC) which
      are included in PASLIB.REL:                                           *)
  
       external function db$base: ^db_static_record;
       external function rt$base: ^rt_static_record;

       (* st$base and st$len are only referenced within DEBSYM, and therefore st_static_record
          will be defined only within that module.  *)

       external function st$base: ^st_static_record;
       external function st$len:  unit_range;

       external procedure sav$io (var saved_iostatus: machine_word; var saved_extstatus: machine_word);
       external procedure res$io (    saved_iostatus: machine_word;     saved_extstatus: machine_word);
  
(*
  2. routine linked into overlaid programs by the overlay manager:      *)
  
       external procedure ld$ovl (ovl_mod_no);
  
(*
  3. routine in the module IOBIN (in IOFILE.MAC) included in PASLIB:    *)
  
       external procedure db$open (var f:       file of disk_block;
                                       name:    packed array [1..*] of char;
                                       opts:    set of io_options);
       (* this is the Debugger's version of RESET - instead of utilizing NEW to allocate
          a file block on the heap it calls DB.FBL (in DBSUP) which will return the
          address of the portion of the Debugger's static area to be used for the file
          block.  *)
 