(* MMSSYM.TYP basic symbol table type definitions for overlay support. *)

$INCLUDE DTIME.TYP[,320156]

const mdlsig = 6;			(* significance of MDL names *)

type
  mdlid = string[mdlsig];		(* to store in the name table *)

  namekind = (areaname, modname, symname);
  symtype = (proc, func, variable, constant, loseg_entry, unknown);
  areaptr = ^anode;			(* basic pointer types *)
  modptr = ^mnode;
  symptr = ^snode;
  nameptr = ^nnode;

  nnode = record			(* name table node *)
    left, right: nameptr;		(* links in binary tree *)
    text: mdlid;			(* actual name of symbol *)
    case kind: namekind of		(* what am i? *)
      areaname: (aptr: areaptr);	(* i point to an area node *)
      modname: (mptr: modptr);		(* or to a module node *)
      symname: (sptr: symptr)		(* or a symbol node, that's all *)
    end;

(* The ODMS symbol table consists of four types of nodes -- name, area,
   module, and symbol nodes -- chained onto a master program node.  The
   name table is an unbalanced binary tree containing the text of the
   name and the kind of the corresponding symbol.  A name node's position
   in the tree is determined not only by its name but also by its kind;
   thus a name can represent entities of different kinds in a program.
   For example, a module can have the same name as an area or symbol.
   Area nodes form a list through their NEXT field, with the last area
   node's NEXT being NIL.  Module nodes are chained through their NEXT
   field in the order in which they were encountered in the MDL.  They
   are also chained within a single area through their MANEXT fields,
   from the MODS field of the area.  Sharable modules are recognized by
   the MAREA field being NIL; the ends of both kinds of lists are recognized
   by NIL pointers.  Symbols are also doubly chained, through their
   MNEXT fields for all symbols in the program in MTV order, and through
   their LNEXT fields for all symbols in a single module, in LTV order
   if specified.  These chains are in fact chains and not lists, with
   the last element pointing back to the first.  This is to detect
   symbols defined in a CONTAINS clause, and therefore chained to a
   module node with its LNEXT field, but not defined in the SYMBOLS
   section, which leaves the MNEXT field NIL.  Resident symbols are
   recognized by the LNEXT field being NIL.  *)

  anode = record			(* area list node *)
    asize: integer;			(* size of area in words *)
    aorig: integer;			(* origin as computed by odms *)
    name: nameptr;			(* name of the area *)
    next: areaptr;			(* next area ID in sequence *)
    mods: modptr;			(* first in list of mods in area *)
    end;

  mnode = record			(* module list node *)
    modid: integer;			(* mod #, >0 noshr, <0 shr *)
    stsize: integer;			(* size of module's static *)
    storig: integer;			(* static origin, from ODMS *)
    name: nameptr;			(* name of the module *)
    marea: areaptr;			(* area for this module *)
    next: modptr;			(* next module in sequence *)
    syms: symptr;			(* list of this mod's symbols *)
    manext: modptr;			(* chain of mods in given area *)
    end;

  snode = record			(* symbol node *)
    stype: symtype;			(* var, proc, or whatever *)
    name: nameptr;			(* pointer to name node *)
    psname: nameptr;			(* pseudonym, if any *)
    smod: modptr;			(* symbol is in this module *)
    mnext: symptr;			(* next symbol on MTV *)
    lnext: symptr;			(* next on my module's LTV *)
    mtvloc,				(* addresses on master and local *)
    ltvloc: integer;			(* transfer vectors respectively *)
    end;

  pnode = record			(* program node *)
    pname: mdlid;			(* to remember program's name *)
    mdlname: file_name;			(* remember name of MDL file *)
    odmsversion: integer;		(* just to verify *)
    alist: areaptr;			(* list of program's areas *)
    mlist: modptr;			(* of program's modules *)
    slist: symptr;			(* of program's symbols *)
    ntree: nameptr;			(* tree of program's names *)
    stsize: integer;			(* overlay static size, declared *)
    mtvsize: integer;			(* declared MTV size *)
    mtvaddr: integer;			(* location of MTV *)
    cre_dt: dtime_int;			(* daytime of run of MDLPRO *)
    losegbreak: integer;		(* after areas, for static *)
    debug: boolean;			(* true or false as needed *)
    multiseg: boolean;			(* true if sharable overlays exist *)
    end;

type
  keyword = (				(* all words in the language *)
    					(* first user commands *)
    use_cmd, compile_cmd, update_cmd,
    delete_cmd, renumber_cmd, pack_cmd,
    build_cmd, print_cmd, verify_cmd,
    help_cmd, stop_cmd, quit_cmd, exit_cmd,
					(* keywords taking filenames *)
    database_cmd, overlay_cmd, assembly_cmd,
					(* keywords taking integers *)
    version_cmd, newversion_cmd,
    files_cmd, channels_cmd,
					(* special ones *)
    using_cmd, info_cmd, area_cmd,
    module_cmd, program_cmd, all_cmd,
    symbol_cmd, resident_cmd
    );

  kwdset = set of keyword;
(* End of MMSSYM.TYP *)
 