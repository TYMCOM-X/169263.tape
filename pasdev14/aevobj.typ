TYPE

(*
Legal object record sizes, number of items
*)
    OBJ_REC_SIZE = 0..512;
    OBJ_REC_ITEMS = 0..512;

(*
Kinds of object file records
*)
    OBJ_REC_ID = (MHD, GSD, TIR, EOM);
   (* Module HeaDer, Global Symbol Directory,
       Text Information and Relocation,
      End Of Module *)

(*
Kinds of global symbol directory entries
*)
    GSD_ENT_ID = (SECTION_DEF, ENTRY_DEF, GLOBAL_DEF, GLOBAL_REF);

(*
Kinds of text information and relocation commands
*)
    TIR_CMD_ID = (STORE_IMM_TEXT,
             STACK_GLOBAL, STACK_WORD, STACK_MODULE_SECTION_REL,
                 STACK_IMAGE_SECTION_REL,
                STORE_LONGWORD, STORE_WORD, STORE_BYTE,
                     STORE_REP_WORD,
                 OPER_ADD, OPER_SUB, OPER_MUL, OPER_DIV, OPER_NEG,
             CTL_SET_RELOC_BASE);

(*
Header for an object file record
*)
    OBJ_REC_HEADER = RECORD
       REC_ID: OBJ_REC_ID;             (* id of this record *)
       REC_LENGTHS: PACKED RECORD      (* object record length info *)
           REC_SIZE: OBJ_REC_SIZE;     (* length of record in storage
                                           units, excluding header *)
     REC_ITEMS: OBJ_REC_ITEMS    (* number of items in record *)
           END
       END;

(*
Standard components of object file records:
*)
    OBJ_BYTE = 0..255;                     (* a byte *)
    OBJ_WORD = 0..65535;          (* a word *)
    OBJ_SECTION = (ABSOLUTE_SEG, CODE_SEG,
      DIRECT_STATIC_SEG, VIRTUAL_STATIC_SEG);
                                       (* the different sections *)
    OBJ_SYMBOL = PACKED ARRAY [1..10] OF CHAR;    (* any identifier *)
    OBJ_SYMBOLNUM = OBJ_WORD;             (* ordinal number of an
                                          external symbol *)

(*
Module header record
consists of one of the following item
*)
    MHD_REC = RECORD
    STRUCTURE_LEVEL: INTEGER;       (* rev number of obj lang *)
  MODULE_NAME: OBJ_SYMBOL;        (* from program/module
                                           statement *)
       CREATION_TIME: PACKED ARRAY [1..18] OF CHAR;
                                  (* dd-mmm-yy hh:mm:ss *)
      CREATOR: PACKED ARRAY [1..35] OF CHAR
                                 (* name of creator program *)
 END;

(*
End of module record
consists of zero or one of the following item
*)
    PROG_ATTRIBUTES = (PROG_CHECKUNDERFLOW, PROG_MASKATTENTION,
  PROG_DUMMY2, PROG_DUMMY3, PROG_DUMMY4, PROG_DUMMY5,
   PROG_DUMMY6, PROG_DUMMY7, PROG_DUMMY8, PROG_DUMMY9,
   PROG_DUMMY10, PROG_DUMMY11, PROG_DUMMY12, PROG_DUMMY13,
       PROG_DUMMY14, PROG_DUMMY15);

    EOM_REC = RECORD
  PROG_FLAGS: PROG_ATTRIBUTES;    (* flags for program *)
       TRANSFER_OFFSET: OBJ_WORD       (* transfer address offset *)
 END;

(*
Global symbol directory record
consists of one or more global symbol directory entries
*)
    GLOBAL_DEF_ATTRIBUTES = (DEF_ENTRY, DEF_DUMMY1,
  DEF_DUMMY2, DEF_DUMMY3, DEF_DUMMY4, DEF_DUMMY5,
       DEF_DUMMY6, DEF_DUMMY7, DEF_DUMMY8, DEF_DUMMY9,
       DEF_DUMMY10, DEF_DUMMY11, DEF_DUMMY12, DEF_DUMMY13,
   DEF_DUMMY14, DEF_DUMMY15);
    GLOBAL_REF_ATTRIBUTES =
       (REF_DUMMY0, REF_DUMMY1, REF_DUMMY2, REF_DUMMY3,
      REF_DUMMY4, REF_DUMMY5, REF_DUMMY6, REF_DUMMY7,
       REF_DUMMY8, REF_DUMMY9, REF_DUMMY10, REF_DUMMY11,
     REF_DUMMY12, REF_DUMMY13, REF_DUMMY14, REF_DUMMY15);

    GSD_ENT = RECORD
  CASE ENT_ID: GSD_ENT_ID OF
    SECTION_DEF: (
                SECTION_INDEX: OBJ_SECTION;
                                   (* the section being defined *)
               SECTION_ALLOCATION: OBJ_WORD
                                  (* number of words in this
                                       module's contribution *)
           );
    GLOBAL_DEF: (
         GLOBAL_DEF_NAME: OBJ_SYMBOL;
                                  (* name of global symbol *)
           GLOBAL_DEF_SECTION: OBJ_SECTION;
                                      (* section in which symbol
                                       is defined *)
              GLOBAL_DEF_FLAGS: SET OF GLOBAL_DEF_ATTRIBUTES;
                                       (* flags for global
                                      definition *)
              GLOBAL_VALUE: OBJ_WORD
                                        (* value or offset
                                       within section *)
          );
    GLOBAL_REF: (
         GLOBAL_REF_NAME: OBJ_SYMBOL;
                                  (* name of global symbol *)
           GLOBAL_REF_SECTION: OBJ_SECTION;
                                      (* section in which symbol
                                       must be defined *)
         GLOBAL_REF_FLAGS: SET OF GLOBAL_REF_ATTRIBUTES
                                        (* flags for global
                                      reference *)
               )
     END;

(*
Text information and relocation record
consists of one or more text information and relocation commands
*)
    TIR_CMD = RECORD
        CASE CMD_ID: TIR_CMD_ID OF
    STORE_IMM_TEXT: (
             IMM_TEXT: PACKED ARRAY [1..*] OF OBJ_WORD
                                     (* emitted text *)
            );
    STACK_GLOBAL: (
               STACK_SYMBOL: OBJ_SYMBOLNUM
                                   (* ordinal number of symbol
                                      whose value is to be
                                          stacked *)
         );
    STACK_WORD: (
         STACK_VALUE: OBJ_WORD   (* value to be stacked *)
             );
    STACK_MODULE_SECTION_REL, STACK_IMAGE_SECTION_REL: (
          STACK_SECTION: OBJ_SECTION;
                                   (* index of section containing
                                           address to be stacked *)
           STACK_WORD: OBJ_WORD
                                  (* offset within section of
                                      address to be stacked *)
           );
    STORE_LONGWORD, STORE_WORD, STORE_BYTE, STORE_REP_WORD: (
             );
    OPER_ADD, OPER_SUB, OPER_MUL, OPER_DIV, OPER_NEG: (
           );
    CTL_SET_RELOC_BASE: (
         )
     END;
    