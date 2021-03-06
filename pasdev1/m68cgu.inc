external var
    code_area,
    cst_area: code_list;
external procedure cdl_init (var area: code_list);
external procedure gen_inst (
          var area: code_list;
          operation: generic_opcodes;
          condition: condition_codes;
          source: op_desc;
          destination: op_desc );
external procedure gen_opcode (
          opc: generic_opcodes );
external procedure gen_r (
          opc: generic_opcodes;
          register: registers;
          operand_size: op_sizes );
external procedure gen_rr (
          opc: generic_opcodes;
          source: registers;
          destination: registers;
          operand_size: op_sizes );
external procedure gen_m (
          opc: generic_opcodes;
          operand: op_desc );
external procedure gen_mm (
          opc: generic_opcodes;
          source: op_desc;
          destination: op_desc );
external procedure gen_rm (
          opc: generic_opcodes;
          source: registers;
          destination: op_desc );
external procedure gen_mr (
          opc: generic_opcodes;
          source: op_desc;
          destination: registers );
external procedure gen_ir (
          opc: generic_opcodes;
          value: integer;
          destination: registers;
          operand_size: op_sizes );
external procedure gen_im (
          opc: generic_opcodes;
          value: integer;
          destination: op_desc );
external procedure gen_rt (
          routine: rt_symbol );
external procedure gen_jump (
          target: def );
external procedure gen_bcc (
          condition: condition_codes;
          target: def );
external procedure gen_scc (
          condition: condition_codes;
          destination: op_desc );
external procedure gen_call (target: def);
external procedure gen_dbcc (
          condition: condition_codes;
          register: registers;
          target: def );
external procedure gen_byte (
          var area: code_list;
          value: reloc_value;
          relative: boolean );
external procedure gen_word (
          var area: code_list;
          value: reloc_value;
          relative: boolean );
external procedure gen_long (
          var area: code_list;
          value: reloc_value;
          relative: boolean );
external procedure gen_len (
          var area: code_list;
          len: integer );
external procedure gen_string (
          var area: code_list;
          str: packed array[1..*] of char );
external procedure gen_set (
          var area: code_list;
          set_const: packed array[0..*] of boolean );
external procedure gen_sreal (
          var area: code_list;
          real_value: real_type );
external procedure gen_dreal (
          var area: code_list;
          real_value: real_type );
external procedure gen_def (
          var area: code_list;
          def_value: def );
external procedure gen_source (
          var area: code_list;
          source_loc: source_id;
          statement_index: 0..63 );
external procedure gen_cmt (
          var area: code_list;
          comment: packed array[1..*] of char );
external procedure gen_asm_label (
          var area: code_list;
          name: packed array[1..8] of char;
          defined_by_pos: boolean;
          static_offset: code_address );
external procedure gen_dtime (
          var area: code_list;
          dt: dtime_int );
external procedure gen_block (
          var area: code_list;
          size: unit_range );
external procedure gen_pc_assert (
          var area: code_list;
          location: code_address );
external procedure gen_align (
          var area: code_list );
external procedure def_init;
external procedure def_purge;
external procedure def_term;
external function def_create (
          class: def_class ): def;
external function def_lookup (
          class: def_class;
          index: id_range ): def;
external procedure def_assign (
          defn: def;
          val: reloc_value );
external procedure def_forward (
          defn: def;
          loc: integer;
          long: boolean;
          patch: integer );
external procedure gen_vnode (var area_list: code_list; value: val_ptr; values_type: typ);
external procedure gen_val (var area_list: code_list; value: val; values_type: typ);
external function gen_cnode (value: val_ptr; values_type: typ): reloc_value;
external function gen_cval (value: val; values_type: typ): reloc_value;
external function gen_cst (value: integer; size: op_sizes): op_desc;
 