(*  Third Normal Form Program include file *)

const

  max_items = 20;

  max_rels = 10;

  name_length = 20;


type

  char_set = set of char;

  item_name = string[name_length];

  item_index = 1..max_items;

  rel_index = 1..max_rels;

  item_set = set of item_index;

  rel_set = item_set;

  relation = ( non_dependent, self, dependent );

  rel_key_type = ( singular, concatenated );

  rel_rec = record
              items : rel_set;
              key_type : rel_key_type;
              num_in_key : 0..max_items;
            end;



var

  legal_input : char_set := [ 'A'..'Z', '0'..'9', '#', '$', '%', '&' ];

  key_set : rel_set;

  dep_set : item_set;

  num_items : 0..max_items;

  num_rels : 0..max_rels;

  item_names : array [ item_index ] of item_name;

  rel_ind : rel_index;

  relation_keys : array [ rel_index ] of rel_rec;

  func_dep_matrix : array [ rel_index, item_index ] of relation;
  