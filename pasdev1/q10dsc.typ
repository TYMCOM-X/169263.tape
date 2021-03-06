type
  str_len_context = ( no_length, actual_length, max_length );

  str_desc = packed record
    base_addr: addr_desc; (* base address of string, address of bounds word
			     for flex strings, address of length word for
			     non-flex verying strings *)
    base_is_bound: boolean; (* TRUE if BASE_ADDR is address of flex string
			       bounds word *)
    len_context: str_len_context; (* indicates whether LEN_ADDR is invalid or
				     actual length or maximum length *)
    len_addr: addr_desc; (* addrress of length of string, not valid if
			    LEN_CONTEXT = NO_LENGTH *)
    max_len: char_range; (* compile-time upper bound on length of string *)
    text_valid: boolean; (* TRUE if TEXT_ADDR field is valid *)
    text_addr: addr_desc; (* address of first char of string *)
    type_desc: expr_type_desc; (* type info about string *)
    trans_code: str_translation; (* indicates any case conversions pending
				    for the string *)
  end;

  free_procedure = procedure;
   