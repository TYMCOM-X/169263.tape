type str_6 = packed array[1..6] of char;
external function radix50 ( code: integer; sym: str_6): pdp10word;
external procedure rel_init;
external procedure rel_code ( word: pdp10word; var ic: code_address;
			      lreloc, rreloc: reloc_value );
external procedure rel_end ( start_addr: def; lowseg_fin, highseg_fin: code_address );
 