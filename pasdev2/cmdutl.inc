type cmd_lookup_record = record
	text: packed array [1..10] of char;
	abbrev: 1 .. 10;
	code: integer;
     end;
external procedure cmd_skip_blanks
      ( packed array [1..*] of char;
	var integer );
external function cmd_check_punct
      ( packed array [1..*] of char;
	var integer;
	char ): boolean;
external function cmd_token
      ( packed array [1..*] of char;
	var integer;
	set of char;
	var string [*] ): boolean;
external function cmd_number
      ( packed array [1..*] of char;
	var integer;
	boolean;
	var integer ): boolean;
external function cmd_lookup
      ( packed array [1..*] of char;
	var integer;
	set of char;
	array [1..*] of cmd_lookup_record;
	var integer ): boolean;
external function cmd_string
      ( packed array [1..*] of char;
	var integer;
	char;
	var string [*] ): boolean;
external function cmd_dqstring
      ( packed array [1..*] of char;
	var integer;
	char;
	var string [*] ): boolean;
external function cmd_file_name
      ( packed array [1..*] of char;
	var integer;
	boolean;
	var string [*] ): boolean;
external function cmd_eol
      ( packed array [1..*] of char;
	var integer ): boolean;
external function cmd_query
      ( packed array [1..*] of char;
	packed array [1..*] of char ): boolean;
external procedure cmd_getline
      ( packed array [1..*] of char;
	var string [*];
	var integer );
external procedure cmd_display_table
      ( array [1..*] of cmd_lookup_record;
	integer;
	integer );
   