(* GET and SET terminal characteristics *)


(* The following type declaration encorporates the user-specifiable
   terminal characteristics. The names chosen here are the VAX name in
   the I/O USERS GUIDE with the "TC_" prefix attached. For further
   information see Table 2-4, in the TERMINAL DRIVER chapter of
   the VAX I/O USERS GUIDE. *)

Type TER_CHARACTERISTICS = (
	TC_PASSALL,	(* All input is binary,pass everything *)
	TC_NOECHO,	(* Terminal won't echo entered input *)
	TC_NOTYPEHD,	(* Allow/disallow terminal typeahead *)
	TC_ESCAPE,	(* Alloe/Disallow escape sequences *)
	TC_HOSTSYNC,	(* Synchronize filling of input buffer *)
	TC_TTYSYNC,	(* Terminal controls synchronization *)
	TC_UNSPEC,	(* UNSPECIFIED *)
	TC_LOWER,	(* Permit upper and lower case *)
	TC_MECHTAB,	(* Terminal has mechanical tabs *)
	TC_WRAP,	(* Wrap lines at right margin *)
	TC_CRFILL,	(* Ter requires fill after CR *)
	TC_LFFILL,	(* Ter requires fill after LF *)
	TC_SCOPE,	(* Terminal is a CRT *)
	TC_REMOTE,	(* Dial-up terminal *)
	TC_HOLDSCREEN,	(* Special DEC[52,55,100] terminals *)
	TC_EIGHTBIT,	(* Use 8-bit ASCII *)
	TC_MBXDSABL,	(* Enable mailbox notification *)
	TC_NOBRDCST,	(* Terminal gets no broadcast msgs *)
	TC_READSYNC,	(* Host explicitly solicits all read operations *)
	TC_MECHFORM,	(* Terminal has mechanica; formfeeds *)
	TC_HALFDUP	(* Set TERMINAL SOFTWARE half-duplex *)
		);

Type TC_SET = Set of TER_CHARACTERISTICS;

     TCERR = ( TC_OK,		(* Operation successful *)
		 TC_ERR		(* All encompassing error code *)
		);

External Procedure SET_TERMINAL_CHARACTERISTICS (
			TERCHAR : TC_SET;
			Var ERR : TCERR );

External Procedure GET_TERMINAL_CHARACTERISTICS (
			Var TERCHAR : TC_SET;
			    Var ERR     : TCERR );
 