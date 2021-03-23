	case uppercase ( line [ end_index ] ) of
	  'A'..'Z':	getchar := -2;
	  '0'..'7':	getchar := -3;
	  '8','9':	getchar := -4;
	  '#':		getchar := -5;
	  '?':		getchar := -6;
	  '*':		getchar := -7;
	  ':':		getchar := -8;
	  '(':		getchar := -9;
	  ')':		getchar := -10;
	  '.':		getchar := -11;
	  '[':		getchar := -12;
	  ',':		getchar := -13;
	  ']':		getchar := -14;
	  '<':		getchar := -15;
	  '>':		getchar := -16;
	  others:	getchar := -1		(* end of data *)
	end (* case *) ;
