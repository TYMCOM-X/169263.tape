public function parse ( var rslt: stk_node ): boolean;

const
    max_int = 32767;

type
    int = 0 .. max_int;

    act_codes =
      ( sh (* shift *),
	rd (* reduce *),
	sr (* shift/reduce *),
	go (* goto *),
	gr (* shift nonterminal/reduce *),
	ac (* accept *),
	er (* error *) );

    act_index = packed record
	first: int;
	last: int
    end;

    act_entry = packed record
	case code: act_codes of
	    sh,
	    go:
	      ( next_state: int );
	    rd,
	    sr,
	    gr:
	      ( production: int );
	    ac,
	    er:
	      ( )
    end;

    prod = packed record
	lhs: int;
	rhs_len: int
    end;

var
    state: array [0..stk_max] of int;
    value: array [1..stk_max] of stk_node;
    stk_ind: 0 .. stk_max;
    lookup_sym: int;
    next_act: act_entry;
    list_index: int;
    ind: int;
    result: stk_node;
