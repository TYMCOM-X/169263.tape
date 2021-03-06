(*********  Transition Matrix  *********)


(*  A transition matrix is a two-dimensional array which represents a finite
    state automaton.  Entries in the matrix are indexed as

	Matrix [State, InputSymbol] .

    Each matrix entry is the next state for the machine to enter when it reads
    the specified input symbol in the specified state.  State 1 is the initial
    state of the FSA.  State 0 is used as for the "dead" state; that is, there
    is no state 0, but matrix entries may show a transition to state 0 when the
    machine has no transition on the specified input symbol in the specified
    state.  *)


type
    transition_matrix = ^ packed array [1..*] of
			    ^ packed array [0..*] of
				number;


(*  A characteristic vector is a vector of characteristic function values
    for the states of an FSA.  Two kinds of characteristic vectors are
    defined:  ReVector's, which map FSA states into characteristic regular
    expressions, and StVectors, which map FSA states into vectors of states
    of individual pattern recognizers.  *)


type
    re_vector = ^ packed array [1..*] of reg_exp;

    states = ^ packed array [1..*] of number;

    st_vector = ^ packed array [1..*] of states;


(*  An acceptance vector indicates whether each state of the FSA is an accepting
    state.  An AccVector indicates accepting states of a recognizer; each entry
    in the vector is a boolean flag.  An AccPatVector indicates accepting states
    of a combined FSA; each entry is either zero or the number of the pattern
    recognized by that state.  *)

type
    acc_vector = ^ packed array [1..*] of boolean;

    acc_pat_vector = ^ packed array [1..*] of number;
   