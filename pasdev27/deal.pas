program deal;

$SYSTEM (pasdev2)cmdutl

type suit = (clubs, diamonds, hearts, spades);
     rank = (two, three, four, five, six, seven, eight, nine, ten, jack, queen, king, ace);
     hand = array [suit] of set of rank;
     fulldeal = array [1..4] of hand;
$PAGE hand predicate descriptions
(*  Hands may be described by predicate expressions.  These expressions are
    represented internally as tree structures, described below.  *)

type predicate = ^ predicate_node;

     predicate_operator = ( and_op, or_op, not_op, rel_op );

     count_code = ( high_card_points, distribution_points, total_points,
		    short_suit, long_suit, quick_tricks );

     count_list = array [count_code] of integer;

     relation = ( lss, eql, gtr );

     predicate_node = record
	case op: predicate_operator of
	    and_op, or_op:
	      ( arg1, arg2: predicate );
	    not_op:
	      ( arg: predicate );
	    rel_op:
	      ( code: count_code;
		rel: set of relation;
		val: integer );
     end;

     pred_list = ^ pred_list_node;

     pred_list_node = record
	next: pred_list;
	pred: predicate;
	name: packed array [1..*] of char;
     end;


var named_predicates: pred_list;
    dist_list: array [0..13] of integer;
$PAGE counthand
(*  COUNTHAND is a function which will examine a hand and compute all the
    interesting counts about it, returning them in a countlist.  *)

function counthand ( h: hand ): count_list;

var len: integer;
    r: rank;
    s: suit;

begin
  counthand [high_card_points] := 0;
  counthand [distribution_points] := 0;
  counthand [short_suit] := 13;
  counthand [long_suit] := 0;
  counthand [quick_tricks] := 0;
  for s := minimum (suit) to maximum (suit) do begin
    for r := jack to ace do begin
      if r in h [s] then
	counthand [high_card_points] := counthand [high_card_points] + ord (r) - ord (jack) + 1;
    end;
    len := 0;
    for r := minimum (rank) to maximum (rank) do begin
      if r in h [s] then
	len := len + 1;
    end;
    counthand [short_suit] := min (counthand [short_suit], len);
    counthand [long_suit] := max (counthand [long_suit], len);
    counthand [distribution_points] := counthand [distribution_points] + dist_list [len];
    if [ace, king] <= h [s] then
      counthand [quick_tricks] := counthand [quick_tricks] + 4
    else if [ace, queen] <= h [s] then
      counthand [quick_tricks] := counthand [quick_tricks] + 3
    else if ace in h [s] then
      counthand [quick_tricks] := counthand [quick_tricks] + 2
    else if [king, queen] <= h [s] then
      counthand [quick_tricks] := counthand [quick_tricks] + 2
    else if king in h [s] then
      counthand [quick_tricks] := counthand [quick_tricks] + 1;
  end;
  counthand [total_points] := counthand [high_card_points] + counthand [distribution_points];
end;
$PAGE testrel
(*  TESTREL will determine whether the relation between two integers is
    "less than", "equal to", or "greater than".  *)

function testrel (a, b: integer): relation;

begin
  if a < b then
    testrel := lss
  else if a = b then
    testrel := eql
  else
    testrel := gtr;
end;
$PAGE testhand
(*  TESTHAND will determine whether a hand with a specified set of counts
    satisfies a specified hand predicate.  *)

function testhand (c: count_list; p: predicate): boolean;

begin
  if p = nil then
    testhand := true
  else begin
    with p^ do begin
      case op of
	and_op:	testhand := testhand (c, arg1) andif testhand (c, arg2);
	or_op:	testhand := testhand (c, arg1) orif testhand (c, arg2);
	not_op:	testhand := not testhand (c, arg);
	rel_op:	testhand := (testrel (c [code], val) in rel);
      end;
    end;
  end;
end;
$PAGE initrandom
(*  INITRANDOM will initialize the random number generator, using the runtime
    of this job as an initial seed.  *)

procedure initrandom;

var x: real;

begin
  x := random (runtime);
end;
$PAGE dealcards
(*  DEALCARDS will produce a randomly generated set of hands in the DESC array.
    It begins by producing a random permutation of the integers from 0 to 51.
    It then translates this into a set of hands, using the convention that the
    first 13 integers are the first hand, the second 13 are the second hand, etc.
    The integers 0 to 51 correspond to the cards 2C, 3C, ... AC, 2D, ... KS, AS.  *)

function dealcards (nhands: 1 .. 4): fulldeal;

const mincard = 0;
      maxcard = 51;

type card = mincard .. maxcard;


function suitof (c: card): suit;
begin
  suitof := suit (c div 13);
end;


function rankof (c: card): rank;
begin
  rankof := rank (c mod 13);
end;
$PAGE
var c, c1, c2: card;
    minc: card;
    p: 1 .. 4;
    s: suit;
    deal: array [card] of card;

begin
  minc := (4 - nhands) * 13;
  for c := mincard to maxcard do
    deal [c] := c;
  for c := maxcard downto minc do begin
    c1 := trunc (random * c);
    c2 := deal [c1];
    deal [c1] := deal [c];
    deal [c] := c2;
  end;
  for p := 1 to nhands do
    for s := minimum (suit) to maximum (suit) do
      dealcards [p, s] := [];
  for c := minc to maxcard do begin
    p := (c - minc) div 13 + 1;
    s := suitof (deal [c]);
    dealcards [p, s] := dealcards [p, s] + [rankof (deal [c])];
  end;
end;

$PAGE suitrow
(*  SUITROW will produce a string representing the cards in a single suit for
    a single hand.  *)

const suitname: array [suit] of char = ( 'C', 'D', 'H', 'S' );
      rankname: array [rank] of string [2] =
	( '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A' );

function suitrow (h: hand; s: suit): string [35];

var r: rank;

begin
  suitrow := suitname [s] || ' -';
  for r := maximum (rank) downto minimum (rank) do begin
    if r in h [s] then
      suitrow := suitrow || ' ' || rankname [r];
  end;
end;
$PAGE helpmessage
procedure helpmessage;

begin
  writeln (tty);
  writeln (tty, 'DEAL is a program to deal bridge hands.  It will accept any of the');
  writeln (tty, 'following commands:');
  writeln (tty);
  writeln (tty, '   DEALS          to deal full four-hand layouts');
  writeln (tty, '   PAIRS          to deal pairs of hands');
  writeln (tty, '   HANDS          to deal single hands');
  writeln (tty, '   DEFINE         to define rules for selecting hands');
  writeln (tty, '   DISTRIBUTION   to define the point count for short and long suits');
  writeln (tty, '   EXIT           to leave the program');
  writeln (tty);
  writeln (tty, 'Any command may be abbreviated to its first three letters.');
  writeln (tty);
  writeln (tty, 'For a description of how to use a particular command, type');
  writeln (tty, '"HELP <command name>".');
  writeln (tty);
end;
$PAGE helpdistrmessage
procedure helpdistrmessage;

begin
  writeln (tty);
  writeln (tty, 'The DISTRIBUTION command tells DEAL how many distribution points to');
  writeln (tty, 'allow for short and long suits.  It has the form:');
  writeln (tty);
  writeln (tty, '   DISTRIBUTION n0 n1 ... nk');
  writeln (tty);
  writeln (tty, 'n0 is the points for a void, n1 the points for a singleton, and so');
  writeln (tty, 'on, up to nk which is the points for any suit with k or more cards.');
  writeln (tty);
  writeln (tty, 'For standard bidding use DISTR 3 2 1 0.');
  writeln (tty);
  writeln (tty, 'For Kaplan-Sheinwold bidding use DISTR 2 1 0 0 0 0 1 2.');
  writeln (tty);
end;
$PAGE helpdefinemessage
procedure helpdefinemessage;

begin
  writeln (tty);
  writeln (tty, 'The DEFINE command names a rule which you can use later for selecting');
  writeln (tty, 'hands.  It has the form:');
  writeln (tty);
  writeln (tty, '   name DEFINE rule');
  writeln (tty);
  writeln (tty, 'The name is any name which is neither an abbreviation of some command');
  writeln (tty, 'nor a count code (see below).  The rule describes characteristics of');
  writeln (tty, 'a hand.  The basic form of a rule is "code relation number", where');
  writeln (tty, 'the code specifies an attribute of a hand.  The codes are:');
  writeln (tty);
  writeln (tty, '   H - the number of high-card points');
  writeln (tty, '   D - the number of distribution points (see HELP DISTRIBUTION)');
  writeln (tty, '   T - the total points');
  writeln (tty, '   Q - the number of quick tricks (use "-" for 1/2)');
  writeln (tty, '   S - the length of the shortest suit');
  writeln (tty, '   L - the length of the longest suit');
  writeln (tty);
  writeln (tty, 'The relations are <, >, <=, >=, =, and ~=.  A previously defined rule');
  writeln (tty, 'name may also be used as a rule.  Rules may be combined with parentheses,');
  writeln (tty, 'and with the logical operators ~ (not), & (and), and | (or).');
  writeln (tty, 'For example:');
  writeln (tty);
  writeln (tty, '   UNBALANCED DEFINE S < 2 | S > 5');
  writeln (tty, '   NOTRUMP DEFINE ~ UNBALANCED & H >= 16 & H <= 18');
  writeln (tty, '   WEAKTWO DEFINE L=6 & T>=9 & T<=11 & (Q=1- | Q=2)');
  writeln (tty);
end;
$PAGE helpdealmessage
procedure helpdealmessage;

begin
  writeln (tty);
  writeln (tty, 'The DEALS command tells the program to print full four-hand layouts.');
  writeln (tty, 'It has the form:');
  writeln (tty);
  writeln (tty, '   [n] DEAL [rule] [, [rule]]*');
  writeln (tty);
  writeln (tty, 'N is the number of deals to print.  It may be omitted, for a single');
  writeln (tty, 'deal.  The rules select the kinds of deals to be printed.  The first');
  writeln (tty, 'rule applies to the south hand, the second to west, the third to north,');
  writeln (tty, 'and the fourth to east.  If no rule is specified for a hand, then any');
  writeln (tty, 'hand may be printed.  For a description of the rules you can use,');
  writeln (tty, 'type HELP DEFINE.  Here are some examples:');
  writeln (tty);
  writeln (tty, '   DEAL');
  writeln (tty, '   10 DEALS NOTRUMP, , h>=8 & H<12');
  writeln (tty, '   10 DEALS , OPENABLE');
  writeln (tty);
end;
$PAGE helppairmessage
procedure helppairmessage;

begin
  writeln (tty);
  writeln (tty, 'The PAIRS command tells the program to print pairs of hands.');
  writeln (tty, 'It has the form:');
  writeln (tty);
  writeln (tty, '   [n] PAIRS [rule] [, [rule]]');
  writeln (tty);
  writeln (tty, 'N is the number of pairs of hands to print.  It may be omitted, for a');
  writeln (tty, 'single pair.  The rules select the kinds of hands to be dealt.  If');
  writeln (tty, 'no rule is specified for a hand, any hand may be printed.  Here are');
  writeln (tty, 'some examples:');
  writeln (tty);
  writeln (tty, '   PAIR NOTRUMP, L>5');
  writeln (tty, '   5 PAIRS');
  writeln (tty, '   10 PAIRS S>=2 & L<=4');
  writeln (tty);
end;
$PAGE helphandmessage
procedure helphandmessage;

begin
  writeln (tty);
  writeln (tty, 'The HANDS command tells the program to print single hands.  It has');
  writeln (tty, 'the form:');
  writeln (tty);
  writeln (tty, '   [n] HANDS [rule]');
  writeln (tty);
  writeln (tty, 'N is the number of hands to be printed.  It may be omitted, for a');
  writeln (tty, 'single hand.  The rule selects the kinds of hands that will be');
  writeln (tty, 'printed.  If it is omitted, any kinds of hands may be printed.');
  writeln (tty, 'For a description of the rules you can use, type HELP DEFINE.');
  writeln (tty, 'Here are some examples:');
  writeln (tty);
  writeln (tty, '   HAND L=6 & T>=9 & T<=11 & (Q=1- | Q=2)');
  writeln (tty, '   10 HANDS');
  writeln (tty, '   20 HANDS H>=10');
  writeln (tty);
end;
$PAGE getconstraints
(*  GETCONSTRAINTS will fill out an array of predicates from a predicate list.  *)

procedure getconstraints ( pl: pred_list; var constraints: array [1..*] of predicate );

var pl1, pl2, pl3: pred_list;
    i: 1 .. 4;

begin
  pl1 := nil;
  pl2 := pl;
  while pl2 <> nil do begin
    pl3 := pl2^.next;
    pl2^.next := pl1;
    pl1 := pl2;
    pl2 := pl3;
  end;
  for i := 1 to upperbound (constraints) do begin
    if pl1 = nil then
      constraints [i] := nil
    else begin
      constraints [i] := pl1^.pred;
      pl1 := pl1^.next;
    end;
  end;
end (* getconstraints *);
$PAGE matchdeal
(*  MATCHDEAL will determine whether an array of hands will satisfy an array of
    constraint predicates.  If so, then it will return an index array indicating
    the order in which the hands must be taken.  *)

function matchdeal ( counts: array [1..4] of count_list;
		     constraints: array [1..4] of predicate;
		     var indices: array [1..4] of 1..4;
		     m, n: 1 .. 4 ): boolean;

var i, ii: 1 .. 4;

begin
  for i := m to n do begin
    ii := indices [i];
    if testhand (counts [ii], constraints [m]) then begin
      indices [i] := indices [m];
      indices [m] := ii;
      if (m = n) orif matchdeal (counts, constraints, indices, m + 1, n) then begin
	matchdeal := true;
	return; (* return with successful match *)
      end;
      indices [m] := indices [i];
      indices [i] := ii;
    end;
    matchdeal := false; (* <---- return with failure *)
  end;
end (* matchdeal *);
$PAGE dohands
procedure dohands (n: integer; pl: pred_list);

var i: integer;
    h: hand;
    s: suit;
    p: predicate;

begin
  if pl = nil
    then p := nil
    else p := pl^.pred;
  i := 0;
  while i <> n do begin
    h := dealcards (1) [1];
    if testhand (counthand (h), p) then begin
      writeln (tty);
      for s:= maximum (suit) downto minimum (suit) do
	write (tty, '   ', suitrow (h, s));
      writeln (tty);
      break;
      i := i + 1;
    end;
  end;
  writeln (tty);
end;
$PAGE dopairs
procedure dopairs (n: integer; pl: pred_list);

var i: integer;
    j: 1 .. 4;
    hands: fulldeal;
    counts: array [1..4] of count_list;
    constraints: array [1..4] of predicate;
    indices: array [1..4] of 1..4;
    s: suit;

begin
  getconstraints (pl, constraints);
  i := 0;
  while i <> n do begin
    hands := dealcards (4);
    for j := 1 to 4 do begin
      counts [j] := counthand (hands [j]);
      indices [j] := j;
    end;
    if matchdeal (counts, constraints, indices, 1, 2) then begin
      writeln (tty);
      writeln (tty);
      writeln (tty);
      for s := maximum (suit) downto minimum (suit) do
	writeln (tty, suitrow (hands [indices [1]], s): 30: l, suitrow (hands [indices [2]], s));
      writeln (tty);
      i := i + 1;
    end;
  end;
end;
$PAGE dodeals
procedure dodeals (n: integer; pl: pred_list);

var i, j: integer;
    hands: fulldeal;
    counts: array [1..4] of count_list;
    constraints: array [1..4] of predicate;
    indices: array [1..4] of 1..4;
    s: suit;

begin
  getconstraints (pl, constraints);
  i := 0;
  while i <> n do begin
    hands := dealcards (4);
    for j := 1 to 4 do begin
      counts [j] := counthand (hands [j]);
      indices [j] := j;
    end;
    if matchdeal (counts, constraints, indices, 1, 4) then begin
      writeln (tty);
      writeln (tty);
      writeln (tty);
      writeln (tty, '':17, 'NORTH');
      writeln (tty);
      for s := maximum (suit) downto minimum (suit) do
	writeln (tty, '':15, suitrow (hands [indices [3]], s));
      writeln (tty);
      writeln (tty, '  WEST':32:l, 'EAST');
      writeln (tty);
      for s := maximum (suit) downto minimum (suit) do
	writeln (tty, suitrow (hands [indices [2]], s):30:l, suitrow (hands [indices [4]], s));
      writeln (tty);
      writeln (tty, '':17, 'SOUTH');
      writeln (tty);
      for s := maximum (suit) downto minimum (suit) do
	writeln (tty, '':15, suitrow (hands [indices [1]], s));
      writeln (tty);
      i := i + 1;
    end;
  end;
end;
$PAGE readcommand
(*  READCOMMAND will read a command line from the terminal, parse it,
    and process it.  *)

procedure readcommand;

$INCLUDE deal.sym

(*  The value of a token is given by a 'sym_value'.  Invalue is a 'sym_value',
    as are the entries in the parse stack.  'Sym_value' is an undiscriminated
    union with variants for the various token kinds.  The proper variant for
    an input token may be selected on the basis of Insymbol; the variant for a
    parse stack entry may be selected on the basis of the current parser state.  *)

type sym_value = record
	case token_type of
	  integersy,
	  distr_cmd_nt,
	  opt_num_nt:
	    ( numval: integer );

	  codesy:
	    ( code: count_code );

	  relsy:
	    ( rel: set of relation );

	  pred_nt,
	  opt_pred_nt:
	    ( pred: predicate );

	  def_head_nt,
	  preds_nt:
	    ( plist: pred_list );

	  others:
	    ( );
     end;
$PAGE getsymbol
(*  GETSYMBOL gets the next token from the command line.  It stores
    the token kind in Insymbol.  If the token is a code, number, or
    relation, its value will be stored in Invalue.  If the token is
    a name string, its value will be stored in Namestring.  *)

var cmdline: string [150];
    cmdidx: integer;

var insymbol: token_type;
    invalue: sym_value;
    namestring: string [40];

procedure getsymbol;

var x : integer;

const
    command_table : array [1..7] of cmd_lookup_record =
      ( ( 'EXIT      ', 3, ord (exitsy) ),
	( 'HELP      ', 3, ord (helpsy) ),
	( 'DISTRIBUTI', 3, ord (distrsy) ),
	( 'DEFINE    ', 3, ord (definesy) ),
	( 'HANDS     ', 3, ord (handssy) ),
	( 'PAIRS     ', 3, ord (pairssy) ),
	( 'DEALS     ', 3, ord (dealssy) ) );

    code_table : array [1..6] of cmd_lookup_record =
      ( ( 'H', 1, ord (high_card_points) ),
	( 'D', 1, ord (distribution_points) ),
	( 'T', 1, ord (total_points) ),
	( 'S', 1, ord (short_suit) ),
	( 'L', 1, ord (long_suit) ),
	( 'Q', 1, ord (quick_tricks) ) );

begin
  if cmd_eol (cmdline, cmdidx) then
    insymbol := eofsy
  else begin
    case cmdline [cmdidx] of

      'A'..'Z':
	if cmd_lookup (cmdline, cmdidx, ['A'..'Z'], command_table, x) then
	  insymbol := token_type (x)
	else if cmd_lookup (cmdline, cmdidx, ['A'..'Z'], code_table, x) then begin
	  insymbol := codesy;
	  invalue.code := count_code (x);
	end
	else begin
	  insymbol := namesy;
	  if not cmd_token (cmdline, cmdidx, ['A'..'Z', '0'..'9'], namestring) then
	    assert (false);
	  x := cmdidx - 1;
	end;

      '0'..'9':
	begin
	  insymbol := integersy;
	  if not cmd_number (cmdline, cmdidx, false, invalue.numval) then
	    assert (false);
	  invalue.numval := invalue.numval * 2;
	  if (cmdidx <= length (cmdline)) andif (cmdline[cmdidx] = '-') then
	    invalue.numval := invalue.numval + 1
	  else
	    cmdidx := cmdidx - 1;
	end;

      '-':
	begin
	  insymbol := integersy;
	  invalue.numval := 1;
	end;

      '~':
	begin
	  if (cmdidx < length (cmdline)) andif (cmdline [cmdidx + 1] = '=') then begin
	    insymbol := relsy;
	    invalue.rel := [lss, gtr];
	    cmdidx := cmdidx + 1;
	  end
	  else
	    insymbol := notsy;
	end;

      '&':
	insymbol := andsy;

      '|':
	insymbol := orsy;

      ',':
	insymbol := commasy;

      '(':
	insymbol := lparensy;

      ')':
	insymbol := rparensy;

      '<':
	begin
	  insymbol := relsy;
	  if (cmdidx < length (cmdline)) andif (cmdline [cmdidx + 1] = '=') then begin
	    invalue.rel := [lss, eql];
	    cmdidx := cmdidx + 1;
	  end
	  else
	    invalue.rel := [lss];
	end;

      '>':
	begin
	  insymbol := relsy;
	  if (cmdidx < length (cmdline)) andif (cmdline [cmdidx + 1] = '=') then begin
	    invalue.rel := [gtr, eql];
	    cmdidx := cmdidx + 1;
	  end
	  else
	    invalue.rel := [gtr];
	end;

      '=':
	begin
	  insymbol := relsy;
	  invalue.rel := [eql];
	end;

      others:
	insymbol := nosymbol;

    end (* case *);
    cmdidx := cmdidx + 1;
  end (* if *);
end;
$PAGE parse
$INCLUDE deal.par
$PAGE readcommand - main routine
begin
  repeat
    cmd_getline ('*', cmdline, cmdidx)
  until cmdline <> '';
  cmdline := uppercase (cmdline);
  parse;
end;
$PAGE deal - main program
const all_zeros: array [0..13] of integer =
      ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );

begin
  rewrite (tty);
  open (tty);
  initrandom;
  named_predicates := nil;
  dist_list := all_zeros;
  loop
    readcommand
  end;
end.
 
 M