module dir_match;

$SYSTEM dtime.typ
$SYSTEM pasdir.typ

(*  DIR_MATCH - returns true if the string in parameter TARGET can be
    matched with the pattern in parameter PATTERN.  Asterisks match any 
    number of consecutive characters in the target;  question marks
    match any single character or the null string.  No escape is
    provided for matching literal question marks or asterisks.  A null
    pattern matches only the null string.  Trailing blanks are 
    regarded as significant.

    This routine is part of the PASDIR RDLIB package.

    DIR_MATCH simply calls the recursive routine DO_MATCH.  The
    formal parameters of DO_MATCH (the target string and the
    pattern) are declared as 'packed array [1..*] of char';
    for most target machines this declaration will insure that
    no string copies are needed to pass the actual parameter.  *)

public function dir_match ( target: dir_fname; pattern: dir_m_str ): boolean;

const 
  wild_card_chars: set of char := ['?', '*'];

$PAGE do_match

(* DO_MATCH is a recursive routine to do the actual pattern matching
   for DIR_MATCH.  Each call to DO_MATCH matches an initial substring of
   pattern passed in - either a non-wildcard string, an '*' followed
   by a non-wildcard string or a string of '?'s followed by a non-wildcard
   string.  *)

type
  flex_fixed_string = packed array [1..*] of char;
  string_index_range = 0..maximum(integer);

function do_match ( target: flex_fixed_string;
		    pattern: flex_fixed_string ): boolean;

var
  pat_idx: string_index_range;
  subpat_len: string_index_range;
  target_base: string_index_range;
  target_idx: string_index_range;
  qmark_count: string_index_range;

begin
  do_match := false;			(* pessimist! *)

  (* Special case the null pattern - it matches only the null string.  *)

  if length ( pattern ) = 0 then begin
    do_match := ( length ( target ) = 0 );
  end

  (* If the first character of the pattern is not a wildcard character
     then see if the substring of the pattern up to the first wildcard
     of the pattern (if any ) matches the initial substring of the target
     string.  If not we return failure, otherwise recursively call
     DO_MATCH with the trailing substrings of the target and pattern.  *)

  else if not (pattern[1] in wild_card_chars) then begin
    pat_idx := search ( pattern, wild_card_chars, length(pattern) + 1 );
    subpat_len := pat_idx - 1;
    if subpat_len <= length ( target ) then begin  (* if >, no match possible *)
      do_match := substr ( target, 1, subpat_len ) =
		  substr ( pattern, 1, subpat_len );
      if do_match then
        do_match := do_match ( substr(target, pat_idx), substr(pattern, pat_idx) );
    end
  end

  (* If the first char of the pattern is an '*' then we attempt to match the
     asterisk and any subsequent string of non-wildcard characters with a
     (possibly non-proper) prefix of the target string.  If we succeed, 
     then DO_MATCH is called recursively to match the remainder of the pattern
     with the remainder of the target string.  *)

  else if pattern[ 1 ] = '*' then begin
    pat_idx := verify ( pattern, wild_card_chars );	(* find idx of 1st non-wild char *)
    
    if pat_idx = 0 then begin	(* '*' at end of pattern matches *)
      do_match := true;			(* any target string *)
    end

    else begin				(* non-wild char follows initial wild string *)
      subpat_len := search ( substr(pattern, pat_idx), wild_card_chars,
		      length(pattern) - pat_idx + 2 ) - 1;
      
      (* Examine successive possible matches within the target string of the
	 non-wildcard substring following the pattern's initial wildcards.
	 Matching substrings are examined until either no more matches are
  	 possible or a recursive call to DO_MATCH succeeds in matching
	 the remainder of the target and pattern.  *)
      
      target_base := 1;

      while ( not do_match ) and
	    ( target_base <= length(target) ) do begin
        target_idx := index ( substr(target, target_base), 
	  substr(pattern, pat_idx, subpat_len), length(target) + 1 ) +
	  target_base - 1;	(* index w/in target of matching substr *)
	if target_idx <= length ( target ) then
	  do_match := do_match ( substr(target, target_idx + subpat_len),
				 substr(pattern, pat_idx + subpat_len) );
	if not do_match then
	  target_base := target_idx + 1;
      end;
    end;
  end  (* '*' case *)

  (* If the initial character of the pattern is a '?' then we attempt
     to match the question mark, any immediately following question
     marks and any following string of non-wildcard characters.
     Matches of the above leading portion of the pattern are attempted
     until either a recursive call matches the trailing, unmatched
     portions of the target and pattern or no more matches of the
     initial portion of the pattern are possible.  *)

  else begin
    pat_idx := verify ( pattern, [ '?' ], length(pattern) + 1 );
    qmark_count := pat_idx - 1;		(* number of adjacent leading '?'s *)

    if ( qmark_count < length ( pattern ) ) andif
       ( pattern[ pat_idx ] = '*' ) then begin
      do_match := do_match ( target, substr(pattern, pat_idx) );
    end

    else if qmark_count = length ( pattern ) then begin
      do_match := length ( target ) <= qmark_count;
    end

    else begin	(* '?'s followed by non-wildcard *)
      subpat_len := search ( substr(pattern, pat_idx), wild_card_chars,
	length(pattern) - pat_idx + 2 ) - 1;
      target_base := 1;

      repeat
	target_idx := index ( substr(target,target_base),
	  substr(pattern, pat_idx, subpat_len), pat_idx + 1 ) + target_base - 1;
        if target_idx <= pat_idx then
	  do_match := do_match ( substr(target, target_idx + subpat_len),
				 substr(pattern, pat_idx + subpat_len) );
	if not do_match then
	  target_base := target_idx + 1;
      until ( do_match ) or	(* match found, or *)
	    ( target_base > pat_idx ) or	(* '?'s exhausted, or *)
	    ( target_base > length(target) );	(* target exhausted *)

    end  (* else *) ;
  end  (* '?' case *) ;
end  (* proc DO_MATCH *);
$PAGE dir_match - body

begin
  dir_match := do_match ( target, pattern );
end.
    