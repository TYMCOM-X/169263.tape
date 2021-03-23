$PAGE test_notrace_option
public procedure test_notrace options trace;
  
  var integer_1 : integer;
      integer_2 : integer;

  begin
    integer_1 := maximum(integer);

    (* Integer overflow should cause an error *)
    integer_2 := integer_1 * integer_1;
    error( 4 );
  end;
procedure test_notrace_option options notrace;

  begin
    test_notrace;
  end;
 