program proc_var;

  procedure add ( x, y : integer );
    var z : integer;

    begin
      z := x + y;
      writeln (tty, 'Add result is ', z );
    end;

  procedure subtract ( x, y : integer );
    var z : integer;

    begin
      z := x - y;
      writeln (tty, 'Subtract result  is ', z);
    end;

  var a, b : integer;
      proc_1 : procedure ( x, y : integer );

  begin
    a := 5;
    b := 3;

    proc_1 := add;
    proc_1 ( a, b );
 
    proc_1 := subtract;
    proc_1 ( a, b );
  end.
  