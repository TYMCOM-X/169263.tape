program rnf6;

exception
    exc1, exc2;

var a, b, c, d : integer;

procedure outer ( x : integer );

exception
    exc2;

    procedure inner_quick ( y : integer );
    begin
      if y = 0 then
	signal (exc1)
      else if y = x then
	signal (exc2)
      else
	a := x - y;
    end;

    procedure inner_nonquick ( y : integer );
    begin
      if y < 0 then
	signal (exc1);
      b := a - y;
    exception
      attention : c := 0;
      exc1      : c := 1;
      exc2      : c := 2;
    end;

begin (* outer *);
  inner_quick (x-a);
  begin
    inner_nonquick (x-b);
  exception
    attention : signal ();
    exc1      : stop;
  end;
  d := a;
exception
  attention : stop;
  exc1      : d := b;
  exc2      : d := c;
end;

begin (* rnf6 *);
  outer (a+b);
exception
  exc1 : exception_message;
  exc2 : exception_message;
end.
  