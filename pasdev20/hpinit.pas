program hpinit;

type
  foo = ^ ech;
  ech = record
    data: string[10];
    next: foo
    end;

var
  head: foo;
  temp: foo;
  str: string [10];

external procedure hpsave
( string[10];
  foo );

begin
  open (tty); rewrite (ttyoutput);
  head := nil;
  loop
    write (tty, 'New string: '); break (tty); readln (tty);
    read (tty, str);
  exit if str = '';
    new (temp);
    temp^.data := str;
    temp^.next := head;
    head := temp
  end;

  hpsave ('###ECH.ECH', head)
end.
 