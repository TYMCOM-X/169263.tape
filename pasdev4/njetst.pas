$TITLE LITTLE BINARY TEST

program test;

var
    rtest_string:string[10];
    test_file: file of *;

procedure error ( error_number: 0..10 );
begin
end;

begin

    update(test_file,'njetst',[retry]);
    if iostatus(test_file) <> io_ok
        then error( 1 );
    if not eof(test_file)
        then error ( 2 );

    read(test_file,rtest_string);
    if iostatus(test_file) = io_ok
        then error( 3 );
    if not eof(test_file)
        then error( 4 );

end.
 