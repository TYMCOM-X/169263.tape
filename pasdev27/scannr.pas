program scannr;

$INCLUDE scannr.typ
$INCLUDE scnlit.typ
$INCLUDE scnfsa.typ

$INCLUDE scnlst
$INCLUDE scnlex
$INCLUDE scnpar
$INCLUDE scnnam
$INCLUDE scnrea
$INCLUDE scnreu
$INCLUDE scnpat
$INCLUDE scnfsa
$INCLUDE scnerr
$INCLUDE getiof
$INCLUDE fio

var
    fsa_matrix: transition_matrix;
    fsa_accept: acc_pat_vector;

begin
  rewrite (tty);
  open (tty);
  loop
    while not getiofiles (input, 'LEX', output, 'LST') do;

    rea_init;
    lexinit;
    initnames;
    initre;
    initpatterns;
    err_init;

    parser;
    endpatterns;

    list_input;

    make_fsa (fsa_matrix, fsa_accept);

    fio_close (listfb);
    clearnames;
    clearre;
    clearpatterns;
    rea_finish;
  end;
end.
    