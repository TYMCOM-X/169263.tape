PROGRAM TEST41 OPTIONS DUMP;

(*  STRING/CHARACTER CONVERSION FOLDING TEST  *)

TYPE
    STRF = PACKED ARRAY [1..5] OF CHAR;
    STRV = STRING [5];

CONST
    C0 = '';
    C1 = 'A';
    C3 = 'PSI';
    C5 = 'GAMMA';
    C7 = 'OMICRON';

    CC0: CHAR = C0;
    CC1: CHAR = C1;
    CC3: CHAR = C3;
    CC5: CHAR = C5;
    CC7: CHAR = C7;

    CF0: STRF = C0;
    CF1: STRF = C1;
    CF3: STRF = C3;
    CF5: STRF = C5;
    CF7: STRF = C7;

    CV0: STRV = C0;
    CV1: STRV = C1;
    CV3: STRV = C3;
    CV5: STRV = C5;
    CV7: STRV = C7;

BEGIN  END.
    