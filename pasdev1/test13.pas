PROGRAM TEST13 OPTIONS DUMP;

TYPE
    COLOR = ( RED, ORANGE, YELLOW, GREEN, BLUE, INDIGO, VIOLET );
    CSET = SET OF COLOR;
    C1SET = SET OF RED .. GREEN;

VAR
    I, J, K: INTEGER;
    SC1, SC2: COLOR;
    PC1, PC2: RED .. GREEN;
    CS1, CS2: CSET;
    CT1, CT2: C1SET;

BEGIN
I := J; PC1 := PC2; SC1 := SC2; CS1 := CS2; CT1 := CT2;
I := SC1; PC1 := J; SC1 := PC1; CS1 := CT1; CT1 := PC1; END.
   