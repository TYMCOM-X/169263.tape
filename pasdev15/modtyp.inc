type        BIT = 0..1;
        HW = 0..777777B;
        BYTE = 0..77B;
        ONETWO = (BITMASK, CONTROL, CODEBLOCK);
	DATEMASK = (MO, DAY, YR, HR, MIN, SEC);
        
        FWORD = packed record
             INT : -400000000000B..377777777777B
             end;
        IOBLOCKSIZE = 1..SIZEBLOCK;
 
 
                        DATEREC = packed record
                                 case boolean of
                                true : (INTF : INTEGER);
                                false: (FOO : packed array[DATEMASK] of BYTE)
                                end;
 
 
        IOBLOCK = packed record
                  case ONETWO of
BITMASK               :(BITS : packed array[1..BITSPERBLOCK] of BIT);
CONTROL               :(FORMAT : packed record
                        POINTER : INTEGER;
                        VERNUM : INTEGER;
                        DATEWD : DATEREC;
                        TESTWD : INTEGER;
                        JRST : packed array[1..36] of BIT;
                        STAT : packed array[1..2] of HW;
                        CODE : packed array[1..2] of HW;
                        VERSIONNUM : INTEGER;
                        MODNAME : packed array[1..36] of BIT;
                        SYMTABLPTR : INTEGER;
                        FILESIZE : packed array[1..2] of HW;
                        OVNUM : INTEGER;
                        REST : packed array[1..116] of INTEGER
                                end
                        );
CODEBLOCK      :(WORDS : packed array[1..SIZEBLOCK] of INTEGER)
                  end;
 
    