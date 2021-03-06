;+
;
; FUNCTION:
;    This batch file is designed to "install" BLISS-36 on a TOPS-10
;    system.  By default the system files will go to SYS:, HLP:, DOC:,
;    and BLI:.
;
; USE:
;    To use this batch file all the files (including this one) should
;    be transfered to an area with sufficient space (8000 blocks).
;    Then this batch file should be run as:
;
;	.SUBMIT INS36.CTL/TIME:00:30:00
;
;    When it has completed the log file should be checked for
;    any problems.  If there are any problems then corrective action
;    should be taken and the process repeated.  The file DEL36.CTL
;    may then be run, if desired.  Read the comments on its first page
;    for details.
;-
;+
; On this page logical device names are assigned to the various places
; where release files are put.  These can be modified to produce
; non-standard installations.
;
;-

; This is where it expects to find things in the first place.
;
ASSIGN DSK: BDSK:

; HELP files are put in BHLP:
;
ASSIGN DSK: BHLP:

; Other documentation files are put in BDOC:
;
ASSIGN DSK: BDOC:

; The compiler object modules are put in BCMP:
;    N.B.: The default is to throw these files away.
;	by copying them onto themselves.
;
ASSIGN DSK: BCMP:

; The BLISS compilers and utility programs are put in BSYS:
;
ASSIGN DSK: BSYS:

; All utilities and sources are put in BUTL:
;
ASSIGN DSK: BUTL:
;+
; Build the BLISS production compiler.
;-
R LINK
@BDSK:SEGCMN.LNK
@BDSK:SEG1.LNK

R LINK
SEG2/MAP
@BDSK:SEGCMN.LNK
@BDSK:SEG2.LNK

R LINK
SEG3/MAP
@BDSK:SEGCMN.LNK
@BDSK:SEG3.LNK

R LINK
SEG4/MAP
@BDSK:SEGCMN.LNK
@BDSK:SEG4.LNK

R LINK
SEG5/MAP
@BDSK:SEGCMN.LNK
@BDSK:SEG5.LNK
;+
; Build the BLISS debugging compiler.
;-
R LINK
@BDSK:DEB36.LNK
; Check to make sure it is the correct compiler.
RUN BDSK:BLISS
BDSK:LSTCHK,BDSK:LSTCHK=BDSK:LSTCHK/VAR:1/CREF
    MACRO EXPECTED_VERSION = '4(210)' %;
^Z
    MACRO LIST_FILE_NAME = 'LSTCHK.LST' %;
^Z

R LINK
BDSK:LSTCHK,BDSK:XPOT10/SEARCH,BDSK:B361AB/EX/GO
;+
;Build the XPORT Structure Dump Generator
;-
R LINK
BDSK:XDUMP/SAV=BDSK:XDUMP,BDSK:XPOT10/SEARCH,BDSK:B361AB/SEARCH/GO
;+
; Check the installed production compiler.
;-
RUN BSYS:BLISS
BDSK:LSTCHK,BDSK:LSTCHK=BDSK:LSTCHK/VAR:1/CREF
    MACRO EXPECTED_VERSION = '4(210)' %;
^Z
    MACRO LIST_FILE_NAME = 'LSTCHK.LST' %;
^Z

R LINK
BDSK:LSTCHK,BDSK:XPOT10/SEARCH,BSYS:B361AB/SEARCH/EX/GO
 