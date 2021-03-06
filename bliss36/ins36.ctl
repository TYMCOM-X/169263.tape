!+
!
! FUNCTION:
!    This batch file is designed to "install" BLISS-36 on a TOPS-10
!    system.  By default the system files will go to SYS:, HLP:, DOC:,
!    and BLI:.
!
! USE:
!    To use this batch file all the files (including this one) should
!    be transfered to an area with sufficient space (8000 blocks).
!    Then this batch file should be run as:
!
!	.SUBMIT INS36.CTL/TIME:00:30:00
!
!    When it has completed the log file should be checked for
!    any problems.  If there are any problems then corrective action
!    should be taken and the process repeated.  The file DEL36.CTL
!    may then be run, if desired.  Read the comments on its first page
!    for details.
!-
!+
! On this page logical device names are assigned to the various places
! where release files are put.  These can be modified to produce
! non-standard installations.
!
!-

! Display the versions being used.
!
.SET WATCH VERSION

! This is where it expects to find things in the first place.
!
.ASSIGN DSK: BDSK:

! HELP files are put in BHLP:
!
.ASSIGN HLP: BHLP:

! Other documentation files are put in BDOC:
!
.ASSIGN DOC: BDOC:

! The compiler object modules are put in BCMP:
!    N.B.: The default is to throw these files away.
!	by copying them onto themselves.
!
.ASSIGN DSK: BCMP:

! The BLISS compilers and utility programs are put in BSYS:
!
.ASSIGN SYS: BSYS:

! All utilities and sources are put in BUTL:
!
.ASSIGN BLI: BUTL:
!+
! Verify that all executable files run properly.
!-
.RUN CVT10
.RUN DMPREL
.RUN MONINT
!+
! Build the BLISS production compiler.
!-
.R LINK
*@BDSK:SEGCMN.LNK
*@BDSK:SEG1.LNK

.R LINK
*SEG2/MAP
*@BDSK:SEGCMN.LNK
*@BDSK:SEG2.LNK

.R LINK
*SEG3/MAP
*@BDSK:SEGCMN.LNK
*@BDSK:SEG3.LNK

.R LINK
*SEG4/MAP
*@BDSK:SEGCMN.LNK
*@BDSK:SEG4.LNK

.R LINK
*SEG5/MAP
*@BDSK:SEGCMN.LNK
*@BDSK:SEG5.LNK
!+
! Build the BLISS debugging compiler.
!-
.R LINK
*@BDSK:DEB36.LNK
! Check to make sure it is the correct compiler.
.RUN BDSK:BLISS
*BDSK:LSTCHK,BDSK:LSTCHK=BDSK:LSTCHK/VAR:1/CREF
*    MACRO EXPECTED_VERSION = '4(210)' %;
*^Z
*    MACRO LIST_FILE_NAME = 'LSTCHK.LST' %;
*^Z
.
.R LINK
*BDSK:LSTCHK,BDSK:XPOT10/SEARCH,BDSK:B361AB/EX/GO
!+
!Build the XPORT Structure Dump Generator
!-
.R LINK
*BDSK:XDUMP/SAV=BDSK:XDUMP,BDSK:XPOT10/SEARCH,BDSK:B361AB/SEARCH/GO
!+
! SET THE PROTECTION FOR ALL FILE TRANSFERS.
! NOTE THAT WHEN A FILE IS COPIED TO A DESTINATION WHERE A COPY
! ALREADY EXISTS, IT INHERITS THE PROTECTION OF THE ORIGINAL.
!-
.SET DEFAULT PROTECTION 155
!+
! Install the compilers.
!-
.COPY BSYS:BLISS.EXE = BDSK:BLISS.EXE
.COPY BSYS:BLSSG2.EXE = BDSK:BLSSG2.EXE
.COPY BSYS:BLSSG3.EXE = BDSK:BLSSG3.EXE
.COPY BSYS:BLSSG4.EXE = BDSK:BLSSG4.EXE
.COPY BSYS:DEB36.EXE = BDSK:DEB36.EXE

!+
! Transfer the OTS rel files to the specified destination.
!-
.COPY BSYS:B361AB.REL = BDSK:B361AB.REL
.COPY BSYS:B361AT.REL = BDSK:B361AT.REL
.COPY BSYS:B361LB.REL = BDSK:B361LB.REL
.COPY BSYS:B361LT.REL = BDSK:B361LT.REL

!+
! Check the installed production compiler.
!-
.RUN BSYS:BLISS
*BDSK:LSTCHK,BDSK:LSTCHK=BDSK:LSTCHK/VAR:1/CREF
*    MACRO EXPECTED_VERSION = '4(210)' %;
*^Z
*    MACRO LIST_FILE_NAME = 'LSTCHK.LST' %;
*^Z
.
.R LINK
*BDSK:LSTCHK,BDSK:XPOT10/SEARCH,BSYS:B361AB/SEARCH/EX/GO
!+
! Install the HELP files.
!-
.COPY BHLP:BLISS.HLP = BDSK:BLISS.HLP
.COPY BHLP:CVT10.HLP = BDSK:CVT10.HLP
.COPY BHLP:DMPREL.HLP = BDSK:DMPREL.HLP
.COPY BHLP:TUTIO.HLP = BDSK:TUTIO.HLP
.COPY BHLP:SIX12.HLP = BDSK:SIX12.HLP
!+
! Install other documentation files.
!-
.COPY BDOC:BLISS.DOC = BDSK:BLISS.DOC
.COPY BDOC:CVT10.DOC = BDSK:CVT10.DOC
.COPY BDOC:MONINT.DOC = BDSK:MONINT.DOC
.COPY BDOC:SIX12.MEM = BDSK:SIX12.MEM
!+
! Transfer the source files for the Version 2 OTS and SIX12.
!-
.COPY BUTL:BLSOTS.MAC = BDSK:BLSOTS.MAC
.COPY BUTL:REG1AB.MAC = BDSK:REG1AB.MAC
.COPY BUTL:REG1AT.MAC = BDSK:REG1AT.MAC
.COPY BUTL:REG1LB.MAC = BDSK:REG1LB.MAC
.COPY BUTL:REG1LT.MAC = BDSK:REG1LT.MAC
.COPY BUTL:SIX12.B36 = BDSK:SIX12.B36
!+
! Transfer the object libraries used to build the BLISS compilers to
! the specified destination.  (Default is device DSK:, which means
! that they are not installed).
!-
.COPY BCMP:BLISS.REL = BDSK:BLISS.REL
.COPY BCMP:DEB36.REL = BDSK:DEB36.REL
!+
! Transfer the various utilities to their destinations.
!-
.COPY BUTL:TUTIO.R36 = BDSK:TUTIO.R36
.COPY BUTL:EZIO10.B36 = BDSK:EZIO10.B36
.COPY BUTL:EZIO10.REL = BDSK:EZIO10.REL
.COPY BUTL:UUOSYM.R36 = BDSK:UUOSYM.R36
.COPY BUTL:UUOSYM.L36 = BDSK:UUOSYM.L36
.COPY BUTL:TENDEF.R36 = BDSK:TENDEF.R36
.COPY BUTL:TENDEF.L36 = BDSK:TENDEF.L36
.COPY BSYS:MONINT.EXE = BDSK:MONINT.EXE
.COPY BSYS:DMPREL.EXE = BDSK:DMPREL.EXE
.COPY BSYS:CVT10.EXE = BDSK:CVT10.EXE
.COPY BUTL:CVT10.SNO = BDSK:CVT10.SNO
.COPY BSYS:XDUMP.EXE = BDSK:XDUMP.EXE
.COPY BUTL:XDUMP.REL = BDSK:XDUMP.REL
.COPY BUTL:XPORT.REQ = BDSK:XPORT.REQ
.COPY BUTL:XPORT.L36 = BDSK:XPORT.L36
.COPY BUTL:XPORT.REL = BDSK:XPOT10.REL
.COPY BUTL:XPORT.DBG = BDSK:XPOT10.DBG
.COPY BUTL:XPOT10.REL = BDSK:XPOT10.REL
.COPY BUTL:XPOT10.DBG = BDSK:XPOT10.DBG
! This page sets up the banner indicating success and/or failure of
! the installation

NRMEND::
! Successful completion
.TYPE NORMAL.BNR

%CERR::
! Error at the EXEC/BATCH level.
.TYPE ERROR.BNR

%ERR::
! Error at the program level.
.TYPE ERROR.BNR

%TERR::
! Ran out of time.
.TYPE ERROR.BNR

%FIN::
    