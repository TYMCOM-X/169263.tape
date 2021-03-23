do pascal
,cmdutl.tym=(pasdev2)cmdutl/enable:tymshare
,filutl.tym=(pasdev2)filutl/enable:tymshare
,cmdutl.adp=(pasdev2)cmdutl/enable:adp
,filutl.adp=(pasdev2)filutl/enable:adp
,cmdutl.m68=(pasdev2)cmdutl/enable:m68
,filutl.m68=(pasdev2)filutl/enable:m68
:tar vax
,cmdutl.vax=(pasdev2)cmdutl
,filutl.vax=(pasdev2)filutl

do runcop
smk
cmdutl.tym
filutl.tym
cmdutl.adp
filutl.adp
cmdutl.m68
filutl.m68
cmdutl.vax
filutl.vax

