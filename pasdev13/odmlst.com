dele foo.tmp
do decmac
,mmovlm/46l=mmovlm
,mmdseg/46l=mmdseg



do format
foo.tmp=/append,list,pass,noxref
mmssym.typ
mmsmdl.typ
mmtcor.inc
mmdbpr.typ
/xref
mmdebm
mmmpub
mmmsym
mmmdls
mmmdlp
mdlpro
mmblds
mmbldp
mmpack
mmdbpr
mmdbop
mmprnt
mmodms

do runcop
jci
mmovlm.lst/macro
mmdseg.lst/macro
foo.tmp

dele foo.tmp,mmovlm.lst,mmdseg.lst
   