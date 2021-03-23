copy mmmsym.pas=mmsym2.pas
do (pasdev1)pascal
/check
/global
mmmpub
/mainseg,nooverlay
mmblds
mmbldp
mmdbpr
mmdbop
mmprnt
mmpack
mmodms
mmmsym
/nomainseg,overlay
mmsym2
mmmdls
mmmdlp
mdlpro

delete mmsym2.pas
