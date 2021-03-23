run odmmon
comp odms
build resident using /define:hstrt.:#65000&
/s rdlib[,320156],paslib[,320155],sys:forlib
build main using mmodms,mmblds,mmbldp,mmdbop,mmdbpr,mmprnt,mmpack,mmmsym&
mmtcor,mmmpub,rename
build mdlpro using mmmdls,mmmdlp,mdlpro,mmsym2
quit
