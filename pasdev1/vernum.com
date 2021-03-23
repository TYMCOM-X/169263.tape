decl all no no pascal.low,pascal.shr,pascmd.low,pascmd.shr,paslst.low,paslst.shr,vaxccg.low,vaxccg.shr
do build
l pascal,pascmd,paslst,vaxccg
decl run run no pascal.low,pascal.shr,pascmd.low,pascmd.shr,paslst.low,paslst.shr,vaxccg.low,vaxccg.shr
