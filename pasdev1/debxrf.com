do pascal
/glob,stat,notrace
debbrk/opt
debbol
debdmp/opt
debio/opt
deblex/opt
debprt/opt
debref
debscp/opt
debsym/opt
debug

do xref
debxrf.xrf=debbrk.sym,debbol.sym,debdmp.sym,debio.sym,deblex.sym,debprt.sym,
debref.sym,debscp.sym,debsym.sym,debug.sym,debasm.sy,deblib.sy
Pascal Debugger

del deb???.sym
