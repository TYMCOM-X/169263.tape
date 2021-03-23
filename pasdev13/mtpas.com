ru (pasdev1)pascal
mtenv
:env mtenv
/deb,noquick
mt01a/mainseg,disable(auto_file_index)
/ov
mtinit
mt02a=mtmod/en(mt02a,storage_overflow,public,others_handler)
mt03a=mtmod/en(mt03a,causeheapoverflow)
mt11a=mtmod/en(mt11a,public,others_handler,user_condition)
mt11b=mtmod/en(mt11b,external)
mt12a=mtmod/en(mt12a)
mt13a=mtmod/en(mt13a,others_handler)

r (upl)com;mtpas2

