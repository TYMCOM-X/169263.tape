do (pasdev1)pascal
/search ((pasdev2),(pasdev1))
stat

do link
stat/ssave=
stat
/sea fio[31024,320156]
/sea paslib[31024,320155]
/sea forlib[31024,320155]
/g
 