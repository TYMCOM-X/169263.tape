delete test.odb
run (pasdev1)odms
compile test.mdo=test
build testma using testa,paslib[31024,320155]/s
build main using test,paslib[31024,320155]/s
exit
