EXAMPLES="numbers primes years factorisations powerfactorisations \
    table powertable allfactorisations allpowerfactorisations"

ghc --make ringPacking.hs && 
    for i in $EXAMPLES ; do echo ${i} ; ./ringPacking -o ${i}.svg -w 1280 --selection=${i} ; done



