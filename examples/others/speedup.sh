# assume $2 to be the nb processors; simulate for speedups
# assume $4 to be the algo

if [ "$4" = "foo" ]
then
  BASE=10
else
  BASE=20
fi

if [ $2 -eq 0 ]
then
  echo exectime $BASE
else
  STAR="*"
  TIME=`echo "($BASE + 2) / $2 $STAR (1.1 ^ $2)" | bc -l`
  echo exectime $TIME
fi
