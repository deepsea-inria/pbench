PROC="0"
ALGO="foo"
N="0"

while [ "$1" != "" ]; do
    PARAM=$1 
    VALUE=$2
    # echo "$PARAM $VALUE"
    case $PARAM in
        -algo)
            ALGO=$VALUE
            shift
            ;;
        -proc)
            PROC=$VALUE
            shift
            ;;
        -n)
            N=$VALUE
            shift
            ;;
        *)
            shift
            #echo "ERROR: unknown parameter \"$PARAM\""
            #exit 1
            ;;
    esac
    shift
done

if [ "$ALGO" = "foo" ]
then
  BASE=10
else
  BASE=20
fi

if [ "$PROC" = "0" ]
then
  echo exectime $BASE
else
  STAR="*"
  TIME=`echo "($BASE + $N + 2) / $PROC $STAR (1.1 ^ $PROC)" | bc -l`
  echo exectime $TIME
fi
