SRC_EXT="lat"
BAD=lattests/bad/*
GOOD=lattests/good/*.${SRC_EXT}
OBJ1=lattests/extensions/objects1/*.${SRC_EXT}
OBJ2=lattests/extensions/objects2/*.${SRC_EXT}
STRUCT=lattests/extensions/struct/*.${SRC_EXT}
RED='\033[0;31m'
NC='\033[0m'
GREEN='\033[0;32m'
for i in $BAD
do
    PLIK=$(echo $i | sed 's/lattests\///g')
    ER=$(./latc $i)
    if [[ "${ER}" =~ "ERROR" ]]
    then
	echo -en "[[ ${GREEN}OK${NC}  ]] ${PLIK}"
    else
	echo -e "[[ ${RED}ŹLE${NC} ]] ${PLIK}"
    fi
done


for i in $GOOD $OBJ1 $OBJ2 $STRUCT
do
    PLIK=$(echo $i | sed 's/lattests\///g')
    ER=$(./latc $i)
    if [[ "${ER}" =~ "OK" ]]
    then
	echo -e "[[ ${GREEN}OK${NC}  ]] ${PLIK}"
    else
	echo -e "[[ ${RED}ŹLE${NC} ]] ${PLIK}"
    fi
done
