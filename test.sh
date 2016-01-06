SRC_EXT="lat"
TEST_FOLDER=lattests
BAD=${TEST_FOLDER}/bad/*
GOOD=${TEST_FOLDER}/good/*.${SRC_EXT}
OBJ1=${TEST_FOLDER}/extensions/objects1/*.${SRC_EXT}
OBJ2=${TEST_FOLDER}/extensions/objects2/*.${SRC_EXT}
STRUCT=${TEST_FOLDER}/extensions/struct/*.${SRC_EXT}
RED='\033[0;31m'
NC='\033[0m'
GREEN='\033[0;32m'
for i in $BAD
do
    PLIK=$(echo $i | sed "s/${TEST_FOLDER}\///g")
    ER=$(./latc $i)
    if [[ "${ER}" =~ "ERROR" ]]
    then
	echo -e "[[ ${GREEN}OK${NC}  ]] ${PLIK}"
    else
	echo -e "[[ ${RED}ŹLE${NC} ]] ${PLIK}"
    fi
done


for i in $GOOD $OBJ1 $OBJ2 $STRUCT
do
    PLIK=$(echo $i | sed "s/${TEST_FOLDER}\///g")
    ER=$(./latc $i)
    if [[ "${ER}" =~ "OK" ]]
    then
	echo -e "[[ ${GREEN}OK${NC}  ]] ${PLIK}"
    else
	echo -e "[[ ${RED}ŹLE${NC} ]] ${PLIK}"
    fi
done
