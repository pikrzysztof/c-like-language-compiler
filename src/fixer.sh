tmp=`mktemp`
for i in Gramatyka/*.hs
do
    echo '{-# OPTIONS_GHC -w #-}' > $tmp
    cat $i >> $tmp
    mv "$tmp" "$i"
done

charFile=`dirname $0`"/Gramatyka/LexLatte.x"

head -n6 "$charFile" > "$tmp"
echo "import Data.Char(ord)" >> "$tmp"
tail -n +7 "$charFile" >> "$tmp"
mv "$tmp" "$charFile"
