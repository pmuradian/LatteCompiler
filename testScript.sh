for i in ./tests/good/*.lat
do
    b=$(basename $i .lat)
    (/usr/local/opt/llvm/bin/lli ./tests/good/$b.bc | diff - ./tests/good/$b.output || echo $i)
done