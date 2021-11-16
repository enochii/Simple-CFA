mkdir bc
cd assign3-tests
cfiles=$(ls .)
for file in $cfiles; do
    prefix=${file:0:6}
    bc_file="$prefix.bc"
    clang -emit-llvm -c -O0 -g3 $file -o $bc_file
    # llvm-dis $bc_file
done
cd ..
mv assign2-tests/*.bc bc
# mv assign2-tests/*.ll bc