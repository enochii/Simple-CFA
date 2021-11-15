
file_dir="assign3-tests"
bc_dir="bc"

mkdir $bc_dir
cd $file_dir
cfiles=$(ls .)
for file in $cfiles; do
    prefix=${file:0:6}
    bc_file="$prefix.bc"
    clang -emit-llvm -c -O0 -g3 $file -o ../$bc_dir/$bc_file
    # break # only 
done
cd ..