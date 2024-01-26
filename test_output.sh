
# Compile compiler
make

# Compile examples using compiler
./LatteCompiler official_examples/good/*.lat

# Run compiled examples and compare output with expected output
for file in official_examples/good/*.lat
do
  # If there is input file for program, then pass it to program
  if [ -f "${file%.*}.input" ]
  then
    lli -opaque-pointers ${file%.*}.bc < "${file%.*}.input" > "${file%.*}.result"
  else
    lli -opaque-pointers ${file%.*}.bc > "${file%.*}.result"
  fi
  diff "${file%.*}.result" "${file%.*}.output"
  # If diff returns 0, then files are the same
  if [ $? -eq 0 ]
  then
    echo "Test ${file%.*} passed"
  else
    echo "Test ${file%.*} failed"
  fi
done
