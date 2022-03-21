## Simple CFA

Simple control flow analysis, assignment 3 of UCAS course <<Adcanced Compiler>>. For more information you can refer to [this blog](https://enochii.github.io/posts/2021/11/16/simple-Control-Flow-Analysis-of-C.html).
  
You can also see:
- [assignment 1: AST Interpreter](https://github.com/enochii/AST-Interpreter)
- [assignment 2: Call Printer](https://github.com/enochii/call-printer)

### build & test

```shell
  mkdir build
  cd build
  # make sure you change your hard-coded LLVM dir in CMakeList.txt first...
  cmake ..
  make
  
  source grade.sh
```

for single test case:

```shell
  source run.sh bc/test01.bc
```

All test cases can be found at `test/`, corresponding ground truths are located at `ground-truth`.
  
### Misc
  
You can also take a look at `note-cn.md` or `detail.md`.
