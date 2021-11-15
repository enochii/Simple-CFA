//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Hello World" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/IR/CallSite.h>

#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>

#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>

#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include <queue>

#include "Liveness.h"
#include "Dataflow.h"

using namespace llvm;
using namespace std;

static ManagedStatic<LLVMContext> GlobalContext;
static LLVMContext &getGlobalContext() { return *GlobalContext; }

struct EnableFunctionOptPass : public FunctionPass {
    static char ID;
    EnableFunctionOptPass() :FunctionPass(ID) {}
    bool runOnFunction(Function & F) override {
        if (F.hasFnAttribute(Attribute::OptimizeNone))
        {
            F.removeFnAttr(Attribute::OptimizeNone);
        }
        return true;
    }
};

char EnableFunctionOptPass::ID = 0;

cl::opt<bool> DumpResult("r", cl::desc("Dump analysis results"),
                         cl::init(false), cl::Hidden);
cl::opt<bool> DumpModule("m", cl::desc("Dump module"),
                         cl::init(false), cl::Hidden);
cl::opt<bool> DebugInfo("d", cl::desc("Dump debug info"),
                         cl::init(false), cl::Hidden);
/// Points to relation
typedef set<Value *> PointsToSet;
static void mergePtsSet(PointsToSet *dest, const PointsToSet& src) {
  dest->insert(src.begin(), src.end());
}
// typedef map<Value *, PointsToSet> PointsToMap;
struct PointsToInfo {
	// bool addMe2Worklist = false;
  map<const Value *, PointsToSet> pointsTo;

  PointsToInfo() : pointsTo() {}
  PointsToInfo(const PointsToInfo &info) : pointsTo(info.pointsTo) {}

  bool operator==(const PointsToInfo &info) const {
    return pointsTo == info.pointsTo;
  }

  bool empty()const {
    return pointsTo.empty();
  }
  /// a empty map
  const static PointsToInfo TOP;
};
const PointsToInfo PointsToInfo::TOP;

inline raw_ostream &operator<<(raw_ostream &out, const PointsToInfo &info) {
  for (auto &item:info.pointsTo) {
    out << item.first->getName() << "-> { ";
    for(auto v:item.second) out << v->getName() << ", ";
    out << "}\n"; 
  }
  return out;
}
/// End of Points to relation

struct FuncPtrPass;
/// Intra-Procedural Visitor
class IntraPointerVisitor : public DataflowVisitor<PointsToInfo> {
  FuncPtrPass *cfaPass = NULL;

  void propagateInfo2Parameters(const CallInst *callInst, const Function *F, PointsToInfo* pstInfo);

  void handleCall(ImmutableCallSite& cs, PointsToInfo* pstInfo);
public:
  IntraPointerVisitor(FuncPtrPass* pass): cfaPass(pass){}

  void merge(PointsToInfo *dest, const PointsToInfo &src) override;

  void compDFVal(Instruction *inst, PointsToInfo *pstInfo) override;
};

///!TODO TO BE COMPLETED BY YOU FOR ASSIGNMENT 3
/// we have 2 kinds of worklist:
/// - global worklist of all functions
/// - local worklist of all BB of a function
/// this algorithm can be generalized as a framework beyond pointer analysis
/// BUT for simplicity we won't do this.
struct FuncPtrPass : public ModulePass {
  friend class IntraPointerVisitor;
  static char ID; // Pass identification, replacement for typeid
  FuncPtrPass() : ModulePass(ID), visitor(this) {}

  /// a function will have a local mapping
  map<const Function*, DataflowResult<PointsToInfo>::Type > gloablMap;
  set<Function*> worklist;
  IntraPointerVisitor visitor;
  /// context insensitive, we merge all the return information
  /// callsite will get points-to set fro, here
  map<Function*, PointsToSet> retPSet;
  map<const Instruction*, set<const Function*>> callees;

  const PointsToSet& getRetInfo(Function *F) {
    return retPSet[F];
  }
  void addRetInfo(Function *F, const PointsToSet& newInfo) {
    if(newInfo.empty()) return;
    mergePtsSet(&retPSet[F], newInfo);
  }
  /// if we add more information, will the IN of entry node of function F change?
  /// if changed, the info will be added into IN[F.entry]
  bool willChange(Function *F, const PointsToInfo &info) {
    auto& mapping = gloablMap[F];
    BasicBlock* entry = &*F->begin();
    auto newInfo = mapping[entry].first;
    visitor.merge(&newInfo, info);
    if(newInfo == mapping[entry].first) return false;
    mapping[entry].first = move(newInfo);
    return true;
  }

  bool willChange(Function *F, ReturnInst* returnSite, const PointsToInfo &info);

  /// add a function to global worklist
  void addFuncToWorklist(Function *F) { worklist.insert(F); }

  void run(Module &M) {
    PointsToInfo initial; // empty mapping
    for(auto it=begin(M); it!=end(M); it++) {
      Function* F = &*it;
      addFuncToWorklist(F);
    }
    /// 
    
    while(!worklist.empty()) {
      Function * F = *worklist.begin();
      worklist.erase(worklist.begin());
      auto &mapping = gloablMap[F];
      compForwardDataflow(F, &visitor, &mapping, initial);
    }
  } 

  bool runOnModule(Module &M) override {
    if(DumpModule) {
      errs() << "Hello: ";
      errs().write_escaped(M.getName()) << '\n';
      M.dump();
      errs() << "------------------------------\n";
    }
    return false;
  }
};

void IntraPointerVisitor::merge(PointsToInfo *dest, const PointsToInfo &src) {
  if(src.empty()) return;
  for(auto &kv:src.pointsTo) {
    auto key = kv.first;
    mergePtsSet(&dest->pointsTo[key], kv.second);
  }
}

void IntraPointerVisitor::compDFVal(Instruction *inst, PointsToInfo *pstInfo) {
  if (isa<DbgInfoIntrinsic>(inst))
    return;
  /// TODO:
  switch (inst->getOpcode())
  {
  // case Instruction::Alloca :
  case Instruction::Call: {
    ImmutableCallSite cs(inst);
    handleCall(cs, pstInfo);
    break;
  }
  case Instruction::Ret: 
    if(inst->getType()->isPointerTy()) {
      auto &src = pstInfo->pointsTo[inst->getOperand(0)];
      cfaPass->addRetInfo(inst->getParent()->getParent(), src);
    }
    break;
  case Instruction::PHI: {
    auto phiInst = cast<PHINode>(inst);
    auto& dest = pstInfo->pointsTo[inst];
    for(unsigned i=0; i<phiInst->getNumIncomingValues(); i++)
      mergePtsSet(&dest, pstInfo->pointsTo[phiInst->getIncomingValue(i)]);
    break;
  }
  case Instruction::GetElementPtr:
    // break; // handle it like a Load...
  case Instruction::Load:
    if(inst->getType()->isPointerTy()) {
      auto loadFrom = inst->getOperand(0);
      auto &src = pstInfo->pointsTo[loadFrom];
      auto &loadTo = pstInfo->pointsTo[inst];
      loadTo = src; // strong update
    }
    break;
  case Instruction::Store:
    /// *x = y;
    if(inst->getOperand(0)->getType()->isPointerTy()) {
      StoreInst* storeInst = cast<StoreInst>(inst);
      auto value = storeInst->getValueOperand();
      auto pointer = storeInst->getPointerOperand();
      auto &dests = pstInfo->pointsTo[pointer];
      if(dests.size() == 1) {
      /// TODO: incorrect!
        auto singleLoc = *dests.begin();
        // S(x) = S/S(x) ∪ S(y)
        pstInfo->pointsTo[singleLoc] = pstInfo->pointsTo[value];
      } else if(dests.empty()) 
        *pstInfo = PointsToInfo::TOP; // remove all information
      else {
        for(auto p:dests) 
          mergePtsSet(&pstInfo->pointsTo[p], pstInfo->pointsTo[value]);
      }
    }
    break;
  default:
    break;
  }
}

void IntraPointerVisitor::propagateInfo2Parameters(const CallInst *callInst, const Function *F, PointsToInfo* pstInfo) {
  auto entry = const_cast<BasicBlock*>(&*F->begin());
  auto &fMap = cfaPass->gloablMap[F];
  
  auto argIt = callInst->arg_begin();
  auto parIt = F->arg_begin();
  while(argIt != callInst->arg_end() && parIt != F->arg_end()) {
    Value* arg = *argIt;
    const Value* par = &*parIt;
    auto &argSet = pstInfo->pointsTo[arg];
    auto &entryIn = fMap[entry].first;
    mergePtsSet(&entryIn.pointsTo[par], argSet);
  }
}

void IntraPointerVisitor::handleCall(ImmutableCallSite& cs, PointsToInfo* pstInfo) {
  if(auto F = cs.getCalledFunction()) {
    if(F->isIntrinsic()) return;
    cfaPass->callees[cs.getInstruction()].insert(F);
    // propagateInfo2Parameters()
  }
}

char FuncPtrPass::ID = 0;
static RegisterPass<FuncPtrPass> X("funcptrpass",
                                   "Print function call instruction");

char Liveness::ID = 0;
static RegisterPass<Liveness> Y("liveness", "Liveness Dataflow Analysis");

static cl::opt<std::string>
  InputFilename(cl::Positional, cl::desc("<filename>.bc"), cl::init(""));

int main(int argc, char **argv) {
  LLVMContext &Context = getGlobalContext();
  SMDiagnostic Err;
  // Parse the command line to read the Inputfilename
  cl::ParseCommandLineOptions(
    argc, argv, "FuncPtrPass \n My first LLVM too which does not do much.\n");

  // Load the input module
  std::unique_ptr<Module> M = parseIRFile(InputFilename, Err, Context);
  if (!M) {
    Err.print(argv[0], errs());
    return 1;
  }

  llvm::legacy::PassManager Passes;
#if LLVM_VERSION_MAJOR == 5
  Passes.add(new EnableFunctionOptPass());
#endif
  /// Transform it to SSA
  Passes.add(llvm::createPromoteMemoryToRegisterPass());

  /// Your pass to print Function and Call Instructions
  // Passes.add(new Liveness());
  Passes.add(new FuncPtrPass());
  Passes.run(*M.get());
#ifndef NDEBUG
  // system("pause");
#endif
}
