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
cl::opt<bool> DumpTrace("t", cl::desc("Dump trace"),
                         cl::init(false), cl::Hidden);

static std::string getValueName (const Value *v);
/// Points to relation
typedef set<const Value *> PointsToSet;
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

inline raw_ostream &operator<<(raw_ostream &out, const PointsToSet &pst) {
  out << "{";
  for(auto v:pst) out << getValueName(v) << ", ";
  out << "} ";
  return out;
}
inline raw_ostream &operator<<(raw_ostream &out, const PointsToInfo &info) {
  for (auto &item:info.pointsTo) {
    if(item.second.empty()) continue;
    out << getValueName(item.first) << "->";
    out << item.second;
  }
  return out;
}
/// End of Points to relation

struct FuncPtrPass;
struct RetInfo {
  PointsToSet pstSet;
  PointsToInfo argsInfo;
  bool operator==(const RetInfo& info) {
    return pstSet==info.pstSet && argsInfo == info.argsInfo;
  }
};
/// Intra-Procedural Visitor
class IntraPointerVisitor : public DataflowVisitor<PointsToInfo> {
  FuncPtrPass *cfaPass = NULL;
  enum ALLOCSITE_TYPE{
    STACK, HEAP
  };
  map<const Value*, ALLOCSITE_TYPE> allocSites;
  map<const Instruction*, const Value*> inst2site;

  bool propagateInfo2Parameters(const CallInst *callInst, const Function *F, PointsToInfo* pstInfo);

  void handleCall(ImmutableCallSite& cs, PointsToInfo* pstInfo);
  RetInfo doCall(const CallInst *callInst, const Function *F, PointsToInfo* pstInfo, bool *changed);
  void strongUpdatePassedObj(PointsToInfo* callerInfo, const PointsToInfo& passedInfo);
public:
  IntraPointerVisitor(FuncPtrPass* pass): cfaPass(pass){}
  void merge(PointsToInfo *dest, const PointsToInfo &src) override;
  void compDFVal(Instruction *inst, PointsToInfo *pstInfo) override;
  PointsToSet getPstSet(const Value* value, PointsToInfo* pstInfo);
  const Value* getOrCreateAllocSite(const Instruction* inst, ALLOCSITE_TYPE type);
  bool isHeapObj(const Value* v);
  bool isStackObj(const Value* v);
  bool isAllocObj(const Value* v);
};

class UniqueQueue {
  typedef const Function* T;
  set<T> st;
  queue<T> que;
public:
  void push(const T& t) {
    if(st.count(t)) return;
    st.insert(t);
    que.push(t);
  }

  void pop() {
    assert(!que.empty());
    auto t = que.front();
    que.pop();
    assert(st.count(t));
    st.erase(t);
  }

  const T& front() {
    assert(!que.empty());
    return que.front();
  }

  bool empty() {
    return que.empty();
  }
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
  // typedef queue<const Function*> FuncWorklist;
  typedef UniqueQueue FuncWorklist;
  FuncWorklist worklist;
  IntraPointerVisitor visitor;
  /// context insensitive, we merge all the return information
  /// callsite will get points-to set fro, here
  map<const Function*, RetInfo> retPSet;
  map<const Function*, set<const Value*>> objPassedByArgs;
  map<int, set<const Function*>> callees;
  map<const Function*, set<const Function*> > callers;
  // PointsToInfo heap;

  LLVMContext *Ctx;
  const RetInfo& getRetInfo(const Function *F) {
    return retPSet[F];
  }
  /// return true if retInfo changes
  bool addRetInfo(Function *F, const PointsToSet& newInfo, const PointsToInfo *calleeInfo) {
    if(DebugInfo) llvm::errs() << "newInfo: " << newInfo;
    // if(newInfo.empty()) return false;
    if(DebugInfo) llvm::errs() << "\norigin: " << retPSet[F].pstSet;
    auto retInfo = retPSet[F];
    mergePtsSet(&retInfo.pstSet, newInfo);
    addPassedObj2RetInfo(F, &retInfo.argsInfo, calleeInfo);
    if(DebugInfo) llvm::errs() << "\nnow: " << retInfo.argsInfo <<"\n";
    if(!(retPSet[F] == retInfo)) {
      llvm::errs() << "return info changes!\n";
      retPSet[F] = retInfo; // strong update
      return true;
    }
    return false;
  }

  // const PointsToSet& getObjSetPassedBy(const Function* F) {}

  void addPassedObj2RetInfo(const Function* F, PointsToInfo* retSiteInfo, const PointsToInfo *calleeInfo) {
    auto& argsObjSet = objPassedByArgs[F];
    for(auto o:argsObjSet) {
      if(!calleeInfo->pointsTo.count(o)) 
        continue;
      const PointsToSet& x = calleeInfo->pointsTo.at(o);
      mergePtsSet(&retSiteInfo->pointsTo[o], x);
    }
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

  void recordCallee(const Instruction* inst, const Function* F) {
    auto D = inst->getDebugLoc();
    assert(D && "no debug line location");
    callees[D.getLine()].insert(F);
  }
  /// add a function to global worklist
  void addFuncToWorklist(const Function *F) { worklist.push(F); }

  void run(Module &M) {
    Ctx = &M.getContext();
    PointsToInfo initial; // empty mapping
    for(auto it=begin(M); it!=end(M); it++) {
      Function* F = &*it;
      addFuncToWorklist(F);
    }
    /// 
    
    while(!worklist.empty()) {
      /// set DOES NOT provide FIFO, which will break our assumption
      /// when abort & re-insert caller into worklist
      // Function * F = const_cast<Function*>(*worklist.begin());
      // worklist.erase(worklist.begin());
      Function *F = const_cast<Function*>(worklist.front());
      worklist.pop();
      if(DebugInfo) {
        llvm::errs() << "pop function: " << F->getName() << "\n";
      }
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
    run(M);
    for(auto& kv:gloablMap) {
      // errs() << kv.first->getName() << ":\n";
      // printDataflowResult<PointsToInfo>(errs(), kv.second);
    }
    dumpResult();
    return false;
  }

  void dumpResult() {
    for(auto kv:callees) {
      // auto D = kv.first->getDebugLoc();
      // assert(D);
      llvm::errs() << kv.first << " : ";
      string res;
      for(auto &f:kv.second) {
        res += f->getName().str() + ", ";
      }
      assert(res.size() > 0 && "no target method!");
      llvm::errs() << res.substr(0, res.size()-2) << "\n";
    }
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
  if(DumpTrace) {
    llvm::errs() << "before: " << *pstInfo << "\n";
    inst->dump();
  }
  switch (inst->getOpcode())
  {
  case Instruction::Alloca: {
    auto allocInst = cast<AllocaInst>(inst);
    if(!allocInst->getAllocatedType()->isIntegerTy()) {
      // if(DebugInfo) {
      //   llvm::errs() << "alloc type: ";
      //   allocInst->getAllocatedType()->dump();
      // }
      auto site = getOrCreateAllocSite(inst, ALLOCSITE_TYPE::STACK);
      pstInfo->pointsTo[inst].clear();
      pstInfo->pointsTo[inst].insert(site);
    }
    break;
  } 
  case Instruction::Call: {
    ImmutableCallSite cs(inst);
    handleCall(cs, pstInfo);
    break;
  }
  case Instruction::Ret: 
    /// TODO: struct?
    {
      PointsToSet src;
      if(inst->getNumOperands() > 0 && inst->getOperand(0)->getType()->isPointerTy()) 
        src = getPstSet(inst->getOperand(0), pstInfo);
      auto curF = inst->getParent()->getParent();
      bool changed = cfaPass->addRetInfo(curF, src, pstInfo);
      if(changed) {
        if(DebugInfo) 
          llvm::errs() << curF->getName() << "'s return set changes\n";
        auto& callers = cfaPass->callers[curF];
        for(auto caller:callers) 
          cfaPass->addFuncToWorklist(caller);
      }
    }
    break;
  case Instruction::PHI: {
    auto phiInst = cast<PHINode>(inst);
    auto& dest = pstInfo->pointsTo[inst];
    for(unsigned i=0; i<phiInst->getNumIncomingValues(); i++) {
      auto pst = getPstSet(phiInst->getIncomingValue(i), pstInfo);
      mergePtsSet(&dest, pst);
    }
    break;
  }
  case Instruction::BitCast:
  case Instruction::GetElementPtr: {
    if(inst->getType()->isPointerTy()) {
      auto loadFrom = inst->getOperand(0);
      // strong update
      pstInfo->pointsTo[inst] = pstInfo->pointsTo[loadFrom];
      break;
    }
  }
  case Instruction::Load:
    if(inst->getType()->isPointerTy()) {
      auto loadFrom = inst->getOperand(0);
      auto &src = pstInfo->pointsTo[loadFrom];
      // auto &loadTo = pstInfo->pointsTo[inst];
      PointsToSet res;
      for(auto v:src) {
        mergePtsSet(&res, getPstSet(v, pstInfo));
      }
      // strong update
      pstInfo->pointsTo[inst] = res;
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
        /// TODO: we only have malloc in this case
        if(isHeapObj(singleLoc)) 
          pstInfo->pointsTo[singleLoc] = getPstSet(value, pstInfo);
          // mergePtsSet(&pstInfo->pointsTo[singleLoc], getPstSet(value, pstInfo));
        // else if(isStackObj(singleLoc))
        //   pstInfo->pointsTo[singleLoc] = getPstSet(value, pstInfo);
        else  pstInfo->pointsTo[singleLoc] = getPstSet(value, pstInfo);
      } else if(dests.empty()) 
        *pstInfo = PointsToInfo::TOP; // remove all information
      else {
        for(auto p:dests) 
          mergePtsSet(&pstInfo->pointsTo[p], getPstSet(value, pstInfo));
      }
    }
    break;
  default:
    break;
  }
   if(DumpTrace) {
    llvm::errs() << "after: " << *pstInfo << "\n";
    // if(!cfaPass->heap.empty())llvm::errs() << "heap: " << cfaPass->heap << "\n";
  }
}

/// return true if callee input changes?
bool IntraPointerVisitor::propagateInfo2Parameters(const CallInst *callInst, const Function *F, PointsToInfo* pstInfo) {
  auto entry = const_cast<BasicBlock*>(&*F->begin());
  auto &fMap = cfaPass->gloablMap[F];
  
  auto argIt = callInst->arg_begin();
  auto parIt = F->arg_begin();
  auto entryIn = fMap[entry].first;
  while(argIt != callInst->arg_end() && parIt != F->arg_end()) {
    Value* arg = *argIt;
    const Value* par = &*parIt;
    if(arg->getType()->isPointerTy()) {
      auto argSet = getPstSet(arg, pstInfo);
      mergePtsSet(&entryIn.pointsTo[par], argSet);
      for(auto p:argSet) {
        // propagate one level obj info, it can be a closure
        if(!isAllocObj(p)) continue;
        cfaPass->objPassedByArgs[F].insert(p);
        mergePtsSet(&entryIn.pointsTo[p], pstInfo->pointsTo[p]);
      }
    }
    ++argIt;
    ++parIt;
  }

  if(!(entryIn == fMap[entry].first)) {
    if(DebugInfo) {
      llvm::errs() << "after passing args: " << fMap[entry].first << "\n";
      llvm::errs() << "before passing args: " << entryIn << "\n";
    }
    fMap[entry].first = entryIn;
    cfaPass->addFuncToWorklist(F);
    return true;
  }
  return false;
}

void IntraPointerVisitor::strongUpdatePassedObj(PointsToInfo* callerInfo, const PointsToInfo& passedInfo) {
  /// only strong update passed objs
  for(auto& kv:passedInfo.pointsTo) {
    auto k = kv.first;
    callerInfo->pointsTo[k] = kv.second;
  }
}

RetInfo IntraPointerVisitor::doCall(const CallInst *callInst, const Function *F, PointsToInfo* pstInfo, bool *changed) {
  cfaPass->callers[F].insert(callInst->getParent()->getParent());
  bool calleeInputChanged = propagateInfo2Parameters(callInst, F, pstInfo);
  assert(changed != NULL);
  *changed = calleeInputChanged;
  return cfaPass->getRetInfo(F);
}

void IntraPointerVisitor::handleCall(ImmutableCallSite& cs, PointsToInfo* pstInfo) {
  auto callInst = cast<CallInst>(cs.getInstruction());
  if(auto F = cs.getCalledFunction()) {
    if(F->isIntrinsic() || !F->isDSOLocal()) return;
    cfaPass->recordCallee(cs.getInstruction(), F);
    if(F->isDeclaration()) {
      // malloc
      if(F->getName().startswith("malloc")) {
        auto site = getOrCreateAllocSite(callInst, ALLOCSITE_TYPE::HEAP);
        pstInfo->pointsTo[callInst].clear();
        pstInfo->pointsTo[callInst].insert(site);
      }
    } else {
      bool changed = false;
      auto ret = doCall(callInst, F, pstInfo, &changed);
      if(!changed) {
        pstInfo->pointsTo[callInst] = ret.pstSet; // strong update
        strongUpdatePassedObj(pstInfo, ret.argsInfo);
      } else {
        *pstInfo = PointsToInfo::TOP;
        cfaPass->addFuncToWorklist(callInst->getParent()->getParent());
      }
    }
  } else {
    auto callValue = cs.getCalledValue();
    auto &candidates = pstInfo->pointsTo[callValue];
    if(candidates.empty())
      *pstInfo = PointsToInfo::TOP;
    else {
      RetInfo allRet;
      bool changed = false;
      for(const Value* v:candidates) {
        // assert(v->getType()->isFunctionTy());
        if(!isa<Function>(v)) {
          llvm::errs() << "call a non-func value!\n";
          callValue->dump();
          v->dump();
        }
        auto F = cast<Function>(v);
        cfaPass->recordCallee(cs.getInstruction(), F);
        // do call
        auto ret = doCall(callInst, F, pstInfo, &changed);
        if(changed) break;
        mergePtsSet(&allRet.pstSet, ret.pstSet);
        merge(&allRet.argsInfo, ret.argsInfo);
      }
      if(!changed) {
        pstInfo->pointsTo[callInst] = allRet.pstSet; // strong update
        strongUpdatePassedObj(pstInfo, allRet.argsInfo);
      } else {
        *pstInfo = PointsToInfo::TOP;
        cfaPass->addFuncToWorklist(callInst->getParent()->getParent());
      }
    }
  }
}

bool IntraPointerVisitor::isHeapObj(const Value* v) {
  return allocSites.count(v) && allocSites[v]==HEAP;
}

bool IntraPointerVisitor::isStackObj(const Value* v) {
  return allocSites.count(v) && allocSites[v]==STACK;
}

bool IntraPointerVisitor::isAllocObj(const Value* v) {
  return allocSites.count(v);
}

PointsToSet IntraPointerVisitor::getPstSet(const Value* value, PointsToInfo* pstInfo) {
  assert(value != NULL);
  PointsToSet ret;
  if(isa<Constant>(value) && !allocSites.count(value)) {
    if(DebugInfo) llvm::errs() << "literal!\n";
    // nice!
    if(!isa<ConstantPointerNull>(value)) 
      ret.insert(value);
    else {
      llvm::errs() << "found a NULL pointer!\n";
    }
    return ret;
  }
  // if(isAllocObj(value)) {
  if(isHeapObj(value)) {
    if(DebugInfo) llvm::errs() << "read heap: " << getValueName(value) << "\n";
    // return cfaPass->heap.pointsTo[value];
    return pstInfo->pointsTo[value];
  }
  return pstInfo->pointsTo[value];
}

const Value* IntraPointerVisitor::getOrCreateAllocSite(const Instruction* inst, ALLOCSITE_TYPE type) {
  if(inst2site.count(inst)) return inst2site[inst];

  static int line = 0; 
  line++;
  if(inst->getDebugLoc()) line = inst->getDebugLoc().getLine();
  Value* site = ConstantInt::get
        (Type::getInt32Ty(*cfaPass->Ctx), line);
  // site->setName(inst->getName()); // not work...
  if(DebugInfo) {
    llvm::errs() << "alloca site of \"" << inst->getName() << "\" ";
    site->dump();
  }
  allocSites.emplace(site, type);
  inst2site.emplace(inst, site);
  return site;
}
////

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

static std::string getValueName (const Value *v) {
  // If we can get name directly
  if (v->getName().str().length() > 0) {
      return "%" + v->getName().str();
  } else if (isa<Instruction>(v)) {
      std::string s = "";
      raw_string_ostream *strm = new raw_string_ostream(s);
      v->print(*strm);
      std::string inst = strm->str();
      size_t idx1 = inst.find("%");
      size_t idx2 = inst.find(" ", idx1);
      if (idx1 != std::string::npos && idx2 != std::string::npos && idx1 == 2) {
          return inst.substr(idx1, idx2 - idx1);
      } else {
          // nothing match
          return "";
      }
  } else {
      std::string s = "";
      raw_string_ostream *strm = new raw_string_ostream(s);
      v->print(*strm);
      std::string inst = strm->str();
      return "\"" + inst + "\"";
  }
}