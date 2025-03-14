#include "llvm/Pass.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

struct Muliply2LSH : public PassInfoMixin<Muliply2LSH> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
        for (auto &F : M.functions()) {
            for (auto &B : F) {
                for (auto &I : B) {
                    if ((I.getOpcode() == Instruction::Mul)) {
                        errs() << "found a multiplication operation " << "!\n";
                    }
                    else if (auto *op = dyn_cast<BinaryOperator>(&I)) {
                        errs() << "found another operation " << I.getOpcodeName() << "!\n";
                        if (op->isCommutative()) {
                            op->swapOperands();
                        } 
                    }
                }
            }
        }
        return PreservedAnalyses::all();
    };
};




}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
    return {
        .APIVersion = LLVM_PLUGIN_API_VERSION,
        .PluginName = "Silly transformation pass",
        .PluginVersion = "v0.1",
        .RegisterPassBuilderCallbacks = [](PassBuilder &PB) {
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel Level) {
                    MPM.addPass(Muliply2LSH());
                });
        }
    };
}
