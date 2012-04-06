//===- Scalarize.cpp - Scalarize LunarGLASS IR ----------------------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2011 LunarG, Inc.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; version 2 of the
// License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.
//
//===----------------------------------------------------------------------===//
//
// Author: Michael Ilseman, LunarG
//
//===----------------------------------------------------------------------===//
//
// Scalarize the IR.
//   * Loads of uniforms become multiple loadComponent calls
//
//   * Reads/writes become read/writeComponent calls
//
//   * Component-wise operations become multiple ops over each component
//
//   * Texture call become recomponsed texture calls
//
//   * Vector ops disappear, with their users referring to the scalarized
//   * components
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Function.h"
#include "llvm/InstrTypes.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"

#include "Passes/PassSupport.h"
#include "Passes/Util/InstructionUtil.h"
#include "Exceptions.h"
#include "Util.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    struct VectorValues {
        VectorValues() : vals()
        { }

        void setComponent(int c, Value* val)
        {
            assert(c >= 0 && c < 4 && "Out of bounds component");
            vals[c] = val;
        }
        Value* getComponent(int c)
        {
            assert(c >= 0 && c < 4 && "Out of bounds component");
            assert(vals[c] && "Requesting non-existing component");
            return vals[c];
        }

        // {Value* x, Value* y, Value* z, Value* w}
        Value* vals[4];
    };

    class Scalarize : public FunctionPass {

    public:
        // Standard pass stuff
        static char ID;

        Scalarize() : FunctionPass(ID)
        {
            initializeScalarizePass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        // An instruction is valid post-scalarization iff it is fully scalar or it
        // is a gla_loadn
        bool isValid(const Instruction*);

        // Take an instruction that produces a vector, and scalarize it
        bool scalarize(Instruction*);
        bool scalarizePerComponent(Instruction*);
        bool scalarizeLoad(LoadInst*);
        bool scalarizeInputIntrinsic(IntrinsicInst*);
        bool scalarizeOutputIntrinsic(IntrinsicInst*);
        bool scalarizeIntrinsic(IntrinsicInst*);
        bool scalarizeExtract(ExtractElementInst*);
        bool scalarizeInsert(InsertElementInst*);
        bool scalarizeShuffleVector(ShuffleVectorInst*);
        bool scalarizeTextureIntrinsic(IntrinsicInst* intr);
        // ...

        // Helpers to make the actual multiple scalar calls, one per
        // component. Updates the given VectorValues's components with the new
        // Values.
        void makeScalarizedCalls(Function*, ArrayRef<Value*>, int numComponents, VectorValues&);

        void makePerComponentScalarizedCalls(Instruction*, ArrayRef<Value*>);
        void makePerComponentScalarizedCalls(Function*, ArrayRef<Value*>, int numComponents, VectorValues&);

        // Makes a scalar form of the given instruction: replaces the operands
        // and chooses a correct return type
        Instruction* createScalarInstruction(Instruction* inst, ArrayRef<Value*>);

        // Gather the specified components in the given values. Returns the
        // component if the given value is a vector, or the scalar itself.
        void gatherComponents(int component, ArrayRef<Value*> args, SmallVectorImpl<Value*>& componentArgs);

        // Get the assigned component for that value. If the value is a scalar,
        // returns the scalar. If it's a constant, returns that component. If
        // it's an instruction, returns the vectorValues of that instruction for
        // that component
        Value* getComponent(int component, Value*);

        // Used for assertion purposes. Whether we can get the component out with
        // a getComponent call
        bool canGetComponent(Value*);

        // Used for assertion purposes. Whether for every operand we can get
        // components with a getComponent call
        bool canGetComponentArgs(User*);

        // Delete the instruction in the deadList
        void dce();

        DenseMap<Value*, VectorValues> vectorVals;
        Module* module;
        IRBuilder<>* builder;

        const Type* intTy;
        const Type* floatTy;

        std::vector<Instruction*> deadList;

    };


} // end namespace

Value* Scalarize::getComponent(int component, Value* v)
{
    assert(canGetComponent(v) && "getComponent called on unhandled vector");

    if (v->getType()->isVectorTy()) {
        if (ConstantVector* c = dyn_cast<ConstantVector>(v)) {
            return c->getOperand(component);
        } else if (isa<ConstantAggregateZero>(v)) {
            return Constant::getNullValue(v->getType());
        } else if (isa<UndefValue>(v)) {
            return UndefValue::get(gla::GetBasicType(v));
        } else {
            return vectorVals[v].getComponent(component);
        }
    } else {
        return v;
    }
}

bool Scalarize::canGetComponent(Value* v)
{
    if (v->getType()->isVectorTy()) {
        if (isa<ConstantVector>(v) || isa<ConstantAggregateZero>(v) || isa<UndefValue>(v)) {
            return true;
        } else {
            assert(isa<Instruction>(v) && "Non-constant non-instuction?");
            return vectorVals.count(v);
        }
    } else {
        return true;
    }
}

bool Scalarize::canGetComponentArgs(User* u)
{
    for (User::op_iterator i = u->op_begin(), e = u->op_end(); i != e; ++i)
        if (! canGetComponent(*i))
            return false;

    return true;
}

void Scalarize::gatherComponents(int component, ArrayRef<Value*> args, SmallVectorImpl<Value*>& componentArgs)
{
    componentArgs.clear();
    for (ArrayRef<Value*>::iterator i = args.begin(), e = args.end(); i != e; ++i)
        componentArgs.push_back(getComponent(component, *i));
}

Instruction* Scalarize::createScalarInstruction(Instruction* inst, ArrayRef<Value*> args)
{
    unsigned op = inst->getOpcode();
    if (inst->isCast()) {
        assert(args.size() == 1 && "incorrect number of arguments for cast op");
        return CastInst::Create((Instruction::CastOps)op, args[0], gla::GetBasicType(inst));
    }

    if (inst->isBinaryOp()) {
        assert(args.size() == 2 && "incorrect number of arguments for binary op");
        return BinaryOperator::Create((Instruction::BinaryOps)op, args[0], args[1]);
    }
    // TODO: CmpInst, Select, per-component LunarGLASS intrinsics
    // TODO: PHInodes here?

    assert(! inst->isTerminator() && "Terminators are not per-component");
    // TODO: assert on Memoryinsts, Call, VAArg, Extract/Insert*, Shuffle,
    // non-cast unarys

    assert(! "Unknown instruction type");
    return 0;

}


void Scalarize::makeScalarizedCalls(Function* f, ArrayRef<Value*> args, int count, VectorValues& vVals)
{
    assert(count > 0 && count <= 4 && "invalid number of vector components");
    for (unsigned int i = 0; i < count; ++i) {
        Value* res;
        SmallVector<Value*, 8> callArgs(args.begin(), args.end());
        callArgs.push_back(ConstantInt::get(intTy, i));

        res = builder->CreateCall(f, callArgs.begin(), callArgs.end());
        vVals.setComponent(i, res);
    }
}

void Scalarize::makePerComponentScalarizedCalls(Function* f, ArrayRef<Value*> args, int count, VectorValues& vVals)
{
    assert(count > 0 && count <= 4 && "invalid number of vector components");

    for (unsigned int i = 0; i < count; ++i) {
        Value* res;

        // Set this component of each arg
        SmallVector<Value*, 8> callArgs(args.size(), 0);
        gatherComponents(i, args, callArgs);
        callArgs.push_back(ConstantInt::get(intTy, i));

        res = builder->CreateCall(f, callArgs.begin(), callArgs.end());
        vVals.setComponent(i, res);
    }
}

void Scalarize::makePerComponentScalarizedCalls(Instruction* inst, ArrayRef<Value*> args)
{
    int count = gla::GetComponentCount(inst);
    assert(count > 0 && count <= 4 && "invalid number of vector components");
    assert(inst->getNumOperands() <= args.size() && "not enough arguments passed for instruction");

    VectorValues& vVals = vectorVals[inst];

    for (unsigned int i = 0; i < count; ++i) {
        // Set this component of each arg
        SmallVector<Value*, 8> callArgs(args.size(), 0);
        gatherComponents(i, args, callArgs);

        Instruction* res = createScalarInstruction(inst, callArgs);

        vVals.setComponent(i, res);
        builder->Insert(res);
    }
}

bool Scalarize::isValid(const Instruction* inst)
{
    // The result
    if (inst->getType()->isVectorTy())
        return false;

    // gla loads are valid
    if (const IntrinsicInst* intr = dyn_cast<const IntrinsicInst>(inst))
        switch (intr->getIntrinsicID()) {
        case Intrinsic::gla_loadComponent:
        case Intrinsic::gla_fLoadComponent:
            return true;
        } // end of switch (intr->getIntrinsicID())

    // The arguments
    for (Instruction::const_op_iterator i = inst->op_begin(), e = inst->op_end(); i != e; ++i) {
        const Value* v = (*i);
        assert(v);
        if (v->getType()->isVectorTy())
            return false;
    }

    return true;
}

bool Scalarize::scalarize(Instruction* inst)
{
    if (isValid(inst))
        return false;

    assert(! vectorVals.count(inst) && "We've already scalarized this somehow?");
    assert((canGetComponentArgs(inst) || IsInputInstruction(inst)) &&
           "Scalarizing an op whose arguments haven't been scalarized ");
    builder->SetInsertPoint(inst);

    if (gla::IsPerComponentOp(inst))
        return scalarizePerComponent(inst);

    if (LoadInst* ld = dyn_cast<LoadInst>(inst))
        return scalarizeLoad(ld);

    if (IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst)) {
        if (IsInputInstruction(intr))
            return scalarizeInputIntrinsic(intr);
        else if (IsOutputInstruction(intr))
            return scalarizeOutputIntrinsic(intr);
        else if (IsTextureInstruction(intr))
            return scalarizeTextureIntrinsic(intr);
        else
            return scalarizeIntrinsic(intr);
    }

    if (ExtractElementInst* extr = dyn_cast<ExtractElementInst>(inst))
        return scalarizeExtract(extr);

    if (InsertElementInst* ins = dyn_cast<InsertElementInst>(inst))
        return scalarizeInsert(ins);

    if (ShuffleVectorInst* sv = dyn_cast<ShuffleVectorInst>(inst))
        return scalarizeShuffleVector(sv);

    return true;
}

bool Scalarize::scalarizeShuffleVector(ShuffleVectorInst* sv)
{
    //     %res = shuffleVector <n x ty> %foo, <n x ty> bar, <n x i32> <...>
    // ==> nothing (just make a new VectorValues with the new components)

    VectorValues& vVals = vectorVals[sv];

    ConstantVector* mask = dyn_cast<ConstantVector>(sv->getOperand(2));
    assert(mask && "Shuffles should have constant masks");

    int size = gla::GetComponentCount(sv);

    for (unsigned int i = 0; i < size; ++i) {
        int select = gla::GetConstantInt(mask->getOperand(i));
        Value* selectee;
        if (select < size) {
            selectee = sv->getOperand(0);
        } else {
            // Choose from the second operand
            select -= size;
            selectee = sv->getOperand(1);
        }

        vVals.setComponent(i, getComponent(select, selectee));
    }

    return true;
}

bool Scalarize::scalarizePerComponent(Instruction* inst)
{
    //     dst  = op <n x ty> %foo, <n x ty> %bar
    // ==> dstx = op ty %foox, ty %barx
    //     dsty = op ty %fooy, ty %bary
    //     ...

    SmallVector<Value*, 4> args(inst->op_begin(), inst->op_end());

    makePerComponentScalarizedCalls(inst, args);

    return true;
}

bool Scalarize::scalarizeLoad(LoadInst* ld)
{
    //     dst  = load ptr
    // ==> dstx = loadComponent ptr, 0
    //     dsty = loadComponent ptr, 1
    //     ...

    const Type* ty = ld->getType();

    const Type* underTy = gla::GetBasicType(ty);

    Intrinsic::ID intrID = underTy->isFloatTy() ? Intrinsic::gla_fLoadComponent
                                                : Intrinsic::gla_loadComponent;
    int count = gla::GetComponentCount(ty);

    VectorValues& vVals = vectorVals[ld];
    const Type* intrTys[2] = {underTy, ld->getOperand(0)->getType()};
    Function* newLoad = Intrinsic::getDeclaration(module, intrID, intrTys, 2);
    Value* arg = ld->getOperand(0);

    makeScalarizedCalls(newLoad, arg, count, vVals);

    return true;
}

bool Scalarize::scalarizeExtract(ExtractElementInst* extr)
{
    //     %res = extractelement <n X ty> %foo, %i
    // ==> nothing (just use %foo's %ith component instead of %res)

    if (! isa<Constant>(extr->getOperand(1))) {
        // TODO: Variably referenced components. Probably handle/emulate through
        // a series of selects.
        gla::UnsupportedFunctionality("Variably referenced vector components");
    }

    int component = gla::GetConstantInt(extr->getOperand(1));
    Value* v = getComponent(component, extr->getOperand(0));
    extr->replaceAllUsesWith(v);

    return true;
}

bool Scalarize::scalarizeInsert(InsertElementInst* ins)
{
    //     %res = insertValue <n x ty> %foo, %i
    // ==> nothing (just make a new VectorValues with the new component)

    if (! isa<Constant>(ins->getOperand(2))) {
        // TODO: Variably referenced components. Probably handle/emulate through
        // a series of selects.
        gla::UnsupportedFunctionality("Variably referenced vector components");
    }

    int component = gla::GetConstantInt(ins->getOperand(2));

    VectorValues& vVals = vectorVals[ins];
    for (unsigned int i = 0; i < gla::GetComponentCount(ins); ++i) {
        vVals.setComponent(i, i == component ? ins->getOperand(1)
                                             : getComponent(i, ins->getOperand(0)));
    }

    return true;
}

bool Scalarize::scalarizeOutputIntrinsic(IntrinsicInst* intr)
{
    //     writeData %location, %mask, %val
    // ==> writeComponent %location, %val, 0
    //     writeComponent %location, %val, 1
    //     ...

    Intrinsic::ID intrID;
    SmallVector<Value*, 5> args(1, intr->getOperand(0)); // Start out with the first arg
    int dataPos = 2; // Which operand index has the data in it, usually 2

    switch (intr->getIntrinsicID()) {
    case Intrinsic::gla_writeData:
        intrID = Intrinsic::gla_writeComponent;
        break;
    case Intrinsic::gla_fWriteData:
        intrID = Intrinsic::gla_fWriteComponent;
        break;

    case Intrinsic::gla_fWriteInterpolant:
        intrID = Intrinsic::gla_fWriteInterpolantComponent;
        args.push_back(intr->getOperand(2)); // Flags
        dataPos = 3;
        break;
    default:
        assert("Unknown output intrinsic");
    }

    args.push_back(intr->getOperand(dataPos)); // Data
    const Type* ty = gla::GetBasicType(intr->getOperand(dataPos));
    int count = gla::GetComponentCount(intr->getOperand(dataPos));

    // TODO: Take mask into account

    Function* newWrite = Intrinsic::getDeclaration(module, intrID, &ty, 1);
    makePerComponentScalarizedCalls(newWrite, args, count, vectorVals[intr]);

    // TODO: remove when runOnFunction's deadlist push is enabled
    deadList.push_back(intr);

    return true;
}

bool Scalarize::scalarizeInputIntrinsic(IntrinsicInst* intr)
{
    //     dst  = read %location, %mask, ...
    // ==> dstx = readComponent %location, ..., 0
    //     dsty = readComponent %location, ..., 1
    //     ...

    Intrinsic::ID intrID;
    const Type* underTy = gla::GetBasicType(intr->getType());
    int count = gla::GetComponentCount(intr);

    const Type* intrTys[2] = {underTy, 0};
    int numTys = 1;

    SmallVector<Value*, 5> args(1, intr->getOperand(0)); // Start out with the first arg

    switch (intr->getIntrinsicID()) {
    case Intrinsic::gla_readData:
        intrID = Intrinsic::gla_readComponent;
        break;
    case Intrinsic::gla_fReadData:
        intrID = Intrinsic::gla_fReadComponent;
        break;
    case Intrinsic::gla_fReadInterpolant:
        intrID = Intrinsic::gla_fReadInterpolantComponent;
        args.push_back(intr->getOperand(2)); // Flags
        break;

    case Intrinsic::gla_fReadInterpolantOffset:
        intrTys[1] = intr->getOperand(3)->getType();
        numTys = 2;
        args.push_back(intr->getOperand(2)); // Flags
        args.push_back(intr->getOperand(3)); // Offset
        break;

    default:
        assert(! "Unknown input intrinsic");

    } // end of switch (intr->getIntrinsicID())

    Function* f = Intrinsic::getDeclaration(module, intrID, intrTys, numTys);
    VectorValues& vVals = vectorVals[intr];

    makeScalarizedCalls(f, args, count, vVals);

    return true;
}

bool Scalarize::scalarizeTextureIntrinsic(IntrinsicInst* intr)
{
    // TODO: high-level comment goes here

    Intrinsic::ID intrID;
    const Type* underTy = gla::GetBasicType(intr);

    // Make the struct type for the return type.
    // TODO: update/fix if some intrinsics don't return four overloadable types
    // in a struct
    const Type* intrTys[10] = {underTy, underTy, underTy, underTy};
    int numTys;

    // Have the first three arguments (type, location, mask) be the same as the
    // original intrinsic.
    // TODO: revise for when this isn't the case, e.g. texelFetches.
    SmallVector<Value*, 32> args;
    args.push_back(intr->getOperand(0));
    args.push_back(intr->getOperand(1));
    args.push_back(intr->getOperand(2));

    switch (intr->getIntrinsicID()) {

    case Intrinsic::gla_fTextureSample: {
        Value* coords = intr->getOperand(3);

        // TODO: find the real width and do different calls and use different data

        // Sample2D
        intrID = Intrinsic::gla_fRTextureSample2;
        intrTys[4] = floatTy;
        intrTys[5] = floatTy;
        numTys = 6;
        args.push_back(getComponent(0, coords)); // x
        args.push_back(getComponent(1, coords)); // y
        break;
    }

    case Intrinsic::gla_textureSample:
    case Intrinsic::gla_textureSampleLodRefZ:
    case Intrinsic::gla_fTextureSampleLodRefZ:
    case Intrinsic::gla_textureSampleLodRefZOffset:
    case Intrinsic::gla_fTextureSampleLodRefZOffset:
    case Intrinsic::gla_textureSampleLodRefZOffsetGrad:
    case Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
    case Intrinsic::gla_texelFetchOffset:
    case Intrinsic::gla_fTexelFetchOffset:
    case Intrinsic::gla_texelGather:
    case Intrinsic::gla_fTexelGather:
    case Intrinsic::gla_texelGatherOffset:
    case Intrinsic::gla_fTexelGatherOffset:
    case Intrinsic::gla_texelGatherOffsets:
    case Intrinsic::gla_fTexelGatherOffsets:
        // TODO: handle
        gla::UnsupportedFunctionality("Unhandled tex op");

    default:
        // TODO: turn into assert when complete
        gla::UnsupportedFunctionality("Unhandled intrinsic");
    } // end of switch (intr->getIntrinsicID())


    Function* f = Intrinsic::getDeclaration(module, intrID, intrTys, numTys);
    Value* res = builder->CreateCall(f, args.begin(), args.end());

    // Create the extracts, and associate each component with the corresponding
    // extract
    VectorValues& vVals = vectorVals[intr];
    const Type* resTy = res->getType();

    // TODO: possibly revise below asserts depending on particular intrinsics/flags
    assert(resTy->isStructTy() && "Non-struct result of an SoA texture call");
    assert(resTy->getNumContainedTypes() == 4 && "Result doesn't have 4 values");
    for (unsigned int i = 0; i < resTy->getNumContainedTypes(); ++i) {
        Value* extr = builder->CreateExtractValue(res, i);
        vVals.setComponent(i, extr);
    }

    return true;
}

bool Scalarize::scalarizeIntrinsic(IntrinsicInst* intr)
{
    // TODO: do all the other intrinsics

    // TODO: identify must-decomponsed intrinsics.

    gla::UnsupportedFunctionality("Unhandled intrinsic");


    return false;
}


bool Scalarize::runOnFunction(Function& F)
{
    bool changed = false;
    module = F.getParent();
    intTy = IntegerType::get(module->getContext(), 32);
    floatTy = Type::getFloatTy(module->getContext());
    builder = new IRBuilder<>(module->getContext());

    typedef ReversePostOrderTraversal<Function*> RPOTType;
    RPOTType rpot(&F);
    for (RPOTType::rpo_iterator bbI = rpot.begin(), bbE = rpot.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = (*bbI)->begin(), instE = (*bbI)->end(); instI != instE; ++instI) {
            bool scalarized = scalarize(instI);
            if (scalarized) {
                changed = true;
                // TODO: uncomment when done
                // deadList.push_back
            }
        }
    }

    dce();

    delete builder;
    builder = 0;

    return changed;
}

void Scalarize::dce()
{
    for (std::vector<Instruction*>::iterator i = deadList.begin(), e = deadList.end(); i != e; ++i) {
        (*i)->dropAllReferences();
        (*i)->eraseFromParent();
    }
}

void Scalarize::getAnalysisUsage(AnalysisUsage& AU) const
{
}

void Scalarize::print(raw_ostream&, const Module*) const
{
    return;
}

char Scalarize::ID = 0;
INITIALIZE_PASS_BEGIN(Scalarize,
                      "scalarize",
                      "Scalarize the IR",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_END(Scalarize,
                    "scalarize",
                    "Scalarize the IR",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createScalarizePass()
{
    return new Scalarize();
}
