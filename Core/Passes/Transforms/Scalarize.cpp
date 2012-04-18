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
        bool scalarizeTextureIntrinsic(IntrinsicInst*);
        bool scalarizePHI(PHINode*);
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

        // List of vector phis that were not completely scalarized because some
        // of their operands hadn't before been visited (i.e. loop variant
        // variables)
        SmallVector<PHINode*, 16> incompletePhis;
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
    if (PHINode* phi = dyn_cast<PHINode>(u)) {
        for (unsigned int i = 0; i < phi->getNumIncomingValues(); ++i)
            if (! canGetComponent(phi->getIncomingValue(i)))
                return false;
    } else {
        for (User::op_iterator i = u->op_begin(), e = u->op_end(); i != e; ++i)
            if (! canGetComponent(*i))
                return false;
    }
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
    // TODO: Refine the below into one large switch

    unsigned op = inst->getOpcode();
    if (inst->isCast()) {
        assert(args.size() == 1 && "incorrect number of arguments for cast op");
        return CastInst::Create((Instruction::CastOps)op, args[0], gla::GetBasicType(inst));
    }

    if (inst->isBinaryOp()) {
        assert(args.size() == 2 && "incorrect number of arguments for binary op");
        return BinaryOperator::Create((Instruction::BinaryOps)op, args[0], args[1]);
    }

    if (isa<PHINode>(inst)) {
        PHINode* res = PHINode::Create(gla::GetBasicType(inst));
        assert(args.size() % 2 == 0 && "Odd number of arguments for a PHI");

        // Loop over pairs of operands: [Value*, BasicBlock*]
        for (unsigned int i = 0; i < args.size(); i += 2) {
            BasicBlock* bb = dyn_cast<BasicBlock>(args[i+1]);
            assert(bb && "Non-basic block incoming block?");
            res->addIncoming(args[i], bb);
        }

        return res;
    }

    if (CmpInst* cmpInst = dyn_cast<CmpInst>(inst)) {
        assert(args.size() == 2 && "incorrect number of arguments for comparison");
        return CmpInst::Create(cmpInst->getOpcode(), cmpInst->getPredicate(), args[0], args[1]);
    }

    if (isa<SelectInst>(inst)) {
        assert(args.size() == 3 && "incorrect number of arguments for select");
        return SelectInst::Create(args[0], args[1], args[2]);
    }

    if (IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst)) {
        if (! gla::IsPerComponentOp(inst))
            gla::UnsupportedFunctionality("Scalarize instruction on a non-per-component intrinsic");

        // TODO: Assumption is that all per-component intrinsics have all their
        // arguments be overloadable. Need to find some way to assert on this
        // assumption. This is due to how getDeclaration operates; it only takes
        // a list of types that fit overloadable slots.
        SmallVector<const Type*, 8> tys(1, gla::GetBasicType(inst->getType()));
        // Call instructions have the decl as a last argument, so skip it
        for (ArrayRef<Value*>::iterator i = args.begin(), e = args.end() - 1; i != e; ++i) {
            tys.push_back(gla::GetBasicType((*i)->getType()));
        }

        Function* f = Intrinsic::getDeclaration(module, intr->getIntrinsicID(), &tys.front(), tys.size());
        return CallInst::Create(f, args.begin(), args.end()-1);
    }

    gla::UnsupportedFunctionality("Currently unsupported instruction: ", inst->getOpcode(),
                                  inst->getOpcodeName());
    return 0;

}


void Scalarize::makeScalarizedCalls(Function* f, ArrayRef<Value*> args, int count, VectorValues& vVals)
{
    assert(count > 0 && count <= 4 && "invalid number of vector components");
    for (int i = 0; i < count; ++i) {
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

    for (int i = 0; i < count; ++i) {
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
    assert((inst->getNumOperands() == args.size() || isa<PHINode>(inst))
           && "not enough arguments passed for instruction");

    VectorValues& vVals = vectorVals[inst];

    for (int i = 0; i < count; ++i) {
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
    assert((canGetComponentArgs(inst) || IsInputInstruction(inst) || isa<PHINode>(inst)) &&
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

    if (PHINode* phi = dyn_cast<PHINode>(inst))
        return scalarizePHI(phi);

    if (isa<ExtractValueInst>(inst) || isa<InsertValueInst>(inst))
        // TODO: need to come up with a struct/array model for scalarization
        gla::UnsupportedFunctionality("Scalarizing struct/array ops");

    if (isa<StoreInst>(inst) || isa<GetElementPtrInst>(inst))
        // TODO: need to come up with a memory/struct/array model for scalarization
        gla::UnsupportedFunctionality("Scalarizing stores/geps");

    gla::UnsupportedFunctionality("Currently unhandled instruction ", inst->getOpcode(), inst->getOpcodeName());
    return false;
}

bool Scalarize::scalarizeShuffleVector(ShuffleVectorInst* sv)
{
    //     %res = shuffleVector <n x ty> %foo, <n x ty> bar, <n x i32> <...>
    // ==> nothing (just make a new VectorValues with the new components)

    VectorValues& vVals = vectorVals[sv];

    ConstantVector* mask = dyn_cast<ConstantVector>(sv->getOperand(2));
    assert(mask && "Shuffles should have constant masks");

    int size = gla::GetComponentCount(sv);

    for (int i = 0; i < size; ++i) {
        // If the mask's selection is undef, that means a "don't care" value, so
        // just assign undef.
        Value* selectVal = mask->getOperand(i);
        if (isa<UndefValue>(selectVal)) {
            vVals.setComponent(i, UndefValue::get(gla::GetBasicType(sv->getOperand(0))));
            continue;
        }

        // Otherwise look up the corresponding component from the correct
        // source.
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

bool Scalarize::scalarizePHI(PHINode* phi)
{
    //     dst = phi <n x ty> [ %foo, %bb1 ], [ %bar, %bb2], ...
    // ==> dstx = phi ty [ %foox, %bb1 ], [ %barx, %bb2], ...
    //     dsty = phi ty [ %fooy, %bb1 ], [ %bary, %bb2], ...

    // If the scalar values are all known up-front, then just make the full
    // phinode now. If they are not yet known (phinode for a loop variant
    // variable), then deferr the arguments until later

    if (canGetComponentArgs(phi)) {
        SmallVector<Value*, 8> args(phi->op_begin(), phi->op_end());

        makePerComponentScalarizedCalls(phi, args);
    } else {
        makePerComponentScalarizedCalls(phi, ArrayRef<Value*>());
        incompletePhis.push_back(phi);
    }


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
    for (int i = 0; i < gla::GetComponentCount(ins); ++i) {
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
        assert(0 && "Unknown output intrinsic");
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
    int numTys = 4;

    // Have the first three arguments (type, location, mask) be the same as the
    // original intrinsic.
    // TODO: revise for when this isn't the case, e.g. texelFetches.
    SmallVector<Value*, 32> args;
    args.push_back(intr->getOperand(0));
    args.push_back(intr->getOperand(1));
    args.push_back(intr->getOperand(2));

    Value* coords = intr->getOperand(3);

    int count = gla::GetComponentCount(coords);

    switch (intr->getIntrinsicID()) {

    case Intrinsic::gla_fTextureSample:

        // basic sample
        switch (count) {
        case 1:  intrID = Intrinsic::gla_fRTextureSample1;  break;
        case 2:  intrID = Intrinsic::gla_fRTextureSample2;  break;
        case 3:  intrID = Intrinsic::gla_fRTextureSample3;  break;
        case 4:  intrID = Intrinsic::gla_fRTextureSample4;  break;
        default: assert(0);
        }

        // one type for each coord
        for (int i = 0; i < count; ++i) {
            args.push_back(getComponent(i, coords));
            intrTys[numTys] = floatTy;
            numTys++;
        }

        break;

    case Intrinsic::gla_fTextureSampleLodRefZ:

        // basic shadow
        switch (count) {
        case 1:  intrID = Intrinsic::gla_fRTextureSampleLodRefZ1;  break;
        case 2:  intrID = Intrinsic::gla_fRTextureSampleLodRefZ2;  break;
        case 3:  intrID = Intrinsic::gla_fRTextureSampleLodRefZ3;  break;
        case 4:  intrID = Intrinsic::gla_fRTextureSampleLodRefZ4;  break;
        default: assert(0);
        }

        // one type for each coord
        for (int i = 0; i < count; ++i) {
            args.push_back(getComponent(i, coords));
            intrTys[numTys] = floatTy;
            numTys++;
        }

        // lod
        args.push_back(intr->getOperand(4));
        intrTys[numTys] = floatTy;
        numTys++;

        // refZ
        args.push_back(intr->getOperand(5));
        intrTys[numTys] = floatTy;
        numTys++;

        break;


    case Intrinsic::gla_textureSample:
    case Intrinsic::gla_textureSampleLodRefZ:
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
        gla::UnsupportedFunctionality("Unhandled tex op: ", intr->getOpcode(),
                                      intr->getCalledFunction()->getNameStr().c_str());

    default:
        // TODO: turn into assert when complete
        gla::UnsupportedFunctionality("Unhandled intrinsic: ", intr->getOpcode(),
                                      intr->getCalledFunction()->getNameStr().c_str());
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

    gla::UnsupportedFunctionality("Unhandled intrinsic: ", intr->getOpcode(),
                                  intr->getCalledFunction()->getNameStr().c_str());

    return true;
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

    // Fill in the incomplete phis
    for (SmallVectorImpl<PHINode*>::iterator phiI = incompletePhis.begin(), phiE = incompletePhis.end();
         phiI != phiE; ++phiI) {
        assert(canGetComponentArgs(*phiI) && "Phi's operands never scalarized");

        // Fill in each component of this phi
        VectorValues& vVals = vectorVals[*phiI];
        for (int c = 0; c < gla::GetComponentCount(*phiI); ++c) {
            PHINode* compPhi = dyn_cast<PHINode>(vVals.getComponent(c));
            assert(compPhi && "Vector phi got scalarized to non-phis?");

            // Loop over pairs of operands: [Value*, BasicBlock*]
            for (unsigned int i = 0; i < (*phiI)->getNumOperands(); i += 2) {
                BasicBlock* bb = dyn_cast<BasicBlock>((*phiI)->getOperand(i+1));
                assert(bb && "Non-basic block incoming block?");
                compPhi->addIncoming(getComponent(c, (*phiI)->getOperand(i)), bb);
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
