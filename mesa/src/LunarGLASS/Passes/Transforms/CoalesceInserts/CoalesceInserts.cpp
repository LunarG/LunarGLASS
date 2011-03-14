//===- CoalesceInserts.cpp - Coalesce insert/extracts into multiInserts -----===//
//
//                     The LLVM Compiler Infrastructure
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
// Coalesce insert/extracts into multiInserts
//
//===----------------------------------------------------------------------===//


#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/InstIterator.h"
#include "CoalesceInserts.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/Type.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Constants.h"
#include "llvm/Intrinsics.h"
#include "llvm/Instructions.h"

#define VERBOSE(exp) if (VerboseP) exp;

using namespace llvm;

namespace {
    // Whether we should print out verbose stuff as the pass runs
    const bool VerboseP = false;

    // Initial sizes for our data structures
    const int NumTypicalInserts = 32;
    const int NumTypicalIntrinsics = 8;

    // Typedefs
    typedef iplist<Instruction>::reverse_iterator reverse_iterator;
    typedef SmallVector<Instruction*,NumTypicalInserts> InstVec;
    typedef SmallSet<Instruction*, NumTypicalInserts> InstSet;
    typedef SmallVector<InstVec*, NumTypicalIntrinsics> GroupVec;

    // Coalesce Inserts/Extracts Pass
    class CoalesceInserts : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;
        CoalesceInserts() : FunctionPass(ID) {}
        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

        // Printing class methods
        static void PrintBlock(BasicBlock&);
        static void PrintCandidates(InstVec&);
        static void PrintGroups(GroupVec&);
        static void PrintMultiInsertIntrinsic(Instruction&);

        // Gather all insert instructions together, putting them in
        // the InstVec
        static void GatherInserts(BasicBlock::InstListType&, InstVec&);

        // Group instructions into the individual multiInserts,
        // putting them in the GroupVec
        static void GroupInserts(InstVec&, GroupVec&);

        // Add the entire insert chain specified by the value to the
        // provided set and vector.
        static void AddInsertChain(Value*, InstSet&, InstVec&);

        // Predicate for whether the instruction is an insert
        static bool IsInsertElement(Instruction&);

        // Predicate for whether the instruction is an extract
        static bool IsExtract(Instruction&);

        // Extract an int from the given value. Assumes that value is
        // a ConstantInt
        static int GetConstantInt(Value*);

        // Given a value, if it's an extract return its offset
        // Return -1 if not an extract
        static int GetOffset(Value*);

        // If the value is an extract instruction, then get the vector
        // extraced from, else return the given value
        static Value* GetExtractFrom(Value*);

    };
} // End  namespace

// Helpers
namespace  {
    // Struct representing the eventual intrinsic
    struct MultiInsertOp {
        int mask;
        Value* xV;
        Value* yV;
        Value* zV;
        Value* wV;
        int xOffset;
        int yOffset;
        int zOffset;
        int wOffset;
        Value* inst;
        Value* original;
    };

    // MultiInsertOp initialize
    void InitSOp(MultiInsertOp& sop, InstVec& vec)
    {
        sop.xOffset = -1;
        sop.yOffset = -1;
        sop.zOffset = -1;
        sop.wOffset = -1;

        sop.xV = NULL;
        sop.yV = NULL;
        sop.zV = NULL;
        sop.wV = NULL;

        sop.mask = 0;
        sop.inst = *(vec.begin());
        sop.original = NULL;
    }

    // Print out a textual representation of the multiInsert operand
    void PrintOperand(Value* v)
    {
        if (v)
            errs() << "|" << *v << " ";
        else
            errs() << "|  null  ";
    }

    // Print out the struct representing a multiInsert
    void PrintMultiInsertOp(MultiInsertOp& sop)
    {
        errs() << "\nMultiInsertOp for" << *sop.inst << ":\n";
        errs() << "  Write mask and offset:\n";
        errs() << "    " << sop.mask;
        errs() << " <| " << sop.xOffset << "  |  " << sop.yOffset << "  |  " << sop.zOffset << "  |  " << sop.wOffset;
        errs() << "\n       <";
        PrintOperand(sop.xV);
        PrintOperand(sop.yV);
        PrintOperand(sop.zV);
        PrintOperand(sop.wV);
        errs() << "\n";
        errs() << "  Original insert destination: " << *sop.original << "\n";
    }

    // Build up the struct representing a multiInsert
    void BuildMultiInsertOp(InstVec& vec, MultiInsertOp& sop)
    {
        // Initialize
        InitSOp(sop, vec);

        // Find the orignal insert destination. It will be the last one
        // listed, due to the groups being in depth-first order
        sop.original = vec.back()->getOperand(0);

        // For each member of the group, set the relevant fields.
        for (InstVec::iterator instI = vec.begin(), instE = vec.end(); instI != instE; ++instI) {
            // Only operate on inserts at the top
            assert(CoalesceInserts::IsInsertElement(**instI));

            // The source operand
            Value* src = (*instI)->getOperand(1);

            // Find the access offset of the underlying extract intrinsic
            int offset = CoalesceInserts::GetOffset(src);

            // The value the extract statement is extracting from
            // If it isn't an extract statement, make it be the scalar
            Value* extractFrom = CoalesceInserts::GetExtractFrom(src);

            // Match up the data with the corresponding field specified in
            // the insert
            int maskOffset = CoalesceInserts::GetConstantInt((*instI)->getOperand(2));
            switch (maskOffset) {
            case 0:
                sop.xOffset = offset;
                sop.xV = extractFrom;
                break;
            case 1:
                sop.yOffset = offset;
                sop.yV = extractFrom;
                break;
            case 2:
                sop.zOffset = offset;
                sop.zV = extractFrom;
                break;
            case 3:
                sop.wOffset = offset;
                sop.wV = extractFrom;
                break;
            default:
                assert(!" Unknown access mask found");
            }

            // Update the mask
            sop.mask |= (1 << maskOffset);
        }
    }

    // Create an intrinsic
    Instruction* MakeMultiInsertIntrinsic(MultiInsertOp& sop, Module* M, LLVMContext& C)
    {
        // Set up types array
        const llvm::Type* intrinsicTypes[6] = {0};

        // Default type to choose
        const llvm::Type* defaultType;

        // Determine if it's a fWriteMask or writeMask, and set types accordingly
        Intrinsic::ID intrinsicID;
        unsigned vecCount = 4;
        switch (sop.inst->getType()->getContainedType(0)->getTypeID()) {
        case Type::FloatTyID:
            defaultType = Type::getFloatTy(C);
            intrinsicID = Intrinsic::gla_fMultiInsert;
            intrinsicTypes[0] = VectorType::get(defaultType, vecCount);

            break;
        case Type::IntegerTyID:
            if (sop.inst->getType()->getContainedType(0)->isIntegerTy(1))
                defaultType = Type::getInt1Ty(C);
            else
                defaultType = Type::getInt32Ty(C);

            intrinsicID = Intrinsic::gla_multiInsert;
            intrinsicTypes[0] = VectorType::get(defaultType, vecCount);
            break;
        default:
            assert(!"Unknown multiInsert intrinsic type");
        }

        // Set up each of the operand types
        intrinsicTypes[1] = sop.original ? sop.original->getType() : defaultType;
        intrinsicTypes[2] = sop.xV       ? sop.xV->getType()       : defaultType;
        intrinsicTypes[3] = sop.yV       ? sop.yV->getType()       : defaultType;
        intrinsicTypes[4] = sop.zV       ? sop.zV->getType()       : defaultType;
        intrinsicTypes[5] = sop.wV       ? sop.wV->getType()       : defaultType;

        int typesCount = 6;

        // Get the function declaration for this intrinsic
        Value* callee = llvm::Intrinsic::getDeclaration(M, intrinsicID, intrinsicTypes, typesCount);

        Value* mask    = ConstantInt::get(Type::getInt32Ty(C), sop.mask);
        Value* xOffset = ConstantInt::get(Type::getInt32Ty(C), sop.xOffset);
        Value* yOffset = ConstantInt::get(Type::getInt32Ty(C), sop.yOffset);
        Value* zOffset = ConstantInt::get(Type::getInt32Ty(C), sop.zOffset);
        Value* wOffset = ConstantInt::get(Type::getInt32Ty(C), sop.wOffset);

        Value* xV = sop.xV ? sop.xV : Constant::getNullValue(defaultType);
        Value* yV = sop.yV ? sop.yV : Constant::getNullValue(defaultType);
        Value* zV = sop.zV ? sop.zV : Constant::getNullValue(defaultType);
        Value* wV = sop.wV ? sop.wV : Constant::getNullValue(defaultType);

        Value* args[] = { sop.original, mask, xV, xOffset, yV, yOffset, zV, zOffset, wV, wOffset };

        return CallInst::Create(callee, args, args+10);
    }

    // Insert the new instruction
    void InsertMultiInsertIntrinsic(InstVec& vec, Instruction* newInst, BasicBlock& bb)
    {
        BasicBlock::InstListType& instList = bb.getInstList();
        for (BasicBlock::InstListType::iterator instI = instList.begin(), instE = instList.end(); instI != instE; ++instI) {
            if (instI->isIdenticalTo(*vec.begin())) {
                assert(CoalesceInserts::IsInsertElement(*instI));

                instList.insertAfter(instI, newInst);
                instI->replaceAllUsesWith(newInst);
            }
        }
    }

} // End  namespace

bool CoalesceInserts::IsInsertElement(Instruction& i)
{
    return strcmp(i.getOpcodeName(), "insertelement") == 0;
}

bool CoalesceInserts::IsExtract(Instruction& i)
{
    return strcmp(i.getOpcodeName(), "extractelement") == 0;
}

int CoalesceInserts::GetConstantInt(Value* val)
{
    ConstantInt* c = dyn_cast<ConstantInt>(val);
    assert(c);

    return c->getSExtValue();
}

int CoalesceInserts::GetOffset(Value* v)
{
    int offset = -1;

    // If the operand is an extract instruction, then get the offset
    if (Instruction* inst = dyn_cast<Instruction>(v)) {
        if (IsExtract(*inst)) {
            offset = GetConstantInt(inst->getOperand(1));
        }
    }

    return offset;
}

Value* CoalesceInserts::GetExtractFrom(Value* v)
{
    Value* ret = v;
    if (Instruction* inst = dyn_cast<Instruction>(v)) {
        if (IsExtract(*inst)) {
            ret = (inst->getOperand(0));
        }
    }

    return ret;
}

void CoalesceInserts::PrintMultiInsertIntrinsic(Instruction& inst)
{
    errs() << "MultiInsert intrinsic: ";
    errs() << inst << "\n";
    errs() << "\n";
}

void CoalesceInserts::PrintBlock(BasicBlock& bb)
{
    errs() << "processing basic block " << bb.getName() << "\n" << bb;
}

void CoalesceInserts::PrintCandidates(InstVec& v)
{
    errs() << "\nThis block's candidates for intrinsic substitution: \n";
    for (InstVec::iterator i = v.begin(), e = v.end(); i != e; ++i) {
        errs() << **i << "\n";
    }
}

void CoalesceInserts::PrintGroups(GroupVec& groupVec)
{
    errs() << "\nThis block's groups: \n";
    int i = 0;
    for (GroupVec::iterator gI = groupVec.begin(), gE = groupVec.end(); gI != gE; ++gI) {
        ++i;
        errs() << "Group " << i << ":";
        for (InstVec::iterator instI = (*gI)->begin(), instE = (*gI)->end(); instI != instE; ++instI) {
            if (instI == ((*gI)->begin()))
                errs() << **instI;
            else
                errs() <<  "   <|> " << **instI;
        }
        errs() << "\n";
    }

}

void CoalesceInserts::GatherInserts(BasicBlock::InstListType& instList, InstVec& result)
{
    for (reverse_iterator i = instList.rbegin(), e = instList.rend(); i != e; ++i) {
        if (IsInsertElement(*i)) {
            result.push_back(&*i);
        }
    }
}

void CoalesceInserts::AddInsertChain(Value* v, InstSet& s, InstVec& vec)
{
    // If it's an instruction and an insert, put it into s and vec,
    // and continue recursively traversing the chain
    if (Instruction* inst = dyn_cast<Instruction>(v)) {
        if (IsInsertElement(*inst)) {
            s.insert(inst);
            vec.push_back(inst);

            AddInsertChain(inst->getOperand(0), s, vec);
        }
    }

    // Base case: not a candidate instruction
    return;
}

void CoalesceInserts::GroupInserts(InstVec& vec, GroupVec& result)
{
    InstSet instSet;
    for (InstVec::iterator i = vec.begin(), e = vec.end(); i != e; ++i) {

        // Convert to an instruction (should not fail)
        Instruction* inst = dyn_cast<Instruction>(&**i);
        assert(inst);

        // If we've already seen it, continue
        if (instSet.count(inst)) {
            continue;
        }

        // Else this is a new group

        // TODO: find a more RAII handling of this
        InstVec* newGroup = new InstVec();

        AddInsertChain(inst, instSet, *newGroup);
        result.push_back(newGroup);
    }
}

bool CoalesceInserts::runOnFunction(Function& F)
{
    Module* M = F.getParent();
    LLVMContext& C = F.getContext();

    bool wasModified = false;

    // For each Basic Block
    for (Function::iterator bb = F.begin(), ebb = F.end(); bb != ebb; ++bb) {

        VERBOSE(PrintBlock(*bb));

        // Gather the candidate instructions
        InstVec v;
        GatherInserts(bb->getInstList(), v);
        VERBOSE(PrintCandidates(v));

        // Group them
        GroupVec groupVec;
        GroupInserts(v, groupVec);
        VERBOSE(PrintGroups(groupVec));

        // For each group, make a MultiInsertOp
        for (GroupVec::iterator gI = groupVec.begin(), gE = groupVec.end(); gI != gE; ++gI) {
            MultiInsertOp sop;

            BuildMultiInsertOp(**gI, sop);

            VERBOSE(PrintMultiInsertOp(sop));

            Instruction* inst = MakeMultiInsertIntrinsic(sop, M, C);

            InsertMultiInsertIntrinsic(**gI, inst, *bb);

            wasModified = true;

            VERBOSE(PrintMultiInsertIntrinsic(*inst));

            // We're done with the group, so delete it
            // TODO: find a more RAII handling of this
            delete *gI;
        }

        // VERBOSE(PrintBlock(*bb));
    }

    return wasModified;
}


void CoalesceInserts::getAnalysisUsage(AnalysisUsage& AU) const
{
    return;
}

void CoalesceInserts::print(raw_ostream &out, const Module* M) const
{
    return;
}

// Rest is pass registration

char CoalesceInserts::ID = 0;
INITIALIZE_PASS(CoalesceInserts,
                "coalesce-inserts",
                "Construct multiInserts out of multiple insert/extracts",
                true,   // Whether it preserves the CFG
                false); // Whether it is an analysis pass


FunctionPass* llvm::createCoalesceInsertsPass()
{
    return new CoalesceInserts();
}


