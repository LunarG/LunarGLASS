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

// LunarGLASS helpers
#include "LunarGLASSBottomIR.h"

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
    };

    class MultiInsertIntrinsic {
    public:
        // Construct the MultiInsertIntrinsic. If last runIt is true,
        // then also run after constructing.
        MultiInsertIntrinsic(Module* m, LLVMContext& c, BasicBlock& bb, InstVec& g, bool runIt=false)
                            : module(m)
                            , llvmcontext(c)
                            , basicblock(bb)
                            , group(g)
                            , intrinsic(NULL)
                            , writeMask(0)
                            , toReplace(NULL)
                            , originalSource(NULL)
        {
            // Initialize each element (for pre-c++0x compliance)
            for (int i = 0; i < 4; ++i) {
                values[i] = NULL;
                offsets[i] = -1;
            }
        }

        // Print the multi-insert intrinsic to stderr
        void dump() const;

        // Run this class's methods to make and insert the intrinsic
        void run()
        {
            buildFromGroup();
            makeIntrinsic();
            insertIntrinsic();
        }

        // Get the intrinsic instruction
        Instruction* getIntrinsic()
        {
            return intrinsic;
        }

    private:
        Module* module;
        LLVMContext& llvmcontext;
        BasicBlock& basicblock;
        InstVec& group;

        Instruction* intrinsic;

        int writeMask;
        Instruction* toReplace;
        Value* originalSource;
        Value* values[4];
        int offsets[4];

        // Build this class's members up.
        void buildFromGroup();

        // Make the intrinsic
        void makeIntrinsic();

        // Insert the instruction in, replacing uses for toReplace
        // with the new instruction
        void insertIntrinsic();

        // Unimplemented unwanted copy and assignment
        MultiInsertIntrinsic(MultiInsertIntrinsic&);
        MultiInsertIntrinsic& operator=(const MultiInsertIntrinsic&);

    };
} // end namespace

// Helpers
// Predicate for whether the instruction is an insert
static bool IsInsertElement(Instruction& i)
{
    return strcmp(i.getOpcodeName(), "insertelement") == 0;
}

// Predicate for whether the instruction is an extract
static bool IsExtract(Instruction& i)
{
    return strcmp(i.getOpcodeName(), "extractelement") == 0;
}

// Given a value, if it's an extract return its offset
// Return -1 if not an extract
static int GetOffset(Value* v)
{
    int offset = -1;

    // If the operand is an extract instruction, then get the offset
    if (Instruction* inst = dyn_cast<Instruction>(v)) {
        if (IsExtract(*inst)) {
            offset = gla::GetConstantInt(inst->getOperand(1));
        }
    }

    return offset;
}

// If the value is an extract instruction, then get the vector
// extraced from, else return the given value
static Value* GetExtractFrom(Value* v)
{
    Value* ret = v;
    if (Instruction* inst = dyn_cast<Instruction>(v)) {
        if (IsExtract(*inst)) {
            ret = (inst->getOperand(0));
        }
    }

    return ret;
}

static void PrintBlock(BasicBlock& bb)
{
    errs() << "processing basic block " << bb.getName() << "\n" << bb;
}

static void PrintCandidates(InstVec& v)
{
    errs() << "\nThis block's candidates for intrinsic substitution: \n";
    for (InstVec::iterator i = v.begin(), e = v.end(); i != e; ++i) {
        errs() << **i << "\n";
    }
}

static void PrintGroups(GroupVec& groupVec)
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

// Gather all insert instructions together, putting them in
// the InstVec
static void GatherInserts(BasicBlock::InstListType& instList, InstVec& result)
{
    for (reverse_iterator i = instList.rbegin(), e = instList.rend(); i != e; ++i) {
        if (IsInsertElement(*i)) {
            result.push_back(&*i);
        }
    }
}

// Add the entire insert chain specified by the value to the
// provided set and vector.
static void AddInsertChain(Value* v, InstSet& s, InstVec& vec)
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

// Group instructions into the individual multiInserts,
// putting them in the GroupVec
static void GroupInserts(InstVec& vec, GroupVec& result)
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

// MultiInsertIntrinsic implementation

void MultiInsertIntrinsic::buildFromGroup()
{
    // The instruction we're replacing is at the front
    toReplace = group.front();

    // Find the orignal insert destination. It will be the last one
    // listed, due to the groups being in depth-first order
    originalSource = group.back()->getOperand(0);

    // For each member of the group, set the relevant fields.
    for (InstVec::iterator instI = group.begin(), instE = group.end(); instI != instE; ++instI) {
        // Only operate on inserts
        assert(IsInsertElement(**instI) && "buildFromGroup supposed to only operate on inserts");

        // The source operand
        Value* src = (*instI)->getOperand(1);

        // Find the access offset of the underlying extract intrinsic
        int offset = GetOffset(src);

        // The value the extract statement is extracting from
        // If it isn't an extract statement, make it be the scalar
        Value* extractFrom = GetExtractFrom(src);

        // Match up the data with the corresponding field specified in
        // the insert
        int maskOffset = gla::GetConstantInt((*instI)->getOperand(2));
        assert(maskOffset <= 4 && " Unknown access mask found");

        offsets[maskOffset] = offset;
        values[maskOffset] = extractFrom;

        // Update the mask
        writeMask |= (1 << maskOffset);
    }
}

void MultiInsertIntrinsic::makeIntrinsic() {
    // Set up types array
    int typesCount = 6;
    const llvm::Type* intrinsicTypes[6] = {0};

    // Default type to choose
    const llvm::Type* defaultType;

    // Determine if it's a fWriteMask or writeMask, and set types accordingly
    Intrinsic::ID intrinsicID;
    unsigned vecCount = 4;
    switch (toReplace->getType()->getContainedType(0)->getTypeID()) {
    case Type::FloatTyID:
        defaultType = Type::getFloatTy(llvmcontext);
        intrinsicID = Intrinsic::gla_fMultiInsert;
        intrinsicTypes[0] = VectorType::get(defaultType, vecCount);
        break;
    case Type::IntegerTyID:
        if (toReplace->getType()->getContainedType(0)->isIntegerTy(1))
            defaultType = Type::getInt1Ty(llvmcontext);
        else
            defaultType = Type::getInt32Ty(llvmcontext);
        intrinsicID = Intrinsic::gla_multiInsert;
        intrinsicTypes[0] = VectorType::get(defaultType, vecCount);
        break;
    default:
        assert(!"Unknown multiInsert intrinsic type");
    }

    // Set up each of the operand types
    intrinsicTypes[1] = originalSource ? originalSource->getType() : defaultType;
    intrinsicTypes[2] = values[0]      ? values[0]->getType()      : defaultType;
    intrinsicTypes[3] = values[1]      ? values[1]->getType()      : defaultType;
    intrinsicTypes[4] = values[2]      ? values[2]->getType()      : defaultType;
    intrinsicTypes[5] = values[3]      ? values[3]->getType()      : defaultType;

    // Get the function declaration for this intrinsic
    Value* callee = llvm::Intrinsic::getDeclaration(module, intrinsicID, intrinsicTypes, typesCount);

    Value* mask    = ConstantInt::get(Type::getInt32Ty(llvmcontext), writeMask);
    Value* xOffset = ConstantInt::get(Type::getInt32Ty(llvmcontext), offsets[0]);
    Value* yOffset = ConstantInt::get(Type::getInt32Ty(llvmcontext), offsets[1]);
    Value* zOffset = ConstantInt::get(Type::getInt32Ty(llvmcontext), offsets[2]);
    Value* wOffset = ConstantInt::get(Type::getInt32Ty(llvmcontext), offsets[3]);

    Value* xV = values[0] ? values[0] : Constant::getNullValue(defaultType);
    Value* yV = values[1] ? values[1] : Constant::getNullValue(defaultType);
    Value* zV = values[2] ? values[2] : Constant::getNullValue(defaultType);
    Value* wV = values[3] ? values[3] : Constant::getNullValue(defaultType);

    Value* args[] = { originalSource, mask, xV, xOffset, yV, yOffset, zV, zOffset, wV, wOffset };

    // Create it
    intrinsic = CallInst::Create(callee, args, args+10);
}

void MultiInsertIntrinsic::insertIntrinsic()
{
    BasicBlock::InstListType& instList = basicblock.getInstList();
    for (BasicBlock::InstListType::iterator instI = instList.begin(), instE = instList.end(); instI != instE; ++instI)
        if (instI->isIdenticalTo(toReplace)) {
            assert(CoalesceInserts::IsInsertElement(*instI));

            instList.insertAfter(instI, intrinsic);
            instI->replaceAllUsesWith(intrinsic);
        }
}

void MultiInsertIntrinsic::dump() const {
    errs() << "\nMultiInsertIntrinsic for" << toReplace << ":\n";
    errs() << "  Write mask and offset:\n";
    errs() << "    " << writeMask;

    errs() << " <";
    for (int i = 0; i < 4; ++i) {
        errs() << "|  " << offsets[i] << "  ";
    }

    errs() << "\n       <";
    for (int i = 0; i < 4; ++i) {
        errs() << "|  ";
        if (values[i])
            errs() << *values[i];
        else
            errs() << "null";
        errs() << "  ";
    }

    errs() << "\n";
    errs() << "  Original insert destination: " << *originalSource << "\n";

    errs() << "\n";
    errs() << "Instrinsic: " << intrinsic;
}

// CoalesceInserts implementation

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
            MultiInsertIntrinsic mii(M, C, *bb, **gI);
            mii.run();

            VERBOSE(mii.dump());

            wasModified = true;

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


