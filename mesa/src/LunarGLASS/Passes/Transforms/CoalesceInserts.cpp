//===- CoalesceInserts.cpp - Coalesce insert/extracts into multiInserts -----===//
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
// Coalesce insert/extracts into multiInserts. This pass works best if
// preceeded by instcombine and proceeded by adce.
//
//===----------------------------------------------------------------------===//


#include "llvm/ADT/ilist.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Type.h"

#include "Transforms.h"

// LunarGLASS helpers
#include "LunarGLASSLlvmInterface.h"

#include <utility>

#define VERBOSE(exp) if (VerboseP) exp;

using namespace llvm;
using namespace gla_llvm;

namespace {
    // Whether we should print out verbose stuff as the pass runs
    const bool VerboseP = false;

    // Initial sizes for our data structures
    const int NumTypicalInserts = 32;
    const int NumTypicalIntrinsics = 8;
    const int GroupSize = 4;

    // Typedefs
    typedef iplist<Instruction>::reverse_iterator reverse_iterator;
    typedef SmallVector<Instruction*,NumTypicalInserts> InstVec;
    typedef SmallSet<Instruction*, NumTypicalInserts> InstSet;
    typedef SmallVector<Instruction*, GroupSize> Group;
    typedef SmallVector<Group*, NumTypicalIntrinsics> GroupVec;

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

    // Class to make all the multiinsert intrinsics in a basic block
    class BBMIIMaker {
    public:
        BBMIIMaker(Module* M, LLVMContext& C, BasicBlock& basicblock)
                  : module(M)
                  , context(C)
                  , bb(basicblock)
                  , modified(false)
        { }

        void run();

        // Whether the BBMIIMaker modificed the basic block
        bool didModify()
        {
            return modified;
        }

        ~BBMIIMaker();

    private:
        GroupVec groups;
        InstVec inserts;
        InstSet handledInsts;

        Module* module;
        LLVMContext& context;
        BasicBlock& bb;
        bool modified;

        // Group instructions into the individual multiInserts
        void groupInserts();

        // Add the entire insert chain specified by the value to instSet
        void addEntireInsertChain(Value* v);

        // Add the left-most insert chain specified by v and not already in s to the
        // provided vector in depth-first pre-order.
        void addLeftInsertChain(Value* v, Group& g, int width, int mask);

        // Gather all insert instructions together
        void gatherInserts();

        // Output convenience methods
        void printGroups();
        void printCandidates();
        void printBlock();

        // Unimplemented unwanted copy and assignment
        BBMIIMaker(BBMIIMaker&);
        BBMIIMaker& operator=(const BBMIIMaker&);

    };

    class MultiInsertIntrinsic {
    public:
        MultiInsertIntrinsic(Module* m, LLVMContext& c, BasicBlock& basicblock, Group& g)
                            : module(m)
                            , llvmcontext(c)
                            , bb(basicblock)
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
        BasicBlock& bb;
        Group& group;

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

        // Insert the instruction in, replacing uses for toReplace with the new
        // instruction
        void insertIntrinsic();

        // Unimplemented unwanted copy and assignment
        MultiInsertIntrinsic(MultiInsertIntrinsic&);
        MultiInsertIntrinsic& operator=(const MultiInsertIntrinsic&);

    };
} // end namespace

// Helpers
// Predicate for whether the instruction is an insert
static bool IsInsertElement(Value* i)
{
    return isa<InsertElementInst>(i);
}

// Predicate for whether the instruction is an extract
static bool IsExtractElement(Value* i)
{
    return isa<ExtractElementInst>(i);
}

// Predicate telling whether the instruction is an insert or extract
static bool IsEitherIE(Value* i) {
    return IsInsertElement(i) || IsExtractElement(i);
}

// Given a value, get the underlying offset and value. If the value is a
// constant, return -1 and the constant. If the value is an extract of a vector,
// then return the vector and the offset into it. If the extract is extracting
// from an insert, then traverse all the way down, eventually returning the
// underlying vector and offset.
static void GetUnderlyingOffsetAndValue(std::pair<Value*,int>& ov)
{
    Instruction* inst = dyn_cast<Instruction>(ov.first);

    // If it's a constant, we're done here.
    if (!inst) {
        return;
    }

    // If it's not an extract or insert, we're done here
    if (!IsExtractElement(inst) && !IsInsertElement(inst)) {
        return;
    }

    // If it's an extract, then traverse down it
    if (IsExtractElement(inst)) {
        // Set the value and offset
        ov.first = inst->getOperand(0);
        ov.second = gla::GetConstantInt(inst->getOperand(1));
        // Continue traversing
        GetUnderlyingOffsetAndValue(ov);
        return;
    }

    // If it's an insert, then see what its offset is and what it's pointing to
    // in order to determine how to traverse it
    if (IsInsertElement(inst)) {

        // The insert instruction's offset that it is writing to
        int insertOffset = gla::GetConstantInt(inst->getOperand(2));

        // If the insert is overwriting the field that we are trying to get at,
        // then continue with it's value, otherwise continue with the insert's
        // target and the same offset.
        if (insertOffset == ov.second) {
            ov.first = inst->getOperand(1);
            ov.second = -1;
        } else {
            ov.first = inst->getOperand(0);
        }

        // If we're still dealing with an insert or extract, keep traversing,
        // otherwise we're done.
        if (IsInsertElement(ov.first) || IsExtractElement(ov.first)) {
            GetUnderlyingOffsetAndValue(ov);
        }
        return;
    }
}

static Value* GetOriginalSource(Value* val)
{
    // If it's an insert, traverse it
    if (Instruction* inst = dyn_cast<Instruction>(val)) {
        if (IsInsertElement(inst)) {
            return GetOriginalSource(inst->getOperand(0));
        }
    }

    // Else return what we have
    return val;
}

void BBMIIMaker::gatherInserts()
{
    BasicBlock::InstListType& instList = bb.getInstList();
    for (reverse_iterator i = instList.rbegin(), e = instList.rend(); i != e; ++i) {
        if (IsInsertElement(&*i)) {
            inserts.push_back(&*i);
        }
    }
}


void BBMIIMaker::printBlock()
{
    errs() << "processing basic block " << bb.getName() << "\n" << bb << "\n";
}

void BBMIIMaker::printCandidates()
{
    errs() << "\n  This block's candidates for intrinsic substitution: \n";
    for (InstVec::iterator i = inserts.begin(), e = inserts.end(); i != e; ++i) {
        errs() << "  " << **i << "\n";
    }
}

void BBMIIMaker::printGroups()
{
    errs() << "\n  This block's groups: \n";
    int i = 0;
    for (GroupVec::iterator gI = groups.begin(), gE = groups.end(); gI != gE; ++gI) {
        ++i;
        errs() << "    Group " << i << ":";
        for (Group::iterator instI = (*gI)->begin(), instE = (*gI)->end(); instI != instE; ++instI) {
            if (instI == ((*gI)->begin()))
                errs() << **instI;
            else
                errs() <<  "   <|> " << **instI;
        }
        errs() << "\n";
    }
}

void BBMIIMaker::addLeftInsertChain(Value* v, Group& vec, int width, int mask)
{
    Instruction* inst = dyn_cast<Instruction>(v);

    // If it's not an insert, or if we've already seen it before,
    // we're done
    if (!inst || !IsInsertElement(inst) || handledInsts.count(inst)) {
        return;
    }

    // If mask is finished, we're done
    if (mask == 0) {
        return;
    }

    // Otherwise, put it in the vector, turn off mask's bit, and continue
    vec.push_back(inst);
    mask &= ~(1 << gla::GetConstantInt(inst->getOperand(2)));
    addLeftInsertChain(inst->getOperand(0), vec, width, mask);
}

void BBMIIMaker::addEntireInsertChain(Value* v)
{
    Instruction* inst = dyn_cast<Instruction>(v);

    // If it's not an insert or extract, or if we've already seen it before,
    // we're done
    if (!inst || !IsEitherIE(inst) || handledInsts.count(inst)) {
        return;
    }

    // If it's an insert, add it
    if (IsInsertElement(inst)) {
        handledInsts.insert(inst);
    }

    // Continue on
    addEntireInsertChain(inst->getOperand(0));
}

void BBMIIMaker::groupInserts()
{
    for (InstVec::iterator i = inserts.begin(), e = inserts.end(); i != e; ++i) {

        // Convert to an instruction (should not fail)
        Instruction* inst = dyn_cast<Instruction>(&**i);
        assert(inst);

        // If we've already seen it, continue
        if (handledInsts.count(inst)) {
            continue;
        }

        // Else this is a new group

        Group* newGroup = new Group();       // note: deallocation handled in destructor

        // Get the width and set up mask to be all 1s
        int width = gla::GetComponentCount(inst->getType());
        int mask = (1 << width) - 1;

        addLeftInsertChain(inst, *newGroup, width, mask);
        addEntireInsertChain(inst);
        groups.push_back(newGroup);
    }
}


// MultiInsertIntrinsic implementation

void MultiInsertIntrinsic::buildFromGroup()
{
    // The instruction we're replacing is at the front
    toReplace = group.front();

    // Find the original insert source.
    originalSource = GetOriginalSource(group.front());

    // For each member of the group, set the relevant fields.
    for (InstVec::iterator instI = group.begin(), instE = group.end(); instI != instE; ++instI) {
        // Only operate on inserts
        assert(IsInsertElement(*instI) && "buildFromGroup supposed to only operate on inserts");

        // The source operand
        Value* src = (*instI)->getOperand(1);

        // Find the access offset of the underlying extract intrinsic
        std::pair<Value*,int> p(src, -1);
        GetUnderlyingOffsetAndValue(p);

        int offset = p.second;
        Value* extractFrom = p.first;
        assert (offset <= 15 && "offset is too big");
        assert (extractFrom);

        // Match up the data with the corresponding field specified in the
        // insert
        int maskOffset = gla::GetConstantInt((*instI)->getOperand(2));
        assert(maskOffset <= 4 && " Unknown access mask found");

        offsets[maskOffset] = offset;
        values[maskOffset] = extractFrom;

        // Update the mask
        writeMask |= (1 << maskOffset);
        assert(writeMask <= 15 && "writeMask is too big");
    }
}

void MultiInsertIntrinsic::makeIntrinsic()
{
    // Set up types array
    int typesCount = 6;
    const llvm::Type* intrinsicTypes[6] = {0};

    // Default type to choose
    const llvm::Type* defaultType;

    // Determine if it's a fWriteMask or writeMask, and set types accordingly
    Intrinsic::ID intrinsicID;
    //unsigned vecCount = 4;
    switch (toReplace->getType()->getContainedType(0)->getTypeID()) {
    case Type::FloatTyID:
        defaultType = Type::getFloatTy(llvmcontext);
        intrinsicID = Intrinsic::gla_fMultiInsert;
        break;
    case Type::IntegerTyID:
        if (toReplace->getType()->getContainedType(0)->isIntegerTy(1))
            defaultType = Type::getInt1Ty(llvmcontext);
        else
            defaultType = Type::getInt32Ty(llvmcontext);
        intrinsicID = Intrinsic::gla_multiInsert;
        break;
    default:
        assert(!"Unknown multiInsert intrinsic type");
    }

    // Set up each of the operand types
    intrinsicTypes[0] = originalSource ? originalSource->getType() : defaultType;
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
    BasicBlock::InstListType& instList = bb.getInstList();
    for (BasicBlock::InstListType::iterator instI = instList.begin(), instE = instList.end(); instI != instE; ++instI)
        if (instI->isIdenticalTo(toReplace)) {
            assert(IsInsertElement(instI));

            instList.insertAfter(instI, intrinsic);
            instI->replaceAllUsesWith(intrinsic);
        }
}

void MultiInsertIntrinsic::dump() const
{
    errs() << "\n  MultiInsertIntrinsic for " << *toReplace << ":\n";
    errs() << "    Write mask and offset:\n";
    errs() << "    " << writeMask;

    errs() << "   <";
    for (int i = 0; i < 4; ++i) {
        errs() << "|  " << offsets[i] << "  ";
    }

    errs() << "\n         <";
    for (int i = 0; i < 4; ++i) {
        errs() << "|  ";
        if (values[i])
            errs() << *values[i];
        else
            errs() << "null";
        errs() << "  ";
    }

    errs() << "\n";
    errs() << "    Original insert destination: " << *originalSource << "\n";
    errs() << "    Instrinsic: " << *intrinsic;
    errs() << "\n";
}

void BBMIIMaker::run()
{
    VERBOSE(printBlock());

    // Gather the candidate instructions
    gatherInserts();
    VERBOSE(printCandidates());

    // Group them
    groupInserts();
    VERBOSE(printGroups());

    // For each group, make a MultiInsertOp
    for (GroupVec::iterator gI = groups.begin(), gE = groups.end(); gI != gE; ++gI) {
        MultiInsertIntrinsic mii(module, context, bb, **gI);
        mii.run();

        VERBOSE(mii.dump());

        modified = true;

    }

    VERBOSE(printBlock());
}

BBMIIMaker::~BBMIIMaker()
{
    for (GroupVec::iterator gI = groups.begin(), gE = groups.end(); gI != gE; ++gI) {
        delete *gI;
    }
}

// CoalesceInserts implementation

bool CoalesceInserts::runOnFunction(Function& F)
{
    Module* M = F.getParent();
    LLVMContext& C = F.getContext();

    bool wasModified = false;

    // For each Basic Block
    for (Function::iterator bb = F.begin(), ebb = F.end(); bb != ebb; ++bb) {
        BBMIIMaker maker(M, C, *bb);
        maker.run();
        wasModified = maker.didModify();
    }

    return wasModified;
}


void CoalesceInserts::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.setPreservesCFG();
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


FunctionPass* gla_llvm::createCoalesceInsertsPass()
{
    return new CoalesceInserts();
}


