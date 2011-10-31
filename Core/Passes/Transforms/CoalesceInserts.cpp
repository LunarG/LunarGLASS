//===- CoalesceInserts.cpp - Coalesce inserts/shuffles into multiInserts --===//
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
// Creates multiInserts for inserts and shuffles. Traces each value to its
// origin by reading through inserts/extracts/shuffles. This works best when
// preceeded by instcombine and proceeded by adce. After this pass and adce,
// there should be no insertElements or shuffleVectors constructing 4-wide or
// smaller vectors.
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

#include "Passes/PassSupport.h"

// LunarGLASS helpers
#include "Util.h"

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
    typedef SmallPtrSet<Instruction*, NumTypicalInserts> InstSet;
    typedef SmallVector<Instruction*, GroupSize> Group;
    typedef SmallVector<Group*, NumTypicalIntrinsics> GroupVec;

    // Coalesce Inserts/Extracts Pass
    class CoalesceInserts : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;

        CoalesceInserts() : FunctionPass(ID)
        {
            initializeCoalesceInsertsPass(*PassRegistry::getPassRegistry());
        }

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
        InstVec vecConstructors;
        InstSet handledInsts;

        Module* module;
        LLVMContext& context;
        BasicBlock& bb;
        bool modified;

        // Group instructions into the individual multiInserts
        void groupVectorConstructors();

        // Add the insertElements that fully specify a multiInsert into the
        // group, and add any subsumed instructions to our handledInsts set so
        // that we don't make multiInserts for them too.
        void addInsertChain(Value* v, Group& g, int mask);

        // Gather all vector constructors together
        void gatherVectorConstructors();

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
// Predicate for whether the instruction constructs a vector. Currently handles
// inserts and shuffles.
static bool IsVectorConstructor(Value* i)
{
    return isa<InsertElementInst>(i) || (isa<ShuffleVectorInst>(i) && gla::GetComponentCount(i) <= 4);
}

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
// scalar, return 0 and the scalar. If the value is an extract of a vector,
// then return the vector and the offset into it. If the extract is extracting
// from an insert, then traverse all the way down, eventually returning the
// underlying vector and offset.
static void GetUnderlyingOffsetAndValue(Value*& value, unsigned& offset)
{
    while (true) {
        Instruction* inst = dyn_cast<Instruction>(value);

        // If it's a constant, we're done here.
        if (!inst) {
            return;
        }

        // If it's not an insert/extract/shuffle, we're done here
        if (! IsExtractElement(inst) && ! IsInsertElement(inst) && ! isa<ShuffleVectorInst>(inst)) {
            return;
        }

        // If it's an extract, then traverse down it
        if (IsExtractElement(inst)) {
            // Set the value and offset
            value  = inst->getOperand(0);
            offset = gla::GetConstantInt(inst->getOperand(1));

            continue;
        }

        // If it's a ShuffleVector, then continue on the appropriate source and with
        // the appropriate offset
        if (isa<ShuffleVectorInst>(inst)) {
            SmallVector<Constant*, 8> elts;
            Constant* shuffleMask = dyn_cast<Constant>(inst->getOperand(2));
            assert(shuffleMask);

            int sourceSize = gla::GetComponentCount(inst->getOperand(0));

            shuffleMask->getVectorElements(elts);
            assert(elts.size() >= offset && "out-of-range offset");

            // Get the offset, if it's undef, then return back an undef value.
            Constant* cOffset = elts[offset];
            if (gla::IsUndef(cOffset)) {
                value = UndefValue::get(value->getType());
                offset = 0;

                return;
            }

            int shuffleOffset = gla::GetConstantInt(cOffset);

            // If we're refering to the second op, adjust the offset, and use that
            // value, otherwise use the first operand.
            if (shuffleOffset > sourceSize - 1) {
                value = inst->getOperand(1);
                offset = shuffleOffset - sourceSize;
            } else {
                value = inst->getOperand(0);
                offset = shuffleOffset;
            }

            continue;
        }

        // If it's an insert, then see what its offset is and what it's pointing to
        // in order to determine how to traverse it
        if (IsInsertElement(inst)) {
            // The insert instruction's offset that it is writing to
            int insertOffset = gla::GetConstantInt(inst->getOperand(2));

            // If the insert is overwriting the field that we are trying to get at,
            // then continue with it's value, otherwise continue with the insert's
            // target and the same offset.
            if (insertOffset == offset) {
                value = inst->getOperand(1);
                offset = 0;
            } else {
                value = inst->getOperand(0);
            }

            continue;
        }
    }
}

static Value* GetOriginalSource(Value* val)
{
    if (Instruction* inst = dyn_cast<Instruction>(val)) {
        // If it's an insert, traverse it
        if (IsInsertElement(inst)) {
            return GetOriginalSource(inst->getOperand(0));
        }

        // We don't traverse through (single-defined-operand) shuffle vectors,
        // but rather use them as our original sources. This greatly simplifies
        // multiInsert construction logic, and still wont potentiate swizzles of
        // swizzles during instruction-canonicalization. The shuffle will have
        // its own multiInsert created for it.
    }

    // Else return what we have
    return val;
}

void BBMIIMaker::gatherVectorConstructors()
{
    BasicBlock::InstListType& instList = bb.getInstList();
    for (reverse_iterator i = instList.rbegin(), e = instList.rend(); i != e; ++i) {
        if (IsVectorConstructor(&*i)) {
            vecConstructors.push_back(&*i);
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
    for (InstVec::iterator i = vecConstructors.begin(), e = vecConstructors.end(); i != e; ++i) {
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

void BBMIIMaker::addInsertChain(Value* v, Group& vec, int mask)
{
    // Go until the mask has been fully accounted for.
    while (mask != 0) {
        Instruction* inst = dyn_cast<Instruction>(v);

        // If it's not an insert, or if we've already seen it before,
        // we're done
        if (! inst || ! IsInsertElement(inst) || handledInsts.count(inst)) {
            return;
        }

        // If it only has one use, then we won't need to make another
        // MultiInsert for it, as it will be part of the current one.
        if (inst->hasOneUse()) {
            handledInsts.insert(inst);
        }

        // Only put it in our vector if it's defining a new component, i.e. mask
        // hasn't been turned off for that component yet
        int selectBit = 1 << gla::GetConstantInt(inst->getOperand(2));
        if (mask & selectBit) {
            vec.push_back(inst);
            mask &= ~selectBit;
        }

        v = inst->getOperand(0);
    }
}

void BBMIIMaker::groupVectorConstructors()
{
    for (InstVec::iterator i = vecConstructors.begin(), e = vecConstructors.end(); i != e; ++i) {

        Instruction* inst = *i;

        // If we've already seen it, continue
        if (handledInsts.count(inst)) {
            continue;
        }

        // Else this is a new group

        Group* newGroup = new Group();       // note: deallocation handled in destructor

        // If we're making a group out of inserts, then group them all
        // together. For shuffles, just put the shuffle in the group.
        if (IsInsertElement(inst)) {
            // Get the width and set up mask to be all 1s
            int width = gla::GetComponentCount(inst->getType());
            int mask = (1 << width) - 1;
            addInsertChain(inst, *newGroup, mask);
        } else {
            assert(isa<ShuffleVectorInst>(inst));
            newGroup->push_back(inst);
        }

        groups.push_back(newGroup);
        handledInsts.insert(inst);
    }
}


// MultiInsertIntrinsic implementation

void MultiInsertIntrinsic::buildFromGroup()
{
    // The instruction we're replacing is at the front
    toReplace = group.front();

    // If we're replacing a shuffle, make a multiInsert into undef where each
    // source is determined by the shuffle's mask (if defined over that
    // component), adjusting the write mask appropriately.
    if (isa<ShuffleVectorInst>(toReplace)) {
        originalSource = UndefValue::get(toReplace->getType());

        SmallVector<Constant*, 4> elts;
        Constant* shuffleMask = dyn_cast<Constant>(toReplace->getOperand(2));
        assert(shuffleMask);

        shuffleMask->getVectorElements(elts);
        assert(elts.size() <= 4);

        int sourceSize = gla::GetComponentCount(toReplace->getOperand(0));
        for (unsigned i = 0; i < elts.size(); ++i) {
            // Don't do anything for undef values
            if (gla::IsUndef(elts[i])) {
                continue;
            }

            writeMask |= (1 << i);

            int shuffleOffset = gla::GetConstantInt(elts[i]);

            Value* value;
            unsigned offset;

            // If we're refering to the second op, adjust the offset, and use that
            // value, otherwise use the first operand.
            if (shuffleOffset > sourceSize - 1) {
                value  = toReplace->getOperand(1);
                offset = shuffleOffset - sourceSize;
            } else {
                value  = toReplace->getOperand(0);
                offset = shuffleOffset;
            }

            GetUnderlyingOffsetAndValue(value, offset);
            values[i]  = value;
            offsets[i] = offset;
        }

        return;
    }

    // Find the original insert source.
    originalSource = GetOriginalSource(toReplace);

    // For each member of the group, set the relevant fields.
    for (InstVec::iterator instI = group.begin(), instE = group.end(); instI != instE; ++instI) {
        // Only operate on inserts
        assert(IsInsertElement(*instI) && "buildFromGroup supposed to only operate on inserts");

        // The source operand
        Value* src = (*instI)->getOperand(1);

        // Find the access offset of the underlying extract intrinsic
        Value* extractFrom = src;
        unsigned offset = 0;
        GetUnderlyingOffsetAndValue(extractFrom, offset);

        assert(offset <= 15 && "offset is too big");
        assert(extractFrom);

        // Match up the data with the corresponding field specified in the
        // insert
        int maskOffset = gla::GetConstantInt((*instI)->getOperand(2));
        assert(maskOffset <= 4 && " Unknown access mask found");

        offsets[maskOffset] = offset;
        values[maskOffset]  = extractFrom;

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

    const Type* i32Ty = Type::getInt32Ty(llvmcontext);

    // Get the function declaration for this intrinsic
    Value* callee = llvm::Intrinsic::getDeclaration(module, intrinsicID, intrinsicTypes, typesCount);

    Value* mask    = ConstantInt::get(i32Ty, writeMask);
    Value* xOffset = offsets[0] == -1 ? UndefValue::get(i32Ty) : ConstantInt::get(i32Ty, offsets[0]);
    Value* yOffset = offsets[1] == -1 ? UndefValue::get(i32Ty) : ConstantInt::get(i32Ty, offsets[1]);
    Value* zOffset = offsets[2] == -1 ? UndefValue::get(i32Ty) : ConstantInt::get(i32Ty, offsets[2]);
    Value* wOffset = offsets[3] == -1 ? UndefValue::get(i32Ty) : ConstantInt::get(i32Ty, offsets[3]);

    Value* xV = values[0] ? values[0] : UndefValue::get(defaultType);
    Value* yV = values[1] ? values[1] : UndefValue::get(defaultType);
    Value* zV = values[2] ? values[2] : UndefValue::get(defaultType);
    Value* wV = values[3] ? values[3] : UndefValue::get(defaultType);

    Value* args[] = { originalSource, mask, xV, xOffset, yV, yOffset, zV, zOffset, wV, wOffset };

    // Create it
    intrinsic = CallInst::Create(callee, args, args+10);
}

void MultiInsertIntrinsic::insertIntrinsic()
{
    BasicBlock::InstListType& instList = bb.getInstList();
    for (BasicBlock::InstListType::iterator instI = instList.begin(), instE = instList.end(); instI != instE; ++instI)
        if (instI->isIdenticalTo(toReplace)) {
            assert(IsVectorConstructor(instI));

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
    gatherVectorConstructors();
    VERBOSE(printCandidates());

    // Group them
    groupVectorConstructors();
    VERBOSE(printGroups());

    modified = ! groups.empty();

    // For each group, make a MultiInsertOp
    for (GroupVec::iterator gI = groups.begin(), gE = groups.end(); gI != gE; ++gI) {
        MultiInsertIntrinsic mii(module, context, bb, **gI);
        mii.run();

        VERBOSE(mii.dump());
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
                false,  // Whether it looks only at CFG
                false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createCoalesceInsertsPass()
{
    return new CoalesceInserts();
}


