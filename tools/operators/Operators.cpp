//===- IntrinsicsLunarGLASS.td - Defines LunarGLASS intrinsics -*- tablegen -*-===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2014 LunarG, Inc.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
// 
//     Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials provided
//     with the distribution.
// 
//     Neither the name of LunarG Inc. nor the names of its
//     contributors may be used to endorse or promote products derived
//     from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
//===----------------------------------------------------------------------===//
//
// Author: John Kessenich, LunarG
// Author: Cody Northrop, LunarG
//
// Types
// f = floating-point scalar only
// F = floating-point scalar and any size floating-point vector
// i = generic integer scalar only
// I = generic integer scalar and any size generic integer vector
// d = scalar forms for floating point and generic integer
// D = scalar and vector forms for floating point and generic integer
// a = scalar forms for floating point, signed integer, and unsigned integer
// A = scalar and vector forms for floating point, signed integer, and unsigned integer
//
// Bottom IR Forms
// - = NA
// P = pass through
// R = recompose
// W = component wise
// X = cross component
// 3 = cross component, vectors of 3 components only
// 4 = cross component, vectors of 4 components only
// O = optionally decomposed
//
// @ = prefix for intrinsics 
// / = prefix for comments, which are printed out verbatim

// Intrinsic Properties
// N - Non commutative
// C - Commutative
//
// Precision
// 0 = Correctly rounded
// F = Fast mode
// A = Accumulated from definition
// C = Correct
// - = NA
//
// Columns are, in order
//   name
//   return types
//   argument types
//   intrinsic property
//   precision
//   bottom IR AoS
//   bottom IR SoA
//   operation style
//   definition or decomposition
//
//===--------------------------------------------------------------------------===//

#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>

#include <vector>

//
// Input processing and analysis.
//
// The input entry is read into, analyzed, and left in the following globals.
//

const char EntryDelim = '#';
const char FieldDelim = '|';

const int MaxEntryLength = 10000;
char EntryString[MaxEntryLength];
char Junk[MaxEntryLength];

int NumOperators = 0; // just for statistics

#define LUNAR_GLASS_LLVM_PREFIX "gla"

const char* IntrinsicPrefix = "llvm." LUNAR_GLASS_LLVM_PREFIX ".";
bool firstIntrHeader = true;

enum {
    EFloatVectorForm,
    EIntVectorForm,
    EFloatScalarForm,
    EIntScalarForm,
    ESignedIntScalarForm,
    ESignedIntVectorForm,
    EUnsignedIntScalarForm,
    EUnsignedIntVectorForm
};

struct TEntry {
    bool intrinsic;
    bool comment;
    bool header;
    bool hasFloatVectorForm;
    bool hasIntVectorForm;
    bool hasFloatScalarForm;
    bool hasIntScalarForm;
    bool hasSignedIntVectorForm;
    bool hasUnsignedIntVectorForm;
    bool hasSignedIntScalarForm;
    bool hasUnsignedIntScalarForm;
    int hardWidth;
    char firstCharLower;
    char firstCharUpper;
    char *name;
    char *returnTypes;
    char *argumentTypes;
    char *intrProperty;
    char *precision;
    char *AoS;
    char *SoA;
    char *style;
    char *definition;
} desc;

char *ConvertDelim(char* start, char delim)
{
    while (*start != delim)
        ++start;

    *start = 0;
    ++start;

    return start;
}

bool HasFloatVector(char *s)
{    
    for (size_t c = 0; c < strlen(s); ++c) {
        switch (s[c]) {
        case 'F': return true;
        case 'A': return true;
        case 'D': return true;
        }
    }

    return false;
}

bool HasIntVector(char *s)
{    
    for (size_t c = 0; c < strlen(s); ++c) {
        switch (s[c]) {
        case 'B': return true;
        case 'I': return true;
        case 'A': return true;
        case 'D': return true;
        }
    }

    return false;
}

bool HasFloatScalar(char *s)
{    
    for (size_t c = 0; c < strlen(s); ++c) {
        switch (s[c]) {
        case 'a': return true;
        case 'd': return true;
        case 'A': return true;
        case 'D': return true;
        case 'F': return true;
        case 'f': return true;
        }
    }

    return false;
}

bool HasIntScalar(char *s)
{    
    for (size_t c = 0; c < strlen(s); ++c) {
        switch (s[c]) {
        case 'a': return true;
        case 'd': return true;
        case 'A': return true;
        case 'D': return true;
        case 'I': return true;
        case 'i': return true;
        case 'B': return true;
        case 'b': return true;
        }
    }

    return false;
}

bool IsGenericInt(char *s)
{    
    for (size_t c = 0; c < strlen(s); ++c) {
        switch (s[c]) {
        case 'd': return true;
        case 'D': return true;
        case 'I': return true;
        case 'i': return true;
        case 'B': return true;
        case 'b': return true;
        }
    }

    return false;
}

bool HasAll(char *s)
{    
    for (size_t c = 0; c < strlen(s); ++c) {
        switch (s[c]) {
        case 'a': return true;
        case 'd': return true;
        case 'A': return true;
        case 'D': return true;
        }
    }

    return false;
}

bool ParseEntry(std::ifstream &source)
{
    static bool first = true;

    // get the whole entry
    source.get(EntryString, MaxEntryLength, EntryDelim);
    if (strlen(EntryString) > MaxEntryLength - 2)
        std::cout << "ENTRY OVERFLOW";

    // throw away the delimiter and carriage return
    source.getline(Junk, MaxEntryLength);  
    if (strlen(Junk) > MaxEntryLength - 2)
        std::cout << "POST-ENTRY OVERFLOW";

    if (!source.good())
        return false;

    desc.name = EntryString;

    // check for being a header
    if (desc.name[0] == '-') {
        desc.header = true;
        while (desc.name[0] == '-')
            ++desc.name;
        first = false;
        return true;
    }
    desc.header = false;

    // check for being a comment
    if (desc.name[0] == '/') {
        desc.comment = true;
        return true;
    }
    desc.comment = false;

    // check for being an intrisic
    if (desc.name[0] == '@') {
        desc.intrinsic = true;
        ++desc.name;
    } else {
        desc.intrinsic = false;
    }

    desc.firstCharLower = tolower(*desc.name);
    desc.firstCharUpper = toupper(*desc.name);

    // find the parts
    desc.returnTypes   = ConvertDelim(desc.name, FieldDelim);
    desc.argumentTypes = ConvertDelim(desc.returnTypes, FieldDelim);
    desc.intrProperty  = ConvertDelim(desc.argumentTypes, FieldDelim);
    desc.precision     = ConvertDelim(desc.intrProperty, FieldDelim);
    desc.AoS           = ConvertDelim(desc.precision, FieldDelim);
    desc.SoA           = ConvertDelim(desc.AoS, FieldDelim);
    desc.style         = ConvertDelim(desc.SoA, FieldDelim);
    desc.definition    = ConvertDelim(desc.style, FieldDelim);

    // check for standard forms this would expand to
    desc.hasFloatVectorForm       = false;
    desc.hasIntVectorForm         = false;
    desc.hasFloatScalarForm       = false;
    desc.hasIntScalarForm         = false;
    desc.hasSignedIntVectorForm   = false;
    desc.hasUnsignedIntVectorForm = false;
    desc.hasSignedIntScalarForm   = false;
    desc.hasUnsignedIntScalarForm = false;

    // figure out which types of operators to produce
    bool intVectors = (HasIntVector(desc.returnTypes) || HasIntVector(desc.argumentTypes));
    bool hasAll = (HasAll(desc.returnTypes) || HasAll(desc.argumentTypes));
    desc.hasFloatVectorForm =  HasFloatVector(desc.returnTypes) || HasFloatVector(desc.argumentTypes);
    desc.hasIntVectorForm   =  desc.hasFloatVectorForm && intVectors && hasAll ||
                              !desc.hasFloatVectorForm && intVectors;
    desc.hasFloatScalarForm =  HasFloatScalar(desc.returnTypes) || HasFloatScalar(desc.argumentTypes);
    desc.hasIntScalarForm   =  desc.hasFloatScalarForm && hasAll ||
                              !desc.hasFloatScalarForm && (HasIntScalar(desc.returnTypes) || HasIntScalar(desc.argumentTypes));

    // integer forms can be either just generic or need signed and unsigned versions
    if (desc.hasIntVectorForm) {
        if (! IsGenericInt(desc.returnTypes)) {
            desc.hasSignedIntVectorForm = true;
            desc.hasUnsignedIntVectorForm = true;
            desc.hasIntVectorForm = false;
        }
    }
    if (desc.hasIntScalarForm) {
        if (! IsGenericInt(desc.returnTypes)) {
            desc.hasSignedIntScalarForm = true;
            desc.hasUnsignedIntScalarForm = true;
            desc.hasIntScalarForm = false;
        }
    }

    // simplify that we don't use scalar form if we have vector form
    if (desc.hasFloatVectorForm)
        desc.hasFloatScalarForm = false;
    if (desc.hasIntVectorForm)
        desc.hasIntScalarForm = false;
    if (desc.hasUnsignedIntVectorForm)
        desc.hasUnsignedIntScalarForm = false;
    if (desc.hasSignedIntVectorForm)
        desc.hasSignedIntScalarForm = false;
    
    desc.hardWidth = 0;    
    for (size_t c = 0; c < strlen(desc.style); ++c) {
        switch (desc.style[c]) {
        case '2': desc.hardWidth = 2; break;
        case '3': desc.hardWidth = 3; break;
        case '4': desc.hardWidth = 4; break;
        }
    }

    if (desc.hardWidth > 1) {
        desc.hasFloatScalarForm = false;
        desc.hasIntScalarForm = false;
    }

    return true;
}

void sendAnnotatedName(std::ofstream &out, int form, TEntry desc)
{
    switch(form) {
    case EFloatVectorForm:          out << "f" << desc.firstCharUpper << &desc.name[1]; break;
    case EIntVectorForm:            out <<        desc.firstCharLower << &desc.name[1]; break;
    case EFloatScalarForm:          out << "f" << desc.firstCharUpper << &desc.name[1]; break;
    case EIntScalarForm:            out <<        desc.firstCharLower << &desc.name[1]; break;
    case ESignedIntScalarForm:      out << "s" << desc.firstCharUpper << &desc.name[1]; break;
    case ESignedIntVectorForm:      out << "s" << desc.firstCharUpper << &desc.name[1]; break;
    case EUnsignedIntScalarForm:    out << "u" << desc.firstCharUpper << &desc.name[1]; break;
    case EUnsignedIntVectorForm:    out << "u" << desc.firstCharUpper << &desc.name[1]; break;
    default:                        
        out << "UNKNOWN_TYPE" << desc.name; break;
    }
}

//
// Output Processing
//

class TOutput {
public:
    TOutput(char *path) : out(path, std::ios_base::trunc) {}
    virtual void dumpEntry() = 0;
    virtual ~TOutput() {};

protected:
    std::ofstream out;
};

class TDocOutput : public TOutput {
public:
    TDocOutput(char* path) : TOutput(path)
    {
        out << "<style> h2 {page-break-before: always;} </style>";
    }
    
    void dumpEntry()
    {
        // first see if it's a header and process that way
        if (desc.header) {
            out << "<b><h2>" << desc.name << "</h2></b>" << std::endl;
            return;
        }

        // ignore comments for now
        if (desc.comment) {
            return;
        }

        // not a header, output the full entry
         
        // output just the base name, before the intrinsic forms
        out << "<BR>";
        out << "<P style=\"page-break-after: avoid\"><B>" << desc.name << "</B></P>";

        // do the table
        out << "<TABLE style=\"page-break-inside: avoid\" ALIGN=CENTER WIDTH=525 BORDER=1 CELLSPACING=0>"
	    "	<TR ALIGN=CENTER>";
                
        out <<
	    "		<TD>"
	    "			<B>Bottom IR AoS</B>"
	    "		</TD><TD>"
	    "			<B>Bottom IR SoA</B>"
	    "		</TD><TD>"
	    "			<B>Component Style</B>"
	    "		</TD><TD>"
	    "			<B>Operation Precision</B>"
	    "		</TD>"
	    "	</TR><TR ALIGN=CENTER>" << std::endl;
        
        // Bottom AoS entry
        out << "<TD>";
        for (size_t c = 0; c < strlen(desc.AoS); ++c) {
            switch (desc.AoS[c]) {
            case 'D': out << "Decomposed"; break;
            case 'O': out << "Optionally Decomposed"; break;
            case 'P': out << "Pass Through"; break;
            case '-': out << "NA"; break;
            default:
                out << "UNKNOWN AoS ENTRY";
            }
        }
        out << "</TD>" << std::endl;

        // Bottom SoA entry
        out << "<TD>";
        for (size_t c = 0; c < strlen(desc.SoA); ++c) {
            switch (desc.SoA[c]) {
            case 'D':                     out << "Decomposed<BR>";              break;
            case 'R': 
                switch (desc.style[c]) {
                case 'W':                 out << "Transposed<BR>" ;             break;
                default:                  out << "Recomposed<BR>";              break;
                }
                break;
            case 'O':                     out << "Optionally Decomposed<BR>";   break;
            case 'P':                     out << "Native SoA<BR>";              break;
            default:                      out << "UNKNOWN SoA ENTRY";           break;
            }
        }
        out << "</TD>" << std::endl;

        // Component Style entry
        out << "<TD>";
        for (size_t c = 0; c < strlen(desc.style); ++c) {
            switch (desc.style[c]) {
            case 'W': out << "Component-Wise<BR>" ; break;
            case 'X': out << "Cross Component<BR>" ; break;
            case '2': out << "2 components only<BR>"; break;
            case '3': out << "3 components only<BR>"; break;
            case '4': out << "4 components only<BR>"; break;
            case 'T': out << "Transposed<BR>"; break;
            default:
                out << "UNKNOWN STYLE ENTRY";
            }
        }
        out << "</TD>" << std::endl;

        // Precision entry
        out << "<TD>";
        if (strlen(desc.precision) > 1) {
            out << desc.precision;
        } else {
            switch (desc.precision[0]) {
            case '0':  out << "Correctly Rounded";          break;
            case 'C':  out << "Correct";                    break;
            case 'F':  out << "Fast";                       break;
            case 'A':  out << "Accumulated from definition";break;
            case '-':  out << "NA";                         break;
            default:
                out << "&lt " << desc.precision << "ULP";
            }
        }
        out << "</TD></TR>" << std::endl;

        // Emit the operation forms
        out << "<TD colspan = 4>";

        bool first = true;
        if (desc.hasIntScalarForm)
            dumpOperator(EIntScalarForm, first);

        if (desc.hasSignedIntScalarForm)
            dumpOperator(ESignedIntScalarForm, first);

        if (desc.hasUnsignedIntScalarForm)
            dumpOperator(EUnsignedIntScalarForm, first);

        if (desc.hasFloatScalarForm)
            dumpOperator(EFloatScalarForm, first);
        
        if (desc.hasIntVectorForm)
            dumpOperator(EIntVectorForm, first);

        if (desc.hasSignedIntVectorForm)
            dumpOperator(ESignedIntVectorForm, first);

        if (desc.hasUnsignedIntVectorForm)
            dumpOperator(EUnsignedIntVectorForm, first);

        if (desc.hasFloatVectorForm)
            dumpOperator(EFloatVectorForm, first);

        out << "</TD></TR>" << std::endl;
  
        // output the definition
        out << "<TD colspan = 4>";
        //out << "<B>Description:</B><BR>";
        out << desc.definition;
        out << "</TR>" << std::endl;

        // End of table
        out << 	"</TABLE>";
    }

    virtual ~TDocOutput()
    {
        //out << NumOperators << " Operators";
    }

private:

    char *getTypeString(int form, char specifier, int hardWidth)
    {
        switch (form) {
        case EFloatVectorForm:
            switch (specifier) {
            case 'A': 
            case 'D': 
            case 'F': 
                switch (hardWidth) {
                case 2:   return "&lt2 x float&gt";
                case 3:   return "&lt3 x float&gt";
                case 4:   return "&lt4 x float&gt";
                default:  return "&ltN x float&gt";
                }
            case 'I':             
                switch (hardWidth) {
                case 2:   return "&lt2 x i32&gt";
                case 3:   return "&lt3 x i32&gt";
                case 4:   return "&lt4 x i32&gt";
                default:  return "&ltN x i32&gt";
                }
            case 'B':             
                switch (hardWidth) {
                case 2:   return "&lt2 x i1&gt";
                case 3:   return "&lt3 x i1&gt";
                case 4:   return "&lt4 x i1&gt";
                default:  return "&ltN x i1&gt";
                }
            case 'a':
            case 'd':
            case 'f': return "float";
            case 'i': return "i32";
            case 'b': return "i1";
            default:  return "UNKNOWN TYPE";
            }
            
        case ESignedIntVectorForm:
        case EUnsignedIntVectorForm:
        case EIntVectorForm:
            switch (specifier) {
            case 'F':             
                switch (hardWidth) {
                case 2:   return "&lt2 x float&gt";
                case 3:   return "&lt3 x float&gt";
                case 4:   return "&lt4 x float&gt";
                default:  return "&ltN x float&gt";
                }
            case 'A': 
            case 'D': 
            case 'I':             
                switch (hardWidth) {
                case 2:   return "&lt2 x i32&gt";
                case 3:   return "&lt3 x i32&gt";
                case 4:   return "&lt4 x i32&gt";
                default:  return "&ltN x i32&gt";
                }
            case 'B':             
                switch (hardWidth) {
                case 2:   return "&lt2 x i1&gt";
                case 3:   return "&lt3 x i1&gt";
                case 4:   return "&lt4 x i1&gt";
                default:  return "&ltN x i1&gt";
                }
            case 'f': return "float";
            case 'a':
            case 'd':
            case 'i': return "i32";
            case 'b': return "i1";
            default:  return "UNKNOWN TYPE";
            }

        case EFloatScalarForm:
            switch (specifier) {
            case 'F':
            case 'A':
            case 'D':  
            case 'a':
            case 'd':
            case 'f': return "float";
            case 'I': 
            case 'i': return "i32";
            case 'B': 
            case 'b': return "i1";
            default:  return "UNKNOWN TYPE";
            }

        case EIntScalarForm:
        case EUnsignedIntScalarForm:
        case ESignedIntScalarForm:
            switch (specifier) {
            case 'F':
            case 'f': return "float";
            case 'A': 
            case 'D': 
            case 'a':
            case 'd':
            case 'I': 
            case 'i': return "i32";
            case 'B': 
            case 'b': return "i1";
            default:  return "UNKNOWN TYPE";
            }
        }

        return "UNKNOWN TYPE";
    }

    void dumpOperator(int form, bool &first)
    {
        if (!first)
            out << "<BR><BR>";
        first = false;

        // dump destinations
        // multiple destinations get wrapped into a struct
        if (strlen(desc.returnTypes) == 1) {
            out << getTypeString(form, desc.returnTypes[0], desc.hardWidth);
            out << " dst";
        } else {
            out << "struct {";
            for (size_t c = 0; c < strlen(desc.returnTypes); ++c) {
                if (c > 0)
                    out << ", ";
                else
                    out << " ";

                out << getTypeString(form, desc.returnTypes[c], desc.hardWidth);
                    
                out << " dst" << c+1;
            }
            out << " }";
        }

        // dump the name
        if (desc.intrinsic) {
            out << " = call";
            out << " " << "<B>" << IntrinsicPrefix;
            sendAnnotatedName(out, form, desc);
            out << "</B> ";
        } else {
            out << " = <B>" << desc.name << "</B> ";
        }

        // dump arguments
        for (size_t c = 0; c < strlen(desc.argumentTypes); ++c) {
            if (c > 0)
                out << ", ";
            
            out << getTypeString(form, desc.argumentTypes[c], desc.hardWidth) << " ";
            out << "arg";

            if (strlen(desc.argumentTypes) > 1)
                out << (c + 1);
        }

        ++NumOperators;
    }

};

class TIntrOutput : public TOutput {
public:
    TIntrOutput(char *path) : TOutput(path) { }

    void dumpEntry()
    {
        
        // first see if it's a header and process that way
        if (desc.header) {
            if(!firstIntrHeader) {
                // End of group
                out << "}" << std::endl << std::endl;
            }
            firstIntrHeader = false;
            out << "// " << desc.name << std::endl;
            out << "let TargetPrefix = \""  LUNAR_GLASS_LLVM_PREFIX "\" in {" << std::endl;
            return;
        }

        // check for comment status
        if(desc.comment) {
            // Emit comments without any special parsing.  name contains remainder of string.
            out << desc.name << std::endl;
            return;
        }

        if(desc.intrinsic) {
            // not a header, output the full entry 
            if (desc.hasIntScalarForm)
                dumpOperator(EIntScalarForm);

            if (desc.hasSignedIntScalarForm)
                dumpOperator(ESignedIntScalarForm);

            if (desc.hasUnsignedIntScalarForm)
                dumpOperator(EUnsignedIntScalarForm);

            if (desc.hasFloatScalarForm)
                dumpOperator(EFloatScalarForm);
        
            if (desc.hasIntVectorForm)
                dumpOperator(EIntVectorForm);

            if (desc.hasSignedIntVectorForm)
                dumpOperator(ESignedIntVectorForm);

            if (desc.hasUnsignedIntVectorForm)
                dumpOperator(EUnsignedIntVectorForm);

            if (desc.hasFloatVectorForm)
                dumpOperator(EFloatVectorForm);
        }
    }

    virtual ~TIntrOutput()
    {   
         // Just emit a close bracket for the final group
         out << "}" << std::endl;
    }

private:

    char *getTypeString(int form, char specifier, int hardWidth)
    {
        switch (form) {
        case EFloatVectorForm:
            switch (specifier) {
            case 'A':
            case 'D': 
            case 'F': return "llvm_anyfloat_ty";
            case 'B':
            case 'I': return "llvm_anyint_ty";
            case 'a':
            case 'd':
            case 'f': return "llvm_float_ty";
            case 'b': return "llvm_i1_ty";
            case 'i': return "llvm_i32_ty";
            default:  return "UNKNOWN TYPE";
            }

        case EIntVectorForm:
        case ESignedIntVectorForm:
        case EUnsignedIntVectorForm:
            switch (specifier) {
            case 'F': return "llvm_anyfloat_ty";
            case 'A':
            case 'D':
            case 'B':
            case 'I': return "llvm_anyint_ty";
            case 'f': return "llvm_float_ty";
            case 'a':
            case 'd':
            case 'i': return "llvm_i32_ty";
            case 'b': return "llvm_i1_ty";
            default:  return "UNKNOWN TYPE";
            }

        case EFloatScalarForm:
            switch (specifier) {
            case 'F':
            case 'A': 
            case 'D':
            case 'a':
            case 'd':
            case 'f': return "llvm_float_ty";
            case 'I': 
            case 'i': return "llvm_i32_ty";
            case 'B':
            case 'b': return "llvm_i1_ty";
            default:  return "UNKNOWN TYPE";
            }

        case EIntScalarForm:
        case ESignedIntScalarForm:
        case EUnsignedIntScalarForm:
            switch (specifier) {
            case 'F':
            case 'f': return "llvm_float_ty";
            case 'A': 
            case 'D':
            case 'a':
            case 'd':
            case 'I': 
            case 'i': return "llvm_i32_ty";
            case 'B':
            case 'b': return "llvm_i1_ty";
            default:  return "UNKNOWN TYPE";
            }
        }

        return "UNKNOWN TYPE";
    }

    char *getPropertyString(char intrProperty)
    {
        switch (intrProperty) {
        case 'N':  return "IntrNoMem";
        case 'C':  return "IntrNoMem, Commutative";
        default:  return "UNKNOWN INTRINSIC PROPERTY";
        }

        return "UNKNOWN INTRINSIC PROPERTY";
    }
    
    void dumpOperator(int form)
    {
        // dump name
        out << "  def int_gla_";

        sendAnnotatedName(out, form, desc);
            
        out << " : Intrinsic<[";
    
        // dump destinations
        for (size_t c = 0; c < strlen(desc.returnTypes); ++c) {
            if (c > 0) { out << ", "; }
            out << getTypeString(form, desc.returnTypes[c], desc.hardWidth);
        }

        out << "], [";

        // dump arguments
        for (size_t c = 0; c < strlen(desc.argumentTypes); ++c) {
            if (c > 0) { out << ", "; }
            out << getTypeString(form, desc.argumentTypes[c], desc.hardWidth);
        }

        out << "], [";

        // dump properties
        for (size_t c = 0; c < strlen(desc.intrProperty); ++c) {
            if (c > 0) { out << ", "; }
            out << getPropertyString(desc.intrProperty[c]);
        }

        out << "]>;" << std::endl;
    }

};

class TSwitchOutput : public TOutput {
public:
    TSwitchOutput(char *path) : TOutput(path) 
    {
    }

    void dumpEntry()
    {
    }

    virtual ~TSwitchOutput()
    {
    }

private:
};

int _tmain(int argc, _TCHAR* argv[])
{
    std::vector<TOutput*> outputs;

    outputs.push_back(new    TDocOutput("GlaDoc.html"));
    outputs.push_back(new   TIntrOutput("IntrinsicsLunarGLASS.td"));
    outputs.push_back(new TSwitchOutput("GlaSwitch.cpp"));
    
    std::ifstream source("Operations.source");
    while (source.good()) {
        if (ParseEntry(source)) {
            for (size_t i = 0; i < outputs.size(); ++i)
                outputs[i]->dumpEntry();
        }
    };
    
    for (size_t i = 0; i < outputs.size(); ++i)
        delete outputs[i];

    source.close();

	return 0;
}
