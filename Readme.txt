Licensing:
 
    LunarGLASS is available via a BSD-style open source license.

Modes of use:

    Use a LunarGLASS stack to compile from a driver:

        [per driver initialization...]
        1. Include LunarGLASSManager.h.  This header includes no other headers.
        2. Use gla::getManager() to get a manager.  (From below, your compiler 
           implementation will have derived a concrete class and factory for
           it.)

        [per compile...]
        3. Pass this manager to a front end that builds LunarGLASS Top IR and 
           saves the llvm module into the manager using manager->setModule()
	   and saves symbol tables using manager->set*Symbols().
        4. Use manager->translateTopToBottom() to create bottom IR.
        5. Use manager->translateBottomToTarget() to translate that to the
           compiler's back end target language.
        6. Use manager->clear() to free up structures specific to the 
           compile, e.g., things set in step 3.

    Make a LunarGLASS back end:

        1. Make a manager that derives from the gla::PrivateManager in 
           PrivateManager.h and provides a factory gla::getManager()
           that makes one of these.
        2. Make a back end that derives from the gla::BackEnd()
           in Backend.h and have it supply methods to describe what form
           of Bottom IR it wants to consume.
        3. Also have your back end derive from gla::BackEndTranslator()
           to fill in all the methods needed to translate from Bottom IR
           to your target.  
        4. You will need to include BottomIR.h to consume the details of 
           Bottom IR.  Between this file, the LunarGLASSTopIR.h file it
           includes, and the specification, you have everything you need
           to interpret Bottom IR.
        5. Your private manager (from 1) will have to make one of these
           back ends (from 2) and back end translators (from 3) when it 
           is created.

    Make a LunarGLASS front end:

        1. Include LunarGLASSManager.h and LunarGLASSTopIR.h in your 
           front end.  Between these and the specification, you have
           everything you need to create Top IR.
        2. Call your front end (as in step 3 in "Use a LunarGLASS 
           stack...") to translate your source language to Top IR
           and leave the llvm module in the manager.
