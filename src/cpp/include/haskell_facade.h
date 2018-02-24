#ifndef HASKELL_FACADE_H
#define HASKELL_FACADE_H

#include <string>

class HaskellFacade {
    public:
        struct FFIResult {
          int status;
          char * str;
        };

        struct AssembleResult {
          int status;
          std::string str;
        };

        /**
         * Construct a new HaskellFacade forwarding the given program arguments
         * to the runtime environment initializer
         *
         * @param argc Pointer to argc forwarded from main
         * @param argv argv forwarded from main
         */

        HaskellFacade(int * argc, char * * argv);

        /**
         * Shut down the Haskell runtime environment
         */

        ~HaskellFacade();

        /**
         * Delegate assembly to Haskell function cAssemble
         *
         * @param  fname     Name of assembly file
         * @param  fcontents Contents of assembly file, as C string
         * @return           Struct containing string result and status code
         */

        AssembleResult assembleFile(const char * fname, const char * fcontents);
};

#endif /* end of include guard: HASKELL_FACADE_H */
