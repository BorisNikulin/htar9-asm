#ifndef HASKELL_FACADE_H
#define HASKELL_FACADE_H

#include <string>

class HaskellFacade {
    public:
        struct AsmResult {
          int status;
          char * str;
        };

        HaskellFacade(int * argc, char * * argv);
        ~HaskellFacade();

        std::string assembleFile(const char * fname, const char * fcontents,
          int * status);
};

#endif /* end of include guard: HASKELL_FACADE_H */
