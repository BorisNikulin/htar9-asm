#pragma once

#include <string>

class HaskellFacade {
    public:
        struct AsmResult {
          int status;
          char * str;
        };

        HaskellFacade(int * argc, char * * argv);
        ~HaskellFacade();

        std::string assembleFile(char * fname, char * fcontents, int * status);
};
