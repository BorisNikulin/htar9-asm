#pragma once

class HaskellFacade {
    public:
        struct AsmResult {
          int status;
          char * str;
        };

        HaskellFacade(int * argc, char * * argv);
        ~HaskellFacade();

        char * assembleFile(char * fname, char * fcontents, int * status);
};
