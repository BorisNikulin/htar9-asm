#pragma once

class HaskellFacade {
    public:
        HaskellFacade(int * argc, char * * argv);
        ~HaskellFacade();

        char * assembleFile(char * fname, char * fcontents);
};
