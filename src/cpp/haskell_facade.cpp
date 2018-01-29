#include "haskell_facade.h"

#include "Main_stub.h"

HaskellFacade::HaskellFacade(int * argc, char * * argv)
{
  hs_init(argc, &argv);
}

HaskellFacade::~HaskellFacade()
{
  hs_exit();
}

char * HaskellFacade::assembleFile(char * fname, char * fcontents)
{
  return (char *)cAssemble(fname, fcontents);
}
