#include "haskell_facade.h"
#include "Main_stub.h"

/**
 * Constructor - initialize Haskell runtime environment
 *
 * @param argc From main - forwarded
 * @param argv From main - forwarded
 */

HaskellFacade::HaskellFacade(int * argc, char * * argv)
{
  hs_init(argc, &argv);
}

/**
 * Destructor - shutdown Haskell runtime environment
 */

HaskellFacade::~HaskellFacade()
{
  hs_exit();
}

/**
 * Send file with given name and contents to Haskell to be assembled
 *
 * @param  fname     filename (for error reporting)
 * @param  fcontents file contents
 * @return           Struct containing string result and status flag
 */

HaskellFacade::AssembleResult HaskellFacade::assembleFile(const char * fname,
  const char * fcontents)
{
  FFIResult * ffiStruct = (FFIResult *)cAssemble((void *)fname,
    (void *)fcontents);

  AssembleResult res{ ffiStruct->status, std::string(ffiStruct->str) };

  delete ffiStruct;

  return res;
}
