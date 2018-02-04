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
 * @return           Pointer to C string of machine code
 */

char * HaskellFacade::assembleFile(char * fname, char * fcontents, int * status)
{
  AsmResult * res = (AsmResult *)cAssemble(fname, fcontents);
  *status = res->status;
  return res->str;
}
