#include "haskell_facade.h"
#include "interpreter.h"
#include "utils.h"
#include "tests.hpp"

#include <iostream>
#include <fstream>
#include <chrono>
#include <string>

static std::string getCode(const std::string infile, HaskellFacade & hf)
{
  using namespace utils;

  // Read file into a C string
  std::string fileContents = FileManager::readFile(infile);

  HaskellFacade::AssembleResult result;

  // Punt to Haskell assembler - result is a C string of binary
  result = hf.assembleFile(infile.c_str(), fileContents.c_str());

  if(result.status)
  {
    throw std::runtime_error(result.str);
  }

  return result.str;
}

// construct a trivial random generator engine from a time-based seed:
static const unsigned int seed = std::chrono::system_clock::now().time_since_epoch().count();
static std::default_random_engine generator (seed);

int main(int argc, char * * argv)
{
  using namespace utils;

  static const std::string multFile = "test/golden/mult.s";
  static const std::string stringFile = "test/golden/string.s";
  static const std::string pairFile = "test/golden/pair.s";

  HaskellFacade hf(&argc, argv);

  std::string multCode = getCode(multFile, hf);
  std::string stringCode = getCode(stringFile, hf);
  std::string pairCode = getCode(pairFile, hf);

  CPU::Interpreter multInter(CPU::CodeParser()(multCode));
  CPU::InterpreterSupervisor multEnv(multInter);

  CPU::Interpreter stringInter(CPU::CodeParser()(stringCode));
  CPU::InterpreterSupervisor stringEnv(stringInter);

  CPU::Interpreter pairInter(CPU::CodeParser()(pairCode));
  CPU::InterpreterSupervisor pairEnv(pairInter);

  MultTest multTest(multEnv, generator);
  multTest.setCount(20000);

  StringTest stringTest(stringEnv, generator);
  stringTest.setCount(20000);

  PairTest pairTest(pairEnv, generator);
  pairTest.setCount(20000);

  std::vector<test::Test *> list { &multTest, &stringTest, &pairTest };

  return test::TestSuite(list).run() ? 0 : 1;
}
