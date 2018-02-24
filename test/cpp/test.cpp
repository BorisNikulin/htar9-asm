#include "haskell_facade.h"
#include "interpreter.h"
#include "utils.h"
#include "sgr.hpp"

#include <iostream>
#include <fstream>
#include <stdexcept>
#include <random>
#include <chrono>
#include <iomanip>
#include <string>
#include <cmath>
#include <climits>

// construct a trivial random generator engine from a time-based seed:
static const unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
static std::default_random_engine generator (seed);

const static int NUM_TESTS = 9261;

static std::string getCode(const std::string infile, HaskellFacade & hf)
{
  using namespace utils;
  using namespace cpp_sgr;

  std::cerr << "Reading assembly from " << infile << std::endl;

  // Read file into a C string
  std::string fileContents = FileManager::readFile(infile);

  HaskellFacade::AssembleResult result;

  // Punt to Haskell assembler - result is a C string of binary
  result = hf.assembleFile(infile.c_str(), fileContents.c_str());

  if(result.status)
  {
    std::cerr << "error:\n" << result.str << std::endl;
    exit(result.status);
  }

  return result.str;
}

static bool verifyMult(HaskellFacade & hf)
{
  using namespace utils;
  using namespace cpp_sgr;

  const static std::string infile = "test/golden/mult.s";

  std::string code = getCode(infile, hf);

  CPU::Interpreter inter(CPU::CodeParser()(code));
  CPU::InterpreterSupervisor env(inter);

  std::uniform_int_distribution<int> byteDistribution(0,255);

  bool multPass = true;

  for(int i = 0; i < NUM_TESTS; i++)
  {
    uint8_t a = byteDistribution(generator);
    uint8_t b = byteDistribution(generator);
    uint8_t c = byteDistribution(generator);

    env.setMemory(1, a);
    env.setMemory(2, b);
    env.setMemory(3, c);

    env.setInit(true);

    env.executeNext(); // skip fin, if at it - else we're just re-clearing ra

    env.setInit(false);

    while(!env.isDone())
    {
      env.executeNext();
    }

    int ans = (env.getMemory(4) << 8) + env.getMemory(5);
    int expected = (a * b * c) & 0b1111'1111'1111'1111;

    if(ans != expected)
    {
      std::cerr << FGColor(Color::BRIGHT_RED) << "\nFAIL\n" <<
        FGColor(Color::RED);
      std::cerr << "In computation of " << (int)a << " * " << (int)b << " * "
      << (int)c << "\n";
      std::cerr << "Expected result: " << expected << "\n";
      std::cerr << "Actual result: " << ans << "\n";
      std::cerr << SGR::NONE;
      multPass = false;
    }
  }

  return multPass;
}

static bool verifyString(HaskellFacade & hf)
{
  using namespace utils;
  using namespace cpp_sgr;

  static const std::string infile = "test/golden/string.s";
  static const int STRING_START = 32;
  static const int STRING_SIZE = 64;

  std::string code = getCode(infile, hf);

  CPU::Interpreter inter(CPU::CodeParser()(code));
  CPU::InterpreterSupervisor env(inter);

  std::uniform_int_distribution<int> byteDistribution(0,255);
  std::uniform_int_distribution<int> patternDistribution(0, 15);

  bool stringPass = true;

  for(int i = 0; i < NUM_TESTS; i++)
  {
    int pattern = patternDistribution(generator);

    env.setMemory(6, pattern);

    for(int i = STRING_START; i < STRING_START + STRING_SIZE; i++)
    {
      env.setMemory(i, byteDistribution(generator));
    }

    env.setInit(true);

    env.executeNext(); // skip fin, if at it - else we're just re-clearing ra

    env.setInit(false);

    while(!env.isDone())
    {
      env.executeNext();
    }

    int ans = env.getMemory(7);

    int expected = 0;
    for(int i = 0; i < STRING_SIZE; i++)
    {
      int target = env.getMemory(STRING_START + i);
      bool match = false;
      for(int i = 0; i < 5; i++)
      {
        if((target & 0b1111) == pattern)
        {
          match = true;
          break;
        }
        target >>= 1;
      }
      if(match)
      {
        expected++;
      }
    }

    if(ans != expected)
    {
      std::cerr << FGColor(Color::BRIGHT_RED) << "\nFAIL\n" <<
        FGColor(Color::RED);
      std::cerr << "In string find of pattern " << pattern << "\n";
      std::cerr << "Expected result: " << expected << "\n";
      std::cerr << "Actual result: " << ans << "\n";
      std::cerr << SGR::NONE;
      stringPass = false;
    }
  }

  return stringPass;
}

static bool verifyPair(HaskellFacade & hf)
{
  using namespace utils;
  using namespace cpp_sgr;

  static const std::string infile = "test/golden/pair.s";
  static const int ARRAY_START = 128;
  static const int ARRAY_SIZE = 20;

  std::string code = getCode(infile, hf);

  CPU::Interpreter inter(CPU::CodeParser()(code));
  CPU::InterpreterSupervisor env(inter);

  std::uniform_int_distribution<int8_t> byteDistribution(-128, 127);

  bool pairPass = true;

  for(int i = 0; i < NUM_TESTS; i++)
  {
    for(int i = ARRAY_START; i < ARRAY_START + ARRAY_SIZE; i++)
    {
      int8_t val = byteDistribution(generator);
      env.setMemory(i, val);
    }

    env.setInit(true);

    env.executeNext(); // skip fin, if at it - else we're just re-clearing ra

    env.setInit(false);

    while(!env.isDone())
    {
      env.executeNext();
    }

    int ans = env.getMemory(127);

    int expected = INT_MAX;
    for(int i = 0; i < ARRAY_SIZE - 1; i++)
    {
      for(int j = i + 1; j < ARRAY_SIZE; j++)
      {
        int8_t b1 = env.getMemory(ARRAY_START + i);
        int8_t b2 = env.getMemory(ARRAY_START + j);
        int curDist = std::abs((int)b1 - (int)b2);

        expected = std::min(curDist, expected);
      }
    }

    if(ans != expected)
    {
      std::cerr << FGColor(Color::BRIGHT_RED) << "\nFAIL\n" <<
        FGColor(Color::RED);
      std::cerr << "In pair distance\n";
      std::cerr << "Expected result: " << expected << "\n";
      std::cerr << "Actual result: " << ans << "\n";
      std::cerr << SGR::NONE;
      pairPass = false;
    }
  }

  return pairPass;
}

int main(int argc, char * * argv)
{
  using namespace utils;
  using namespace cpp_sgr;

  bool pass = false, multPass = false, stringPass = false, pairPass = false;

  auto start = std::chrono::system_clock::now();

  HaskellFacade hf(&argc, argv);

  try {
    multPass = verifyMult(hf);
    stringPass = verifyString(hf);
    pairPass = verifyPair(hf);
    pass = multPass && stringPass && pairPass;
  }
  catch(std::runtime_error e)
  {
    std::cerr << FGColor(Color::RED) << "\nError: " << e.what() << std::endl;
    exit(-1);
  }

  auto end = std::chrono::system_clock::now();

  std::chrono::duration<double> elapsed_seconds = end - start;

  if(multPass)
  {
    std::cerr << "\nmult: " << FGColor(Color::GREEN) << "OK\n" <<
      SGR::NONE;
  }
  else
  {
    pass = false;
    std::cerr << "\nmult: " << FGColor(Color::BRIGHT_RED) << "FAIL\n" <<
      SGR::NONE;
  }

  if(stringPass)
  {
    std::cerr << "string: " << FGColor(Color::GREEN) << "OK\n" <<
      SGR::NONE;
  }
  else
  {
    pass = false;
    std::cerr << "string: " << FGColor(Color::BRIGHT_RED) << "FAIL\n" <<
      SGR::NONE;
  }

  if(pairPass)
  {
    std::cerr << "pair: " << FGColor(Color::GREEN) << "OK\n" <<
      SGR::NONE;
  }
  else
  {
    pass = false;
    std::cerr << "pair: " << FGColor(Color::BRIGHT_RED) << "FAIL\n" <<
      SGR::NONE;
  }

  if(pass)
  {
    std::cerr << FGColor(Color::GREEN) << "\nAll " << NUM_TESTS <<
    " tests passed (" << std::setprecision(2) << elapsed_seconds.count() <<
    "s)\n" << SGR::NONE;

    return 0;
  }
  else
  {
    return 1;
  }
}
