#include "haskell_facade.h"
#include "interpreter.h"
#include "utils.h"

#include <iostream>
#include <fstream>
#include <stdexcept>
#include <random>
#include <chrono>
#include <iomanip>
#include <string>

// construct a trivial random generator engine from a time-based seed:
static const unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
static std::default_random_engine generator (seed);

const static int NUM_TESTS = 9261;

static std::string getCode(const std::string infile, HaskellFacade & hf)
{
  using namespace utils;

  std::cerr << "Reading assembly from " << infile << std::endl;

  // Read file into a C string
  std::string fileContents = FileManager::readFile(infile);

  std::string result;
  int status;

  // Punt to Haskell assembler - result is a C string of binary
  result = hf.assembleFile(infile.c_str(), fileContents.c_str(), &status);

  if(status)
  {
    std::cerr << "error:\n" << result << std::endl;
    exit(status);
  }

  return result;
}

static bool verifyMult(HaskellFacade & hf)
{
  using namespace utils;

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
    env.executeNext();

    env.setInit(false);

    while(!env.isDone())
    {
      env.executeNext();
    }

    int ans = (env.getMemory(4) << 8) + env.getMemory(5);
    int expected = (a * b * c) & 0b1111'1111'1111'1111;

    if(ans != expected)
    {
      std::cerr << Color(Color::BRIGHT_RED) << "\nFAIL\n" <<
        Color(Color::RED);
      std::cerr << "In computation of " << (int)a << " * " << (int)b << " * "
      << (int)c << "\n";
      std::cerr << "Expected result: " << expected << "\n";
      std::cerr << "Actual result: " << ans << "\n";
      std::cerr << Color(Color::NONE);
      multPass = false;
    }
  }

  return multPass;
}

static bool verifyString(HaskellFacade & hf)
{
  using namespace utils;

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
    env.executeNext();

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
      std::cerr << Color(Color::BRIGHT_RED) << "\nFAIL\n" <<
        Color(Color::RED);
      std::cerr << "In string find of pattern " << pattern << "\n";
      std::cerr << "Expected result: " << expected << "\n";
      std::cerr << "Actual result: " << ans << "\n";
      std::cerr << Color(Color::NONE);
      stringPass = false;
    }
  }

  return stringPass;
}

int main(int argc, char * * argv)
{
  using namespace utils;

  bool pass = false, multPass = false, stringPass = false;
  //bool pairPass = false;

  auto start = std::chrono::system_clock::now();

  HaskellFacade hf(&argc, argv);

  try {
    multPass = verifyMult(hf);
    stringPass = verifyString(hf);
    //pairPass = verifyPair();
    pass = multPass && stringPass; //&& pairPass;
  }
  catch(std::runtime_error e)
  {
    std::cerr << Color(Color::RED) << "\nError: " << e.what() << std::endl;
    exit(-1);
  }

  auto end = std::chrono::system_clock::now();

  std::chrono::duration<double> elapsed_seconds = end - start;

  if(multPass)
  {
    std::cerr << "\nmult: " << Color(Color::GREEN) << "OK\n" <<
      Color(Color::NONE);
  }
  else
  {
    pass = false;
    std::cerr << "\nmult: " << Color(Color::BRIGHT_RED) << "FAIL\n" <<
      Color(Color::NONE);
  }

  if(stringPass)
  {
    std::cerr << "string: " << Color(Color::GREEN) << "OK\n" <<
      Color(Color::NONE);
  }
  else
  {
    pass = false;
    std::cerr << "string: " << Color(Color::BRIGHT_RED) << "FAIL\n" <<
      Color(Color::NONE);
  }

  if(pass)
  {
    std::cerr << Color(Color::GREEN) << "\nAll " << NUM_TESTS <<
    " tests passed (" << std::setprecision(1) << elapsed_seconds.count() <<
    "s)\n" << Color(Color::GREEN);

    return 0;
  }
  else
  {
    return 1;
  }
}
