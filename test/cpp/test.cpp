#include "haskell_facade.h"
#include "interpreter.h"
#include "utils.h"

#include <iostream>
#include <fstream>
#include <stdexcept>
#include <random>
#include <chrono>
#include <iomanip>

const static int NUM_TESTS = 9261;

static bool verifyMult(int argc, char * * argv)
{
  using namespace utils;

  const std::string infile = "test/golden/mult.s";

  std::cerr << "Reading assembly from " << infile << std::endl;

  // Read file into a C string
  std::string fileContents = FileManager::readFile(infile);

  std::string result;
  int status;
  {
    // Init haskell handler
    HaskellFacade hf(&argc, argv);

    // Punt to Haskell assembler - result is a C string of binary
    result = hf.assembleFile(infile.c_str(), fileContents.c_str(), &status);
  }

  if(status)
  {
    std::cerr << "error:\n" << result << std::endl;
    exit(status);
  }

  CPU::Interpreter inter(CPU::CodeParser()(result));
  CPU::InterpreterSupervisor env(inter);

  // construct a trivial random generator engine from a time-based seed:
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::default_random_engine generator (seed);

  std::uniform_int_distribution<int> distribution(0,255);

  bool multPass = true;

  for(int i = 0; i < NUM_TESTS; i++)
  {
    env.setInit(true);

    env.executeNext(); // skip fin, if at it
    env.executeNext(); // execute reset, if at it, to lower done flag

    env.setInit(false);

    uint8_t a = distribution(generator);
    uint8_t b = distribution(generator);
    uint8_t c = distribution(generator);

    env.setMemory(1, a);
    env.setMemory(2, b);
    env.setMemory(3, c);

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

int main(int argc, char * * argv)
{
  using namespace utils;

  bool pass = false;
  bool multPass = false;

  auto start = std::chrono::system_clock::now();

  try {
    multPass = verifyMult(argc, argv);
    pass = multPass;
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
    std::cerr << "\nmult: " << Color(Color::RED) << "FAIL\n" <<
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
