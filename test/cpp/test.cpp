#include "../../src/cpp/haskell_facade.h"
#include "../../src/cpp/ncurses_display.h"

#include <iostream>
#include <fstream>
#include <stdexcept>
#include <random>
#include <chrono>
#include <iomanip>

const static int NUM_TESTS = 500;

/**
 * Reads a file into a C string, returns a unique pointer to that string
 *
 * @param fname String containing path to file to read
 */

static std::unique_ptr<char[]> readFile(const std::string & fname)
{
  std::ifstream ifs;
  int length;
  ifs.open(fname);

  if(!ifs.good())
  {
    throw std::runtime_error("Could not open requested input file.");
  }

  // seek to end of file
  ifs.seekg(0, std::ios::end);

  // get current position - this is the length of the necessary buffer
  length = ifs.tellg();
  //seek back to the beginning of the file for a re-read
  ifs.seekg(0, std::ios::beg);

  // make a buffer of the necessary size
  char * buffer = new char[length];

  ifs.read(buffer, length);
  ifs.close();

  return std::unique_ptr<char[]>(buffer);
}

int main(int argc, char * * argv)
{
  std::string infile = "test/golden/mult.s";
  std::cerr << "Reading assembly from " << infile << std::endl;

  auto start = std::chrono::system_clock::now();

  try
  {
    // Read file into a C string
    std::unique_ptr<char[]> fileContents = std::move(readFile(infile));

    std::string result;
    int status;
    {
      // Init haskell handler
      HaskellFacade hf(&argc, argv);

      // Punt to Haskell assembler - result is a C string of binary
      result = hf.assembleFile((char *)infile.c_str(),
        (char *)fileContents.get(), &status);
    }

    if(status)
    {
      std::cerr << "error:\n" << result << std::endl;
      exit(status);
    }

    std::cerr << "Assembly complete. ";

    std::cerr << "Starting interpreter.\n\n";

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
        std::cerr << "\033[1;31m" << "\nFAIL\n" << "\033[0;31m";
        std::cerr << "In computation of " << (int)a << " * " << (int)b << " * "
        << (int)c << "\n";
        std::cerr << "Expected result: " << expected << "\n";
        std::cerr << "Actual result: " << ans << "\n";
        std::cerr << "\033[0m";
        multPass = false;
      }
    }

    auto end = std::chrono::system_clock::now();

    std::chrono::duration<double> elapsed_seconds = end - start;

    bool pass = true;

    if(multPass)
    {
      std::cerr << "\nmult: " << "\033[32m" << "OK\n" << "\033[0m";
    }
    else
    {
      pass = false;
      std::cerr << "\nmult: " << "\033[1;31m" << "FAIL\n" << "\033[0m";
    }

    if(pass)
    {
      std::cerr << "\033[32m" << "\nAll " << NUM_TESTS << " tests passed (" <<
      std::setprecision(1) << elapsed_seconds.count() << "s)\n" "\033[0m";
    }

    return 0;
  }
  catch(std::runtime_error e)
  {
    std::cerr << "\nError: " << e.what() << std::endl;
    return -2;
  }

  return 0;
}
