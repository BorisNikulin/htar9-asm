#include "test_suite.h"
#include "interpreter.h"
#include "utils.h"
#include "haskell_facade.h"

#include <climits>
#include <cmath>
#include <random>
#include <sstream>
#include <stdexcept>

class MultTest : public test::Test
{
public:

  MultTest(CPU::InterpreterSupervisor & env, std::default_random_engine &
    generator) : test::Test("mult"), env(env), generator(generator)
  {
  }

  ~MultTest() = default;

  void pre()
  {
    std::uniform_int_distribution<int> byteDistribution(0,255);

    a = byteDistribution(generator);
    b = byteDistribution(generator);
    c = byteDistribution(generator);

    env.setMemory(1, a);
    env.setMemory(2, b);
    env.setMemory(3, c);

    env.setInit(true);

    env.executeNext(); // skip fin, if at it - else we're just re-clearing ra

    env.setInit(false);
  }

  test::TestResult run()
  {
    while(!env.isDone())
    {
      env.executeNext();
    }

    int ans = (env.getMemory(4) << 8) + env.getMemory(5);
    int expected = (a * b * c) & 0b1111'1111'1111'1111;

    bool pass;
    std::stringstream msgStream;
    if(ans == expected)
    {
      pass = true;
    }
    else
    {
      pass = false;
      msgStream << "In computation of " << a << " * " << b << " * " << c <<
        "\n";
      msgStream << "Expected: " << expected << ", ";
      msgStream << "got: " << ans << "\n";
    }

    return test::TestResult{ msgStream.str(), pass };
  }

private:
  CPU::InterpreterSupervisor & env;
  std::default_random_engine & generator;
  uint8_t a, b, c;
};

class StringTest : public test::Test
{
public:

  StringTest(CPU::InterpreterSupervisor & env, std::default_random_engine &
    generator) : test::Test("string"), env(env),
    generator(generator)
  {
  }

  ~StringTest() = default;

  void pre()
  {
    std::uniform_int_distribution<int> byteDistribution(0,255);
    std::uniform_int_distribution<int> patternDistribution(0, 15);

    pattern = patternDistribution(generator);

    env.setMemory(6, pattern);

    for(int i = STRING_START; i < STRING_START + STRING_SIZE; i++)
    {
      env.setMemory(i, byteDistribution(generator));
    }

    env.setInit(true);

    env.executeNext(); // skip fin, if at it - else we're just re-clearing ra

    env.setInit(false);
  }

  test::TestResult run()
  {
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
      for(int j = 0; j < 5; j++)
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

    bool pass;
    std::stringstream msgStream;
    if(ans == expected)
    {
      pass = true;
    }
    else
    {
      pass = false;
      msgStream << "In string compare\n";
      msgStream << "Expected: " << expected << ", ";
      msgStream << "got: " << ans << "\n";
    }

    return test::TestResult{ msgStream.str(), pass };
  }

private:
  static const int STRING_START = 32;
  static const int STRING_SIZE = 64;

  CPU::InterpreterSupervisor & env;
  std::default_random_engine & generator;

  int pattern;
};

class PairTest : public test::Test
{
public:

  PairTest(CPU::InterpreterSupervisor & env, std::default_random_engine &
    generator) : test::Test("pair"), env(env),
    generator(generator)
  {
  }

  ~PairTest() = default;

  void pre()
  {
    std::uniform_int_distribution<int8_t> byteDistribution(-128, 127);

    for(int i = ARRAY_START; i < ARRAY_START + ARRAY_SIZE; i++)
    {
      int8_t val = byteDistribution(generator);
      env.setMemory(i, val);
    }

    env.setInit(true);

    env.executeNext(); // skip fin, if at it - else we're just re-clearing ra

    env.setInit(false);
  }

  test::TestResult run()
  {
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

    bool pass;
    std::stringstream msgStream;
    if(ans == expected)
    {
      pass = true;
    }
    else
    {
      pass = false;
      msgStream << "In pair distance\n";
      msgStream << "Expected: " << expected << ", ";
      msgStream << "got: " << ans << "\n";
    }

    return test::TestResult{ msgStream.str(), pass };
  }

private:
  static const int ARRAY_START = 128;
  static const int ARRAY_SIZE = 20;

  CPU::InterpreterSupervisor & env;
  std::default_random_engine & generator;

  int pattern;
};
