#include "test_suite.h"
#include "sgr.hpp"

#include <iomanip>

namespace test
{
  using namespace cpp_sgr;

  SeriesResult Test::runSeries()
  {
    long numPass = 0;
    long numFail = 0;
    std::vector<std::string> messages;
    bool pass = true;

    for(long i = 0; i < ct; i++)
    {
      try
      {
        this->pre();
        TestResult tr = this->run();
        this->post();

        messages.push_back(tr.msg);
        if(tr.pass)
        {
          numPass++;
        }
        else
        {
          pass = false;
          numFail++;
        }
      }
      catch(const std::exception & e)
      {
        pass = false;
        numFail++;
        messages.push_back(e.what());
      }
      catch(...)
      {
        pass = false;
        numFail++;
        messages.push_back("Unspecified runtime error");
      }
    }

    return SeriesResult{ numPass, numFail, messages, name, pass };
  }

  TestSuite::TestSuite(std::vector<Test *> testList) :
    testList(testList)
  {
  }

  bool TestSuite::run()
  {
    bool pass = true;
    std::chrono::duration<double> total_seconds;

    unsigned long numPass = 0;
    unsigned long numFail = 0;

    for(Test * test : testList)
    {
      auto start = std::chrono::system_clock::now();

      SeriesResult result = test->runSeries();

      auto end = std::chrono::system_clock::now();

      std::chrono::duration<double> test_seconds = end - start;

      total_seconds += test_seconds;

      numPass += result.numPass;
      numFail += result.numFail;

      if(result.pass)
      {
        std::cerr << std::left << std::setw(10) << (result.name + ": ") <<
        green_fg << "OK\n" << reset;
         if(result.numPass > 1)
         {
           std::cerr << "\t" << result.numPass << " tests completed";
           if(test_seconds.count() > .01)
           {
             std::cerr << std::setprecision(2) << " (" << test_seconds.count()
             << "s)";
           }
           std::cerr << "\n";
         }
      }
      else
      {
        unsigned long testCt = result.numPass + result.numFail;

        pass = false;
        std::cerr << std::left << std::setw(10) << (result.name + ": ") <<
        b_red_fg << "FAIL\n" << reset;
        if(testCt > 1)
        {
          std::cerr << "\t" << result.numPass << " of " << (testCt) <<
          " tests passed";
          if(test_seconds.count() > .01)
          {
            std::cerr << std::setprecision(2) << " (" << test_seconds.count()
            << "s)";
          }
          std::cerr << "\n";
        }

        if(test->isVerbose())
        {
          for(auto msg : result.messages)
          {
            std::cerr << msg << "\n";
          }
        }
      }
    }

    if(pass)
    {
      std::cerr << green_fg << "\nAll " << numPass <<
      " tests passed (" << std::setprecision(2) << total_seconds.count() <<
      "s)\n" << reset;
    }
    else
    {
      std::cerr << red_fg << "\nSome tests failing. (" << std::setprecision(2)
      << total_seconds.count() << "s)\n" << reset;
      std::cerr << "Tests passed: " << green_fg << numPass << "\n" << reset;
      std::cerr << "Tests failed: " << red_fg << numFail << "\n" << reset;
    }

    return pass;
  }
}
