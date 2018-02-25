#ifndef TEST_SUITE_H
#define TEST_SUITE_H

#include <chrono>
#include <string>
#include <vector>

namespace test
{

  struct TestResult
  {
    const std::string msg;
    const bool pass;
  };

  struct SeriesResult
  {
    const long numPass;
    const long numFail;
    const std::vector<std::string> messages;
    const std::string name;
    const bool pass;
  };

  class Test
  {
  public:
    Test(const std::string name) :
    name(name), ct(1), verbose(false) { }

    virtual SeriesResult runSeries();
    virtual TestResult run() = 0;
    virtual void pre() { };
    virtual void post() { };

    void setCount(const long ct) { this->ct = ct; }
    void setVerbose(const bool verbose) { this->verbose = verbose; }
    bool isVerbose() { return verbose; }

  private:
    const std::string name;
    long ct;
    bool verbose;
  };

  class TestSuite
  {
  public:
    TestSuite(std::vector<Test *> testList);

    ~TestSuite() = default;

    virtual bool run();
  private:
    std::vector<Test *> testList;
  };
}

#endif /* end of include guard: TEST_SUITE_H */
