/*
  Code adapted from
   https://stackoverflow.com/questions/865668/how-to-parse-command-line-arguments-in-c
 */

#pragma once

#include <string>
#include <vector>

class ArgumentParser {
    public:
        ArgumentParser(const int & argc, const char ** argv);

        const std::string & getCmdOption(const std::string & option) const;
        std::string popCmdOption(const std::string & option);

        bool cmdOptionExists(const std::string &option) const;

        const std::vector<std::string> & getRemainingTokens() const;
    private:
        std::vector <std::string> tokens;
};
