#include "argument_parser.h"

#include <algorithm>

/*
  Code adapted from
   https://stackoverflow.com/questions/865668/how-to-parse-command-line-arguments-in-c
 */

ArgumentParser::ArgumentParser (const int & argc, const char * * argv)
{
    for (int i=1; i < argc; ++i)
    {
        this->tokens.push_back(std::string(argv[i]));
    }
}

const std::string & ArgumentParser::getCmdOption(const std::string &
  option) const
{
    std::vector<std::string>::const_iterator itr;
    itr = std::find(this->tokens.begin(), this->tokens.end(), option);
    if(itr != this->tokens.end() && ++itr != this->tokens.end()){
        return *itr;
    }
    static const std::string empty_string("");
    return empty_string;
}

/**
 * Same as getCmdOption, but removes the flag and argument tokens from further
 * processing
 *
 * @param  option String representation of the flag to check
 * @return        String representation of the option's argument, or empty if
 *                option didn't exist
 */

std::string ArgumentParser::popCmdOption(const std::string & option)
{
  std::vector<std::string>::const_iterator itr;
  itr = std::find(this->tokens.begin(), this->tokens.end(), option);
  auto prevItr = itr;

  if(itr != this->tokens.end() && ++itr != this->tokens.end()){
      std::string result = *itr;
      this->tokens.erase(prevItr, ++itr);
      return result;
  }

  static const std::string empty_string("");
  return empty_string;
}

bool ArgumentParser::cmdOptionExists(const std::string &option) const
{
    return std::find(this->tokens.begin(), this->tokens.end(), option)
           != this->tokens.end();
}

/**
 * Return a vector of tokens that have not yet been popped
 */

const std::vector<std::string> & ArgumentParser::getRemainingTokens() const
{
  return tokens;
}
