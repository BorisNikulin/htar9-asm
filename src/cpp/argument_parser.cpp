#include "argument_parser.h"

#include <algorithm>

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

const std::vector<std::string> & ArgumentParser::getRemainingTokens() const
{
  return tokens;
}
