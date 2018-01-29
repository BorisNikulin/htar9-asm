#include "interpreter.h"
#include "argument_parser.h"
#include "haskell_facade.h"

#include <iostream>
#include <fstream>
#include <memory>
#include <exception>
#include <utility>

std::unique_ptr<char[]> readFile(const std::string & fname)
{
  std::ifstream ifs;
  int length;
  ifs.open(fname);

  if(!ifs.good())
  {
    throw std::runtime_error("Could not open requested input file.");
  }

  ifs.seekg(0, std::ios::end);
  length = ifs.tellg();
  ifs.seekg(0, std::ios::beg);
  char * buffer = new char[length];
  ifs.read(buffer, length);
  ifs.close();

  return std::unique_ptr<char[]>(buffer);
}

int main(int argc, char * * argv)
{
  ArgumentParser args(argc, (const char * *)argv);

  bool output = false;
  const std::string & outfile = args.popCmdOption(std::string("-o"));

  if(outfile.empty())
  {
    std::cerr << "Requested output but no outfile provided." << std::endl;
    return -1;
  }
  else
  {
    output = true;
    std::cout << "Writing machine code to " << outfile << std::endl;
  }

  const std::vector<std::string> & remTokens = args.getRemainingTokens();

  if(remTokens.empty())
  {
    std::cerr << "No input file provided." << std::endl;
    return -1;
  }

  const std::string & infile = remTokens.front();
  std::cout << "Reading assembly from " << infile << std::endl;

  try
  {
    std::unique_ptr<char[]> fileContents = std::move(readFile(infile));

    //std::cout << fileContents.get() << std::endl;

    HaskellFacade hf(&argc, argv);

    char * result = hf.assembleFile((char *)infile.c_str(), (char *)fileContents.get());

    std::cout << result << std::endl;
  }
  catch(std::runtime_error e)
  {
    std::cerr << "\nError: " << e.what() << std::endl;
    return -1;
  }

  return 0;
}
