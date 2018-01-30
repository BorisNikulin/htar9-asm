#include "argument_parser.h"
#include "haskell_facade.h"
#include "ncurses_display.h"

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

  bool output = args.cmdOptionExists("-o");
  bool formatOutput = args.cmdOptionExists("-f");

  args.popCmdOption("-f");

  const std::string & outfile = args.popCmdOption(std::string("-o"));

  if(output && outfile.empty())
  {
    std::cerr << "Requested output but no outfile provided." << std::endl;
    return -1;
  }
  else if(output)
  {
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

    HaskellFacade hf(&argc, argv);

    char * result = hf.assembleFile((char *)infile.c_str(), (char *)fileContents.get());

    std::cout << "Assembly complete. ";

    if(output)
    {
      std::ofstream ofs;
      ofs.open(outfile);

      std::cout << "Writing result to " << outfile << (formatOutput ?
        " as formatted binary." : " as unformatted binary.") << std::endl;

      if(!ofs.good())
      {
        throw std::runtime_error("Could not write to specified file: " + outfile);
      }

      std::string resStr = std::string(result);

      if(!formatOutput)
      {
        ofs << resStr << std::endl;
      }
      else
      {
        for(std::size_t i = 0; i < resStr.size();)
        {
          std::string sub = resStr.substr(i, 3);
          i += 3;
          if(i % 9 != 0)
          {
            ofs << sub << "_";
          }
          else
          {
            ofs << sub << "\n";
          }
        }
      }


      ofs.close();

      std::cout << "Write complete." << std::endl;

      return 0;
    }
    else
    {
      std::cout << "Starting emulator." << std::endl;

      NCursesDisplay curses(std::make_unique<Interpreter>(8, 256, result));
      curses.start();

      return 0;
    }
  }
  catch(std::runtime_error e)
  {
    std::cerr << "\nError: " << e.what() << std::endl;
    return -2;
  }
}
