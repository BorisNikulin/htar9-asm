#include "argument_parser.h"
#include "haskell_facade.h"
#include "ncurses_display.h"

#include <iostream>
#include <fstream>
#include <memory>
#include <exception>
#include <utility>

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

/**
 * Print program usage statement
 */

static void printUsage()
{
  std::cerr << "htar9-asm-exe" << " - HTAR9 Assembler and Interpreter\n";
  std::cerr << "Usage:" << " [-h]" << " [-f]" << " [-o outfile]" << " src_file";
  std::cerr << "\n\nsrc_file: HTAR9 assembly (.s) file";
  std::cerr << "\n\nAvailable options: \n";
  std::cerr << "-h\t\t\tShow this help text\n";
  std::cerr << "-f\t\t\tOutput machine code as formatted binary instead of unformatted - does nothing if -o not specified\n";
  std::cerr << "-o outfile\t\tOutput assembled machine code to the specified file - do not run interpreter\n";
  std::cerr << std::endl;
}


int main(int argc, char * * argv)
{
  ArgumentParser args(argc, (const char * *)argv);

  // Check if we're printing usage statement
  if(argc == 1 || args.cmdOptionExists("-h"))
  {
    printUsage();
    return 0;
  }

  // If we need to output, set flag
  bool output = args.cmdOptionExists("-o");
  // If formatted output is requested, set flag
  bool formatOutput = args.cmdOptionExists("-f");

  // Remove -f flag if present
  args.popCmdOption("-f");

  // Remove -o flag if present, return outfile if supplied
  const std::string & outfile = args.popCmdOption(std::string("-o"));

  // If output was requested (-o) and outfile is not supplied, error
  if(output && outfile.empty())
  {
    std::cerr << "Requested output but no outfile provided." << std::endl;
    return -1;
  }
  else if(output)
  {
    std::cout << "Writing machine code to " << outfile << std::endl;
  }

  // List of remaining arguments
  const std::vector<std::string> & remTokens = args.getRemainingTokens();

  // No filename provided
  if(remTokens.empty())
  {
    std::cerr << "No input file provided." << std::endl;
    return -1;
  }
  // Too many remaining arguments - issue warning, but no error
  else if(remTokens.size() > 1)
  {
    std::cerr << "Warning: multiple source files provided. Ignoring all but first." << std::endl;
  }

  // Front of list will be the source file
  const std::string & infile = remTokens.front();
  std::cout << "Reading assembly from " << infile << std::endl;

  try
  {
    // Read file into a C string
    std::unique_ptr<char[]> fileContents = std::move(readFile(infile));

    // Init haskell handler
    HaskellFacade hf(&argc, argv);

    // Punt to Haskell assembler - result is a C string of binary
    char * result = hf.assembleFile((char *)infile.c_str(), (char *)fileContents.get());

    std::cout << "Assembly complete. ";

    // Output requested
    if(output)
    {
      // Open outfile for writing
      std::ofstream ofs;
      ofs.open(outfile);

      std::cout << "Writing result to " << outfile << (formatOutput ?
        " as formatted binary." : " as unformatted binary.") << std::endl;

      if(!ofs.good())
      {
        throw std::runtime_error("Could not write to specified file: " + outfile);
      }

      // Convert result to a std::string
      std::string resStr = std::string(result);

      // Raw output
      if(!formatOutput)
      {
        ofs << resStr << std::endl;
      }
      // Formatted output - insert an underscore every 3rd bit, and a newline
      // every 9th
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
    // No output requested - start interpreter
    else
    {
      std::cout << "Starting interpreter." << std::endl;

      NCursesDisplay curses(std::unique_ptr<Interpreter>(new Interpreter(8, 256,
        result)));
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
