#include "haskell_facade.h"
#include "ncurses_display.h"
#include "utils.h"
#include "cxxopts.hpp"

#include <getopt.h>
#include <cstring>
#include <iostream>
#include <fstream>
#include <memory>
#include <exception>
#include <utility>

int main(int argc, char * * argv)
{
  using namespace utils;

  std::vector<std::string> positional;
  std::string infile;
  std::string outfile;

  cxxopts::Options options("htar9-asm-exe", "\nAssembler front end and \
interpreter for HTAR9 microarchitecture");

  options.add_options()
  ("h,help", "Show this usage message")
  ("positional", "Input files", cxxopts::value(positional))
  ("input", "Input file", cxxopts::value(infile), "INFILE")
  ;

  options.add_options("Interpreter")
  ("i,interpret", "Start interpreter after successful assembly")
  ;

  options.add_options("Output")
  ("o,output", "Write unformatted machine code to OUTFILE",
    cxxopts::value(outfile), "OUTFILE")
  ("f,formatted", "Write formatted machine code to OUTFILE",
    cxxopts::value(outfile), "OUTFILE")
  ;

  options.parse_positional({"input", "positional"});
  options.positional_help("input file");

  options.custom_help("[-i] [-o OUTFILE | -f OUTFILE] [-h] [--input]");

  try
  {
    auto args = options.parse(argc, argv);

    if(args.count("help"))
    {
      std::cerr << options.help({"", "Output", "Interpreter"}) << "\n";
      exit(0);
    }

    if(args.count("output") && args.count("formatted"))
    {
      std::cerr << "Usage error: Cannot request formatted and unformatted\
 output" << std::endl;
      exit(64);
    }

    if(positional.size() > 0)
    {
      std::cerr << FGColor(Color::YELLOW) <<
      "Warning: Multiple input files specified. Ignoring extras.\n" <<
      Color(Color::NONE);
    }

    std::cout << "Reading assembly from \"" << infile << "\"\n";

    // Read file into a string
    std::string fileContents = FileManager::readFile(infile);

    std::string result;
    int status = 0;
    {
      // Init Haskell handler
      HaskellFacade hf(&argc, argv);

      // Punt to Haskell assembler - result is a C string of binary
      result = hf.assembleFile(infile.c_str(), fileContents.c_str(),
        &status);
    }

    if(status)
    {
      std::cerr << FGColor(Color::RED) << "error:\n" << Color(Color::NONE) <<
      result << std::endl;
      exit(status);
    }

    std::cout << "Assembly complete.\n";

    // Output requested
    if(args.count("output"))
    {
      FileManager::writeCode(outfile, result, false);
    }
    else if(args.count("formatted"))
    {
      FileManager::writeCode(outfile, result, true);
    }

    if(args.count("interpret"))
    {
      std::cout << "Starting interpreter." << std::endl;

      CPU::Interpreter inter(CPU::CodeParser()(result));

      ncurses_tui::NCursesDisplay curses(inter);
      curses.start();
    }

    return 0;
  }
  catch(const cxxopts::OptionException& e)
  {
    std::cerr << FGColor(Color::RED) << "Usage error: " << Color(Color::NONE)
    << e.what() << std::endl;
    exit(64);
  }
  catch(const std::runtime_error & e)
  {
    std::cerr << FGColor(Color::RED) << "Error: " << Color(Color::NONE)
    << e.what() << std::endl;
    exit(1);
  }
}
