#include "haskell_facade.h"
#include "ncurses_display.h"
#include "utils.h"
#include "cxxopts.hpp"
#include "sgr.hpp"

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
  using namespace cpp_sgr;

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

    // Only one output mode allowed
    if(args.count("output") && args.count("formatted"))
    {
      std::cerr << "Usage error: Cannot request formatted and unformatted\
 output" << std::endl;
      exit(64);
    }

    // First positional arg is read into infile, so anything in positional
    // indicates multiple input files
    if(positional.size() > 0)
    {
      std::cerr << FGColor(Color::YELLOW) <<
      "Warning:" << SGR::NONE <<
      " Multiple input files specified. Ignoring extras.\n";
    }
    // No input file specified
    else if(infile.empty())
    {
      std::cerr << FGColor(Color::RED) <<
      "Usage error: " << SGR::NONE << "no input files specified\n";
      exit(64);
    }

    std::cout << "Reading assembly from \"" << infile << "\"\n";

    // Read file into a string
    std::string fileContents = FileManager::readFile(infile);

    HaskellFacade::AssembleResult assembleResult;

    // Limit scope so HaskellFacade is destroyed as soon as possible
    {
      // Init Haskell handler
      HaskellFacade hf(&argc, argv);

      // Punt to Haskell assembler
      assembleResult = hf.assembleFile(infile.c_str(), fileContents.c_str());
    }

    // If Haskell flagged an error, report it and terminate
    if(assembleResult.status)
    {
      std::cerr << FGColor(Color::RED) << "error:\n" << SGR::NONE <<
      assembleResult.str << std::endl;
      exit(assembleResult.status);
    }

    std::cout << "Assembly complete.\n";

    // Unformatted output requested
    if(args.count("output"))
    {
      FileManager::writeCode(outfile, assembleResult.str, false);
    }
    // Formatted output requested
    else if(args.count("formatted"))
    {
      FileManager::writeCode(outfile, assembleResult.str, true);
    }

    // Interpreter run requested
    if(args.count("interpret"))
    {
      std::cout << "Starting interpreter.\n";

      CPU::Interpreter inter(CPU::CodeParser()(assembleResult.str));

      ncurses_tui::NCursesEnvironment ncurses; // start the ncurses environment

      ncurses_tui::NCursesDisplay cursesDisplay(inter);
      cursesDisplay.start();
    }

    return 0;
  }
  catch(const cxxopts::OptionException & e)
  {
    std::cerr << FGColor(Color::RED) << "Usage error: " << SGR::NONE
    << e.what() << std::endl;
    exit(64);
  }
  catch(const CPU::Interpreter::UnrecognizedInstruction & e)
  {
    std::cerr << FGColor(Color::RED) << "Error: " << SGR::NONE
    << e.what() << std::endl;
    exit(1);
  }
  catch(const std::runtime_error & e)
  {
    std::cerr << FGColor(Color::RED) << "Error: " << SGR::NONE
    << e.what() << std::endl;
    exit(-1);
  }
}
