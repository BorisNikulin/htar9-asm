#include "haskell_facade.h"
#include "ncurses_display.h"

#include <getopt.h>
#include <cstring>
#include <iostream>
#include <fstream>
#include <memory>
#include <exception>
#include <utility>

#ifndef OUT_FLAG
#define OUT_FLAG 0x2
#endif

#ifndef FORM_FLAG
#define FORM_FLAG 0x1
#endif

/**
 * Print program usage statement
 */

static void printUsage()
{
  std::cerr << "htar9-asm-exe" << " - HTAR9 Assembler and Interpreter\n";
  std::cerr << "Usage:" << " [-h]" << " [-f]" << " [-o outfile]" << " src_file";
  std::cerr << "\n\nsrc_file: HTAR9 assembly (.s) file";
  std::cerr << "\n\nAvailable options: \n";
  std::cerr << "-h --help\t\t\tShow this help text\n";
  std::cerr << "-f --formatted\t\t\tOutput machine code as formatted binary instead of unformatted - does nothing if -o not specified\n";
  std::cerr << "-o --output outfile\t\tOutput assembled machine code to the specified file - do not run interpreter\n";
  std::cerr << std::endl;
}

/**
 * Parse command line arguments for use by main
 *
 * @param argc    argc from main
 * @param argv    argv from main
 * @param flags   pointer to flag integer
 * @param outfile pointer to c-string - will contain outfile, if any
 * @param infile  pointer to c-string - will contain infile
 */

static void parseArgs(int argc, char * * argv, int * flags, char * * outfile,
  char * * infile)
{
  struct option long_options[] =
  {
    {"help",      no_argument,        0,    'h' },
    {"formatted", no_argument,        0,    'f' },
    {"output",    required_argument,  0,    'o' },
    {0,           0,                  0,     0  }
  };

  int c;

  while(1)
  {
    int option_index = 0;
    c = getopt_long(argc, argv, "hfo:", long_options, &option_index);

    if(c == -1)
    {
      break;
    }
    else
    {
      switch(c)
      {
        // output
        case 'o':
          *flags |= OUT_FLAG;
          *outfile = new char[strlen(optarg)];
          strcpy(*outfile, optarg);
          break;
        // formatted output
        case 'f':
          *flags |= FORM_FLAG;
          break;
        // help
        case 'h':
          printUsage();
          exit(0);
        // unrecognized arg
        case '?':
          break;
        default:
          abort();
      }
    }
  }

  if(!(*flags & OUT_FLAG) && (*flags & FORM_FLAG))
  {
    std::cerr << "Warning: formatted output requested but -o not provided" << std::endl;
  }

  // remaining non-option arguments
  if(optind < argc)
  {
    *infile = new char[strlen(argv[optind])];
    strcpy(*infile, argv[optind]);
  }
  // no remaining args
  else
  {
    std::cerr << "Error: No input file specified." << std::endl;
    exit(64); // usage error
  }
}

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

int main(int argc, char * * argv)
{
  int flags;
  // Pointer to c-string containing output filename
  char * outfile;
  // Pointer to c-string containing input filename
  char * infile;

  parseArgs(argc, argv, &flags, &outfile, &infile);

  // If we need to output, set flag
  bool output = flags & OUT_FLAG;
  // If formatted output is requested, set flag
  bool formatOutput = flags & FORM_FLAG;

  // If output was requested (-o) and outfile is not supplied, error
  if(output)
  {
    std::cout << "Writing machine code to " << outfile << std::endl;
  }

  std::cout << "Reading assembly from " << infile << std::endl;

  try
  {
    // Read file into a C string
    std::unique_ptr<char[]> fileContents = std::move(readFile(infile));

    int status;

    // Init haskell handler
    HaskellFacade hf(&argc, argv);

    // Punt to Haskell assembler - result is a C string of binary
    char * result = hf.assembleFile((char *)infile, (char *)fileContents.get(), &status);

    delete[] infile;

    if(status)
    {
      std::cerr << "error:\n" << result << std::endl;
      exit(status);
    }

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
        throw std::runtime_error("Could not write to specified file");
      }

      delete[] outfile;

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
