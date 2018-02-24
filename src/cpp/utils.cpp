#include "utils.h"

#include <fstream>
#include <iostream>
#include <stdexcept>

namespace utils {

  /**
   * Reads a file into a C string, returns a unique pointer to that string
   *
   * @param fname String containing path to file to read
   */

  std::string FileManager::readFile(const std::string & fname)
  {
    std::ifstream ifs;
    ifs.open(fname);

    if(!ifs.good())
    {
      throw std::runtime_error("Could not open requested input file.");
    }

    std::string content((std::istreambuf_iterator<char>(ifs)),
      (std::istreambuf_iterator<char>()));

    return content;
  }

  void FileManager::writeCode(const std::string & outfile, const std::string &
    code, bool formatted)
  {
    // Open outfile for writing
    std::ofstream ofs;
    ofs.open(outfile);

    std::cout << "Writing result to \"" << outfile << (formatted ?
      "\" as formatted binary." : "\" as unformatted binary.") << std::endl;

    if(!ofs.good())
    {
      throw std::runtime_error("Could not write to specified file.");
    }

    // Raw output
    if(!formatted)
    {
      ofs << code << std::endl;
    }
    // Formatted output - insert an underscore every 3rd bit, and a newline
    // every 9th
    else
    {
      for(std::size_t i = 0; i < code.size();)
      {
        std::string sub = code.substr(i, 3);
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
  }
}
