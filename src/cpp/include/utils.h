#ifndef APP_UTILS_H
#define APP_UTILS_H

#include <string>

namespace utils {

  class FileManager
  {
  public:

    /**
     * Read the given file into a string
     *
     * @param  infile File to read
     * @return        String containing file's contents
     */

    static std::string readFile(const std::string & infile);

    /**
     * Writes machine code to the specified file, optionally formatted
     *
     * @param outfile   File to write to
     * @param code      Machine code to write
     * @param formatted Flag specifying formatted input
     */

    static void writeCode(const std::string & outfile, const std::string & code,
      bool formatted = false);
  };

}

#endif /* end of include guard: APP_UTILS_H */
