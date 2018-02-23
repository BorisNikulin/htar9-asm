#ifndef APP_UTILS_H
#define APP_UTILS_H

#include <string>
#include <iostream>

namespace utils {

  class FileManager
  {
  public:
    static std::string readFile(const std::string & infile);
    static void writeCode(const std::string & outfile, const std::string & code,
      bool formatted);
  };

  class Color
  {
    friend std::ostream & operator<<(std::ostream & out, const Color & c)
    {
      return out << c.code;
    }

  public:
    enum ANSIColor {
      BLACK=30, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE,
      BRIGHT_BLACK=90, BRIGHT_RED, BRIGHT_GREEN, BRIGHT_YELLOW,
      BRIGHT_BLUE, BRIGHT_MAGENTA, BRIGHT_CYAN, BRIGHT_WHITE
    };

    static const std::string NONE;

    Color(const std::string ansiString);

  protected:
    static const std::string escape;

    static bool verifyColorComponent(const int c) noexcept;
    
  private:
    std::string code;
  };

  class FGColor : public Color
  {
  public:
    FGColor(const ANSIColor c);
    FGColor(const int r, const int g, const int b);
  };

  class BGColor : public Color
  {
  public:
    BGColor(const ANSIColor c);
    BGColor(const int r, const int g, const int b);
  };

}

#endif /* end of include guard: APP_UTILS_H */
