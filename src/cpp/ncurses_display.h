#include "interpreter.h"

#include <memory>
#include <ncurses.h>

namespace ncurses_tui {

  using namespace CPU;

  struct Point
  {
  public:
    Point() : Point(0, 0) { }
    Point(int line, int col) : line(line), col(col) { }

    int line, col;
  };

  struct Dimension
  {
  public:
    Dimension() : Dimension(0, 0) { }
    Dimension(int height, int width) : height(height), width(width) { }

    int height, width;
  };

  class NCursesWindow
  {
  public:
    NCursesWindow(Point origin, Dimension dim, bool bordered);
    NCursesWindow(Dimension dim, Point origin, bool bordered);
    ~NCursesWindow() { delwin(win); delwin(superwin); }

    void printw(const std::string msg, ...) noexcept;
    void mvprintw(const Point pt, const std::string msg, ...) noexcept;
    void drawLine(const Point pt, const int n) noexcept;
    void colorOn(const int pair_id) noexcept;
    void colorOff(const int pair_id) noexcept;
    std::string readString(int ct = 0, int line = 0, int col = 0);
    void clear() noexcept;
    void hide() noexcept;

    Dimension getDimensions() const noexcept { return dim; }
  private:
    WINDOW * superwin;
    WINDOW * win;

    Dimension dim;
    bool bordered;

    void redraw() noexcept;
    void update() noexcept;
  };

  class NCursesDisplay
  {
  public:
    NCursesDisplay(Interpreter & inter);
    ~NCursesDisplay();
    void start();

  private:
    InterpreterSupervisor controller; // Interpreter controller
    int maxY, maxX; // max X and Y of the terminal
    std::vector<std::string> insnList;

    class InputHandler
    {
    public:
      InputHandler(NCursesDisplay & disp);

      void enterSingle();
      void enterConsole();

    private:
      NCursesDisplay & disp;
      bool cursesMode = false; // false = normal, true = pseudo-terminal

      bool setCursesMode(bool console) noexcept;
      bool executeCommand(const std::string cmd);
      std::vector<std::string> tokenizeCommand(const std::string cmd) const
        noexcept;
    };

    friend InputHandler;

    // Drawing constants
    static const int statusWidth = 40;
    static const int msgWidth = 60;
    static const int consoleWidth = 30;

    // ncurses windows for the display segments
    NCursesWindow * statusWin;
    NCursesWindow * consoleWin;
    NCursesWindow * msgWin;
    NCursesWindow * insnWin;

    void static initializeCurses();
    void static endCurses();
    void updateStatus();
    void stepAndUpdate(int ct = 1);
    void step();
    void runUntilDone();
  };
}
