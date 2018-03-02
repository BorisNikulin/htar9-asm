#ifndef NCURSES_DISPLAY_H
#define NCURSES_DISPLAY_H

#include "interpreter.h"

#include <memory>
#include <ncurses.h>

namespace ncurses_tui {

  using namespace CPU;

  struct NCursesError : public std::exception
  {
    const char * what() const throw()
    {
      return "ncurses could not be properly configured.";
    }
  };

  /**
   * Represents a (line, col) tuple for use with ncurses functions
   */

  struct Point
  {
  public:

    /**
     * Construct a Point with the given line no. and column no.
     *
     * @param line Line number
     * @param col  Column number
     */

    Point(int line, int col) : line(line), col(col) { }
    Point() : Point(0, 0) { }

    int line, col;
  };

  /**
   * Represents a (height, width) tuple for use with ncurses functions
   */

  struct Dimension
  {
  public:

    /**
     * Construct a Dimension with the given width and height
     *
     * @param height Height
     * @param width  Width
     */

    Dimension(int height, int width) : height(height), width(width) { }

    int height, width;
  };

  /**
   * Resource manager for the ncurses runtime environment - ensures
   * ncurses initialization and shutdown
   */

  class NCursesEnvironment
  {
  public:
    NCursesEnvironment();
    ~NCursesEnvironment();
  };

  /**
   * Wrapper for an ncurses WINDOW; disposal is automatically managed by
   * the class destructor and functions are provided to manipulate the
   * underlying WINDOW
   */

  class NCursesWindow
  {
  public:

    /**
     * Constructs a new window at the given Point with the given dimensions;
     * a border is created if specified, and window contents will be placed
     * inside the border
     *
     * @param origin   Location of window
     * @param dim      Dimensions of window (including border, if applicable)
     * @param bordered (optional - default false) True if border, else false
     */

    NCursesWindow(Point origin, Dimension dim, bool bordered = false);

    /**
     * Alternate argument ordering for main constructor
     *
     * @param dim      Dimensions of window (including border, if applicable)
     * @param origin   Location of window
     * @param bordered (optional - default false) True if border, else false
     */

    NCursesWindow(Dimension dim, Point origin, bool bordered = false);

    /**
     * Cleans up WINDOW resources on destruction
     */

    ~NCursesWindow()
    {
      if(win == superwin)
      {
        delwin(win);
      }
      else
      {
        delwin(win);
        delwin(superwin);
      }
    }

    /**
     * wprintw-like function to show message in the window at (0,0)
     *
     * @param msg     Format string of message
     * @param VARARGS Variables for format specifier values
     */

    void printw(const std::string msg, ...) noexcept;

    /**
     * mvwprintw-like function to show message in the window at pt
     *
     * @param pt      Point at which to display message, within the window
     * @param msg     Format string of message
     * @param VARARGS Variables for format specifier values
     */

    void mvprintw(const Point pt, const std::string msg, ...) noexcept;

    /**
     * vline wrapper; draw a vertical line at the given point in the window
     * with the given length
     *
     * @param pt Point at which to draw line
     * @param n  Line length
     */

    void drawVLine(const Point pt, const int n) noexcept;

    /**
     * Start drawing with the specified color pair
     *
     * @param pair_id Color pair id
     */

    void colorOn(const int pair_id) noexcept;

    /**
     * Disable drawing with the specified color pair
     *
     * @param pair_id Color pair id
     */

    void colorOff(const int pair_id) noexcept;

    /**
     * Move the cursor to the given point within the window and read a string
     * of up to length ct
     *
     * @param  ct Maximum number of characters to read
     * @param  pt Point at which to move cursor (default (0, 0))
     * @return    string that was read
     */

    std::string readString(const int ct = 4096, Point pt = Point(0, 0)) const
      noexcept;

    /**
     * Clear the contents of the window (border unaffected if applicable)
     */

    void clear() noexcept;

    /**
     * Clear the window, including border
     */

    void hide() noexcept;

    /**
     * @return Dimensions of this window
     */

    Dimension getDimensions() const noexcept { return dim; }
  private:
    WINDOW * superwin;
    WINDOW * win;

    Dimension dim;
    bool bordered;

    /**
     * Redraw the border, if applicable, then update
     */

    void redraw() noexcept;

    /**
     * Refresh appropriate windows so that the window appears onscreen
     */

    void update() noexcept;
  };

  class NCursesDisplay
  {
  public:

    /**
     * Construct an NCursesDisplay around the given Interpreter
     *
     * @param inter Interpreter to be controlled by the display
     */

    NCursesDisplay(Interpreter & inter);

    /**
     * Destroy all created windows upon destruction, then shut down ncurses
     */

    ~NCursesDisplay();

    /**
     * Start looping for user input
     */

    void start();

  private:

    InterpreterSupervisor controller; // Interpreter controller
    int maxY, maxX; // max X and Y of the terminal
    std::vector<std::string> insnList;

    /**
     * Class to which input handling is delegated by the NCursesDisplay - has
     * full access to display's internals
     */

    class InputHandler
    {
    public:

      /**
       * Construct an InputHandler to manage input for the given display
       *
       * @param disp Display for which to manage input
       */

      InputHandler(NCursesDisplay & disp);

      /**
       * Loop for single-key inputs
       */

      void enterSingle();

      /**
       * Enter console mode and loop for string input
       */

      void enterConsole();

    private:
      NCursesDisplay & disp;
      bool cursesMode = false; // false = normal, true = pseudo-terminal

      /**
       * Sets ncurses to either emulate a console (echo, line-buffering,
       *  cursor) or operate normally (no echo, raw input, no cursor)
       *
       * @param  console true to emulate console, else false
       * @return         previous curses mode
       */

      bool setCursesMode(bool console) noexcept;

      /**
       * Execute the given command on the environment
       *
       * @param  cmd String representation of command to run
       * @return     True if command flagged exit console, else false
       */

      bool executeCommand(const std::string cmd);

      /**
       * Splits the command string by spaces
       *
       * @param cmd String representation of command to be split
       */

      std::vector<std::string> tokenizeCommand(const std::string cmd) const
        noexcept;
    };

    friend InputHandler; // Delegate input handling to this class, which still
                         // needs access to the interpreter and display

    // Drawing constants
    static const int statusWidth = 40;
    static const int msgWidth = 60;
    static const int consoleWidth = 30;

    // ncurses windows for the display segments
    NCursesWindow statusWin;
    NCursesWindow consoleWin;
    NCursesWindow msgWin;
    NCursesWindow insnWin;

    /**
     * Update the status display (registers, flags, instruction list)
     */

    void updateStatus();

    /**
     * Execute ct machine cycles and update the status display
     *
     * @param ct Number of machine cycles to execute
     */

    void stepAndUpdate(int ct = 1);

    /**
     * Execute a single machine cycle
     */

    void step();

    /**
     * Continue executing machine cycles until the CPU raises the done line
     */

    void runUntilDone();
  };
}


#endif /* end of include guard: NCURSES_DISPLAY_H */
