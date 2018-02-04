#include "interpreter.h"

#include <memory>
#include <ncurses.h>

class NCursesDisplay
{
public:
  NCursesDisplay(std::unique_ptr<Interpreter> env);
  ~NCursesDisplay();
  void start();

private:
  std::unique_ptr<Interpreter> env; // pointer to interpreter environment
  bool terminated = false; // program being run currently at its end?
  bool cursesMode = false; // false = normal, true = pseudo-terminal
  int maxY, maxX; // max X and Y of the terminal

  // Drawing constants
  static const int statusWidth = 30, statusHeight = 14;
  static const int msgWidth = 60, msgHeight = 3;
  static const int consoleWidth = 30, consoleHeight = 1;

  // ncurses windows for the display segments
  WINDOW * statusWin;
  WINDOW * consoleWin;
  WINDOW * msgWin;

  void initializeCurses();
  bool setCursesMode(bool console);
  void endCurses();
  void updateStatus();
  void showMsg(std::string msg, ...);
  void stepAndUpdate(int ct = 1);
  void updateSDisplay();
  void updateInsnDisplay();
};
