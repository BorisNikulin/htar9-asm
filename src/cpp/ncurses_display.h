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
  std::unique_ptr<Interpreter> env;
  bool terminated = false;
  bool cursesMode = false;
  int maxY, maxX;
  static const int statusWidth = 30, statusHeight = 14;
  static const int msgWidth = 60, msgHeight = 3;
  static const int consoleWidth = 30, consoleHeight = 1;
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
