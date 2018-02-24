#include "ncurses_display.h"

#include <exception>
#include <stdexcept>
#include <iostream>
#include <cstdarg>

namespace ncurses_tui {

  NCursesWindow::NCursesWindow(Point origin, Dimension dim, bool bordered) :
  dim(dim), bordered(bordered)
  {
    // stdscr is initialized by a call to initscr(), else it is NULL
    if(!stdscr)
    {
      throw std::logic_error("Attempt to create ncurses window prior to \
initscr() call");
    }

    superwin = newwin(dim.height, dim.width, origin.line, origin.col);

    // if window is to have a border, create a subwindow one pixel smaller on
    // all sides and parent it to the superwindow that will hold the actual
    // border; this is so the border is never overwritten
    if(bordered)
    {
      win = derwin(superwin, dim.height - 2, dim.width - 2, 1, 1);
    }
    // if no border, then no reason for a subwindow
    else
    {
      win = superwin;
    }
  }

  NCursesWindow::NCursesWindow(Dimension dim, Point origin, bool bordered) :
  NCursesWindow(origin, dim, bordered)
  {
  }

  void NCursesWindow::printw(const std::string msg, ...) noexcept
  {
    va_list arglist;

    va_start(arglist, msg);

    wmove(win, 0, 0);
    vw_printw(win, msg.c_str(), arglist); // printw version taking a va_list

    va_end(arglist);

    redraw();
  }

  /**
   * wrapper for mvwprintw - move to location in window and print message
   *
   * @param pt      Point describing location in window at which to print
   * @param msg     Format string of message
   * @param VARARGS Variables for format specifier values
   */

  void NCursesWindow::mvprintw(const Point pt, std::string msg, ...) noexcept
  {
    va_list arglist;

    va_start(arglist, msg);

    wmove(win, pt.line, pt.col);
    vw_printw(win, msg.c_str(), arglist); // printw version taking a va_list

    va_end(arglist);

    redraw();
  }

  void NCursesWindow::drawVLine(const Point pt, const int n) noexcept
  {
    mvwvline(win, pt.line, pt.col, 0, n);

    redraw();
  }

  void NCursesWindow::colorOn(const int pair_id) noexcept
  {
    wattron(win, COLOR_PAIR(pair_id));
  }

  void NCursesWindow::colorOff(const int pair_id) noexcept
  {
    wattroff(win, COLOR_PAIR(pair_id));
  }

  std::string NCursesWindow::readString(const int ct, Point pt) const noexcept
  {
    int bufsize;
    if(ct <= 0)
    {
      bufsize = 4096;
    }
    else
    {
      bufsize = ct + 1;
    }

    // buffer for input
    char str[bufsize];
    mvwgetnstr(win, pt.line, pt.col, str, bufsize - 1);

    return std::string(str);
  }

  void NCursesWindow::clear() noexcept
  {
    werase(win);

    update();
  }

  void NCursesWindow::hide() noexcept
  {
    werase(superwin);

    update();
  }

  void NCursesWindow::redraw() noexcept
  {
    if(bordered)
    {
      box(superwin, 0, 0);
    }

    update();
  }

  void NCursesWindow::update() noexcept
  {
    refresh();

    touchwin(superwin);
    wnoutrefresh(win);
    wnoutrefresh(superwin);
    doupdate();
  }

  /**
   * Construct the display, init ncurses, setup windows
   *
   * @param env Pointer to Interpreter environment - display will take
   * ownership of the object
   */

  NCursesDisplay::NCursesDisplay(Interpreter & inter) : controller(inter),
  insnList(controller.getInstructions()),
  statusWin(Dimension{ height: 15, width: statusWidth },
    Point{line: 1, col: 3 * getmaxx(stdscr) / 4 - statusWidth / 2 }, true),
  consoleWin(Dimension{ height: 1, width: consoleWidth },
    Point{line: 26, col: getmaxx(stdscr) / 2 - consoleWidth / 2 }, false),
  msgWin(Dimension{ height: 3, width: msgWidth }, Point{
    line: 20, col: getmaxx(stdscr) / 2 - msgWidth / 2 }, true),
  insnWin(Dimension{ height: 15, width: statusWidth },
    Point{line: 1, col: 1 * getmaxx(stdscr) / 4 - statusWidth / 2 }, true)
  {
  }

  /**
   * Destructor - kill windows and shutdown ncurses
   */

  NCursesDisplay::~NCursesDisplay()
  {
  }

  /**
   * Start task loop for user input
   */

  void NCursesDisplay::start()
  {
    // Print usage, pause for one key, clear screen, draw initial
    // status window

    mvprintw(0, 0, "Usage: \n\n");
    printw("Press q to quit.\n");
    printw("Press s to step forward one instruction.\n");
    printw("Press n to toggle the init line on the CPU.\n");
    printw("Press r to reset the program counter to 0.\n\n");

    printw("Press i to open a console for more complex commands: \n");
    printw("'setm address value' to set memory values.\n");
    printw("'readm address' to read memory values.\n");
    printw("'continue' to run until done flag is raised.\n");
    printw("'exit' to close console.\n\n");

    printw("Press any key to continue.");

    getch();

    clear();

    updateStatus();

    InputHandler handler(*this);

    handler.enterSingle();
  }

  void NCursesDisplay::stepAndUpdate(int ct)
  {
    try {
      for(; ct > 0; ct--)
      {
        step();
      }
      // update status window once after all steps complete
      updateStatus();
    }
    catch(const CPU::Interpreter::InvalidPC & e)
    {
      msgWin.clear();
      msgWin.printw(e.what());
    }
  }

  void NCursesDisplay::step()
  {
    controller.executeNext();
  }

  void NCursesDisplay::runUntilDone()
  {
    while(!controller.isDone())
    {
      step();
    }
    updateStatus();
  }

  /**
   * Update and redraw the status window
   */

  void NCursesDisplay::updateStatus()
  {
    for(int i = 0; i < 8; i++)
    {
      statusWin.mvprintw(Point(i, 0), "r%d = %u (%#x)\n", i,
        controller.getRegister(i), controller.getRegister(i));
    }

    int pc = controller.getPC();

    statusWin.mvprintw(Point(9, 0), "pc = %u (%#x)\n",pc,
      controller.getPC());
    statusWin.mvprintw(Point(10, 0), "s = %u\n", controller.getS());
    statusWin.mvprintw(Point(11, 0), "init = %u\n", controller.getInit());
    statusWin.mvprintw(Point(12, 0), "done = %u\n", controller.isDone());

    insnWin.clear();

    for(int i = pc - (13/2); i <= pc + (13/2); i++)
    {
      if(i < 0 || i >= (int)insnList.size())
      {
        continue;
      }

      if(i == pc)
      {
        insnWin.colorOn(2);
      }

      insnWin.mvprintw(Point(i - (pc - (13/2)), 0), "%3u   %s", i,
        insnList[i].c_str());

      insnWin.colorOff(2);
    }

    insnWin.drawVLine(Point(0, 4), 13);
  }

  NCursesEnvironment::NCursesEnvironment()
  {
    if(initscr() == NULL)
    {
      throw NCursesError();
    }

    if(has_colors() != 0)
    {
      start_color();
      init_pair(1, COLOR_WHITE, COLOR_BLACK);
      init_pair(2, COLOR_GREEN, COLOR_BLACK);

      if(can_change_color())
      {
        init_color(COLOR_GREEN, 0, 1000, 0);
      }
    }

    attron(COLOR_PAIR(1));

    if(cbreak() == ERR || noecho() == ERR || curs_set(0) == ERR ||
      keypad(stdscr, true) == ERR)
    {
        throw NCursesError();
    }
  }

  NCursesEnvironment::~NCursesEnvironment()
  {
    // no check to ensure this succeeds - if it doesn't, we'd just terminate
    // anyway
    endwin();
  }

  NCursesDisplay::InputHandler::InputHandler(NCursesDisplay & disp) : disp(disp)
  {
  }

  void NCursesDisplay::InputHandler::enterSingle()
  {
    bool done = false;

    while(!done)
    {
      int key = getch();

      switch(key)
      {
        // Quit
        case 'q':
          done = true;
          break;
        // Step
        case 's':
          disp.stepAndUpdate();
          break;
        // Input (pseudo-console mode)
        case 'i':
          this->enterConsole();
          break;
        // Reset PC
        case 'r':
          disp.controller.resetPC();
          disp.updateStatus();
          disp.msgWin.clear();
          break;
        // Toggle init flag
        case 'n':
          disp.controller.setInit(!disp.controller.getInit());
          disp.updateStatus();
          break;
      }
    }
  }

  void NCursesDisplay::InputHandler::enterConsole()
  {
    setCursesMode(true);

    bool done = false;

    while(!done)
    {
      disp.consoleWin.clear();
      disp.consoleWin.printw("$ > "); // print prompt

      // unsure why we need to specify the coordinates - otherwise the cursor
      // sits at the start of the line until you start typing, then jumps to the
      // correct place
      std::string input = disp.consoleWin.readString(30, Point(0, 4));

      done = executeCommand(input);
    }

    disp.msgWin.hide();
    disp.consoleWin.hide();

    setCursesMode(false);
  }

  bool NCursesDisplay::InputHandler::executeCommand(const std::string cmd)
  {
    std::vector<std::string> cmdTokens = tokenizeCommand(cmd);

    if(cmdTokens.size() == 0)
    {
      return false;
    }
    else
    {
      std::string op = cmdTokens[0];

      if(op.compare("readm") == 0)
      {
        if(cmdTokens.size() != 2)
        {
          disp.msgWin.clear();
          disp.msgWin.printw("Invalid number of arguments to readm");
          return false;
        }

        long addr = std::stoi(cmdTokens[1], nullptr, 10);
        try {
          word val = disp.controller.getMemory(addr);

          disp.msgWin.clear();
          disp.msgWin.printw("Memory address %u has value %u\n", addr, val);
        }
        catch(const std::out_of_range & e)
        {
          disp.msgWin.printw("Invalid memory address");
        }
      }
      else if(op.compare("setm") == 0)
      {
        if(cmdTokens.size() != 3)
        {
          disp.msgWin.clear();
          disp.msgWin.printw("Invalid number of arguments to setm");
          return false;
        }

        long addr = std::stoi(cmdTokens[1], nullptr, 10);
        long val = std::stoi(cmdTokens[2], nullptr, 10);

        try {
          disp.controller.setMemory(addr, val);

          disp.msgWin.clear();
          disp.msgWin.printw("Set memory address %u to value %u\n", addr, val);
        }
        catch(const std::out_of_range & e)
        {
          disp.msgWin.clear();
          disp.msgWin.printw("Invalid memory address");
        }
      }
      else if(op.compare("exit") == 0 || op.compare("quit") == 0 ||
        op.compare("q") == 0)
      {
        return true;
      }
      else if(op.compare("continue") == 0)
      {
        disp.runUntilDone();
      }
      else
      {
        disp.msgWin.clear();
        disp.msgWin.mvprintw(Point(0, 0), "Unrecognized command");
      }
    }
    return false;
  }

  bool NCursesDisplay::InputHandler::setCursesMode(bool console) noexcept
  {
    if(console)
    {
      nocbreak();
      echo();
      curs_set(1);
    }
    else
    {
      cbreak();
      noecho();
      curs_set(0);
    }

    bool modeTemp = cursesMode;
    cursesMode = console;
    return modeTemp;
  }

  std::vector<std::string> NCursesDisplay::InputHandler::tokenizeCommand(
    const std::string cmd) const noexcept
  {
    std::vector<std::string> elems;

    std::stringstream sstream(cmd);
    std::string item;
    while (std::getline(sstream, item, ' '))
    {
      elems.push_back(item);
    }

    return elems;
  }
}
