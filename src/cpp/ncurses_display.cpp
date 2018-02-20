#include "ncurses_display.h"

#include <exception>
#include <stdexcept>
#include <iostream>
#include <cstdarg>

namespace ncurses_tui {

  NCursesWindow::NCursesWindow(Point origin, Dimension dim, bool bordered) :
  bordered(bordered)
  {
    if(!stdscr)
    {
      throw std::logic_error("Attempt to create ncurses window prior to \
initscr() call");
    }

    superwin = newwin(dim.height, dim.width, origin.line, origin.col);
    if(bordered)
    {
      win = derwin(superwin, dim.height - 2, dim.width - 2, 1, 1);
    }
    else
    {
      win = derwin(superwin, dim.height, dim.width, 0, 0);
    }
  }

  NCursesWindow::NCursesWindow(Dimension dim, Point origin, bool bordered) :
  NCursesWindow(origin, dim, bordered)
  {
  }

  /**
   * printf-like function to show message in the message window
   *
   * @param msg     Format string of message
   * @param VARARGS Variables for format specifier values
   */

  void NCursesWindow::printw(const std::string msg, ...) noexcept
  {
    // use the v-flavor of snprintf to generate the string,
    // as ncurses does not have v-variants of its print functions
    char vbuffer[100];
    va_list arglist;

    va_start(arglist, msg);

    vsnprintf(vbuffer, 100, msg.c_str(), arglist);

    // print generated string
    mvwprintw(win, 0, 0, vbuffer);

    va_end(arglist);

    redraw();
  }

  void NCursesWindow::mvprintw(const Point pt, std::string msg, ...) noexcept
  {
    // use the v-flavor of snprintf to generate the string,
    // as ncurses does not have v-variants of its print functions
    char vbuffer[100];
    va_list arglist;

    va_start(arglist, msg);

    vsnprintf(vbuffer, 100, msg.c_str(), arglist);

    // print generated string
    mvwprintw(win, pt.line, pt.col, vbuffer);

    va_end(arglist);

    redraw();
  }

  void NCursesWindow::drawLine(const Point pt, const int n) noexcept
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

  std::string NCursesWindow::readString(int ct, int line, int col)
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
    mvwgetnstr(win, line, col, str, bufsize - 1);

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
  insnList(controller.getInstructions())
  {
    initializeCurses();

    getmaxyx(stdscr, maxY, maxX);

    // Windows are horizontally centered
    statusWin = new NCursesWindow(Dimension{ height: 15, width: statusWidth },
      Point{line: 1, col: 3 * maxX / 4 - statusWidth / 2 }, true);

    insnWin = new NCursesWindow(Dimension{ height: 15, width: statusWidth },
      Point{line: 1, col: 1 * maxX / 4 - statusWidth / 2 }, true);

    msgWin = new NCursesWindow(Dimension{ height: 3, width: msgWidth }, Point{
      line: 20, col: maxX / 2 - msgWidth / 2 }, true);

    consoleWin = new NCursesWindow(Dimension{ height: 1, width: consoleWidth },
      Point{line: 26, col: maxX / 2 - consoleWidth / 2 }, false);
  }

  /**
   * Destructor - kill windows and shutdown ncurses
   */

  NCursesDisplay::~NCursesDisplay()
  {
    delete statusWin;
    delete insnWin;
    delete consoleWin;
    delete msgWin;

    endCurses();
  }

  /**
   * Start task loop for user input
   */

  void NCursesDisplay::start()
  {
    // Print usage, pause for one key, clear screen, draw initial
    // status window

    mvprintw(0, 0, "Usage: \n\nPress q to quit.\nPress s to step forward one \
instruction.\nPress i to open a command console for more complex commands.\
\nUse 'setm address value' to set memory values.\nUse 'readm address' to read \
memory values.\nType 'exit' to close console.\n\nPress any key to continue.");

    getch();

    clear();

    updateStatus();

    InputHandler handler(*this);

    handler.enterSingle();
  }

  /**
   * Execute ct instructions and update status window
   *
   * @param ct Number of instructions to execute (default 1)
   */

  void NCursesDisplay::stepAndUpdate(int ct)
  {
    for(; ct > 0; ct--)
    {
      step();
    }
    // update status window once
    updateStatus();
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
      statusWin->mvprintw(Point(i, 0), "r%d = %u (%#x)\n", i,
        controller.getRegister(i), controller.getRegister(i));
    }

    int pc = controller.getPC();

    statusWin->mvprintw(Point(9, 0), "pc = %u (%#x)\n",pc,
      controller.getPC());
    statusWin->mvprintw(Point(10, 0), "s = %u\n", controller.getS());
    statusWin->mvprintw(Point(11, 0), "init = %u\n", controller.getInit());
    statusWin->mvprintw(Point(12, 0), "done = %u\n", controller.isDone());

    insnWin->clear();

    for(int i = pc - (13/2); i <= pc + (13/2); i++)
    {
      if(i < 0 || i >= (int)insnList.size())
      {
        continue;
      }

      if(i == pc)
      {
        insnWin->colorOn(2);
      }

      insnWin->mvprintw(Point(i - (pc - (13/2)), 0), "%3u   %s", i,
        insnList[i].c_str());

      insnWin->colorOff(2);
    }

    insnWin->drawLine(Point(0, 4), 13);
  }

  /**
   * Setup ncurses environment - raw input mode, no echo, no cursor, keypad
   */

  void NCursesDisplay::initializeCurses()
  {
    if(initscr() == NULL)
    {
      throw std::runtime_error("Failed to initialize ncurses.");
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

    if(cbreak() == ERR || noecho() == ERR || curs_set(0) == ERR
      || keypad(stdscr, true) == ERR)
    {
      endCurses();
      throw std::runtime_error("Initialized ncurses but terminal could not be\
 properly configured.");
    }
  }

  /**
   * Shutdown ncurses to restore terminal state
   */

  void NCursesDisplay::endCurses()
  {
    // end ncurses
    if(endwin() == ERR)
    {
      throw std::runtime_error("Failed to shut down ncurses - terminal may be \
left in an inoperable state.");
    }
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
          disp.msgWin->clear();
          break;
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
      disp.consoleWin->clear();
      disp.consoleWin->printw("$ > "); // print prompt

      // unsure why we need to specify the coordinates - otherwise the cursor
      // sits at the start of the line until you start typing, then jumps to the
      // correct place
      std::string input = disp.consoleWin->readString(30, 0, 4);

      done = executeCommand(input);
    }

    disp.msgWin->hide();
    disp.consoleWin->hide();

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
          disp.msgWin->clear();
          disp.msgWin->printw("Invalid number of arguments to readm");
          return false;
        }

        long addr = std::stoi(cmdTokens[1], nullptr, 10);
        word val = disp.controller.getMemory(addr);

        disp.msgWin->clear();
        disp.msgWin->printw("memory address %u has value %u\n", addr, val);
      }
      else if(op.compare("setm") == 0)
      {
        if(cmdTokens.size() != 3)
        {
          disp.msgWin->printw("Invalid number of arguments to setm");
          return false;
        }

        long addr = std::stoi(cmdTokens[1], nullptr, 10);
        long val = std::stoi(cmdTokens[2], nullptr, 10);
        disp.controller.setMemory(addr, val);

        disp.msgWin->clear();
        disp.msgWin->printw("set memory address %u to value %u\n", addr, val);
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
        disp.msgWin->clear();
        disp.msgWin->mvprintw(Point(0, 0), "Unrecognized command");
      }
    }
    return false;
  }

  /**
   * Sets ncurses to either emulate a console (echo, line-buffering,
   *  cursor) or operate normally (no echo, raw input, no cursor)
   *
   * @param  console true to emulate console, else false
   * @return         previous curses mode
   */

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
