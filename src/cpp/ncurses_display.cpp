#include "ncurses_display.h"

#include <exception>
#include <iostream>
#include <cstdarg>

/**
 * Construct the display, init ncurses, setup windows
 *
 * @param env Pointer to Interpreter environment - display will take
 * ownership of the object
 */

NCursesDisplay::NCursesDisplay(std::unique_ptr<Interpreter> env) : env(std::move(env))
{
  initializeCurses();

  getmaxyx(stdscr, maxY, maxX);

  // Windows are horizontally centered
  statusWin = newwin(statusHeight, statusWidth, 1, maxX / 2 - statusWidth / 2);

  msgWin = newwin(msgHeight, msgWidth, 16, maxX / 2 - msgWidth / 2);

  consoleWin = newwin(consoleHeight, consoleWidth, 23, maxX / 2 - consoleWidth / 2);
}

/**
 * Destructor - kill windows and shutdown ncurses
 */

NCursesDisplay::~NCursesDisplay()
{

  delwin(statusWin);
  delwin(consoleWin);
  delwin(msgWin);

  endCurses();
}

/**
 * Start task loop for user input
 */

void NCursesDisplay::start()
{
  // Signal to stop looping for input
  bool done = false;

  // Print usage, pause for one key, clear screen, draw initial
  // status window

  mvprintw(0, 0, "Usage: \n\nPress q to quit.\nPress s to step forward one \
instruction.\nPress i to open a command console for more complex commands.\
\nUse 'setm address value' to set memory values.\nUse 'readm address' to read \
memory values.\nType 'exit' to close console.\n\nPress any key to continue.");

  getch();

  wclear(stdscr);

  updateStatus();

  // Loop for input

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
        stepAndUpdate();
        break;

      // Input (pseudo-console mode)
      case 'i':
        // Set ncurses mode to line-buffered, echo, show cursor
        setCursesMode(true);

        while(true)
        {
          wclear(consoleWin); // clear the previous command
          wmove(consoleWin, 1, 1); // move cursor to origin
          wprintw(consoleWin, "$ > "); // print prompt

          refresh();

          // buffer for input
          char str[50];
          wgetnstr(consoleWin, str, 50); // block for string input (line-buffered)

          // convert raw input to std::string
          std::string input(str);

          // Close console
          if(input == "exit")
          {
            break;
          }
          else
          {
            std::size_t idx;
            // read memory
            if((idx = input.find("readm")) != std::string::npos)
            {
              std::size_t addr = strtol(str + 5, nullptr, 10);
              word val = env->getMemory(addr);

              showMsg("memory address %u has value %u\n", addr, val);
            }
            // set memory
            else if((idx = input.find("setm")) != std::string::npos)
            {
              char * arg1ptr;
              std::size_t addr = strtol(str + 4, &arg1ptr, 10);
              word val = (word)strtol(arg1ptr, nullptr, 10);
              env->setMemory(addr, val);

              showMsg("set memory address %u to value %u\n", addr, val);
            }
            else
            {
              showMsg("Unrecognized command\n");
            }
          }
        }

        // return to normal ncurses operation, clear console
        setCursesMode(false);
        wclear(consoleWin);

        refresh();
        wrefresh(consoleWin);

        updateStatus();
        break;
      // Reset PC
      case 'r':
        env->resetPC();
        updateStatus();
        wclear(msgWin);
        refresh();
        wrefresh(msgWin);
        terminated = false; // program no longer at end
        break;
    }
  }
}

/**
 * Execute ct instructions and update status window
 *
 * @param ct Number of instructions to execute (default 1)
 */

void NCursesDisplay::stepAndUpdate(int ct)
{
  // Do nothing if program is at its end
  if(terminated)
  {
    return;
  }

  try
  {
    // execute ct instructions
    for(; ct > 0; ct--)
    {
      env->executeNext();
    }
    // update status window once
    updateStatus();
  }
  // Interpreter signals program completion with an exception
  catch(Interpreter::DoneInterrupt e)
  {
    showMsg("Program terminated: reload? (y/n)\n");
    int response = getch();
    switch(response)
    {
      // reset
      case 'y':
        env->resetPC();
        updateStatus();
        terminated = false;
        break;
      // do nothing - still allow terminal interaction
      default:
        terminated = true;
        showMsg("Running suspended until PC reset ('r').");
        break;
    }
  }
}

/**
 * Update and redraw the status window
 */

void NCursesDisplay::updateStatus()
{
  for(int i = 0; i < 8; i++)
  {
    mvwprintw(statusWin, (i + 1), 1, "r%d = %u (%#x)\n", i, env->getRegister(i), env->getRegister(i));
  }

  mvwprintw(statusWin, 10, 1, "pc = %u (%#x)\n", env->getPC(), env->getPC());
  mvwprintw(statusWin, 11, 1, "s = %u\n", env->getS());
  mvwprintw(statusWin, 12, 1, "prev insn: %s\n", env->getLastInstruction().c_str());

  box(statusWin, 0, 0);

  refresh();
  wrefresh(statusWin);
}

/**
 * printf-like function to show message in the message window
 *
 * @param msg     Format string of message
 * @param VARARGS Variables for format specifier values
 */

void NCursesDisplay::showMsg(std::string msg, ...)
{
  // clear message
  wclear(msgWin);

  // use the v-flavor of snprintf to generate the string,
  // as ncurses does not have v-variants of its print functions
  char vbuffer[msg.size() + 1];
  va_list arglist;

  va_start(arglist, msg);

  vsnprintf(vbuffer, msg.size() + 1, msg.c_str(), arglist);

  // print generated string
  mvwprintw(msgWin, 1, 1, vbuffer);

  va_end(arglist);

  box(msgWin, 0, 0);

  refresh();
  wrefresh(msgWin);
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

  if(raw() == ERR || noecho() == ERR || curs_set(0) == ERR
    || keypad(stdscr, true) == ERR)
  {
    endCurses();
    throw std::runtime_error("Initialized ncurses but terminal could not be properly configured.");
  }
}

/**
 * Sets ncurses to either emulate a console (echo, line-buffering,
 *  cursor) or operate normally (no echo, raw input, no cursor)
 *
 * @param  console true to emulate console, else false
 * @return         previous curses mode
 */

bool NCursesDisplay::setCursesMode(bool console)
{
  if(console)
  {
    noraw();
    echo();
    curs_set(1);
  }
  else
  {
    raw();
    noecho();
    curs_set(0);
  }

  bool modeTemp = cursesMode;
  cursesMode = console;
  return modeTemp;
}

/**
 * Shutdown ncurses to restore terminal state
 */

void NCursesDisplay::endCurses()
{
  // end ncurses
  if(endwin() == ERR)
  {
    throw std::runtime_error("Failed to shut down ncurses - terminal may be left in an inoperable state.");
  }
}
