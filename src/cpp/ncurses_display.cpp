#include "ncurses_display.h"

#include <exception>
#include <iostream>
#include <cstdarg>

NCursesDisplay::NCursesDisplay(std::unique_ptr<Interpreter> env) : env(std::move(env))
{
  initializeCurses();

  getmaxyx(stdscr, maxY, maxX);

  statusWin = newwin(statusHeight, statusWidth, 1, maxX / 2 - statusWidth / 2);

  msgWin = newwin(msgHeight, msgWidth, 16, maxX / 2 - msgWidth / 2);

  consoleWin = newwin(consoleHeight, consoleWidth, 23, maxX / 2 - consoleWidth / 2);
}

NCursesDisplay::~NCursesDisplay()
{

  delwin(statusWin);
  delwin(consoleWin);
  delwin(msgWin);

  endCurses();
}

void NCursesDisplay::start()
{
  bool done = false;

  mvprintw(0, 0, "Usage: \nPress q to quit.\nPress s to step forward one instruction.\n\
Press i to open a command console for more complex commands.\nType 'exit' to \
close console.\n\nPress any key to continue.");

  getch();

  wclear(stdscr);

  updateStatus();

  while(!done)
  {
    int key = getch();
    switch(key)
    {
      case 'q':
        done = true;
        break;
      case 's':
        stepAndUpdate();
        break;
      case 'i':
        setCursesMode(true);

        while(true)
        {
          wclear(consoleWin);
          wmove(consoleWin, 1, 1);
          wprintw(consoleWin, "$ > ");

          refresh();

          char str[50];
          wgetnstr(consoleWin, str, 50);

          std::string input(str);

          if(input == "exit")
          {
            break;
          }
          else
          {
            std::size_t idx;
            if((idx = input.find("readm")) != std::string::npos)
            {
              std::size_t addr = strtol(str + 5, nullptr, 10);
              word val = env->getMemory(addr);

              showMsg("memory address %u has value %u\n", addr, val);
            }
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
        setCursesMode(false);
        wclear(consoleWin);

        refresh();
        wrefresh(consoleWin);

        updateStatus();
        break;
      case 'r':
        env->resetPC();
        updateStatus();
        terminated = false;
        break;
    }
  }
}

void NCursesDisplay::stepAndUpdate(int ct)
{
  if(terminated)
  {
    return;
  }

  try
  {
    for(; ct > 0; ct--)
    {
      env->executeNext();
    }
    updateStatus();
  }
  catch(Interpreter::DoneInterrupt e)
  {
    showMsg("Program terminated: reload? (y/n)\n");
    int response = getch();
    switch(response)
    {
      case 'y':
        env->resetPC();
        updateStatus();
        terminated = false;
        break;
      default:
        terminated = true;
        showMsg("Running suspended until PC reset ('r').");
        break;
    }
  }
}

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

void NCursesDisplay::showMsg(std::string msg, ...)
{
  wclear(msgWin);

  char vbuffer[msg.size() + 1];
  va_list arglist;

  va_start(arglist, msg);

  vsnprintf(vbuffer, msg.size() + 1, msg.c_str(), arglist);

  mvwprintw(msgWin, 1, 1, vbuffer);

  va_end(arglist);

  box(msgWin, 0, 0);

  refresh();
  wrefresh(msgWin);
}

void NCursesDisplay::initializeCurses()
{
  if(initscr() == NULL)
  {
    throw std::runtime_error("Failed to initialize ncurses.");
  }

  if(cbreak() == ERR || noecho() == ERR || curs_set(0) == ERR
    || keypad(stdscr, true) == ERR)
  {
    endCurses();
    throw std::runtime_error("Initialized ncurses but terminal could not be properly configured.");
  }
}

bool NCursesDisplay::setCursesMode(bool console)
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

void NCursesDisplay::endCurses()
{
  // end ncurses
  if(endwin() == ERR)
  {
    throw std::runtime_error("Failed to shut down ncurses - terminal may be left in an inoperable state.");
  }
}
