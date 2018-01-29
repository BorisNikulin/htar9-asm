#pragma once

#include <vector>
#include <cstddef>
#include <exception>

typedef unsigned char word;
typedef unsigned short pc_t;
typedef unsigned long insn_t;

class Interpreter
{
public:
  struct DoneInterrupt : public std::exception
  {
    const char * what() const throw()
    {
      return "Program terminated";
    }
  };

  struct InvalidInstruction : public std::exception
  {
    const char * what() const throw()
    {
      return "Instruction width greater than 9 bits";
    }
  };

  struct UnrecognizedInstruction : public std::exception
  {
    const char * what() const throw()
    {
      return "Instruction not implemented";
    }
  };

  Interpreter(std::size_t numReg, std::size_t memSize);
  ~Interpreter() = default;

  word getRegister(std::size_t idx);
  word getMemory(std::size_t idx);
  void setRegister(std::size_t idx, word value);
  void setMemory(std::size_t idx, word value);

  void executeInsn(insn_t insn);

private:
  struct Instruction
  {
  public:
    Instruction(insn_t insn);
    ~Instruction() = default;

    insn_t getValue();
    int getChunk(std::size_t chunk);

  private:
    insn_t val;
  };

  std::vector<word> registers;
  std::vector<word> memory;
  pc_t pc;
  bool s;
  const std::size_t ARITHMETIC_REGISTER = 0;
};
