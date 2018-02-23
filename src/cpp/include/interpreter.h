#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <vector>
#include <string>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <sstream>

namespace CPU {

  typedef uint8_t word;
  typedef uint16_t pc_t;

  class Interpreter
  {
    friend class InterpreterSupervisor;
  public:
    struct UnrecognizedInstruction : public std::exception
    {
      const char * what() const throw()
      {
        return "Instruction not implemented - machine code is suspect.";
      }
    };

    struct InvalidPC : public std::exception
    {
      const char * what() const throw()
      {
        return "Program counter addressed instruction that does not exist.";
      }
    };

    struct Instruction
    {
    public:
      enum InstructionType { MV, LD, STR, DIST, MIN, ADD, ADDI, SUB, SUBI, AND,
        ANDI, LSHFT, LSHFTI, RSHFT, RSHFTI, BCS, BA, FIN, RESET };

      struct Operand
      {
      public:
        enum OperandType { SIGNED_IMMEDIATE, UNSIGNED_IMMEDIATE, REGISTER,
          NONE };

        Operand() : type(NONE), value(0)
        {

        }

        Operand(OperandType type, std::string insn) : type(type)
        {
          switch(type)
          {
            case SIGNED_IMMEDIATE:
              value = extractSignedImmediate(insn);
            break;
            case UNSIGNED_IMMEDIATE:
              value = extractUnsignedImmediate(insn);
            break;
            case REGISTER:
              value = extractRegister(insn);
            break;
            case NONE:
              value = 0;
            break;
          }
        }

        OperandType getType() const noexcept { return type; }
        int getValue() const noexcept { return value; }

      private:
        OperandType type;
        int value;

        static int extractRegister(const std::string insn);
        static int extractSignedImmediate(const std::string insn);
        static int extractUnsignedImmediate(const std::string insn);
      };

      Instruction(const std::string insn);
      ~Instruction() = default;

      void setInit(bool init);

      InstructionType getType() const noexcept;
      Operand getOperand() const noexcept;
      std::string getAssembly() const noexcept;

    private:
      InstructionType type;
      Operand operand;
      std::string assembly;
    };

    Interpreter(std::vector<std::string> insns);
    ~Interpreter() = default;

    bool isDone() const noexcept { return done; }
    void setInit(bool init) noexcept { this->init = init; }

    void executeNext();

  private:
    std::vector<word> registers;
    std::vector<word> memory;
    std::vector<Instruction> programMemory;
    pc_t pc;
    bool s;
    bool done;
    bool init;
    const std::size_t ARITHMETIC_REGISTER = 0;

    void executeInsn(Instruction insn);
  };

  class InterpreterSupervisor
  {
  public:
    InterpreterSupervisor(Interpreter & inter) : inter(inter)
    {

    }

    void executeNext() { inter.executeNext(); }
    word getRegister(const std::size_t idx) const;
    word getMemory(const std::size_t idx) const;
    std::vector<std::string> getInstructions() const noexcept;
    std::string coreDump() const noexcept;
    pc_t getPC() const noexcept { return inter.pc; }
    bool getS() const noexcept { return inter.s; }
    bool isDone() const noexcept { return inter.isDone(); }
    bool getInit() const noexcept { return inter.init; }
    void setInit(bool init) noexcept { inter.setInit(init); }

    void setRegister(std::size_t idx, word value);
    void setMemory(std::size_t idx, word value);
    void resetPC() noexcept;
  private:
    Interpreter & inter;
  };

  class CodeParser
  {
  public:
    std::vector<std::string> operator()(std::string code) const noexcept;
  };

}

#endif /* end of include guard: INTERPRETER_H */
