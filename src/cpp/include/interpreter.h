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

  /**
   * Class supporting the execution of HTAR9 instructions in a simulated CPU
   * environment
   */

  class Interpreter
  {
    friend class InterpreterSupervisor; // Expose interpreter internals only
                                        // to supervisor class
  public:

    /**
     * Exception raised when an unrecognized machine code instruction is loaded
     * into the interpreter
     */

    struct UnrecognizedInstruction : public std::exception
    {
      const char * what() const throw()
      {
        return "Instruction not implemented - machine code is suspect.";
      }
    };

    /**
     * Exception raised if the PC moves out of the bounds of instruction memory
     */

    struct InvalidPC : public std::exception
    {
      const char * what() const throw()
      {
        return "Program counter addressed instruction that does not exist.";
      }
    };

    /**
     * Class representing a machine instruction
     */

    class Instruction
    {
    public:
      enum InstructionType { MV, LD, STR, DIST, MIN, ADD, ADDI, SUB, SUBI, AND,
        ANDI, LSHFT, LSHFTI, RSHFT, RSHFTI, BCS, BA, FIN, RESET };

      /**
       * Struct representing an operand for a machine instruction
       */

      struct Operand
      {
      public:
        enum OperandType { SIGNED_IMMEDIATE, UNSIGNED_IMMEDIATE, REGISTER,
          NONE };

        /**
         * Construct a default Operand - type is NONE
         */

        Operand() : type(NONE), value(0)
        {
        }

        /**
         * Construct an Operand with the given type, extracting the value
         * from the given instruction
         *
         * @param type Operand type
         * @param insn Instruction from which to parse value
         */

        Operand(const OperandType type, const std::string insn) : type(type),
        value(getOperandFromInsn(type, insn))
        {
        }

        /**
         * @return The type of this Operand
         */

        OperandType getType() const noexcept { return type; }

        /**
         * @return The value of this Operand
         */

        int getValue() const noexcept { return value; }

      private:
        OperandType type;
        int value;

        /**
         * Extract register index from instruction
         *
         * @param  insn Instruction from which to parse register index
         * @return      Register index
         */

        static int extractRegister(const std::string insn);

        /**
         * Extract signed immediate from instruction
         *
         * @param  insn Instruction from which to parse immediate
         * @return      Signed immediate value
         */

        static int extractSignedImmediate(const std::string insn);

        /**
         * Extract unsigned immediate from instruction
         *
         * @param  insn Instruction from which to parse unsigned immediate
         * @return      Unsigned immediate value
         */

        static int extractUnsignedImmediate(const std::string insn);

        /**
         * Extracts the operand from the given instruction, using the specified
         * operand type to decide how to parse
         *
         * @param  type Type of operand to extract
         * @param  insn Instruction from which to extract
         * @return      Value of extracted operand
         */

        static int getOperandFromInsn(const OperandType type, const std::string
          insn);
      };

      /**
       * Construct an Instruction from the given machine code
       *
       * @param insn Length 9 string of ASCII binary machine code
       */

      Instruction(const std::string insn);

      ~Instruction() = default;

      /**
       * @return Type of this instruction
       */

      InstructionType getType() const noexcept;

      /**
       * @return Operand of this instruction
       */

      Operand getOperand() const noexcept;

      /**
       * @return Assembly mnemonic of this instruction
       */

      std::string getAssembly() const noexcept;

    private:
      InstructionType type;
      Operand operand;
      std::string assembly;
    };

    /**
     * Construct an Interpreter and initialize instruction memory with the given
     * instruction list
     *
     * @param insns Vector of machine code strings to load into instruction
     * memory
     */

    Interpreter(std::vector<std::string> insns);

    ~Interpreter() = default;

    /**
     * @return Value of the done flag
     */

    bool isDone() const noexcept { return done; }

    /**
     * Set the value of the init line
     *
     * @param init New init value
     */

    void setInit(bool init) noexcept { this->init = init; }

    /**
     * Execute the instruction pointed to by the program counter
     */

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

    /**
     * Execute the given Instruction
     *
     * @param insn Instruction to be executed
     */

    void executeInsn(Instruction insn);
  };

  /**
   * Class exposing methods to probe the internal status of an Interpreter, and
   * directly modify internal state outside the normal interface of the CPU
   */

  class InterpreterSupervisor
  {
  public:

    /**
     * Constructs a new InterpreterSupervisor managing the given Interpreter
     *
     * @param Interpreter to manage
     */

    InterpreterSupervisor(Interpreter & inter) : inter(inter)
    {

    }

    /**
     * Execute the next instruction in the interpreter instruction memory
     */

    void executeNext() { inter.executeNext(); }

    /**
     * Get the value of the register at the given index
     *
     * @param  idx Register index
     * @return     Register value
     */

    word getRegister(const std::size_t idx) const;

    /**
     * Get the value of memory at the given address
     *
     * @param  idx Memory address
     * @return     Memory value
     */

    word getMemory(const std::size_t idx) const;

    /**
     * Get a list of the instructions loaded in instruction memory,
     * as string mnemonic representations
     */

    std::vector<std::string> getInstructions() const noexcept;

    /**
     * UNIMPLEMENTED
     *
     * @return UNIMPLEMENTED
     */

    std::string coreDump() const noexcept;

    /**
     * @return The current value of the program counter
     */

    pc_t getPC() const noexcept { return inter.pc; }

    /**
     * @return The current value of the status register
     */

    bool getS() const noexcept { return inter.s; }

    /**
     * @return The current value of the done line
     */

    bool isDone() const noexcept { return inter.isDone(); }

    /**
     * @return The current value of the init line
     */

    bool getInit() const noexcept { return inter.init; }

    /**
     * Set the value of the init line
     *
     * @param init New init value
     */

    void setInit(bool init) noexcept { inter.setInit(init); }

    /**
     * Set the value of the register at the given index
     *
     * @param idx   Index of register to set
     * @param value New value
     */

    void setRegister(std::size_t idx, word value);

    /**
     * Set the value of the memory cell at the given address
     *
     * @param idx   Address of memory cell to set
     * @param value New value
     */

    void setMemory(std::size_t idx, word value);

    /**
     * Forces program counter to value 0
     */

    void resetPC() noexcept;
  private:
    Interpreter & inter;
  };

  /**
   * Functor converting a raw machine code dump into individual 9-bit machine
   * code strings
   */

  class CodeParser
  {
  public:

    /**
     * Converts the given machine code string into a list of 9-bit machine code
     * strings
     */

    std::vector<std::string> operator()(std::string code) const noexcept;
  };

}

#endif /* end of include guard: INTERPRETER_H */
