#include "interpreter.h"

#include <cstdlib>
#include <cstring>

/**
 * Constructor - reads machine code into list of instructions and sets up
 * execution environment
 *
 * @param numReg  Number of registers in the CPU
 * @param memSize Size of main memory, in bytes
 * @param insns   C string of machine code to interpret
 */

Interpreter::Interpreter(std::size_t numReg, std::size_t memSize, char * insns)
  : registers(numReg), memory(memSize), pc(0), s(false)
{
  char buffer[10];

  // Read 9 bits of machine code into an instruction until none remain
  while(*insns != '\0')
  {
    std::memcpy(buffer, insns, 9);
    buffer[9] = '\0';
    // convert string to long, read as ASCII binary
    insn_t ins = std::strtol(buffer, nullptr, 2);

    // Push instruction onto program memory list
    programMemory.emplace_back(ins);

    insns += 9;
  }
}

/**
 * Return value of specified register
 *
 * @param  idx Index of register
 * @return     Register value
 */

word Interpreter::getRegister(std::size_t idx)
{
  return registers[idx];
}

/**
 * Return value of specified memory unit
 *
 * @param  idx Memory address
 * @return     Unit value
 */

word Interpreter::getMemory(std::size_t idx)
{
  return memory[idx];
}

/**
 * @return Current program counter value
 */

pc_t Interpreter::getPC()
{
  return pc;
}

/**
 * @return String representation of the last-executed instruction
 */

std::string Interpreter::getLastInstruction()
{
  return lastInsn.str();
}

/**
 * @return State of the status register
 */

bool Interpreter::getS()
{
  return s;
}

/**
 * Sets specified regsiter to the given value
 *
 * @param idx   Index of register
 * @param value Value to set
 */

void Interpreter::setRegister(std::size_t idx, word value)
{
  registers[idx] = value;
}

/**
 * Sets specified memory unit to the given value
 *
 * @param idx   Memory address
 * @param value Value to set
 */

void Interpreter::setMemory(std::size_t idx, word value)
{
  memory[idx] = value;
}

/**
 * Resets program counter to 0
 */

void Interpreter::resetPC()
{
  pc = 0;
}

/**
 * Executes the next instruction
 */

void Interpreter::executeNext()
{
  // If PC overruns instruction memory, throw exception
  if(pc >= programMemory.size())
  {
    throw InvalidPC();
  }

  this->executeInsn(programMemory[pc]);
}

/**
 * Executes the given instruction
 *
 * @param insn Instruction to execute
 */

void Interpreter::executeInsn(Interpreter::Instruction insn)
{
  // high 3 bits of insn
  int opcode1 = insn.getChunk(2);

  // reset prev insn stream
  lastInsn.str("");

  // 000-series insns
  if(insn.getChunk(2) == 0)
  {
    // middle 3 bits of insn
    int opcode2 = insn.getChunk(1);

    // 000111-series insns (simple)
    if(opcode2 == 7)
    {
      // low 3 bits of insn
      int opcode3 = insn.getChunk(0);

      switch(opcode3)
      {
        // fin
        case 0:
          lastInsn << "fin";
          throw DoneInterrupt();
        // reset
        case 1:
          lastInsn << "reset";
          resetPC();
          break;
        default:
          throw UnrecognizedInstruction();
      }
    }
    // single-register insns
    else
    {
      // register number = 7 - low 3 bits
      int reg = 7 - insn.getChunk(0);

      switch(opcode2)
      {
        // mv
        case 0:
          lastInsn << "mv r" << reg;
          registers[reg] = registers[ARITHMETIC_REGISTER];
          // reset arithmetic register on all moves
          registers[ARITHMETIC_REGISTER] = 0;
        break;
        // str
        case 2:
          lastInsn << "str r" << reg;
          memory[registers[reg]] = registers[ARITHMETIC_REGISTER];
        break;
        //ld
        case 3:
          lastInsn << "ld r" << reg;
          registers[ARITHMETIC_REGISTER] = memory[registers[reg]];
        break;
        default:
          throw UnrecognizedInstruction();
      }
      pc++;
    }
  }
  // 11-series insns (branches)
  else if(opcode1 >= 6)
  {
    // only 2 branch types, represented by lowest bit of opcode1
    int branch_type = opcode1 & 1;

    int insn_val = insn.getValue();

    // get unsigned part of value
    int branch_distance = insn_val & 0b11'111;

    // if branch is negative, perform sign extension
    if(insn_val & 0b100'000)
    {
      branch_distance |= 0xFF'FF'FF'E0;
    }

    switch(branch_type)
    {
      // branch if set
      case 0:
        lastInsn << "bch " << branch_distance;
        if(s)
        {
          pc += branch_distance;
        }
        else
        {
          pc++;
        }
        break;
      // branch always
      case 1:
        lastInsn << "ba " << branch_distance;
        pc += branch_distance;
        break;
      default:
        throw UnrecognizedInstruction();
    }
  }
  // arithmetic insns
  else
  {
    int operand = insn.getValue() & 0b111'111;

    int val;

    std::stringstream operandStr;

    // reserve top 8 values for registers - pull value from reg
    if(operand >= (0b111'111 - 8))
    {
      int reg = 7 - insn.getChunk(0);

      val = registers[reg];

      operandStr << "r" << reg;
    }
    // value is an immediate
    else
    {
      val = operand;

      operandStr << val;
    }

    switch(opcode1)
    {
      // add
      case 1:
        // mask to look at the msb of each word
        {
          word result = registers[ARITHMETIC_REGISTER] + val;

          lastInsn << "add " << operandStr.str();
          registers[ARITHMETIC_REGISTER] = result & 0b1111'1111;

          // carry-out is bit 9 of the result - flip this
          s = !(result & 0b1'0000'0000);
        }
        break;
      // sub
      case 2:
        lastInsn << "sub " << operandStr.str();
        registers[ARITHMETIC_REGISTER] -= val;
        s = (registers[ARITHMETIC_REGISTER] != 0);
        break;
      // and
      case 3:
        lastInsn << "and " << operandStr.str();
        registers[ARITHMETIC_REGISTER] &= val;
        s = (registers[ARITHMETIC_REGISTER] == 0);
        break;
      // lshft
      case 4:
        lastInsn << "lshft " << operandStr.str();
        registers[ARITHMETIC_REGISTER] <<= val;
        s = (registers[ARITHMETIC_REGISTER] != 0);
        break;
      // rhsft
      case 5:
        lastInsn << "rshft " << operandStr.str();
        registers[ARITHMETIC_REGISTER] >>= val;
        s = (registers[ARITHMETIC_REGISTER] != 0);
        break;
      default:
        throw UnrecognizedInstruction();
    }
    pc++;
  }
}

/**
 * Construct an Instruction from the given machine code
 *
 * @param insn Machine code, as integral value
 */

Interpreter::Instruction::Instruction(insn_t insn)
{
  // insns are 9 bits - anything higher than 2^9-1 is garbage
  if(insn >= 1024)
  {
    throw InvalidInstruction();
  }
  else
  {
    val = insn;
  }
}

/**
 * Return integral machine code value
 */

insn_t Interpreter::Instruction::getValue()
{
  return val;
}

/**
 * Return 3-bit right-justified section of machine code integral value
 *
 * @param chunk Index of 3-bit section (0 - low bits, 1 - middle, 2 - high)
 */

int Interpreter::Instruction::getChunk(std::size_t chunk)
{
  unsigned int mask = (0b111 << (chunk * 3));
  return (val & mask) >> (chunk * 3);
}
