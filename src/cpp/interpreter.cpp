#include "interpreter.h"

Interpreter::Interpreter(std::size_t numReg, std::size_t memSize) :
  registers(numReg), memory(memSize), pc(0), s(false)
{

}

word Interpreter::getRegister(std::size_t idx)
{
  return registers[idx];
}

word Interpreter::getMemory(std::size_t idx)
{
  return memory[idx];
}

void Interpreter::setRegister(std::size_t idx, word value)
{
  registers[idx] = value;
}

void Interpreter::setMemory(std::size_t idx, word value)
{
  memory[idx] = value;
}

void Interpreter::executeInsn(insn_t code)
{
  Interpreter::Instruction insn(code);

  int opcode1 = insn.getChunk(2);

  // 000-series insns
  if(insn.getChunk(2) == 0)
  {
    int opcode2 = insn.getChunk(1);

    // 000111-series insns (simple)
    if(opcode2 == 7)
    {
      int opcode3 = insn.getChunk(0);

      switch(opcode3)
      {
        // fin
        case 0:
          throw DoneInterrupt();
        default:
          throw UnrecognizedInstruction();
      }
      pc++;
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
          registers[reg] = registers[ARITHMETIC_REGISTER];
        break;
        // str
        case 2:
          memory[registers[reg]] = registers[ARITHMETIC_REGISTER];
        break;
        //ld
        case 3:
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

    // reserve top 8 values for registers - pull value from reg
    if(operand >= (0b111'111 - 8))
    {
      int reg = 7 - insn.getChunk(0);

      val = registers[reg];
    }
    // value is an immediate
    else
    {
      val = operand;
    }

    switch(opcode1)
    {
      // add
      case 1:
        registers[ARITHMETIC_REGISTER] += val;
        break;
      // sub
      case 2:
        registers[ARITHMETIC_REGISTER] -= val;
        break;
      // and
      case 3:
        registers[ARITHMETIC_REGISTER] &= val;
        break;
      // lshft
      case 4:
        registers[ARITHMETIC_REGISTER] <<= val;
        break;
      // rhsft
      case 5:
        registers[ARITHMETIC_REGISTER] >>= val;
        break;
      default:
        throw UnrecognizedInstruction();
    }
    pc++;
  }
}

Interpreter::Instruction::Instruction(insn_t insn)
{
  if(insn >= 1024)
  {
    throw InvalidInstruction();
  }
  else
  {
    val = insn;
  }
}

insn_t Interpreter::Instruction::getValue()
{
  return val;
}

int Interpreter::Instruction::getChunk(std::size_t chunk)
{
  unsigned int mask = (0b111 << (chunk * 3));
  return (val & mask) >> (chunk * 3);
}
