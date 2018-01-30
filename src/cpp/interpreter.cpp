#include "interpreter.h"

#include <cstdlib>
#include <cstring>

Interpreter::Interpreter(std::size_t numReg, std::size_t memSize, char * insns)
  : registers(numReg), memory(memSize), pc(0), s(false)
{
  char buffer[10];

  while(*insns != '\0')
  {
    std::memcpy(buffer, insns, 9);
    buffer[9] = '\0';
    insn_t ins = std::strtol(buffer, nullptr, 2);

    programMemory.emplace_back(ins);

    insns += 9;
  }
}

word Interpreter::getRegister(std::size_t idx)
{
  return registers[idx];
}

word Interpreter::getMemory(std::size_t idx)
{
  return memory[idx];
}

pc_t Interpreter::getPC()
{
  return pc;
}

std::string Interpreter::getLastInstruction()
{
  return lastInsn.str();
}

bool Interpreter::getS()
{
  return s;
}

void Interpreter::setRegister(std::size_t idx, word value)
{
  registers[idx] = value;
}

void Interpreter::setMemory(std::size_t idx, word value)
{
  memory[idx] = value;
}

void Interpreter::resetPC()
{
  pc = 0;
}

void Interpreter::executeNext()
{
  this->executeInsn(programMemory[pc]);
}

void Interpreter::executeInsn(Interpreter::Instruction insn)
{
  int opcode1 = insn.getChunk(2);

  lastInsn.str("");

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
          lastInsn << "fin";
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
          lastInsn << "mv r" << reg;
          registers[reg] = registers[ARITHMETIC_REGISTER];
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
          word mask = 0b10000000;
          word firstOp = registers[ARITHMETIC_REGISTER] & mask;
          word secondOp = val & mask;

          lastInsn << "add " << operandStr.str();
          registers[ARITHMETIC_REGISTER] += val;

          word result = registers[ARITHMETIC_REGISTER] & mask;

          // infer whether there was a carry out generated or not based on msbs
          s = (((firstOp & secondOp) != 0) || ((firstOp ^ secondOp) && !(result)));
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
        s = (registers[ARITHMETIC_REGISTER] != 0);
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
