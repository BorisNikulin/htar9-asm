#include "interpreter.h"

#include <cstdlib>
#include <cstring>
#include <algorithm>
#include <iomanip>

namespace CPU {

  Interpreter::Interpreter(std::vector<std::string> insns)
    : registers(8, 0), memory(256, 0), pc(0), s(false), done(false), init(false)
  {
    for(std::string insn : insns)
    {
      programMemory.emplace_back(insn);
    }
  }

  void Interpreter::executeNext()
  {
    // If PC overruns instruction memory, throw exception
    if(pc >= programMemory.size())
    {
      throw InvalidPC();
    }

    this->executeInsn(programMemory[pc]);
  }

  void Interpreter::executeInsn(Interpreter::Instruction insn)
  {
    done = false;
    bool branch = false, reset = false;

    word alu_op1 = registers[ARITHMETIC_REGISTER];
    word alu_op2;

    // select operand type - register or immediate
    switch(insn.getOperand().getType())
    {
      // register
      case Instruction::Operand::REGISTER:
        alu_op2 = registers[insn.getOperand().getValue()];
      break;

      // immediate or don't care
      default:
        alu_op2 = insn.getOperand().getValue();
      break;
    }

    // execute instruction
    switch(insn.getType())
    {
      case Instruction::MV:
        registers[insn.getOperand().getValue()] = alu_op1;
        // reset arithmetic register on all moves
        registers[ARITHMETIC_REGISTER] = 0;
      break;

      case Instruction::LD:
        registers[ARITHMETIC_REGISTER] = memory.at(alu_op2);
      break;

      case Instruction::STR:
        memory.at(alu_op2) = alu_op1;
      break;

      case Instruction::DIST:
        {
          // interpret operands as signed 8-bit numbers
          int8_t alu_op1_signed = alu_op1;
          int8_t alu_op2_signed = alu_op2;

          // cast to int to perform sign extension and widen them into a type
          // that will now overflow when subtraction is performed; take
          // absolute value of the subtraction
          int result = std::abs((int)alu_op1_signed - (int)alu_op2_signed);

          registers[ARITHMETIC_REGISTER] = result & 0b1111'1111;
        }
      break;

      case Instruction::MIN:
        registers[ARITHMETIC_REGISTER] = std::min(alu_op1, alu_op2);
      break;

      case Instruction::ADD:
      case Instruction::ADDI:
        {
          int result = alu_op1 + alu_op2;

          registers[ARITHMETIC_REGISTER] = result & 0b1111'1111;
          // carry-out is bit 9 of the result - flip this
          s = !(result & 0b1'0000'0000);
        }
      break;

      case Instruction::SUB:
      case Instruction::SUBI:
        registers[ARITHMETIC_REGISTER] = alu_op1 - alu_op2;
        s = (registers[ARITHMETIC_REGISTER] != 0);
      break;

      case Instruction::AND:
      case Instruction::ANDI:
        registers[ARITHMETIC_REGISTER] = alu_op1 & alu_op2;
        s = (registers[ARITHMETIC_REGISTER] == 0);
      break;

      case Instruction::LSHFT:
      case Instruction::LSHFTI:
        registers[ARITHMETIC_REGISTER] = alu_op1 << alu_op2;
        s = (registers[ARITHMETIC_REGISTER] != 0);
      break;

      case Instruction::RSHFT:
      case Instruction::RSHFTI:
        registers[ARITHMETIC_REGISTER] = alu_op1 >> alu_op2;
        s = (registers[ARITHMETIC_REGISTER] != 0);
      break;

      case Instruction::BCS:
        branch = s;
      break;

      case Instruction::BA:
        branch = true;
      break;

      case Instruction::FIN:
        done = true;
      break;

      case Instruction::RESET:
        reset = true;
      break;
    }

    // case 1: init causes pc to ignore done if PC != 0
    // case 2: normal operation
    // in either of these cases, update PC, else stall
    if((init && pc != 0) || (!init && !done))
    {
      if(reset)
      {
        pc = 0;
      }
      else if(branch)
      {
        pc += insn.getOperand().getValue();
      }
      else
      {
        pc += 1;
      }
      done = false;
    }
  }

  /**
   * Construct an Instruction from the given machine code
   *
   * @param insn Machine code, a string
   */

  Interpreter::Instruction::Instruction(const std::string insn)
  {
    std::stringstream mnemonicStream;

    mnemonicStream << std::left << std::setw(6);

    std::stringstream operandStream;

    // 000-series instructions (simple & memory)
    if(insn.compare(0, 3, "000") == 0)
    {
      // Simple instructions
      if(insn.compare(3, 3, "111") == 0)
      {
        // FIN
        if(insn.compare(6, 3, "000") == 0)
        {
          mnemonicStream << "fin";
          type = FIN;
        }
        // RESET
        else if(insn.compare(6, 3, "001") == 0)
        {
          mnemonicStream << "reset";
          type = RESET;
        }
        else
        {
          throw UnrecognizedInstruction();
        }
      }
      // Register-only instructions
      else
      {
        operand = Operand(Operand::REGISTER, insn);
        if(insn.compare(3, 3, "000") == 0)
        {
          mnemonicStream << "mv";
          operandStream << "r" << operand.getValue();
          type = MV;
        }
        else if(insn.compare(3, 3, "010") == 0)
        {
          mnemonicStream << "str";
          operandStream << "r" << operand.getValue();
          type = STR;
        }
        else if(insn.compare(3, 3, "011") == 0)
        {
          mnemonicStream << "ld";
          operandStream << "r" << operand.getValue();
          type = LD;
        }
        else if(insn.compare(3, 3, "100") == 0)
        {
          mnemonicStream << "dist";
          operandStream << "r" << operand.getValue();
          type = DIST;
        }
        else if(insn.compare(3, 3, "101") == 0)
        {
          mnemonicStream << "min";
          operandStream << "r" << operand.getValue();
          type = MIN;
        }
        else
        {
          throw UnrecognizedInstruction();
        }
      }
    }
    // 11-series instructions (branches)
    else if(insn.compare(0, 2, "11") == 0)
    {
      bool branch_always = insn.compare(2, 1, "1") == 0;

      operand = Operand(Operand::SIGNED_IMMEDIATE, insn);

      if(branch_always)
      {
        mnemonicStream << "ba";
        operandStream << operand.getValue();
        type = BA;
      }
      else
      {
        mnemonicStream << "bcs";
        operandStream << operand.getValue();
        type = BCS;
      }
    }
    else
    {
      auto isRegister = [](std::string insn) -> bool
      {
        return insn.compare(3, 3, "111") == 0;
      };

      bool reg = isRegister(insn);

      if(reg)
      {
        operand = Operand(Operand::REGISTER, insn);
        operandStream << "r" << operand.getValue();
      }
      else
      {
        operand = Operand(Operand::UNSIGNED_IMMEDIATE, insn);
        operandStream << operand.getValue();
      }

      if(insn.compare(0, 3, "001") == 0)
      {
        mnemonicStream << "add";
        type = reg ? ADD : ADDI;
      }
      else if(insn.compare(0, 3, "010") == 0)
      {
        mnemonicStream << "sub";
        type = reg ? SUB : SUBI;
      }
      else if(insn.compare(0, 3, "011") == 0)
      {
        mnemonicStream << "and";
        type = reg ? AND : ANDI;
      }
      else if(insn.compare(0, 3, "100") == 0)
      {
        mnemonicStream << "lshft";
        type = reg ? LSHFT : LSHFTI;
      }
      else
      {
        mnemonicStream << "rshft";
        type = reg ? RSHFT : RSHFTI;
      }
    }

    assembly = mnemonicStream.str() + operandStream.str();
  }

  Interpreter::Instruction::InstructionType Interpreter::Instruction::getType()
    const noexcept
  {
    return type;
  }

  Interpreter::Instruction::Operand Interpreter::Instruction::getOperand()
    const noexcept
  {
    return operand;
  }

  std::string Interpreter::Instruction::getAssembly() const noexcept
  {
    return assembly;
  }

  int Interpreter::Instruction::Operand::extractRegister(const std::string insn)
  {
    return 7 - std::stoi(insn.substr(6, 3), nullptr, 2);
  }

  int Interpreter::Instruction::Operand::extractSignedImmediate(const std::string insn)
  {
    // convert string to int
    int raw = std::stoi(insn.substr(3,6), nullptr, 2);
    // get unsigned part of value
    int branch_distance = raw & 0b11'111;

    // if branch is negative, perform sign extension
    if(raw & 0b100'000)
    {
      branch_distance |= 0xFF'FF'FF'E0;
    }

    return branch_distance;
  }

  int Interpreter::Instruction::Operand::extractUnsignedImmediate(const std::string insn)
  {
    // convert string to int
    return std::stoi(insn.substr(3,6), nullptr, 2);
  }

  int Interpreter::Instruction::Operand::getOperandFromInsn(const OperandType
    type, const std::string insn)
  {
    switch(type)
    {
      case SIGNED_IMMEDIATE:
        return extractSignedImmediate(insn);
      case UNSIGNED_IMMEDIATE:
        return extractUnsignedImmediate(insn);
      case REGISTER:
        return extractRegister(insn);
      default:
        return 0;
    }
  }

  word InterpreterSupervisor::getRegister(const std::size_t idx) const
  {
    return inter.registers.at(idx);
  }
  word InterpreterSupervisor::getMemory(const std::size_t idx) const
  {
    return inter.memory.at(idx);
  }

  std::vector<std::string> InterpreterSupervisor::getInstructions() const
    noexcept
  {
    std::vector<std::string> res;
    for(Interpreter::Instruction insn : inter.programMemory)
    {
      res.push_back(insn.getAssembly());
    }
    return res;
  }

  /* TODO awaiting standard implementation

  std::string InterpreterSupervisor::coreDump() const noexcept
  {

  }

  */

  void InterpreterSupervisor::setRegister(std::size_t idx, word value)
  {
    inter.registers.at(idx) = value;
  }

  void InterpreterSupervisor::setMemory(std::size_t idx, word value)
  {
    inter.memory.at(idx) = value;
  }

  void InterpreterSupervisor::resetPC() noexcept
  {
    inter.pc = 0;
  }

  std::vector<std::string> CodeParser::operator()(std::string code) const
   noexcept
  {
    std::vector<std::string> res;

    for(std::string::size_type i = 0; i < code.length(); i+=9)
    {
      res.emplace_back(code.substr(i, 9));
    }

    return res;
  }

}
