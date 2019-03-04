// Create Date:    2017.05.05
// Latest rev:     2018.02.10
// Created by:     J Eldon
// Design name:    CSE141L
// Module Name:    Ins_Mem

// Generic instruction memory
// same format as any lookup table
// width = 9 bits (per assignment spec.)
// depth = 2**IW (default IW=16)
module Ins_Mem #(parameter IW=16)(
  input       [IW-1:0] insn_addr,
  output logic[   8:0] instruction);

// 2**IW elements, 9 bits each
  logic [8:0] inst_rom [2**IW];
// load machine code program into instruction ROM
  initial
	$readmemb("comb.o", inst_rom);

// continuous combinational read output
assign instruction = inst_rom[insn_addr];

endmodule
