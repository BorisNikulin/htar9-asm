// Create Date:    2017.05.05
// Latest rev:     2018.02.10
// Created by:     J Eldon
// Design Name:    CSE141L
// Module Name:    Data_Mem

// Generic data memory design for CSE141L projects
// width = 8 bits (per assignment spec.)
// depth = 2**W (default value of W = 8, may be changed)
module Data_Mem #(parameter AW=8)(
  input              clk,
  input    [AW-1:0]  dm_access_addr,
  input              dm_read_en,
  input              dm_write_en,
  input       [7:0]  dm_write_val,
  output logic[7:0]  dm_read_o);

  logic [7:0] my_memory [2**AW];

// optional initialization of memory
//  initial
//    $readmemh("dataram_init.list", my_memory);

// read from memory, e.g. on load instruction
  always_comb
    if(dm_read_en) begin
      dm_read_o = my_memory[dm_access_addr];
    end else
      dm_read_o = 8'bZ;	// z denotes high-impedance/undriven

// write to memory, e.g. on store instruction
  always_ff @ (posedge clk)
    if(dm_write_en) begin
      my_memory[dm_access_addr] = dm_write_val;
    end

endmodule
