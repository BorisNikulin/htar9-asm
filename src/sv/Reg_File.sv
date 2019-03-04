// Create Date:    2017.05.05
// Latest rev:     2018.02.10
// Created by:     J Eldon
// Design Name:    CSE141L
// Module Name:    Reg_File

// register file with asynchronous read and synchronous write
module Reg_File #(parameter raw = 3)
    (input             clk,
    input   [raw-1:0]  rf_read_addr,
    input   [2:0]      rf_write_addr,
    input              rf_write_en,
    input        [7:0] rf_write_val,
	input 			   rf_reset_acc,
	output logic [7:0] rf_read_o,
    output logic [7:0] rf_acc_o
                );

logic [7:0] RF [2**raw];

assign rf_read_o = RF [rf_read_addr];
assign rf_acc_o = RF [0];

always_ff @ (posedge clk) begin
	// clocked write to target address
  if (rf_write_en) begin
		RF [rf_write_addr] <= rf_write_val;
	end
	// synchronous reset of accumulator (if accumulator
	// is addressed by write, write is discarded for 0)
	if (rf_reset_acc) begin
		RF[0] <= 0;
	end
end
endmodule
