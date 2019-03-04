// Create Date:     2017.11.05
// Latest rev date: 2018.02.10
// Created by:      J Eldon
// Design Name:     CSE141L
// Module Name:     htar9 (top of microprocessor design)

import definitions::*;

module htar9(
  input 				clk,
        				init,
  output logic 	done
);
  parameter IW = 16;
	/* Flow control signals */
  wire[IW-1:0] 			 PC;
	logic              abs_jump,
										 rel_jump;
	logic signed[5:0]  branch_offset;
	logic							 reset;
	/* Next instruction */
	wire[8:0]       	 instruction;
	/* Execution signals */
	alu_op_code        alu_op;
	logic      				 alu_operand_selector;
	logic[7:0] 				 alu_operand;
	logic[2:0]				 rf_read_addr;
  wire[7:0]       	 rf_read_o,
                     rf_acc_o,
                     alu_result;
	/* Condition register */
	logic 						 cc_i;
  logic              cc_o;
	/* Writeback signals */
  logic 						 rf_write_en;
	logic      				 rf_write_addr_selector;
  logic[2:0] 				 rf_write_addr;
	logic							 rf_reset_acc;
	logic      				 rf_write_val_selector;
	logic[7:0] 				 rf_write_val;
	/* Memory signals */
	logic      				 dm_read_en;
	logic      				 dm_write_en;
  logic[7:0] 				 dm_read_o;

assign rf_read_addr = 3'b111 - instruction[2:0];
assign branch_offset = alu_result;

/* MUX logic */
// Writeback to instruction register (mov only) or accumulator?
assign rf_write_addr = rf_write_addr_selector ? (3'b111 - instruction[2:0]) : 3'b000;
// Writeback from memory or from ALU result?
assign rf_write_val = rf_write_val_selector ? dm_read_o : alu_result;
// Use register from instruction's value or immediate?
assign alu_operand = alu_operand_selector ? rf_read_o : instruction[5:0];

Prg_Ctr pc1(.*);

Decoder dc1 (.*);

Ins_Mem #(.IW(16)) InstROM1(
  .insn_addr (PC),
  .instruction
	);


Reg_File #(.raw(3)) rf1	 (.*);

ALU alu1(
	.operand_val 			   (alu_operand),
  .accumulator_val	   (rf_acc_o),
  .cc_i,
  .operation					 (alu_op),
  .result              (alu_result),
	.cc_o
	);

Data_Mem dm1(
   .clk,
   .dm_access_addr 	 (alu_result),
   .dm_read_en       (1'b1), // mem read always on
   .dm_write_en,
   .dm_write_val     (rf_acc_o), // store (from RF)
   .dm_read_o
);

always_ff @(posedge clk)   // one-bit condition code
  cc_i <= cc_o;

endmodule
