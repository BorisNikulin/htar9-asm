// Create Date:    2018.02.10
// Latest rev:     2018.02.10
// Created by:     M Hatch, D Toor
// Design name:    CSE141L
// Module Name:    Decoder

import definitions::*;

module Decoder (
  input  logic [8:0]  instruction,
	input  logic 				cc_i,
  output logic        alu_operand_selector,
  output alu_op_code  alu_op,
  output logic        rel_jump,
  output logic        abs_jump,
  output logic        rf_write_addr_selector,
  output logic        rf_write_val_selector,
	output logic 				rf_reset_acc,
  output logic        done,
	output logic				reset,
  output logic        rf_write_en,
  output logic        dm_read_en,
  output logic        dm_write_en
  );

  assign dm_read_en = 1'b1;					 // reading from memory always enabled
  assign abs_jump = 1'b0;						 // absolute jumps unsupported

	always@(*) begin
		if (instruction ==? FIN) begin
			alu_operand_selector = 1'bx;      // don't care
			alu_op = PASS_ACC;             // don't care
			rel_jump = 0;
			rf_write_addr_selector = 1'bx;    // don't care
			rf_write_val_selector = 1'bx;     // don't care
			rf_reset_acc = 0;
			done = 1;
			reset = 0;
			rf_write_en = 0;
			dm_write_en = 0;
		end
		else if (instruction ==? RESET) begin
			alu_operand_selector = 1'bx;      // don't care
			alu_op = PASS_ACC;             // don't care
			rel_jump = 0;
			rf_write_addr_selector = 1'bx;    // don't care
			rf_write_val_selector = 1'bx;     // don't care
			rf_reset_acc = 0;
			done = 0;
			reset = 1;
			rf_write_en = 0;
			dm_write_en = 0;
		end
		else if (instruction ==? ZSERIES) begin
			alu_operand_selector = 1; // read as register
			rel_jump = 0;
			done = 0;
			reset = 0;
			case(instruction[5:3])
				// mov
				3'b000: begin
					alu_op = PASS_ACC; // passthrough accumulator value to be written back
					rf_write_addr_selector = 1; // write to specified address
					rf_write_val_selector = 0;  // write from ALU result (accumulator val)
					rf_write_en = 1;
					dm_write_en = 0;
					rf_reset_acc = 1;
				end
				// str
				3'b010: begin
					alu_op = PASS_VAL; // passthrough register value as pointer to memory
					rf_write_addr_selector = 1'bx; // don't care
					rf_write_val_selector = 1'bx;  // don't care
					rf_write_en = 0;
					dm_write_en = 1;
					rf_reset_acc = 0;
				end
				// ld
				3'b011: begin
					alu_op = PASS_VAL; // passthrough register value as pointer to memory
					rf_write_addr_selector = 0; // write to accumulator
					rf_write_val_selector = 1;  // write from data memory
					rf_write_en = 1;
					dm_write_en = 0;
					rf_reset_acc = 0;
				end
				// dist
				3'b100: begin
					alu_op = DIST;
					rf_write_addr_selector = 0; // write to accumulator
					rf_write_val_selector = 0; // write from ALU result
					rf_write_en = 1;
					dm_write_en = 0;
					rf_reset_acc = 0;
				end
				// min
				3'b101: begin
					alu_op = MIN;
					rf_write_addr_selector = 0; // write to accumulator
					rf_write_val_selector = 0; // write from ALU result
					rf_write_en = 1;
					dm_write_en = 0;
					rf_reset_acc = 0;
				end
				default: begin
					alu_op = UNK;
					rf_write_addr_selector = 1'bZ;
					rf_write_val_selector = 1'bZ;
					rf_write_en = 1'bZ;
					dm_write_en = 1'bZ;
					rf_reset_acc = 1'bZ;
				end
			endcase
		end
		// branch class operations
		else if (instruction ==? BRANCH) begin
			alu_op = PASS_VAL;				// pass immediate through to PC unit
			alu_operand_selector = 0; // read as immediate
			rf_reset_acc = 0;
			rf_write_addr_selector = 1'bx; // don't care
			rf_write_val_selector = 1'bx;  // don't care
			rf_write_en = 0;
			dm_write_en = 0;
			done = 0;
			reset = 0;
			case(instruction[6:6])
				// bcs
				1'b0: begin
					rel_jump = cc_i;
				end
				// ba
				1'b1: begin
					rel_jump = 1;
				end
			endcase
		end
		// arithmetic class operations
		else begin
			// top 8 operand values are reserved for register IDs, so
			// operand is a register
			if (instruction[5:3] == 3'b111)
				alu_operand_selector = 1;
			// operand is an immediate
			else
				alu_operand_selector = 0;

			rel_jump = 0;
			rf_write_addr_selector = 0; // write to accumulator
			rf_write_val_selector = 0;  // writeback from register
			rf_reset_acc = 0;
			done = 0;
			reset = 0;
			rf_write_en = 1;
			dm_write_en = 0;
			case(instruction[8:6])
				3'b001:
					alu_op = ADD;
				3'b010:
					alu_op = SUB;
				3'b011:
					alu_op = AND;
				3'b100:
					alu_op = LSHFT;
				3'b101:
					alu_op = RSHFT;
				default:
					alu_op = UNK;
			endcase
		end
	end
endmodule
