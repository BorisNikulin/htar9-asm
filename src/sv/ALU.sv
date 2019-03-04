// Create Date:     2017.05.05
// Latest rev date: 2018.02.10
// Created by:      J Eldon
// Design Name:     CSE141L
// Module Name:     ALU (Arithmetic Logical Unit)


// Includes new enum op_mnemonic to make instructions
// appear literally on waveform.
import definitions::*;

module ALU (input  [7:0]       operand_val,
            input  [7:0]       accumulator_val,
            input  [3:0]       operation,
            input logic        cc_i,
            output logic [7:0] result,
						output logic       cc_o
            );

wire signed[7:0] signed_operand_val;
wire signed[7:0] signed_accumulator_val;
wire signed[8:0] difference;

assign signed_operand_val = operand_val;
assign signed_accumulator_val = accumulator_val;
assign difference = signed_operand_val - signed_accumulator_val;

always_comb
  begin
  case (operation)
	// passthrough operand_val
  PASS_VAL: begin
         cc_o 	= cc_i;
         result = operand_val;
       end
	// passthrough accumulator_val
	PASS_ACC: begin
				 cc_o 	= cc_i;
				 result = accumulator_val;
			end
  ADD: begin
         {cc_o, result} = operand_val + accumulator_val;
         cc_o = !cc_o; // cc set if no carry out
      end
  SUB: begin
         result = accumulator_val - operand_val;
				 // cc set if result nonzero
				 if(result != 0) begin
            cc_o = 1;
         end
         else begin
            cc_o = 0;
         end
      end
  AND: begin
         result = accumulator_val & operand_val;
				 // cc set if result zero
         if(result == 0) begin
            cc_o = 1;
         end
         else begin
            cc_o = 0;
         end
      end
  LSHFT: begin
	       result = accumulator_val << operand_val;
				 // cc set if result nonzero
         if(result != 0) begin
            cc_o = 1;
         end
         else begin
            cc_o = 0;
         end
	 	 end
	RSHFT: begin
		     result = accumulator_val >> operand_val;
				 // cc set if result nonzero
         if(result != 0) begin
            cc_o = 1;
         end
         else begin
            cc_o = 0;
         end
     end
	 DIST: begin
	 			cc_o = cc_i;
				if(difference < 0) begin
					result = 0 - difference;
				end
				else result = difference;
	 	 end
	 MIN: begin
	 			cc_o = cc_i;
	 			result = (accumulator_val < operand_val) ? accumulator_val : operand_val;
	 	 end
	 // unknown operation
	 default: begin
	 			result = 8'bZZZZZZZZ;
				cc_o = 1'bZ;
			end
     endcase
  end
endmodule
