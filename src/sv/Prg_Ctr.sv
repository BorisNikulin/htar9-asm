// Create Date:    17:44:49 2012.16.02
// Latest rev:     2018.02.10
// Design Name:    CSE141L
// Module Name:    Prg_Ctr

// generic program counter
module Prg_Ctr(
  input              abs_jump,
                     rel_jump,
  input signed[5:0]  branch_offset,
  input 						 reset,
	input 						 init,
  input 						 done,
  input 						 clk,
  output logic[15:0] PC
  );

	// sign-extend the branch offset to width of PC
	logic signed[15:0] branch_offset_extended;
	assign branch_offset_extended = branch_offset;

	initial PC = 0;

  always @(posedge clk) begin
		if(init) begin
			if(PC == 0)
				PC <= 0;
			else begin
				if(reset)
					PC <= 0;
				else if(abs_jump)
					PC <= branch_offset;
				else if(rel_jump)
					PC <= PC + branch_offset_extended;
				else
					PC <= PC+1;
			end
		end
		else begin
			if(done)
				PC <= PC;
			else begin
				if(reset)
					PC <= 0;
				else if(abs_jump)
					PC <= branch_offset;
				else if(rel_jump)
					PC <= PC + branch_offset_extended;
				else
					PC <= PC+1;
			end
		end
	end

endmodule
