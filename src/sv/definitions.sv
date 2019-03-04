//This file defines the parameters used in the alu
package definitions;

typedef enum logic[3:0] {
  PASS_VAL = 4'b0000,
	PASS_ACC = 4'b0001,
  ADD = 4'b0010,
  SUB = 4'b0011,
  AND = 4'b0100,
  LSHFT = 4'b0101,
  RSHFT = 4'b0110,
	DIST = 4'b0111,
	MIN = 4'b1000,
	UNK = 4'b1111
	 } alu_op_code;

 typedef enum logic[8:0] {
	 FIN = 9'b000111000,
	 RESET = 9'b000111001,
	 ZSERIES = 9'b000xxxxxx,
	 BRANCH = 9'b11xxxxxxx
 } cpu_op_class;

endpackage // defintions
