module Char_To_Color(
	input		[5:0]	char_index,
	input		[7:0]	char_data,
	input		[11:0]	bcol,
	input		[11:0]	fcol,
	output reg	[11:0]	color
);

initial begin
	color <= 0;
end

always @(*) begin
	case (char_index & 6'b000111)
		0: color <= (char_data & 8'b10000000) ? fcol : bcol;
		1: color <= (char_data & 8'b01000000) ? fcol : bcol;
		2: color <= (char_data & 8'b00100000) ? fcol : bcol;
		3: color <= (char_data & 8'b00010000) ? fcol : bcol;
		4: color <= (char_data & 8'b00001000) ? fcol : bcol;
		5: color <= (char_data & 8'b00000100) ? fcol : bcol;
		6: color <= (char_data & 8'b00000010) ? fcol : bcol;
		7: color <= (char_data & 8'b00000001) ? fcol : bcol;
		default : color <= 12'b000000000000;
	endcase
end

endmodule