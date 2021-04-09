module Bus_Single_DFF(
	input						clk,
	input		[WIDTH-1:0]		D,
	output reg  [WIDTH-1:0]		Q
);

parameter WIDTH = 6;

always @(posedge clk) begin
	Q <= D;
end

endmodule