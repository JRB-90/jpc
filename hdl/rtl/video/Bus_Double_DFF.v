module Bus_Double_DFF(
	input						clk,
	input		[WIDTH-1:0]		D,
	output reg  [WIDTH-1:0]		Q
);

parameter WIDTH = 6;

reg	[WIDTH-1:0]	R;

always @(posedge clk) begin
	R <= D;
	Q <= R;
end

endmodule