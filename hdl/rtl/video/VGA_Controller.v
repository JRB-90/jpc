module VGA_Controller(
	input				clk,
	input				rst_n,
	
	output reg			vga_hs,
	output reg			vga_vs,
	output reg			is_drawing,
	output reg	[9:0] 	px,
	output reg	[9:0] 	py
);

`include "VGA_Params.h"

initial begin
	is_drawing = 1'b0;
	px = 0;
	py = 0;
	vga_hs = 1'b1;
	vga_vs = 1'b1;
end

always @(posedge clk) begin
	if (rst_n) begin
		is_drawing <= (px < pixelsH) && (py < pixelsV);
		vga_hs <= ~((px >= (pixelsH + frontH)) && (px <= (pixelsH + frontH + syncH)));
		vga_vs <= ~((py >= (pixelsV + frontV)) && (py <= (pixelsV + frontV + syncV)));
	end else begin
		is_drawing <= 1'b0;
		vga_hs = 1'b1;
		vga_vs = 1'b1;
	end
end

always @(posedge clk) begin
	if (rst_n) begin
		if (px == sizeH) begin
			px <= 0;
		end else begin
			px <= px + 1;
		end
	end else begin
		px <= 0;
	end
end

always @(posedge clk) begin
	if (rst_n) begin
		if (px == sizeH) begin
			if (py == sizeV) begin
				py <= 0;
			end else begin
				py <= py + 1;
			end
		end
	end else begin
		py <= 0;
	end
end

endmodule
