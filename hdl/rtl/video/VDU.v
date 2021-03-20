`include "ASCII_To_Char_Address.v"
`include "Bus_Double_DFF.v"
`include "Char_To_Color.v"
`include "Color_Decoder.v"
`include "Screen_Address_Decoder.v"
`include "VGA_Controller.v"

// Check if we need to compile fake memory, ie. we are compiling from
// iverilog and cannot compile vendor (like Altera altsyncram)
// specific BRAM
`ifdef FAKE_BRAM
// TODO - Include some fake memory here
`else
`include "Char_ROM.v"
`endif

module VDU(
	input				fst_clk,
	input				pixel_clk,
	input				rst_n,
	input		[7:0]	screen_buf_data,
	
	output				vga_hs,
	output				vga_vs,
	output 		[3:0] 	vga_r,
	output 		[3:0] 	vga_g,
	output		[3:0] 	vga_b,
	output				screen_buf_rden,
	output		[13:0]	screen_buf_addr
);

localparam	BACKGROUND_COL = 0;
localparam	FOREGROUND_COL = 12'hFFF;

wire	[11:0]	back_col;
wire	[11:0]	fore_col;

reg		[7:0]	cntl_reg_0 = BACKGROUND_COL[7:0];
reg		[7:0]	cntl_reg_1 = BACKGROUND_COL[11:8];
reg		[7:0]	cntl_reg_2 = FOREGROUND_COL[7:0];
reg		[7:0]	cntl_reg_3 = FOREGROUND_COL[11:0];

wire 			is_drawing;
wire 	[9:0]	px;
wire 	[9:0]	py;
wire 	[13:0]	buffer_addr;
wire 	[10:0]	rom_address;
wire	[5:0]	char_addr1;
wire	[5:0]	char_addr2;
wire	[5:0]	char_addr3;
wire	[7:0]	char_data;
wire 	[11:0]	color;

assign back_col[11:0] = { cntl_reg_1[3:0], cntl_reg_1[7:0] };
assign fore_col[11:0] = { cntl_reg_3[3:0], cntl_reg_2[7:0] };
assign screen_buf_rden = is_drawing;
assign screen_buf_addr = buffer_addr;

VGA_Controller vga_cont(
	.clk(pixel_clk),
	.rst_n(rst_n),
	.vga_hs(vga_hs),
	.vga_vs(vga_vs),
	.is_drawing(is_drawing),
	.px(px),
	.py(py)
);

Screen_Address_Decoder screen_decode(
	.px(px),
	.py(py),
	.char_addr(char_addr1),
	.buffer_addr(buffer_addr)
);

Bus_Double_DFF bd1_2(
	.clk(pixel_clk),
	.D(char_addr1),
	.Q(char_addr2)
);

ASCII_To_Char_Address acii_addr(
	.char_index(char_addr2),
	.ascii_value(screen_buf_data),
	.ROM_Address(rom_address)
);

`ifdef FAKE_BRAM
// TODO - Instantiate some fake memory here
`else
Char_ROM crom(
	.clock(pixel_clk),
	.rden(is_drawing),
	.address(rom_address),
	.q(char_data)
);
`endif

Bus_Double_DFF bd2_3(
	.clk(pixel_clk),
	.D(char_addr2),
	.Q(char_addr3)
);

Char_To_Color char_col(
	.char_index(char_addr3),
	.char_data(char_data),
	.bcol(back_col),
	.fcol(fore_col),
	.color(color)
);

Color_Decoder col_decode(
	.en(is_drawing),
	.color(color),
	.r_out(vga_r),
	.g_out(vga_g),
	.b_out(vga_b)
);

endmodule