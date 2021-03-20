`include "6502/CPU_6502.v"
`include "6522/VIA_6522.v"
`include "6551/ACIA_6551.v"
`include "video/VDU.v"

// Check if we need to compile fake memory, ie. we are compiling from
// iverilog and cannot compile vendor (like Altera altsyncram)
// specific BRAM
`ifdef FAKE_BRAM
// TODO - Include some fake memory here
`else
//`include "memory/ROM.v"
//`include "memory/RAM.v"
`endif

module J6502_System (
    input               fst_clk,
    input               phi2,
    input               uart_clk,
    input               pixel_clk,
    input               res_n,

    // VIA1 connections
    output      [7:0]   via1_pa,
    output      [7:0]   via1_pb,

    // ACIA1 connections
    input               acia1_rx,
    input               acia1_cts_n,
    input               acia1_dcd_n,
    input               acia1_dsr_n,
    output              acia1_tx,
    output              acia1_rts_n,
    output              acia1_dtr_n,

    output				vga_hs,
	output				vga_vs,
	output 		[3:0] 	vga_r,
	output 		[3:0] 	vga_g,
	output		[3:0] 	vga_b
);

// Wire / Reg declarations
wire				rw_n;
wire                irq_n;
wire                via1_irq_n;
wire                acia_irq_n;
wire    [15:0]	    cpu_addr_out;
wire    [15:0]      mem_addr_in;
wire	[7:0]		cpu_data_in;
wire	[7:0]		cpu_data_out;
wire	[7:0]		rom_data_out;
wire	[7:0]		ram_data_out;
wire	[7:0]		io_data_out;
wire    [7:0]       via1_data_out;
wire    [7:0]       acia1_data_out;
wire				rom_sel_n;
wire				ram_sel_n;
wire				io_sel_n;
wire				rom_rd_en;
wire				ram_rd_en;
wire				ram_wr_en;
wire                vram_buf_rden;
wire    [14:0]      vram_buf_addr;
wire    [14:0]      vram_addr_in;
wire    [7:0]       vram_char_out;

// Address decoding
assign rom_sel_n = ~cpu_addr_out[15];
assign ram_sel_n = ~(rom_sel_n && ~cpu_addr_out[14]);
assign io_sel_n = ~(rom_sel_n && cpu_addr_out[14]);
assign via1_sel_n = ~cpu_addr_out[13];
assign acia1_sel = cpu_addr_out[12] && ~io_sel_n;
assign rom_rd_en = ~rom_sel_n && rw_n ? 1 : 0;
assign ram_rd_en = ~ram_sel_n && rw_n ? 1 : 0;
assign ram_wr_en = ~ram_sel_n && ~rw_n ? 1 : 0;

// Data bus MUX
assign mem_addr_in = cpu_addr_out;
assign io_data_out = via1_sel_n ? via1_data_out : acia1_data_out;
assign cpu_data_in = rom_sel_n ? (ram_sel_n ? io_data_out : ram_data_out) : rom_data_out;

// IRQ combining
assign irq_n = via1_irq_n && acia1_irq_n;

assign vram_addr_in = vram_buf_addr + 16'h2000;

CPU_6502 cpu(
    .clk(fst_clk),
    .phi(phi2),
    .res(res_n),
    .so(1),
    .rdy(1),
    .nmi(1),
    .irq(irq_n),
    .dbi(cpu_data_in),

    .rw(rw_n),
    .ab(cpu_addr_out),
    .dbo(cpu_data_out)
);

`ifdef FAKE_BRAM
// TODO - Instantiate some fake memory here
`else
RAM ram(
	.clock(fst_clk),
	
 	// .address_a(mem_addr_in[14:0]),
    // .rden_a(ram_rd_en),
    // .wren_a(ram_wr_en),
	// .data_a(cpu_data_out),
	 
	.address_a(phi2 ? 16'h2222 : mem_addr_in[14:0]),
	.rden_a(phi2 ? 0 : ram_rd_en),
	.wren_a(phi2 ? 1 : ram_wr_en),
	.data_a(phi2 ? 8'h44 : cpu_data_out),

    .address_b(vram_addr_in),
    .rden_b(vram_buf_rden),
	 
	.q_a(ram_data_out),
    .q_b(vram_char_out),
);

ROM rom(
	.clock(fst_clk),
	.rden(rom_rd_en),
	.address(mem_addr_in[14:0]),
	
	.q(rom_data_out)
);
`endif
 
VIA_6522 via1(
    .CLK(fst_clk),
    .ENA_4(1),
    .RESET_L(res_n),
    .I_P2_H(phi2),
    .I_RW_L(rw_n),
    .I_CS1(cpu_addr_out[13]),
    .I_CS2_L(io_sel_n),
    .I_RS(cpu_addr_out[3:0]),
    .I_DATA(cpu_data_out),

    .O_IRQ_L(via1_irq_n),
    .O_DATA(via1_data_out),
    .O_PA(via1_pa),
    .O_PB(via1_pb)
);

ACIA_6551 acia1(
    .PH_2(phi2),
    .XTAL_CLK_IN(uart_clk),
    .RESET_N(res_n),
    .RW_N(rw_n),
    .IRQ(acia1_irq_n),
    .CS(acia1_sel),
    .DI(cpu_data_out),
    .RXDATA_IN(acia1_rx),
    .CTS(acia1_cts_n),
    .DCD(acia1_dcd_n),
    .DSR(acia1_dsr_n),
    
    .RS(cpu_addr_out[1:0]),
    .DO(acia1_data_out),
    .TXDATA_OUT(acia1_tx),
    .RTS(acia1_rts_n),
    .DTR(acia1_dtr_n)
);

VDU video(
    .fst_clk(fst_clk),
    .pixel_clk(pixel_clk),
    .rst_n(res_n),
    .screen_buf_data(vram_char_out),

    .vga_hs(vga_hs),
    .vga_vs(vga_vs),
    .vga_r(vga_r),
    .vga_g(vga_g),
    .vga_b(vga_b),
    .screen_buf_rden(vram_buf_rden),
    .screen_buf_addr(vram_buf_addr)
);

endmodule