`include "6502/CPU_6502.v"
`include "6522/VIA_6522.v"

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
    input               phi1,
    input               phi2,
    input               res_n,

    output      [7:0]   via1_pa,
    output      [7:0]   via1_pb
);

// Wire / Reg declarations
wire				rw_n;
wire                irq_n;
wire    [15:0]	    cpu_addr_out;
wire    [15:0]      mem_addr_in;
wire	[7:0]		cpu_data_in;
wire	[7:0]		cpu_data_out;
wire	[7:0]		rom_data_out;
wire	[7:0]		ram_data_out;
wire	[7:0]		io_data_out;
wire				rom_sel_n;
wire				ram_sel_n;
wire				io_sel_n;
wire				rom_rd_en;
wire				ram_rd_en;
wire				ram_wr_en;

// Address decoding
assign rom_sel_n = ~cpu_addr_out[15];
assign ram_sel_n = ~(rom_sel_n && ~cpu_addr_out[14]);
assign io_sel_n = ~(rom_sel_n && cpu_addr_out[14]);
assign rom_rd_en = ~rom_sel_n && rw_n ? 1 : 0;
assign ram_rd_en = ~ram_sel_n && rw_n ? 1 : 0;
assign ram_wr_en = ~ram_sel_n && ~rw_n ? 1 : 0;

assign mem_addr_in = cpu_addr_out;
assign cpu_data_in = rom_sel_n ? (ram_sel_n ? io_data_out : ram_data_out) : rom_data_out;

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
	
 	.address(mem_addr_in[14:0]),
    .rden(ram_rd_en),
    .wren(ram_wr_en),
	.data(cpu_data_out),
	 
	// .address(phi2 ? 16'h2222 : mem_addr_in),
	// .rden(phi2 ? 0 : ram_rd_en),
	// .wren(phi2 ? 1 : ram_wr_en),
	// .data(phi2 ? 8'h44 : cpu_data_out),
	 
	.q(ram_data_out)
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

    .O_IRQ_L(irq_n),
    .O_DATA(io_data_out),
    .O_PA(via1_pa),
    .O_PB(via1_pb)
);

endmodule