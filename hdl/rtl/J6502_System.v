`include "6502/CPU_6502.v"
`include "6522/VIA_6522.v"

module J6502_System (
    input               fst_clk,
    input               phi1,
    input               phi2,
    input               res_n,
    input       [7:0]   cpu_data_in,

    output              rw_n,
    output      [15:0]  address,
    output      [7:0]   cpu_data_out
);

CPU_6502 cpu(
    .clk(fst_clk),
    .phi(phi2),
    .res(res_n),
    .so(1),
    .rdy(1),
    .nmi(1),
    .irq(1),
    .rw(rw_n),
    .ab(address),
    .dbi(cpu_data_in),
    .dbo(cpu_data_out)
);

VIA_6522 via1(

);

endmodule