module Screen_Address_Decoder(
	input	[9:0]	px,
	input	[9:0]	py,
	output	[5:0]	char_addr,
	output	[13:0]	buffer_addr
);

`include "VGA_Params.h"

assign char_addr = (px & 10'b0000000111) + ((py & 10'b0000000111) << 3);
assign buffer_addr = (px >> 3) + ((pixelsH >> 3) * (py >> 3));

endmodule