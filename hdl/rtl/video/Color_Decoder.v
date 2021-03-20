module Color_Decoder(
	input				en,
	input		[11:0]	color,
	output		[3:0]	r_out,
	output		[3:0]	g_out,
	output		[3:0]	b_out
);

assign r_out = en ? color[3:0] : 0;
assign g_out = en ? color[7:4] : 0;
assign b_out = en ? color[11:8] : 0;

endmodule