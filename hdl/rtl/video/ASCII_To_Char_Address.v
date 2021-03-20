module ASCII_To_Char_Address(
	input		[5:0]	char_index,
	input    	[7:0]	ascii_value,
	output      [10:0]	ROM_Address
);

assign ROM_Address = (ascii_value * 8) + (char_index >> 3);

endmodule