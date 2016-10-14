`include "vscale_ctrl_constants.vh"

module vscale_src_b_merged_mux
  #(
    parameter DWIDTH = 32,
    parameter SHARES = 2
  )(
    input  [`SRC_B_SEL_WIDTH-1:0]     src_b_sel,
    input  [DWIDTH-1:0]               imm,
    input  [DWIDTH-1:0]               rs2_data_merged,
    output reg [DWIDTH-1:0]           alu_src_b_merged
  );


  always @( * ) begin
    case( src_b_sel )
      `SRC_B_RS2  : alu_src_b_merged = rs2_data_merged;
      `SRC_B_IMM  : alu_src_b_merged = imm;
      `SRC_B_FOUR : alu_src_b_merged = { { DWIDTH-3{1'b0} }, 3'h4 };
      default     : alu_src_b_merged = 0;
    endcase // case( src_b_sel )
  end

endmodule // vscale_src_b_merged_mux
