`include "vscale_ctrl_constants.vh"

module vscale_src_a_merged_mux
  #(
    parameter DWIDTH = 32,
    parameter SHARES = 2
  )(
    input  [`SRC_A_SEL_WIDTH-1:0]     src_a_sel,
    input  [DWIDTH-1:0]               PC_EX,
    input  [DWIDTH-1:0]               rs1_data_merged,
    output reg [DWIDTH-1:0]           alu_src_a_merged
  );


  always @( * ) begin
    case( src_a_sel )
      `SRC_A_PC  : alu_src_a_merged = PC_EX;
      default    : alu_src_a_merged = rs1_data_merged;
    endcase // case( src_a_sel )
  end

endmodule // vscale_src_a_merged_mux
