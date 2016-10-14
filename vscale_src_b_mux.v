`include "vscale_ctrl_constants.vh"

module vscale_src_b_mux
  #(
    parameter DWIDTH = 32,
    parameter SHARES = 2
  )(
    input  [`SRC_B_SEL_WIDTH-1:0]     src_b_sel,
    input  [DWIDTH-1:0]               imm,
    input  [DWIDTH*SHARES-1:0]        rs2_data,
    output reg [DWIDTH*SHARES-1:0]    alu_src_b
  );


  always @( * ) begin
    case( src_b_sel )
      `SRC_B_IMM  : alu_src_b = { { (SHARES-1)*DWIDTH{1'b0} }, imm };
      default     : alu_src_b = rs2_data;
    endcase // case( src_b_sel )
  end

endmodule // vscale_src_b_mux
