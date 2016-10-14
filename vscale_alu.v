`include "vscale_alu_ops.vh"

module vscale_alu
  #(
    parameter DWIDTH = 32,
    parameter SHARES = 2
  )(
    input  [`ALU_OP_WIDTH-1:0]  OpxDI,
    input  [DWIDTH-1:0]         In1xDI,
    input  [DWIDTH-1:0]         In2xDI,
    output reg [DWIDTH-1:0]     OutxDO
  );


  always @( * ) begin
    case( OpxDI )
      `ALU_OP_ADD  : OutxDO = In1xDI + In2xDI; // Only used for address operations!!
      `ALU_OP_SEQ  : OutxDO = { 31'b0, In1xDI == In2xDI };
      `ALU_OP_SNE  : OutxDO = { 31'b0, In1xDI != In2xDI };
      `ALU_OP_SLT  : OutxDO = { 31'b0, $signed( In1xDI ) <  $signed( In2xDI ) };
      `ALU_OP_SGE  : OutxDO = { 31'b0, $signed( In1xDI ) >= $signed( In2xDI ) };
      `ALU_OP_SLTU : OutxDO = { 31'b0, In1xDI <  In2xDI };
      `ALU_OP_SGEU : OutxDO = { 31'b0, In1xDI >= In2xDI };
      default      : OutxDO = 0;
    endcase // case( OpxDI )
  end

endmodule // vscale_alu_shared
