`include "vscale_alu_ops.vh"

module vscale_alu_masked
  #(
    parameter DWIDTH = 32,
    parameter SHARES = 2,
    parameter SHAMT_WIDTH = $clog2( DWIDTH )
  )(
    input                                         ClkxCI,
    input                                         RstxBI,
    input                                         ReqValidxSI,
    input  [`ALU_OP_WIDTH-1:0]                    OpxDI,
    input  [DWIDTH*SHARES-1:0]                    In1xDI,
    input  [DWIDTH*SHARES-1:0]                    In2xDI,
    input  [SHAMT_WIDTH-1:0]                      ShamtxDI,
    input  [DWIDTH*((SHARES*(SHARES-1))/2)-1:0]   RandomDOMxDI,
    output [DWIDTH*SHARES-1:0]                    OutxDO
  );


  reg                       EnAndOrxSB;
  reg                       UseOrxSB;
  reg                       AndOrOutxSB;

  reg  [DWIDTH*SHARES-1:0]  AluResxDP;
  reg  [DWIDTH*SHARES-1:0]  AluResxDN;
  wire [DWIDTH*SHARES-1:0]  AndOrxDV;

  wire [SHARES-1:0]         In1xDVA   [0:DWIDTH-1];
  wire [SHARES-1:0]         In2xDVA   [0:DWIDTH-1];
  wire [SHARES-1:0]         AndOrxDVA [0:DWIDTH-1];

  wire [((SHARES*(SHARES-1))/2)-1:0]  RandomDOMxDVA [0:DWIDTH-1];


  // Generate 2D array with input vectors for all masked_and_or instances
  generate
    for( genvar i = 0; i < DWIDTH; i = i + 1 ) begin
      for( genvar j = 0; j < SHARES; j = j + 1 ) begin
        assign In1xDVA[i][j] = In1xDI[DWIDTH*j+i];
        assign In2xDVA[i][j] = In2xDI[DWIDTH*j+i];

        assign AndOrxDV[DWIDTH*j+i]  = AndOrxDVA[i][j];
      end
    end
  endgenerate

  // Generate 2D array with random input vectors for all masked_and_or instances
  generate
    for( genvar i = 0; i < DWIDTH; i = i + 1 ) begin
      for( genvar j = 0; j < ((SHARES*(SHARES-1))/2); j = j + 1 ) begin
        assign RandomDOMxDVA[i][j] = RandomDOMxDI[DWIDTH*j+i];
      end
    end
  endgenerate

  // Generate DWIDTH elements of masked_and_or
  generate
    for( genvar i = 0; i < DWIDTH; i = i + 1 ) begin
      masked_and_or
        #(
          .SHARES(SHARES)
        ) and_or (
          .ClkxCI(ClkxCI),
          .RstxBI(RstxBI),
          .EnxSI(EnAndOrxSB),
          .UseOrxSI(UseOrxSB),
          .XxDI(In1xDVA[i]),
          .YxDI(In2xDVA[i]),
          .ZxDI(RandomDOMxDVA[i]),
          .QxDO(AndOrxDVA[i])
        );
    end
  endgenerate

  always @( posedge ClkxCI ) begin : sync
    if( ReqValidxSI ) begin
      AluResxDP <= AluResxDN;
      if( EnAndOrxSB ) begin
        AndOrOutxSB <= 1;
      end else begin
        AndOrOutxSB <= 0;
      end
    end
  end


  always @( * ) begin : alu_op_comb
    integer i;

    UseOrxSB    = 1'b0;
    EnAndOrxSB  = 0;
    AluResxDN   = In2xDI;
    case( OpxDI )
      `ALU_OP_SLL : begin
        for( i = 0; i < SHARES; i = i + 1 ) begin
          AluResxDN[DWIDTH*i+:DWIDTH] = In1xDI[DWIDTH*i+:DWIDTH] << ShamtxDI;
        end
      end
      `ALU_OP_XOR : begin
        AluResxDN = In1xDI ^ In2xDI;
      end
      `ALU_OP_OR : begin
        UseOrxSB   = 1'b1;
        EnAndOrxSB = ReqValidxSI;
      end
      `ALU_OP_AND : begin
        EnAndOrxSB = ReqValidxSI;
      end
      `ALU_OP_SRL : begin
        for( i = 0; i < SHARES; i = i + 1 ) begin
          AluResxDN[DWIDTH*i+:DWIDTH] = In1xDI[DWIDTH*i+:DWIDTH] >> ShamtxDI;
        end
      end
      `ALU_OP_SRA : begin
        for( i = 0; i < SHARES; i = i + 1 ) begin
          AluResxDN[DWIDTH*i+:DWIDTH] = $signed( In1xDI[DWIDTH*i+:DWIDTH] ) >>> ShamtxDI;
        end
      end
    endcase // case( OpxDI )
  end

  assign OutxDO = (AndOrOutxSB) ? AndOrxDV : AluResxDP;

endmodule // vscale_alu_shared
