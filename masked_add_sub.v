module masked_add_sub
  #(
    parameter DWIDTH = 32,
    parameter SHARES = 2
  )(
    input                                         ClkxCI,
    input                                         RstxBI,
    input                                         ReqValidxSI,
    output reg                                    ReqReadyxSO,
    input                                         ReqSubtractxSI,
    input  [DWIDTH*SHARES-1:0]                    ReqIn1xDI,
    input  [DWIDTH*SHARES-1:0]                    ReqIn2xDI,
    input  [DWIDTH*((SHARES*(SHARES-1))/2)-1:0]   RandomDOM1xDI,
    input  [DWIDTH*((SHARES*(SHARES-1))/2)-1:0]   RandomDOM2xDI,
    output [DWIDTH*SHARES-1:0]                    RespResultxDO,
    output reg                                    RespValidxSO
  );

  parameter LOG2_DWIDTH    = $clog2( DWIDTH );
  parameter LOGLOG2_DWIDTH = $clog2( LOG2_DWIDTH );

  localparam AS_STATE_WIDTH = 1;
  localparam S_IDLE    = 0;
  localparam S_COMPUTE = 1;

  reg                                 En_P_and_G_shiftxSB;
  reg                                 En_P_and_P_shiftxSB;
  reg                                 RespValidxSN;
  reg  [AS_STATE_WIDTH-1:0]           StatexDP;
  reg  [AS_STATE_WIDTH-1:0]           StatexDN;
  reg  [LOGLOG2_DWIDTH-1:0]           CounterxDP;

  reg  [SHARES-1:0]                   CinxDP;
  wire [((SHARES*(SHARES-1))/2)-1:0]  RandomDOM1xDVA [0:DWIDTH-1];
  wire [((SHARES*(SHARES-1))/2)-1:0]  RandomDOM2xDVA [0:DWIDTH-1];


  reg  [DWIDTH*SHARES-1:0]  GxDN;
  reg  [DWIDTH*SHARES-1:0]  GxDP;
  reg  [DWIDTH*SHARES-1:0]  P0xDP;

  reg  [DWIDTH*SHARES-1:0]  G_shiftxDV;
  reg  [DWIDTH*SHARES-1:0]  P_shiftxDV;
  reg  [DWIDTH*SHARES-1:0]  P_and_G_In1xDV;
  reg  [DWIDTH*SHARES-1:0]  P_and_G_In2xDV;
  reg  [DWIDTH*SHARES-1:0]  P_and_P_In1xDV;
  wire [DWIDTH*SHARES-1:0]  P_and_G_shiftxDV;
  wire [DWIDTH*SHARES-1:0]  P_and_P_shiftxDV;
  wire [DWIDTH*SHARES-1:0]  G_xorxDV;
  wire [DWIDTH*SHARES-1:0]  ReqIn2AddSubxDV;

  wire [SHARES-1:0]         P_and_G_In1xDVA   [0:DWIDTH-1];
  wire [SHARES-1:0]         P_and_G_In2xDVA   [0:DWIDTH-1];
  wire [SHARES-1:0]         P_and_P_In1xDVA   [0:DWIDTH-1];
  wire [SHARES-1:0]         P_shiftxDVA       [0:DWIDTH-1];
  wire [SHARES-1:0]         P_and_G_shiftxDVA [0:DWIDTH-1];
  wire [SHARES-1:0]         P_and_P_shiftxDVA [0:DWIDTH-1];


  // Invert one vector of second input for subtraction
  assign ReqIn2AddSubxDV[DWIDTH*SHARES-1:DWIDTH] = ReqIn2xDI[DWIDTH*SHARES-1:DWIDTH];
  assign ReqIn2AddSubxDV[DWIDTH-1:0] = ( ReqSubtractxSI ) ? ~ReqIn2xDI[DWIDTH-1:0] : ReqIn2xDI[DWIDTH-1:0];

  assign G_xorxDV = P_and_G_shiftxDV ^ GxDP;

  generate
    for( genvar i = 0; i < SHARES; i = i + 1 ) begin
      assign RespResultxDO[DWIDTH*i+1+:DWIDTH-1] = P0xDP[DWIDTH*i+1+:DWIDTH-1] ^ G_xorxDV[DWIDTH*i+:DWIDTH-1];
      assign RespResultxDO[DWIDTH*i] = P0xDP[DWIDTH*i] ^ CinxDP[i];
    end
  endgenerate

  generate
    for( genvar i = 0; i < DWIDTH; i = i + 1 ) begin
      for( genvar j = 0; j < SHARES; j = j + 1 ) begin
        assign P_and_G_In1xDVA[i][j] = P_and_G_In1xDV[DWIDTH*j+i];
        assign P_and_G_In2xDVA[i][j] = P_and_G_In2xDV[DWIDTH*j+i];
        assign P_and_P_In1xDVA[i][j] = P_and_P_In1xDV[DWIDTH*j+i];
        assign P_shiftxDVA[i][j]     = P_shiftxDV[DWIDTH*j+i];

        assign P_and_G_shiftxDV[DWIDTH*j+i] = P_and_G_shiftxDVA[i][j];
        assign P_and_P_shiftxDV[DWIDTH*j+i] = P_and_P_shiftxDVA[i][j];
      end
    end
  endgenerate

  generate
    for( genvar i = 0; i < DWIDTH; i = i + 1 ) begin
      for( genvar j = 0; j < ((SHARES*(SHARES-1))/2); j = j + 1 ) begin
        assign RandomDOM1xDVA[i][j] = RandomDOM1xDI[DWIDTH*j+i];
        assign RandomDOM2xDVA[i][j] = RandomDOM2xDI[DWIDTH*j+i];
      end
    end
  endgenerate

  generate
    for( genvar i = 0; i < DWIDTH; i = i + 1 ) begin
      masked_and_or
        #(
          .OPERATOR("AND"),
          .SHARES(SHARES)
        ) P_and_G_shift (
          .ClkxCI(ClkxCI),
          .RstxBI(RstxBI),
          .EnxSI(En_P_and_G_shiftxSB),
          .XxDI(P_and_G_In1xDVA[i]),
          .YxDI(P_and_G_In2xDVA[i]),
          .ZxDI(RandomDOM1xDVA[i]),
          .QxDO(P_and_G_shiftxDVA[i])
        );

      masked_and_or
        #(
          .OPERATOR("AND"),
          .SHARES(SHARES)
        ) P_and_P_shift (
          .ClkxCI(ClkxCI),
          .RstxBI(RstxBI),
          .EnxSI(En_P_and_P_shiftxSB),
          .XxDI(P_and_P_In1xDVA[i]),
          .YxDI(P_shiftxDVA[i]),
          .ZxDI(RandomDOM2xDVA[i]),
          .QxDO(P_and_P_shiftxDVA[i])
        );
    end
  endgenerate


  always @( posedge ClkxCI ) begin : sync
    if( RstxBI ) begin
      GxDP <= 0;
      StatexDP <= S_IDLE;
      CounterxDP <= 0;
      RespValidxSO <= 0;
    end else begin
      RespValidxSO <= RespValidxSN;
      StatexDP     <= StatexDN;
      GxDP         <= GxDN;

      case( StatexDP )
        S_IDLE : begin
          if( ReqValidxSI ) begin
            CounterxDP <= 1;
            P0xDP  <= ReqIn1xDI ^ ReqIn2AddSubxDV;
            CinxDP <= { 0, ReqSubtractxSI };
          end // if( ReqValidxSI )
        end
        S_COMPUTE : CounterxDP <= CounterxDP + 1;
        default : GxDP <= G_xorxDV;
      endcase // case( StatexDP )
    end // else ( RstxBI )
  end // always @( posedge ClkxCI ) : sync


  always @( * ) begin : comb
    reg  [DWIDTH*SHARES-1:0]  P_and_X_In1xDV;

    // State dependent values:
    StatexDN            = StatexDP;
    En_P_and_G_shiftxSB = 0;
    En_P_and_P_shiftxSB = 0;
    P_and_G_In1xDV      = 0;
    P_and_G_In2xDV      = 0;
    P_and_P_In1xDV      = 0;
    RespValidxSN        = 0;
    ReqReadyxSO         = ( StatexDP == S_IDLE );
    case( StatexDP )
      S_IDLE : begin
        GxDN = 0;
        if( ReqValidxSI ) begin
          En_P_and_G_shiftxSB = 1;
          StatexDN = S_COMPUTE;
          P_and_G_In1xDV = ReqIn1xDI;
          P_and_G_In2xDV = ReqIn2AddSubxDV;
        end
      end
      S_COMPUTE : begin
        En_P_and_G_shiftxSB = 1;
        GxDN = G_xorxDV;
        if( CounterxDP == LOG2_DWIDTH ) begin
          StatexDN = S_IDLE;
          RespValidxSN = 1;
        end else begin
          En_P_and_P_shiftxSB = 1;
        end
        P_and_X_In1xDV = ( CounterxDP == 1 ) ? P0xDP : P_and_P_shiftxDV;
        P_and_G_In1xDV = P_and_X_In1xDV;
        P_and_G_In2xDV = G_shiftxDV;
        P_and_P_In1xDV = P_and_X_In1xDV;
      end
      default : StatexDN = S_IDLE;
    endcase // case( StatexDP )
  end // always @( * ) : comb


  always @( * ) begin : comb_shift_G
    integer i;

    // Multiplexer for shifted vectors
    for( i = 0; i < SHARES; i = i + 1 ) begin
      case( CounterxDP )
        1 : begin
//        G_shiftxDV = { G_xorxDV[30:0], CinxDP };
          G_shiftxDV[DWIDTH*i+1+:DWIDTH-1] = G_xorxDV[DWIDTH*i+:DWIDTH-1];
          G_shiftxDV[DWIDTH*i] = CinxDP[i];
        end
        2 : begin
//        G_shiftxDV = { G_xorxDV[29:0], CinxDP, 1'b0 };
          G_shiftxDV[DWIDTH*i+2+:DWIDTH-2] = G_xorxDV[DWIDTH*i+:DWIDTH-2];
          G_shiftxDV[DWIDTH*i+:2] = { CinxDP[i], 1'b0 };
        end
        3 : begin
//        G_shiftxDV = { G_xorxDV[27:0], CinxDP, 3'b0 };
          G_shiftxDV[DWIDTH*i+4+:DWIDTH-4] = G_xorxDV[DWIDTH*i+:DWIDTH-4];
          G_shiftxDV[DWIDTH*i+:4] = { CinxDP[i], 3'b0 };
        end
        4 : begin
//        G_shiftxDV = { G_xorxDV[23:0], CinxDP, 7'b0 };
          G_shiftxDV[DWIDTH*i+8+:DWIDTH-8] = G_xorxDV[DWIDTH*i+:DWIDTH-8];
          G_shiftxDV[DWIDTH*i+:8] = { CinxDP[i], 7'b0 };
        end
        5 : begin
//        G_shiftxDV = { G_xorxDV[15:0], CinxDP, 15'b0 };
          G_shiftxDV[DWIDTH*i+16+:DWIDTH-16] = G_xorxDV[DWIDTH*i+:DWIDTH-16];
          G_shiftxDV[DWIDTH*i+:16] = { CinxDP[i], 15'b0 };
        end
        default : begin
//        G_shiftxDV = { G_xorxDV[30:0], CinxDP };
          G_shiftxDV[DWIDTH*i+1+:DWIDTH-1] = G_xorxDV[DWIDTH*i+1+:DWIDTH-1];
          G_shiftxDV[DWIDTH*i] = CinxDP[i];
        end
      endcase // case( CounterxDP )
    end // for
  end // always @( * ) : comb_shift_G

  always @( * ) begin : comb_shift_P
    case( CounterxDP )
      1 : P_shiftxDV = { P0xDP[DWIDTH*SHARES-2:0], P0xDP[DWIDTH*SHARES-1] };
      2 : P_shiftxDV = { P_and_P_shiftxDV[DWIDTH*SHARES-3:0], P_and_P_shiftxDV[DWIDTH*SHARES-1:DWIDTH*SHARES-2] };
      3 : P_shiftxDV = { P_and_P_shiftxDV[DWIDTH*SHARES-5:0], P_and_P_shiftxDV[DWIDTH*SHARES-1:DWIDTH*SHARES-4] };
      4 : P_shiftxDV = { P_and_P_shiftxDV[DWIDTH*SHARES-9:0], P_and_P_shiftxDV[DWIDTH*SHARES-1:DWIDTH*SHARES-8] };
      default : P_shiftxDV = { P0xDP[DWIDTH*SHARES-2:0], P0xDP[DWIDTH*SHARES-1] };
    endcase // case( CounterxDP )
  end // always @( * ) : comb_shift_P

endmodule // masked_add_sub
