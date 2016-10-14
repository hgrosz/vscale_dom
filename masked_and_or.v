module masked_and_or
  #(
    parameter OPERATOR = "",
    parameter SHARES = 2
  )(
    input                                 ClkxCI,
    input                                 RstxBI,
    input                                 EnxSI,
    input                                 UseOrxSI,

    input  [SHARES-1:0]                   XxDI,
    input  [SHARES-1:0]                   YxDI,
    input  [((SHARES*(SHARES-1))/2)-1:0]  ZxDI,

    output [SHARES-1:0]                   QxDO
  );


  wire [SHARES*SHARES-1:0]  Xi_and_Yj;
  reg  [SHARES*SHARES-1:0]  FFxDN;
  reg  [SHARES*SHARES-1:0]  FFxDP;

  wire [SHARES-1:0]         XxDV;
  wire [SHARES-1:0]         YxDV;
  reg  [SHARES-1:0]         QxDV;

  generate
    if( OPERATOR == "AND" ) begin
      assign XxDV = XxDI;
      assign YxDV = YxDI;
      assign QxDO = QxDV;
    end else if( OPERATOR == "OR" ) begin
      assign XxDV = { XxDI[SHARES-1:1], ~XxDI[0] };
      assign YxDV = { YxDI[SHARES-1:1], ~YxDI[0] };
      assign QxDO = { QxDV[SHARES-1:1], ~QxDV[0] };
    end else begin // BOTH
      assign XxDV = { XxDI[SHARES-1:1], (XxDI[0] ^ UseOrxSI) };
      assign YxDV = { YxDI[SHARES-1:1], (YxDI[0] ^ UseOrxSI) };

      reg  UseOrxSP;
      always @( posedge ClkxCI ) begin : sync_both
        if( RstxBI )
          UseOrxSP <= 0;
        else
          if( EnxSI )
            UseOrxSP <= UseOrxSI;
      end // always @( posedge ClkxCI ) : sync_both

      assign QxDO = { QxDV[SHARES-1:1], (QxDV[0] ^ UseOrxSP) };
    end // BOTH
  endgenerate

  generate
    for( genvar i = 0; i < SHARES; i = i + 1 ) begin
      for( genvar j = 0; j < SHARES; j = j + 1 ) begin
        assign Xi_and_Yj[SHARES*i+j] = XxDV[i] & YxDV[j];
      end // loop j
    end // loop i
  endgenerate


  always @( posedge ClkxCI ) begin : sync
    if( RstxBI ) begin
      FFxDP <= 0;
    end else begin
      if( EnxSI )
        FFxDP <= FFxDN;
    end
  end // always @( posedge ClkxCI ) : sync

  always @( * ) begin : comb
    integer i, j;
    for( i = 0; i < SHARES; i = i + 1 ) begin
      for( j = 0; j < SHARES; j = j + 1 ) begin
        if( i == j ) begin
          FFxDN[SHARES*i+j] = Xi_and_Yj[SHARES*i+j];
        end else if( j > i ) begin
          FFxDN[SHARES*i+j] = Xi_and_Yj[SHARES*i+j] ^ ZxDI[i+j*(j-1)/2];
        end else begin // j < i
          FFxDN[SHARES*i+j] = Xi_and_Yj[SHARES*i+j] ^ ZxDI[j+i*(i-1)/2];
        end
      end // loop j
      QxDV[i] = ^FFxDP[SHARES*i+:SHARES];
    end // loop i
  end // always @( * ) : comb

endmodule // masked_and_or
