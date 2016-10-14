`include "rv32_opcodes.vh"
`include "vscale_csr_addr_map.vh"
`include "vscale_ctrl_constants.vh"

module vscale_csr_file
  (
    input                         clk,
    input                         reset,
    input  [`CSR_ADDR_WIDTH-1:0]  addr,
    input  [`CSR_CMD_WIDTH-1:0]   cmd,
    input  [`XPR_LEN-1:0]         wdata,
    output wire [`PRV_WIDTH-1:0]  prv,
    output                        illegal_access,
    output reg  [`XPR_LEN-1:0]    rdata,
    input                         retire,
    input                         exception,
    input  [`ECODE_WIDTH-1:0]     exception_code,
    input                         eret,
    input  [`XPR_LEN-1:0]         exception_load_addr,
    input  [`XPR_LEN-1:0]         exception_PC,
    output [`XPR_LEN-1:0]         handler_PC,
    output [`XPR_LEN-1:0]         epc,
    // PRNG control
    output reg [1:0]              PrngCtrl,
    // Trigger
    output reg                    Trigger
  );

  reg  [`CSR_COUNTER_WIDTH-1:0]   cycle_full;
  reg  [`CSR_COUNTER_WIDTH-1:0]   time_full;
  reg  [`CSR_COUNTER_WIDTH-1:0]   instret_full;
  reg  [5:0]                      priv_stack;
  reg  [`XPR_LEN-1:0]             mtvec;
  reg                             mtie;
  reg                             msie;
  reg                             mtip;
  reg                             msip;
  reg  [`XPR_LEN-1:0]             mtimecmp;
  reg  [`CSR_COUNTER_WIDTH-1:0]   mtime_full;
  reg  [`XPR_LEN-1:0]             mscratch;
  reg  [`XPR_LEN-1:0]             mepc;
  reg  [`ECODE_WIDTH-1:0]         mecode;
  reg                             mint;
  reg  [`XPR_LEN-1:0]             mbadaddr;

  wire                            ie;

  wire [`XPR_LEN-1:0]             mcpuid;
  wire [`XPR_LEN-1:0]             mimpid;
  wire [`XPR_LEN-1:0]             mhartid;
  wire [`XPR_LEN-1:0]             mstatus;
  wire [`XPR_LEN-1:0]             mtdeleg;
  wire [`XPR_LEN-1:0]             mie;
  wire [`XPR_LEN-1:0]             mip;
  wire [`XPR_LEN-1:0]             mcause;

  wire                            mtimer_expired;

  wire                            system_en;
  wire                            system_wen;
  wire                            illegal_region;
  reg                             defined;
  reg  [`XPR_LEN-1:0]             wdata_internal;
  wire                            uinterrupt;
  wire                            minterrupt;
  reg                             interrupt_taken;
  reg  [`ECODE_WIDTH-1:0]         interrupt_code;

  wire                            code_imem;

  reg                             EnCounter;

  wire [`XPR_LEN-1:0]             padded_prv = prv;
  assign handler_PC = mtvec + ( padded_prv << 5 );

  assign prv = priv_stack[2:1];
  assign ie = priv_stack[0];

  assign system_en = cmd[2];
  assign system_wen = cmd[1] || cmd[0];

  assign illegal_region = ( system_wen && ( addr[11:10] == 2'b11 ) )
                          || ( system_en && addr[9:8] > prv );

  assign illegal_access = illegal_region || ( system_en && !defined );

  always @( * ) begin
    if( system_wen ) begin
      case( cmd )
        `CSR_SET : wdata_internal = rdata | wdata;
        `CSR_CLEAR : wdata_internal = rdata & ~wdata;
        default : wdata_internal = wdata;
      endcase // case( cmd )
    end else begin
      wdata_internal = wdata;
    end
  end

  assign uinterrupt = 1'b0;
  assign minterrupt = ( mtie && mtimer_expired );

  always @( * ) begin
    interrupt_code = `ICODE_TIMER;
    case( prv )
      `PRV_U  : interrupt_taken = ( ie && uinterrupt ) || minterrupt;
      `PRV_M  : interrupt_taken = ( ie && minterrupt );
      default : interrupt_taken = 1'b1;
    endcase // case( prv )
  end

  assign mcpuid  = ( 1 << 20 ) | ( 1 << 8 ); // 'I' and 'U' bits set
  assign mimpid  = 32'h8000; // Implementation ID register: 0x8000 -> anonymous source
  assign mhartid = 0;

  always @( posedge clk ) begin
    if( reset ) begin
      priv_stack <= 6'b000110;
    end else if( system_wen && addr == `CSR_ADDR_MSTATUS ) begin
      priv_stack <= wdata_internal[5:0];
    end else if( exception ) begin
      // no delegation to U means all exceptions go to M
      priv_stack <= { priv_stack[2:0], 2'b11, 1'b0 };
    end else if( eret ) begin
      priv_stack <= { 2'b00, 1'b1, priv_stack[5:3] };
    end
  end // always @( posedge clk )

  assign epc = mepc;

  // this implementation has SD, VM, MPRV, XS, and FS set to 0
  assign mstatus = { 26'b0, priv_stack };

  assign mtdeleg = 0;

  assign mtimer_expired = ( mtimecmp == mtime_full[0+:`XPR_LEN] );

  always @( posedge clk ) begin
    if( reset ) begin
      mtip <= 0;
      msip <= 0;
    end else begin
      if( mtimer_expired )
        mtip <= 1;
      if( system_wen && addr == `CSR_ADDR_MTIMECMP )
        mtip <= 0;
      if( system_wen && addr == `CSR_ADDR_MIP ) begin
        mtip <= wdata_internal[7];
        msip <= wdata_internal[3];
      end
    end // else: !if( reset )
  end // always @( posedge clk )
  assign mip = { mtip, 3'b0, msip, 3'b0};


  always @( posedge clk ) begin
    if( reset ) begin
      mtie <= 0;
      msie <= 0;
    end else if( system_wen && addr == `CSR_ADDR_MIE ) begin
      mtie <= wdata_internal[7];
      msie <= wdata_internal[3];
    end
  end // always @( posedge clk )
  assign mie = { mtie, 3'b0, msie, 3'b0 };

  always @( posedge clk ) begin
    if( exception || interrupt_taken )
      mepc <= exception_PC & { { 30{1'b1} }, 2'b0 };
    if( system_wen && addr == `CSR_ADDR_MEPC )
      mepc <= wdata_internal & { { 30{1'b1} }, 2'b0 };
  end

  always @( posedge clk ) begin
    if( reset ) begin
      mecode <= 0;
      mint   <= 0;
    end else if( system_wen && addr == `CSR_ADDR_MCAUSE ) begin
      mecode <= wdata_internal[3:0];
      mint   <= wdata_internal[31];
    end else begin
      if( interrupt_taken ) begin
        mecode <= interrupt_code;
        mint   <= 1'b1;
      end else if( exception ) begin
        mecode <= exception_code;
        mint   <= 1'b0;
      end
    end // else: !if( reset )
  end // always @( posedge clk )
  assign mcause = { mint, 27'b0, mecode };

  assign code_imem = ( exception_code == `ECODE_INST_ADDR_MISALIGNED )
                     || ( exception_code == `ECODE_INST_ADDR_MISALIGNED );

  always @( posedge clk ) begin
    if( exception )
      mbadaddr <= ( code_imem ) ? exception_PC : exception_load_addr;
    if( system_wen && addr == `CSR_ADDR_MBADADDR )
      mbadaddr <= wdata_internal;
  end

  always @( * ) begin
    case( addr )
      `CSR_ADDR_CYCLE     : begin rdata = cycle_full[0+:`XPR_LEN];          defined = 1'b1; end
      `CSR_ADDR_TIME      : begin rdata = time_full[0+:`XPR_LEN];           defined = 1'b1; end
      `CSR_ADDR_INSTRET   : begin rdata = instret_full[0+:`XPR_LEN];        defined = 1'b1; end
      `CSR_ADDR_CYCLEH    : begin rdata = cycle_full[`XPR_LEN+:`XPR_LEN];   defined = 1'b1; end
      `CSR_ADDR_TIMEH     : begin rdata = time_full[`XPR_LEN+:`XPR_LEN];    defined = 1'b1; end
      `CSR_ADDR_INSTRETH  : begin rdata = instret_full[`XPR_LEN+:`XPR_LEN]; defined = 1'b1; end
      `CSR_ADDR_MCPUID    : begin rdata = mcpuid;                           defined = 1'b1; end
      `CSR_ADDR_MIMPID    : begin rdata = mimpid;                           defined = 1'b1; end
      `CSR_ADDR_MHARTID   : begin rdata = mhartid;                          defined = 1'b1; end
      `CSR_ADDR_MSTATUS   : begin rdata = mstatus;                          defined = 1'b1; end
      `CSR_ADDR_MTVEC     : begin rdata = mtvec;                            defined = 1'b1; end
      `CSR_ADDR_MTDELEG   : begin rdata = mtdeleg;                          defined = 1'b1; end
      `CSR_ADDR_MIE       : begin rdata = mie;                              defined = 1'b1; end
      `CSR_ADDR_MTIMECMP  : begin rdata = mtimecmp;                         defined = 1'b1; end
      `CSR_ADDR_MTIME     : begin rdata = mtime_full[0+:`XPR_LEN];          defined = 1'b1; end
      `CSR_ADDR_MTIMEH    : begin rdata = mtime_full[`XPR_LEN+:`XPR_LEN];   defined = 1'b1; end
      `CSR_ADDR_MSCRATCH  : begin rdata = mscratch;                         defined = 1'b1; end
      `CSR_ADDR_MEPC      : begin rdata = mepc;                             defined = 1'b1; end
      `CSR_ADDR_MCAUSE    : begin rdata = mcause;                           defined = 1'b1; end
      `CSR_ADDR_MBADADDR  : begin rdata = mbadaddr;                         defined = 1'b1; end
      `CSR_ADDR_MIP       : begin rdata = mip;                              defined = 1'b1; end
      `CSR_ADDR_CYCLEW    : begin rdata = cycle_full[0+:`XPR_LEN];          defined = 1'b1; end
      `CSR_ADDR_TIMEW     : begin rdata = time_full[0+:`XPR_LEN];           defined = 1'b1; end
      `CSR_ADDR_INSTRETW  : begin rdata = instret_full[0+:`XPR_LEN];        defined = 1'b1; end
      `CSR_ADDR_CYCLEHW   : begin rdata = cycle_full[`XPR_LEN+:`XPR_LEN];   defined = 1'b1; end
      `CSR_ADDR_TIMEHW    : begin rdata = time_full[`XPR_LEN+:`XPR_LEN];    defined = 1'b1; end
      `CSR_ADDR_INSTRETHW : begin rdata = instret_full[`XPR_LEN+:`XPR_LEN]; defined = 1'b1; end
      // non-standard
      `CSR_ADDR_TRIGGER   : begin rdata = { 31'b0, Trigger };    defined = 1'b1; end
      `CSR_EN_COUNTER     : begin rdata = { 31'b0, EnCounter };  defined = 1'b1; end
      `CSR_PRNG_CTRL      : begin rdata = { 30'b0, PrngCtrl };   defined = 1'b1; end
      default : begin rdata = 0; defined = 1'b0; end
    endcase // case( addr )
  end // always @( * )


  always @( posedge clk ) begin
    if( reset ) begin
      cycle_full   <= 0;
      time_full    <= 0;
      instret_full <= 0;
      mtime_full   <= 0;
      mtvec        <= 'h100;
      Trigger      <= 1'b0;
      EnCounter    <= 1'b1;
      PrngCtrl     <= 2'b01;
    end else begin
      if( EnCounter ) begin
        cycle_full <= cycle_full + 1;
        time_full  <= time_full + 1;
        if( retire ) begin
          instret_full <= instret_full + 1;
        end
        mtime_full <= mtime_full + 1;
      end
      if( system_wen ) begin
        case( addr )
          `CSR_ADDR_CYCLE     : cycle_full[0+:`XPR_LEN]           <= wdata_internal;
          `CSR_ADDR_TIME      : time_full[0+:`XPR_LEN]            <= wdata_internal;
          `CSR_ADDR_INSTRET   : instret_full[0+:`XPR_LEN]         <= wdata_internal;
          `CSR_ADDR_CYCLEH    : cycle_full[`XPR_LEN+:`XPR_LEN]    <= wdata_internal;
          `CSR_ADDR_TIMEH     : time_full[`XPR_LEN+:`XPR_LEN]     <= wdata_internal;
          `CSR_ADDR_INSTRETH  : instret_full[`XPR_LEN+:`XPR_LEN]  <= wdata_internal;
          // mcpuid is read-only
          // mimpid is read-only
          // mhartid is read-only
          // mstatus handled separately
          `CSR_ADDR_MTVEC     : mtvec                             <= wdata_internal & { { 30{1'b1} }, 2'b0 };
          // mtdeleg constant
          // mie handled separately
          `CSR_ADDR_MTIMECMP  : mtimecmp                          <= wdata_internal;
          `CSR_ADDR_MTIME     : mtime_full[0+:`XPR_LEN]           <= wdata_internal;
          `CSR_ADDR_MTIMEH    : mtime_full[`XPR_LEN+:`XPR_LEN]    <= wdata_internal;
          `CSR_ADDR_MSCRATCH  : mscratch                          <= wdata_internal;
          // mepc handled separately
          // mcause handled separately
          // mbadaddr handled separately
          // mip handled separately
          `CSR_ADDR_CYCLEW    : cycle_full[0+:`XPR_LEN]           <= wdata_internal;
          `CSR_ADDR_TIMEW     : time_full[0+:`XPR_LEN]            <= wdata_internal;
          `CSR_ADDR_INSTRETW  : instret_full[0+:`XPR_LEN]         <= wdata_internal;
          `CSR_ADDR_CYCLEHW   : cycle_full[`XPR_LEN+:`XPR_LEN]    <= wdata_internal;
          `CSR_ADDR_TIMEHW    : time_full[`XPR_LEN+:`XPR_LEN]     <= wdata_internal;
          `CSR_ADDR_INSTRETHW : instret_full[`XPR_LEN+:`XPR_LEN]  <= wdata_internal;
          // non-standard
          `CSR_ADDR_TRIGGER   : Trigger                           <= wdata_internal[0];
          `CSR_EN_COUNTER     : EnCounter                         <= wdata_internal[0];
          `CSR_PRNG_CTRL      : PrngCtrl                          <= wdata_internal[1:0];
          default : ;
        endcase // case( addr )
      end // if( system_wen )
    end // else: !if( reset )
  end // always @( posedge clk )

endmodule // vscale_csr_file
