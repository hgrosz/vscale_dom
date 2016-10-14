`include "vscale_ctrl_constants.vh"
`include "vscale_alu_ops.vh"
`include "rv32_opcodes.vh"
`include "vscale_csr_addr_map.vh"
`include "vscale_md_constants.vh"
`include "masking.vh"

module vscale_pipeline
  #(
    parameter SHARES = 2,
    parameter RND_WIDTH = `XPR_LEN*(SHARES*(SHARES-1))
  )(
    input                         clk,
    input                         reset,
    input                         imem_wait,
    output [`XPR_LEN-1:0]         imem_addr,
    input  [`XPR_LEN-1:0]         imem_rdata,
    input                         imem_badmem_e,
    input                         dmem_wait,
    output                        dmem_en,
    output                        dmem_wen,
    output [`MEM_TYPE_WIDTH-1:0]  dmem_size,
    output [`XPR_LEN-1:0]         dmem_addr,
    output [`XPR_LEN*SHARES-1:0]  dmem_wdata,
    input  [`XPR_LEN*SHARES-1:0]  dmem_rdata,
    input                         dmem_badmem_e,
    // Random generator signals
    output                        prng_rst,
    output                        prng_en,
    input  [RND_WIDTH-1:0]        prng_rnd,
    // Trigger signal
    output                        Trigger
  );

  parameter SHAMT_WIDTH = $clog2( `XPR_LEN );


  function [`XPR_LEN-1:0] store_data;
    input  [`XPR_LEN-1:0]         addr;
    input  [`XPR_LEN-1:0]         data;
    input  [`MEM_TYPE_WIDTH-1:0]  mem_type;
    begin
      case( mem_type )
        `MEM_TYPE_SB : store_data = {4{data[7:0]}};
        `MEM_TYPE_SH : store_data = {2{data[15:0]}};
        default : store_data = data;
      endcase // case( mem_type )
    end
  endfunction // store_data

  function [`XPR_LEN-1:0] load_data;
    input [`XPR_LEN-1:0]          addr;
    input [`XPR_LEN-1:0]          data;
    input [`MEM_TYPE_WIDTH-1:0]   mem_type;
    reg   [`XPR_LEN-1:0]          shifted_data;
    reg   [`XPR_LEN-1:0]          b_extend;
    reg   [`XPR_LEN-1:0]          h_extend;
    begin
      shifted_data = data >> { addr[1:0], 3'b0 };
      b_extend = { {24{shifted_data[7]} }, 8'b0 };
      h_extend = { {16{shifted_data[15]} }, 16'b0 };
      case( mem_type )
        `MEM_TYPE_LB  : load_data = (shifted_data & `XPR_LEN'hff)   | b_extend;
        `MEM_TYPE_LH  : load_data = (shifted_data & `XPR_LEN'hffff) | h_extend;
        `MEM_TYPE_LBU : load_data = (shifted_data & `XPR_LEN'hff);
        `MEM_TYPE_LHU : load_data = (shifted_data & `XPR_LEN'hffff);
        default : load_data = shifted_data;
      endcase // case( mem_type )
    end
  endfunction // load_data

  wire [`PC_SRC_SEL_WIDTH-1:0]                 PC_src_sel;
  wire [`XPR_LEN-1:0]                          PC_PIF;


  reg  [`XPR_LEN-1:0]                          PC_IF;

  wire                                         kill_IF;
  wire                                         stall_IF;


  reg  [`XPR_LEN-1:0]                          PC_ID;
  reg  [`INST_WIDTH-1:0]                       inst_ID;
  wire [`IMM_TYPE_WIDTH-1:0]                   imm_type_ID;
  wire [`XPR_LEN-1:0]                          imm_ID;
  wire                                         kill_ID;
  wire                                         stall_ID;

  reg  [`XPR_LEN-1:0]                          PC_EX;
  reg  [`INST_WIDTH-1:0]                       inst_EX;
  reg  [`XPR_LEN-1:0]                          imm_EX;
  wire                                         kill_EX;
  wire                                         stall_EX;

  wire [`SRC_A_SEL_WIDTH-1:0]                  src_a_sel;
  wire [`SRC_B_SEL_WIDTH-1:0]                  src_b_sel;
  wire [`REG_ADDR_WIDTH-1:0]                   rs1_addr;
  wire [`REG_ADDR_WIDTH-1:0]                   rs2_addr;
  wire [`ALU_OP_WIDTH-1:0]                     alu_op;
  wire [`XPR_LEN-1:0]                          alu_out;
  wire                                         cmp_true;
  wire                                         bypass_rs1;
  wire                                         bypass_rs2;
  wire                                         merge_rs1;
  wire                                         merge_rs2;
  wire [`MEM_TYPE_WIDTH-1:0]                   dmem_type;

  wire                                         as_req_valid;
  wire                                         as_req_ready;
  wire                                         as_req_subtract;
  wire                                         as_resp_valid;
  wire [`XPR_LEN*SHARES-1:0]                   as_resp_result;

  wire                                         md_req_valid;
  wire                                         md_req_ready;
  wire                                         md_req_in_1_signed;
  wire                                         md_req_in_2_signed;
  wire [`MD_OUT_SEL_WIDTH-1:0]                 md_req_out_sel;
  wire [`MD_OP_WIDTH-1:0]                      md_req_op;
  wire                                         md_resp_valid;
  wire [`XPR_LEN-1:0]                          md_resp_result;

  reg  [`XPR_LEN-1:0]                          PC_WB;
  reg  [`XPR_LEN-1:0]                          alu_out_WB;

  wire                                         kill_WB;
  wire                                         stall_WB;
  wire [`REG_ADDR_WIDTH-1:0]                   reg_to_wr_WB;
  wire                                         wr_reg_WB;
  wire [`WB_SRC_SEL_WIDTH-1:0]                 wb_src_sel_WB;
  reg  [`MEM_TYPE_WIDTH-1:0]                   dmem_type_WB;

  // Shared data segments
  wire [RND_WIDTH/2-1:0]                       rand_dom_1 = prng_rnd[RND_WIDTH/2-1:0];
  wire [RND_WIDTH/2-1:0]                       rand_dom_2 = prng_rnd[RND_WIDTH-1:RND_WIDTH/2];
  wire [`XPR_LEN*SHARES-1:0]                   rand_zero_1;
  wire [`XPR_LEN*SHARES-1:0]                   rand_zero_2;
  wire [`XPR_LEN*SHARES-1:0]                   rs1_data;
  wire [`XPR_LEN*SHARES-1:0]                   rs1_data_bypassed;
  reg  [`XPR_LEN*SHARES-1:0]                   rs1_data_bypassed_reg;
  reg  [`XPR_LEN*SHARES-1:0]                   rs1_data_bypassed_to_merge_reg;
  wire [`XPR_LEN*SHARES-1:0]                   rs2_data;
  wire [`XPR_LEN*SHARES-1:0]                   rs2_data_bypassed;
  reg  [`XPR_LEN*SHARES-1:0]                   rs2_data_bypassed_reg;
  reg  [`XPR_LEN*SHARES-1:0]                   rs2_data_bypassed_to_merge_reg;
  wire [`XPR_LEN*SHARES-1:0]                   alu_src_b;
  wire [`XPR_LEN*SHARES-1:0]                   masked_alu_out;

  wire [`XPR_LEN*SHARES-1:0]                   load_data_WB;
  wire [`XPR_LEN*SHARES-1:0]                   wb_data_WB;
  wire [`XPR_LEN*SHARES-1:0]                   wb_data_rnd_WB;
  reg  [`XPR_LEN*SHARES-1:0]                   wb_data_masked_WB;

  // Merged data segments
  wire [`XPR_LEN-1:0]                          rs1_data_bypassed_merged;
  wire [`XPR_LEN-1:0]                          rs2_data_bypassed_merged;
  wire [`XPR_LEN-1:0]                          alu_src_a_merged;
  wire [`XPR_LEN-1:0]                          alu_src_b_merged;
  reg  [`XPR_LEN-1:0]                          wb_data_merged_WB;

  // CSR management
  wire [`CSR_ADDR_WIDTH-1:0]                   csr_addr;
  wire [`CSR_CMD_WIDTH-1:0]                    csr_cmd;
  wire                                         csr_imm_sel;
  wire [`PRV_WIDTH-1:0]                        prv;
  wire                                         illegal_csr_access;
  wire [`XPR_LEN-1:0]                          csr_wdata;
  wire [`XPR_LEN-1:0]                          csr_rdata;
  reg  [`XPR_LEN-1:0]                          csr_rdata_WB;
  wire                                         retire_WB;
  wire                                         exception_WB;
  wire [`ECODE_WIDTH-1:0]                      exception_code_WB;
  wire [`XPR_LEN-1:0]                          handler_PC;
  wire                                         eret;
  wire [`XPR_LEN-1:0]                          epc;

  // PRNG control
  wire [1:0]                                   PrngCtrl;

  // UART
  wire [7:0]                                   DataToUart;
  wire [7:0]                                   DataFromUart;
  wire [3:0]                                   UartRD;
  wire [3:0]                                   UartWR;

  vscale_ctrl ctrl
    (
      .clk(clk),
      .reset(reset),
      .inst_ID(inst_ID),
      .inst_EX(inst_EX),
      .imem_wait(imem_wait),
      .imem_badmem_e(imem_badmem_e),
      .dmem_wait(dmem_wait),
      .dmem_badmem_e(dmem_badmem_e),
      .cmp_true(cmp_true),
      .PC_src_sel(PC_src_sel),
      .imm_type_ID(imm_type_ID),
      .src_a_sel_EX(src_a_sel),
      .src_b_sel_EX(src_b_sel),
      .bypass_rs1(bypass_rs1),
      .bypass_rs2(bypass_rs2),
      .merge_rs1_ID(merge_rs1),
      .merge_rs2_ID(merge_rs2),
      .alu_op_EX(alu_op),
      .dmem_en(dmem_en),
      .dmem_wen(dmem_wen),
      .dmem_size(dmem_size),
      .dmem_type(dmem_type),
      .as_req_valid(as_req_valid),
      .as_req_ready(as_req_ready),
      .as_req_subtract(as_req_subtract),
      .as_resp_valid(as_resp_valid),
      .md_req_valid(md_req_valid),
      .md_req_ready(md_req_ready),
      .md_req_op(md_req_op),
      .md_req_in_1_signed(md_req_in_1_signed),
      .md_req_in_2_signed(md_req_in_2_signed),
      .md_req_out_sel(md_req_out_sel),
      .md_resp_valid(md_resp_valid),
      .wr_reg_WB(wr_reg_WB),
      .reg_to_wr_WB(reg_to_wr_WB),
      .wb_src_sel_WB(wb_src_sel_WB),
      .stall_IF(stall_IF),
      .kill_IF(kill_IF),
      .stall_ID(stall_ID),
      .kill_ID(kill_ID),
      .stall_EX(stall_EX),
      .kill_EX(kill_EX),
      .stall_WB(stall_WB),
      .kill_WB(kill_WB),
      .exception_WB(exception_WB),
      .exception_code_WB(exception_code_WB),
      .retire_WB(retire_WB),
      .csr_cmd_EX(csr_cmd),
      .csr_imm_sel_EX(csr_imm_sel),
      .illegal_csr_access(illegal_csr_access),
      .prv(prv),
      .eret(eret)
    );

  vscale_PC_mux
    #(
      .SHARES(SHARES)
    ) PCmux(
      .PC_src_sel(PC_src_sel),
      .inst_DX(inst_EX),
      .rs1_data(rs1_data_bypassed_merged),
      .PC_IF(PC_IF),
      .PC_DX(PC_EX),
      .handler_PC(handler_PC),
      .epc(epc),
      .PC_PIF(PC_PIF)
    );

  assign imem_addr = PC_PIF;

  // IF stage pipeline
  always @( posedge clk ) begin
    if( reset ) begin
      PC_IF <= `XPR_LEN'h200;
    end else if( ~stall_IF ) begin
      PC_IF <= PC_PIF;
    end
  end

  // ID stage pipeline
  always @( posedge clk ) begin
    if( reset ) begin
      PC_ID   <= 0;
      inst_ID <= `RV_NOP;
    end else if( ~stall_ID ) begin
      rs1_data_bypassed_reg <= rs1_data_bypassed;
      rs2_data_bypassed_reg <= rs2_data_bypassed;
      rs1_data_bypassed_to_merge_reg <= merge_rs1 ? rs1_data_bypassed : 0;
      rs2_data_bypassed_to_merge_reg <= merge_rs2 ? rs2_data_bypassed : 0;
      if( kill_IF ) begin
        inst_ID <= `RV_NOP;
      end else begin
        PC_ID   <= PC_IF;
        inst_ID <= imem_rdata;
      end
    end
  end // always @( posedge clk )

  assign rs1_data_bypassed = rand_zero_1 ^ ( bypass_rs1 ? wb_data_WB : rs1_data );
  assign rs2_data_bypassed = rand_zero_2 ^ ( bypass_rs2 ? wb_data_WB : rs2_data );

  assign rs1_addr = inst_ID[19:15];
  assign rs2_addr = inst_ID[24:20];

  vscale_regfile
    #(
      .SHARES(SHARES)
    ) regfile (
      .clk(clk),
      .ra1(rs1_addr),
      .rd1(rs1_data),
      .ra2(rs2_addr),
      .rd2(rs2_data),
      .wen(wr_reg_WB),
      .wa(reg_to_wr_WB),
      .wd(wb_data_rnd_WB)
    );

  vscale_imm_gen imm_gen
    (
      .inst(inst_ID),
      .imm_type(imm_type_ID),
      .imm(imm_ID)
    );

  `MERGE_SHARES( rs1_data_bypassed_merged, rs1_data_bypassed_to_merge_reg, `XPR_LEN, SHARES );
  `MERGE_SHARES( rs2_data_bypassed_merged, rs2_data_bypassed_to_merge_reg, `XPR_LEN, SHARES );

  // EX stage pipeline
  always @( posedge clk ) begin
    if( reset ) begin
      PC_EX   <= 0;
      inst_EX <= `RV_NOP;
      imm_EX  <= 0;
//      imm_merged_EX <= 0;
    end else if( ~stall_EX ) begin
      if( kill_ID ) begin
        inst_EX <= `RV_NOP;
      end else begin
        PC_EX   <= PC_ID;
        inst_EX <= inst_ID;
        imm_EX  <= imm_ID;
      end
    end
  end // always @( posedge clk )

  vscale_src_a_merged_mux
    #(
      .DWIDTH(`XPR_LEN),
      .SHARES(SHARES)
    ) src_a_merged_mux (
      .src_a_sel(src_a_sel),
      .PC_EX(PC_EX),
      .rs1_data_merged(rs1_data_bypassed_merged),
      .alu_src_a_merged(alu_src_a_merged)
    );

  vscale_src_b_mux
    #(
      .DWIDTH(`XPR_LEN),
      .SHARES(SHARES)
    ) src_b_mux (
      .src_b_sel(src_b_sel),
      .imm(imm_EX),
      .rs2_data(rs2_data_bypassed_reg),
      .alu_src_b(alu_src_b)
    );

  vscale_src_b_merged_mux
    #(
      .DWIDTH(`XPR_LEN),
      .SHARES(SHARES)
    ) src_b_merged_mux (
      .src_b_sel(src_b_sel),
      .imm(imm_EX),
      .rs2_data_merged(rs2_data_bypassed_merged),
      .alu_src_b_merged(alu_src_b_merged)
    );

  vscale_alu
    #(
      .DWIDTH(`XPR_LEN),
      .SHARES(SHARES)
    ) alu (
      .OpxDI(alu_op),
      .In1xDI(alu_src_a_merged),
      .In2xDI(alu_src_b_merged),
      .OutxDO(alu_out)
    );

  vscale_alu_masked
    #(
      .DWIDTH(`XPR_LEN),
      .SHARES(SHARES),
      .SHAMT_WIDTH(SHAMT_WIDTH)
    ) alu_masked (
      .ClkxCI(clk),
      .RstxBI(reset),
      .ReqValidxSI(~stall_EX),
      .OpxDI(alu_op),
      .In1xDI(rs1_data_bypassed_reg),
      .In2xDI(alu_src_b),
      .ShamtxDI(alu_src_b_merged[SHAMT_WIDTH-1:0]),
      .RandomDOMxDI(rand_dom_1),
      .OutxDO(masked_alu_out)
    );

  masked_add_sub
    #(
      .DWIDTH(`XPR_LEN),
      .SHARES(SHARES)
    ) as (
      .ClkxCI(clk),
      .RstxBI(reset),
      .ReqValidxSI(as_req_valid),
      .ReqReadyxSO(as_req_ready),
      .ReqSubtractxSI(as_req_subtract),
      .ReqIn1xDI(rs1_data_bypassed_reg),
      .ReqIn2xDI(alu_src_b),
      .RandomDOM1xDI(rand_dom_1),
      .RandomDOM2xDI(rand_dom_2),
      .RespResultxDO(as_resp_result),
      .RespValidxSO(as_resp_valid)
    );

  vscale_mul_div md
    (
      .clk(clk),
      .reset(reset),
      .req_valid(md_req_valid),
      .req_ready(md_req_ready),
      .req_in_1_signed(md_req_in_1_signed),
      .req_in_2_signed(md_req_in_2_signed),
      .req_out_sel(md_req_out_sel),
      .req_op(md_req_op),
      .req_in_1(rs1_data_bypassed_merged),
      .req_in_2(rs2_data_bypassed_merged),
      .resp_valid(md_resp_valid),
      .resp_result(md_resp_result)
    );


  assign cmp_true  = alu_out[0];
  assign dmem_addr = alu_out;


  assign prng_rst = PrngCtrl[1];
  assign prng_en  = PrngCtrl[0];

  generate
    if( SHARES < 3 ) begin
      assign rand_zero_1 = { 2{rand_dom_1} };
      assign rand_zero_2 = { 2{rand_dom_2} };
    end else begin
      assign rand_zero_1[0+:`XPR_LEN] = rand_dom_1[0+:`XPR_LEN]; // <- R1(0)
      assign rand_zero_2[0+:`XPR_LEN] = rand_dom_2[0+:`XPR_LEN]; // <- R2(0)
      for( genvar i = 1; i < SHARES-1; i = i + 1 ) begin
        if( i % 2 == 0 ) begin // even number of i
          assign rand_zero_1[`XPR_LEN*i+:`XPR_LEN] = rand_dom_1[`XPR_LEN*i+:`XPR_LEN] ^ rand_dom_2[`XPR_LEN*(i-1)+:`XPR_LEN]; // <- R1(2) ^ R2(1) / R1(4) ^ R2(3) / R1(6) ^ R2(5) / ...
          assign rand_zero_2[`XPR_LEN*i+:`XPR_LEN] = rand_dom_2[`XPR_LEN*i+:`XPR_LEN] ^ rand_dom_1[`XPR_LEN*(i-1)+:`XPR_LEN]; // <- R1(1) ^ R2(2) / R1(3) ^ R2(4) / R1(5) ^ R2(6) / ...
        end else begin // odd number of i
          assign rand_zero_1[`XPR_LEN*i+:`XPR_LEN] = rand_dom_2[`XPR_LEN*i+:`XPR_LEN] ^ rand_dom_1[`XPR_LEN*(i-1)+:`XPR_LEN]; // <- R1(0) ^ R2(1) / R1(2) ^ R2(3) / R1(4) ^ R2(5) / ...
          assign rand_zero_2[`XPR_LEN*i+:`XPR_LEN] = rand_dom_1[`XPR_LEN*i+:`XPR_LEN] ^ rand_dom_2[`XPR_LEN*(i-1)+:`XPR_LEN]; // <- R1(1) ^ R2(0) / R1(3) ^ R2(2) / R1(5) ^ R2(4) / ...
        end
      end
      if( SHARES % 2 == 0 ) begin // even number of shares
        assign rand_zero_1[`XPR_LEN*(SHARES-1)+:`XPR_LEN] = rand_dom_1[`XPR_LEN*(SHARES-2)+:`XPR_LEN];
        assign rand_zero_2[`XPR_LEN*(SHARES-1)+:`XPR_LEN] = rand_dom_2[`XPR_LEN*(SHARES-2)+:`XPR_LEN];
      end else begin // odd number of shares
        assign rand_zero_1[`XPR_LEN*(SHARES-1)+:`XPR_LEN] = rand_dom_2[`XPR_LEN*(SHARES-2)+:`XPR_LEN];
        assign rand_zero_2[`XPR_LEN*(SHARES-1)+:`XPR_LEN] = rand_dom_1[`XPR_LEN*(SHARES-2)+:`XPR_LEN];
      end
    end
  endgenerate


  // WB stage pipeline
  always @( posedge clk ) begin
    if( reset ) begin
`ifndef SYNTHESIS
      PC_WB             <= $random;
      alu_out_WB        <= $random;
`endif
    end else if( ~stall_WB ) begin
      PC_WB             <= PC_EX;
      alu_out_WB        <= alu_out;
      csr_rdata_WB      <= csr_rdata;
      dmem_type_WB      <= dmem_type;
    end
  end


  always @( * ) begin
    case( wb_src_sel_WB )
      `WB_SRC_MALU : wb_data_masked_WB = masked_alu_out;  // RV32_LUI, RV32_STORE, RV32_OP[_IMM] (except ADD[I] and LT[U])
      `WB_SRC_MEM  : wb_data_masked_WB = load_data_WB;    // RV32_LOAD -> unprotected ALU ADD for address
      `WB_SRC_AS   : wb_data_masked_WB = as_resp_result;  // RV32_OP[_IMM] -> add only!
      default      : wb_data_masked_WB = masked_alu_out;
    endcase // case( wb_src_sel_WB )
  end

  always @( * ) begin
    case( wb_src_sel_WB )
      `WB_SRC_CSR  : wb_data_merged_WB = csr_rdata_WB;    // RV32_SYSTEM
      `WB_SRC_MD   : wb_data_merged_WB = md_resp_result;  // RV32_OP -> multiply/divide only!
      `WB_SRC_ALU  : wb_data_merged_WB = alu_out_WB;      // RV32_AUIPC, RV32_JAL[R] -> unprotected ALU ADD to stores PC + 4
      default      : wb_data_merged_WB = alu_out_WB;
    endcase // case( wb_src_sel_WB )
  end

  assign wb_data_WB = ( wb_src_sel_WB[2] )
                      ? { { SHARES-1{`XPR_LEN'b0} }, wb_data_merged_WB }
                      : wb_data_masked_WB;

  assign wb_data_rnd_WB = rand_zero_2 ^ wb_data_WB;


  generate
    for( genvar i = 0; i < SHARES; i = i + 1 ) begin
      assign load_data_WB[`XPR_LEN*i+:`XPR_LEN] = { load_data( alu_out_WB, dmem_rdata[`XPR_LEN*i+:`XPR_LEN], dmem_type_WB ) };
    end // loop i
  endgenerate

  generate
    for( genvar i = 0; i < SHARES; i = i + 1 ) begin
      assign dmem_wdata[`XPR_LEN*i+:`XPR_LEN] = { store_data( alu_out, rs2_data_bypassed_reg[`XPR_LEN*i+:`XPR_LEN], dmem_type ) };
    end // loop i
  endgenerate


  // CSR

  assign csr_addr  = inst_EX[31:20];
  assign csr_wdata = ( csr_imm_sel ) ? inst_EX[19:15] : rs1_data_bypassed_merged;


  vscale_csr_file csr
    (
      .clk(clk),
      .reset(reset),
      .addr(csr_addr),
      .cmd(csr_cmd),
      .wdata(csr_wdata),
      .prv(prv),
      .illegal_access(illegal_csr_access),
      .rdata(csr_rdata),
      .retire(retire_WB),
      .exception(exception_WB),
      .exception_code(exception_code_WB),
      .exception_load_addr(alu_out_WB),
      .exception_PC(PC_WB),
      .epc(epc),
      .eret(eret),
      .handler_PC(handler_PC),
      // PRNG control
      .PrngCtrl(PrngCtrl),
      // Trigger
      .Trigger(Trigger)
    );

endmodule // vscale_pipeline
