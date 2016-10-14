`include "vscale_ctrl_constants.vh"
`include "vscale_alu_ops.vh"
`include "rv32_opcodes.vh"
`include "vscale_csr_addr_map.vh"
`include "vscale_md_constants.vh"

module vscale_ctrl
  (
    input                               clk,
    input                               reset,
    input  [`INST_WIDTH-1:0]            inst_ID,
    input  [`INST_WIDTH-1:0]            inst_EX,
    input                               imem_wait,
    input                               imem_badmem_e,
    input                               dmem_wait,
    input                               dmem_badmem_e,
    input                               cmp_true,
    input  [`PRV_WIDTH-1:0]             prv,
    output reg  [`PC_SRC_SEL_WIDTH-1:0] PC_src_sel,
    output reg  [`IMM_TYPE_WIDTH-1:0]   imm_type_ID,
    output                              bypass_rs1,
    output                              bypass_rs2,
    output                              merge_rs1_ID,
    output                              merge_rs2_ID,
    output reg  [`SRC_A_SEL_WIDTH-1:0]  src_a_sel_EX,
    output reg  [`SRC_B_SEL_WIDTH-1:0]  src_b_sel_EX,
    output reg  [`ALU_OP_WIDTH-1:0]     alu_op_EX,
    output wire                         dmem_en,
    output wire                         dmem_wen,
    output reg  [2:0]                   dmem_size,
    output reg  [`MEM_TYPE_WIDTH-1:0]   dmem_type,
    output                              as_req_valid,
    input                               as_req_ready,
    output reg                          as_req_subtract,
    input                               as_resp_valid,
    output                              md_req_valid,
    input                               md_req_ready,
    output reg                          md_req_in_1_signed,
    output reg                          md_req_in_2_signed,
    output reg  [`MD_OP_WIDTH-1:0]      md_req_op,
    output reg  [`MD_OUT_SEL_WIDTH-1:0] md_req_out_sel,
    input                               md_resp_valid,
    output wire                         eret,
    output reg  [`CSR_CMD_WIDTH-1:0]    csr_cmd_EX,
    output reg                          csr_imm_sel_EX,
    input                               illegal_csr_access,
    output wire                         wr_reg_WB,
    output reg  [`REG_ADDR_WIDTH-1:0]   reg_to_wr_WB,
    output reg  [`WB_SRC_SEL_WIDTH-1:0] wb_src_sel_WB,
    output wire                         stall_IF,
    output wire                         kill_IF,
    output wire                         stall_ID,
    output wire                         kill_ID,
    output wire                         stall_EX,
    output wire                         kill_EX,
    output wire                         stall_WB,
    output wire                         kill_WB,
    output wire                         exception_WB,
    output wire [`ECODE_WIDTH-1:0]      exception_code_WB,
    output wire                         retire_WB
  );

  // IF stage ctrl pipeline registers
  reg                                   replay_IF;

  // IF stage ctrl signals
  wire                                  ex_IF;

  // ID stage ctrl pipeline registers
  reg                                   had_ex_ID;
  reg                                   prev_killed_ID;

  // ID stage ctrl signals
  wire [6:0]                            opcode_ID    = inst_ID[6:0];
  wire [2:0]                            funct3_ID    = inst_ID[14:12];
  wire [6:0]                            funct7_ID    = inst_ID[31:25];
  wire [11:0]                           funct12_ID   = inst_ID[31:20];
  wire [`REG_ADDR_WIDTH-1:0]            rs1_addr_ID  = inst_ID[19:15];
  wire [`REG_ADDR_WIDTH-1:0]            rs2_addr_ID  = inst_ID[24:20];
  wire [`REG_ADDR_WIDTH-1:0]            reg_to_wr_ID = inst_ID[11:7];
  wire [2:0]                            dmem_size_ID;
  wire [`MEM_TYPE_WIDTH-1:0]            dmem_type_ID;
  reg  [`SRC_A_SEL_WIDTH-1:0]           src_a_sel_ID;
  reg  [`SRC_B_SEL_WIDTH-1:0]           src_b_sel_ID;
  reg  [`WB_SRC_SEL_WIDTH-1:0]          wb_src_sel_ID;
  reg  [`ALU_OP_WIDTH-1:0]              alu_op_ID;
  reg  [`CSR_CMD_WIDTH-1:0]             csr_cmd_unkilled_ID;
  wire [`CSR_CMD_WIDTH-1:0]             csr_cmd_ID;
  reg                                   merge_rs1_unkilled_ID;
  reg                                   merge_rs2_unkilled_ID;
  reg                                   illegal_instr_unkilled_ID;
  reg                                   ecall_unkilled_ID;
  reg                                   ebreak_unkilled_ID;
  reg                                   eret_unkilled_ID;
  reg                                   fence_i_ID;
  wire                                  csr_imm_sel_ID;
  wire [`ALU_OP_WIDTH-1:0]              srl_or_sra_ID;
  reg  [`ALU_OP_WIDTH-1:0]              alu_op_arith_ID;
  reg                                   branch_op_unkilled_ID;
  reg                                   dmem_en_unkilled_ID;
  reg                                   dmem_wen_unkilled_ID;
  reg                                   jal_unkilled_ID;
  reg                                   jalr_unkilled_ID;
  reg                                   wr_reg_unkilled_ID;
  reg                                   uses_as_unkilled_ID;
  reg                                   uses_md_unkilled_ID;
  reg                                   as_req_subtract_ID;
  wire                                  branch_op_ID;
  wire                                  jal_ID;
  wire                                  jalr_ID;
  wire                                  eret_ID;
  wire                                  illegal_instruction_ID;
  wire                                  ecall_ID;
  wire                                  ebreak_ID;

  wire                                  ex_ID;
  wire                                  killed_ID;
  wire                                  dmem_en_ID;
  wire                                  dmem_wen_ID;
  wire                                  wr_reg_ID;
  wire                                  uses_as_ID;
  wire                                  uses_md_ID;

  wire                                  wait_rs1_ID;
  wire                                  wait_rs2_ID;

  // EX stage ctrl pipeline registers
  reg                                   branch_op_EX;
  reg                                   dmem_en_unkilled_EX;
  reg                                   dmem_wen_unkilled_EX;
  reg                                   jal_unkilled_EX;
  reg                                   jalr_unkilled_EX;
  reg                                   merge_rs1_EX;
  reg                                   merge_rs2_EX;
  reg                                   wr_reg_unkilled_EX;
  reg  [`WB_SRC_SEL_WIDTH-1:0]          wb_src_sel_EX;
  reg                                   uses_as_unkilled_EX;
  reg                                   uses_md_unkilled_EX;
  reg                                   fence_i_EX;
  reg                                   ecall_EX;
  reg                                   ebreak_EX;
  reg                                   eret_unkilled_EX;
  reg                                   illegal_instruction_EX;
  reg                                   prev_killed_EX;
  reg                                   had_ex_EX;

  // EX stage ctrl signals
  reg  [`ECODE_WIDTH-1:0]               ex_code_EX;
  wire [`REG_ADDR_WIDTH-1:0]            reg_to_wr_EX = inst_EX[11:7];
  wire                                  branch_taken;
  wire                                  jal;
  wire                                  jalr;
  wire                                  redirect;
  wire                                  wr_reg_EX;
  wire                                  uses_as_EX;
  wire                                  uses_md_EX;

  // WB stage ctrl pipeline registers
  reg                                   wr_reg_unkilled_WB;
  reg                                   had_ex_WB;
  reg  [`ECODE_WIDTH-1:0]               prev_ex_code_WB;
  reg                                   store_in_WB;
  reg                                   dmem_en_WB;
  reg                                   prev_killed_WB;
  reg                                   uses_as_WB;
  reg                                   uses_md_WB;

  // WB stage ctrl signals
  wire                                  ex_WB;
  reg  [`ECODE_WIDTH-1:0]               ex_code_WB;
  wire                                  dmem_access_exception;
  wire                                  exception = ex_WB;
  wire                                  killed_WB;
  wire                                  load_in_WB;

  // Hazard signals
  reg                                   uses_rs1_ID;
  reg                                   uses_rs2_ID;
  wire                                  raw_rs1;
  wire                                  raw_rs2;

  // IF stage ctrl =========================================

  always @( posedge clk ) begin
    if( reset ) begin
      replay_IF <= 1'b1;
    end else begin
      replay_IF <= (redirect && imem_wait) || ((fence_i_ID || fence_i_EX) && (dmem_wen || store_in_WB));
    end
  end

  assign kill_IF  = stall_IF || ex_IF || ex_ID || ex_WB || redirect || replay_IF;
  assign stall_IF = ( (imem_wait && !redirect) || stall_ID ) && !exception;
  assign ex_IF    = imem_badmem_e && !imem_wait && !redirect && !replay_IF;

  // ID stage ctrl =========================================

  always @( posedge clk ) begin
    if( reset ) begin
      had_ex_ID      <= 0;
      prev_killed_ID <= 0;
    end else if( !stall_ID ) begin
      had_ex_ID      <= ex_IF;
      prev_killed_ID <= kill_IF;
    end
  end

  assign wait_rs1_ID = wr_reg_EX && (rs1_addr_ID == reg_to_wr_EX) && (rs1_addr_ID != 0) && uses_rs1_ID;
  assign wait_rs2_ID = wr_reg_EX && (rs2_addr_ID == reg_to_wr_EX) && (rs2_addr_ID != 0) && uses_rs2_ID;

  assign kill_ID   = stall_ID || ex_ID || ex_EX || ex_WB || redirect;
  assign stall_ID  = stall_EX || (fence_i_ID && dmem_wen) || uses_as_EX || uses_md_EX || wait_rs1_ID || wait_rs2_ID;
  assign ex_ID     = had_ex_ID;
  assign killed_ID = prev_killed_ID || kill_ID;

  assign branch_op_ID = branch_op_unkilled_ID && !kill_ID;
  assign jal_ID       = jal_unkilled_ID       && !kill_ID;
  assign jalr_ID      = jalr_unkilled_ID      && !kill_ID;
  assign eret_ID      = eret_unkilled_ID      && !kill_ID;
  assign dmem_en_ID   = dmem_en_unkilled_ID   && !kill_ID;
  assign dmem_wen_ID  = dmem_wen_unkilled_ID  && !kill_ID;
  assign wr_reg_ID    = wr_reg_unkilled_ID    && !kill_ID;
  assign uses_as_ID   = uses_as_unkilled_ID   && !kill_ID;
  assign uses_md_ID   = uses_md_unkilled_ID   && !kill_ID;
  assign merge_rs1_ID = merge_rs1_unkilled_ID && !kill_ID;
  assign merge_rs2_ID = merge_rs2_unkilled_ID && !kill_ID;

  assign ecall_ID     = ecall_unkilled_ID     && !kill_ID;
  assign ebreak_ID    = ebreak_unkilled_ID    && !kill_ID;
  assign illegal_instruction_ID = illegal_instr_unkilled_ID && !kill_ID;

  assign dmem_size_ID = { 1'b0, funct3_ID[1:0] };
  assign dmem_type_ID = funct3_ID;
  assign csr_imm_sel_ID = funct3_ID[2];

  always @( * ) begin
    illegal_instr_unkilled_ID = 1'b0;
    csr_cmd_unkilled_ID       = `CSR_IDLE;
    ecall_unkilled_ID         = 1'b0;
    ebreak_unkilled_ID        = 1'b0;
    eret_unkilled_ID          = 1'b0;
    fence_i_ID                = 1'b0;
    branch_op_unkilled_ID     = 1'b0;
    jal_unkilled_ID           = 1'b0;
    jalr_unkilled_ID          = 1'b0;
    uses_rs1_ID               = 1'b1;
    uses_rs2_ID               = 1'b0;
    merge_rs1_unkilled_ID     = 1'b0;
    merge_rs2_unkilled_ID     = 1'b0;
    imm_type_ID               = `IMM_I;
    src_a_sel_ID              = `SRC_A_RS1;
    src_b_sel_ID              = `SRC_B_IMM;
    alu_op_ID                 = `ALU_OP_SRCB;
    dmem_en_unkilled_ID       = 1'b0;
    dmem_wen_unkilled_ID      = 1'b0;
    wr_reg_unkilled_ID        = 1'b0;
    wb_src_sel_ID             = `WB_SRC_MALU;
    uses_as_unkilled_ID       = 1'b0;
    as_req_subtract_ID        = 1'b0;
    uses_md_unkilled_ID       = 1'b0;
    case( opcode_ID )
      `RV32_LOAD : begin // ALU ADD for address offset
        alu_op_ID             = `ALU_OP_ADD;
        merge_rs1_unkilled_ID = 1'b1;
        dmem_en_unkilled_ID   = 1'b1;
        wr_reg_unkilled_ID    = 1'b1;
        wb_src_sel_ID         = `WB_SRC_MEM;
      end
      `RV32_STORE : begin // ALU ADD for address offset
        alu_op_ID             = `ALU_OP_ADD;
        merge_rs1_unkilled_ID = 1'b1;
        uses_rs2_ID           = 1'b1;
        imm_type_ID           = `IMM_S;
        dmem_en_unkilled_ID   = 1'b1;
        dmem_wen_unkilled_ID  = 1'b1;
      end
      `RV32_BRANCH : begin // NO ALU ADD
        uses_rs2_ID  = 1'b1;
        src_b_sel_ID = `SRC_B_RS2;
        merge_rs1_unkilled_ID = 1'b1;
        merge_rs2_unkilled_ID = 1'b1;
        branch_op_unkilled_ID = 1'b1;
        case( funct3_ID )
          `RV32_FUNCT3_BEQ  : alu_op_ID = `ALU_OP_SEQ;
          `RV32_FUNCT3_BNE  : alu_op_ID = `ALU_OP_SNE;
          `RV32_FUNCT3_BLT  : alu_op_ID = `ALU_OP_SLT;
          `RV32_FUNCT3_BLTU : alu_op_ID = `ALU_OP_SLTU;
          `RV32_FUNCT3_BGE  : alu_op_ID = `ALU_OP_SGE;
          `RV32_FUNCT3_BGEU : alu_op_ID = `ALU_OP_SGEU;
          default : illegal_instr_unkilled_ID = 1'b1;
        endcase // case( funct3_ID )
      end
      `RV32_JAL : begin // ALU ADD for address PC + 4
        alu_op_ID          = `ALU_OP_ADD;
        jal_unkilled_ID    = 1'b1;
        uses_rs1_ID        = 1'b0;
        src_a_sel_ID       = `SRC_A_PC;
        src_b_sel_ID       = `SRC_B_FOUR;
        wb_src_sel_ID      = `WB_SRC_ALU;
        wr_reg_unkilled_ID = 1'b1;
      end
      `RV32_JALR : begin // ALU ADD for address PC + 4
        alu_op_ID             = `ALU_OP_ADD;
        jalr_unkilled_ID      = 1'b1;
        merge_rs1_unkilled_ID = 1'b1;
        src_a_sel_ID          = `SRC_A_PC;
        src_b_sel_ID          = `SRC_B_FOUR;
        wb_src_sel_ID         = `WB_SRC_ALU;
        wr_reg_unkilled_ID    = 1'b1;
        illegal_instr_unkilled_ID = ( funct3_ID != 0 );
      end
      `RV32_MISC_MEM : begin // NO ALU
        case( funct3_ID )
          `RV32_FUNCT3_FENCE : begin
            if( (inst_ID[31:28] == 0) && (rs1_addr_ID == 0) && (reg_to_wr_ID == 0) )
              ; // most fences are no-ops
            else
              illegal_instr_unkilled_ID = 1'b1;
          end
          `RV32_FUNCT3_FENCE_I : begin
            if( (inst_ID[31:20] == 0) && (rs1_addr_ID == 0) && (reg_to_wr_ID == 0) )
              fence_i_ID = 1'b1;
            else
              illegal_instr_unkilled_ID = 1'b1;
          end
          default : illegal_instr_unkilled_ID = 1'b1;
        endcase // case( funct3_ID )
      end
      `RV32_OP_IMM : begin
        wr_reg_unkilled_ID = 1'b1;
        alu_op_ID          = alu_op_arith_ID;
        if( (funct3_ID == `RV32_FUNCT3_SLT) || (funct3_ID == `RV32_FUNCT3_SLTU) ) begin
          merge_rs1_unkilled_ID = 1'b1;
          wb_src_sel_ID         = `WB_SRC_ALU;
        end
        if( (funct3_ID == `RV32_FUNCT3_ADD_SUB) && (reg_to_wr_ID != 0) ) begin
          if( (rs1_addr_ID == 0) || (inst_ID[31:20] == 0) ) begin // exception for addi zero (move operation / load immediate, faster)
            alu_op_ID = `ALU_OP_XOR;
          end else begin
            uses_as_unkilled_ID = 1'b1;
            wb_src_sel_ID       = `WB_SRC_AS;
          end
        end
      end
      `RV32_OP : begin
        uses_rs2_ID        = 1'b1;
        src_b_sel_ID       = `SRC_B_RS2;
        wr_reg_unkilled_ID = 1'b1;
        alu_op_ID          = alu_op_arith_ID;
        if( (funct3_ID == `RV32_FUNCT3_SLT) || (funct3_ID == `RV32_FUNCT3_SLTU) ) begin
          merge_rs1_unkilled_ID = 1'b1;
          merge_rs2_unkilled_ID = 1'b1;
          wb_src_sel_ID         = `WB_SRC_ALU;
        end
        if( (funct3_ID == `RV32_FUNCT3_ADD_SUB) && !funct7_ID[0] && (reg_to_wr_ID != 0) ) begin
          if( (rs1_addr_ID == 0) || (rs2_addr_ID == 0) ) begin
            alu_op_ID = `ALU_OP_XOR;
          end else begin
            uses_as_unkilled_ID = 1'b1;
            as_req_subtract_ID  = funct7_ID[5];
            wb_src_sel_ID       = `WB_SRC_AS;
          end
        end
        if( (funct3_ID == `RV32_FUNCT3_SLL) || (funct3_ID == `RV32_FUNCT3_SRA_SRL) ) begin
          merge_rs2_unkilled_ID = 1'b1;
        end
        if( funct7_ID == `RV32_FUNCT7_MUL_DIV ) begin
          merge_rs1_unkilled_ID = 1'b1;
          merge_rs2_unkilled_ID = 1'b1;
          uses_md_unkilled_ID   = 1'b1;
          wb_src_sel_ID         = `WB_SRC_MD;
        end
      end
      `RV32_SYSTEM : begin
        merge_rs1_unkilled_ID = ( !funct3_ID[2] && (funct3_ID[1:0] != 2'b00) );
        wb_src_sel_ID         = `WB_SRC_CSR;
        wr_reg_unkilled_ID    = ( funct3_ID != `RV32_FUNCT3_PRIV );
        case( funct3_ID )
          `RV32_FUNCT3_PRIV : begin
            if( (rs1_addr_ID == 0) && (reg_to_wr_ID == 0) ) begin
              case( funct12_ID )
                `RV32_FUNCT12_ECALL  : ecall_unkilled_ID  = 1'b1;
                `RV32_FUNCT12_EBREAK : ebreak_unkilled_ID = 1'b1;
                `RV32_FUNCT12_ERET   : begin
                  if( prv == 0 )
                    illegal_instr_unkilled_ID = 1'b1;
                  else
                    eret_unkilled_ID = 1'b1;
                end
                default : illegal_instr_unkilled_ID = 1'b1;
              endcase // case( funct12_ID )
            end // if( (rs1_addr_ID == 0) && (reg_to_wr_ID == 0) )
          end // case: `RV32_FUNCT3_PRIV
          `RV32_FUNCT3_CSRRW  : csr_cmd_unkilled_ID = ( rs1_addr_ID == 0 ) ? `CSR_READ : `CSR_WRITE;
          `RV32_FUNCT3_CSRRS  : csr_cmd_unkilled_ID = ( rs1_addr_ID == 0 ) ? `CSR_READ : `CSR_SET;
          `RV32_FUNCT3_CSRRC  : csr_cmd_unkilled_ID = ( rs1_addr_ID == 0 ) ? `CSR_READ : `CSR_CLEAR;
          `RV32_FUNCT3_CSRRWI : csr_cmd_unkilled_ID = ( rs1_addr_ID == 0 ) ? `CSR_READ : `CSR_WRITE;
          `RV32_FUNCT3_CSRRSI : csr_cmd_unkilled_ID = ( rs1_addr_ID == 0 ) ? `CSR_READ : `CSR_SET;
          `RV32_FUNCT3_CSRRCI : csr_cmd_unkilled_ID = ( rs1_addr_ID == 0 ) ? `CSR_READ : `CSR_CLEAR;
          default : illegal_instr_unkilled_ID = 1'b1;
        endcase // case( funct3_ID )
      end
      `RV32_AUIPC : begin // ALU ADD PC + IMM_U
        alu_op_ID          = `ALU_OP_ADD;
        uses_rs1_ID        = 1'b0;
        src_a_sel_ID       = `SRC_A_PC;
        imm_type_ID        = `IMM_U;
        wb_src_sel_ID      = `WB_SRC_ALU;
        wr_reg_unkilled_ID = 1'b1;
      end
      `RV32_LUI : begin
        uses_rs1_ID        = 1'b0;
        imm_type_ID        = `IMM_U;
        wr_reg_unkilled_ID = 1'b1;
      end
      default : illegal_instr_unkilled_ID = 1'b1;
    endcase // case( opcode )
  end // always @( * )

  assign srl_or_sra_ID = ( funct7_ID[5] ) ? `ALU_OP_SRA : `ALU_OP_SRL;

  always @( posedge clk ) begin
    md_req_op          <= `MD_OP_MUL;
    md_req_in_1_signed <= 0;
    md_req_in_2_signed <= 0;
    md_req_out_sel     <= `MD_OUT_LO;
    case( funct3_ID )
      `RV32_FUNCT3_MUL : begin
      end
      `RV32_FUNCT3_MULH : begin
        md_req_in_1_signed <= 1;
        md_req_in_2_signed <= 1;
        md_req_out_sel     <= `MD_OUT_HI;
      end
      `RV32_FUNCT3_MULHSU : begin
        md_req_in_1_signed <= 1;
        md_req_out_sel     <= `MD_OUT_HI;
      end
      `RV32_FUNCT3_MULHU : begin
        md_req_out_sel     <= `MD_OUT_HI;
      end
      `RV32_FUNCT3_DIV : begin
        md_req_op          <= `MD_OP_DIV;
        md_req_in_1_signed <= 1;
        md_req_in_2_signed <= 1;
      end
      `RV32_FUNCT3_DIVU : begin
        md_req_op          <= `MD_OP_DIV;
      end
      `RV32_FUNCT3_REM : begin
        md_req_op          <= `MD_OP_REM;
        md_req_in_1_signed <= 1;
        md_req_in_2_signed <= 1;
        md_req_out_sel     <= `MD_OUT_REM;
      end
      `RV32_FUNCT3_REMU : begin
        md_req_op          <= `MD_OP_REM;
        md_req_out_sel     <= `MD_OUT_REM;
      end
    endcase
  end

  always @( * ) begin
    case( funct3_ID )
      `RV32_FUNCT3_SLL     : alu_op_arith_ID = `ALU_OP_SLL;
      `RV32_FUNCT3_SLT     : alu_op_arith_ID = `ALU_OP_SLT;
      `RV32_FUNCT3_SLTU    : alu_op_arith_ID = `ALU_OP_SLTU;
      `RV32_FUNCT3_XOR     : alu_op_arith_ID = `ALU_OP_XOR;
      `RV32_FUNCT3_SRA_SRL : alu_op_arith_ID = srl_or_sra_ID;
      `RV32_FUNCT3_OR      : alu_op_arith_ID = `ALU_OP_OR;
      `RV32_FUNCT3_AND     : alu_op_arith_ID = `ALU_OP_AND;
      default              : alu_op_arith_ID = `ALU_OP_SRCB;
    endcase // case( funct3_ID )
  end // always @( * )

  // EX stage ctrl =========================================

  always @( posedge clk ) begin
    if( reset ) begin
      dmem_size <= 0;
      dmem_type <= 0;
      merge_rs1_EX  <= 0;
      merge_rs2_EX  <= 0;
      branch_op_EX  <= 0;
      src_a_sel_EX  <= 0;
      src_b_sel_EX  <= 0;
      wb_src_sel_EX <= 0;
      alu_op_EX     <= 0;
      csr_cmd_EX    <= 0;
      ecall_EX      <= 0;
      ebreak_EX     <= 0;
      csr_imm_sel_EX   <= 0;
      as_req_subtract  <= 0;
      jal_unkilled_EX  <= 0;
      jalr_unkilled_EX <= 0;
      eret_unkilled_EX <= 0;
      wr_reg_unkilled_EX   <= 0;
      dmem_en_unkilled_EX  <= 0;
      dmem_wen_unkilled_EX <= 0;
      uses_as_unkilled_EX  <= 0;
      uses_md_unkilled_EX  <= 0;
      illegal_instruction_EX <= 0;
      prev_killed_EX <= 0;
      had_ex_EX      <= 0;
      fence_i_EX     <= 0;
    end else if( !stall_EX ) begin
      if( !kill_ID ) begin
        dmem_size <= dmem_size_ID;
        dmem_type <= dmem_type_ID;
        fence_i_EX      <= fence_i_ID;
        src_a_sel_EX    <= src_a_sel_ID;
        src_b_sel_EX    <= src_b_sel_ID;
        wb_src_sel_EX   <= wb_src_sel_ID;
        alu_op_EX       <= alu_op_ID;
        csr_cmd_EX      <= csr_cmd_unkilled_ID;
        csr_imm_sel_EX  <= csr_imm_sel_ID;
        as_req_subtract <= as_req_subtract_ID;
      end else begin
        csr_cmd_EX <= `CSR_IDLE;
      end
      merge_rs1_EX  <= merge_rs1_ID;
      merge_rs2_EX  <= merge_rs2_ID;
      branch_op_EX  <= branch_op_ID;
      ecall_EX      <= ecall_ID;
      ebreak_EX     <= ebreak_ID;
      jal_unkilled_EX  <= jal_ID;
      jalr_unkilled_EX <= jalr_ID;
      eret_unkilled_EX <= eret_ID;
      wr_reg_unkilled_EX   <= wr_reg_ID;
      dmem_en_unkilled_EX  <= dmem_en_ID;
      dmem_wen_unkilled_EX <= dmem_wen_ID;
      uses_as_unkilled_EX  <= uses_as_ID;
      uses_md_unkilled_EX  <= uses_md_ID;
      illegal_instruction_EX <= illegal_instruction_ID;
      prev_killed_EX <= killed_ID;
      had_ex_EX      <= ex_ID;
    end
  end

  assign kill_EX   = stall_EX || ex_EX || ex_WB;
  assign stall_EX  = stall_WB || (fence_i_EX && store_in_WB) || !as_req_ready || !md_req_ready;
  assign new_ex_EX = ebreak_EX || ecall_EX || illegal_instruction_EX || illegal_csr_access;
  assign ex_EX     = had_ex_EX || (new_ex_EX && !stall_EX);
  assign killed_EX = prev_killed_EX || kill_EX;

  assign branch_taken = branch_op_EX && cmp_true && !kill_EX;
  assign jal          = jal_unkilled_EX          && !kill_EX;
  assign jalr         = jalr_unkilled_EX         && !kill_EX;
  assign eret         = eret_unkilled_EX         && !kill_EX;
  assign dmem_en      = dmem_en_unkilled_EX      && !kill_EX;
  assign dmem_wen     = dmem_wen_unkilled_EX     && !kill_EX;
  assign wr_reg_EX    = wr_reg_unkilled_EX       && !kill_EX;
  assign uses_as_EX   = uses_as_unkilled_EX      && !kill_EX;
  assign uses_md_EX   = uses_md_unkilled_EX      && !kill_EX;

  assign redirect = branch_taken || jal || jalr || eret;

  always @( * ) begin
    ex_code_EX = `ECODE_INST_ADDR_MISALIGNED;
    if( had_ex_EX ) begin
      ex_code_EX = `ECODE_INST_ADDR_MISALIGNED;
    end else if( illegal_instruction_EX ) begin
      ex_code_EX = `ECODE_ILLEGAL_INST;
    end else if( illegal_csr_access ) begin
      ex_code_EX = `ECODE_ILLEGAL_INST;
    end else if( ebreak_EX ) begin
      ex_code_EX = `ECODE_BREAKPOINT;
    end else if( ecall_EX ) begin
      ex_code_EX = `ECODE_ECALL_FROM_U + prv;
    end
  end // always @( * )

  assign as_req_valid = uses_as_EX;
  assign md_req_valid = uses_md_EX;

  always @( * ) begin
    if( exception ) begin
      PC_src_sel = `PC_HANDLER;
    end else if( replay_IF || (stall_IF && !imem_wait) ) begin
      PC_src_sel = `PC_REPLAY;
    end else if( eret ) begin
      PC_src_sel = `PC_EPC;
    end else if( branch_taken ) begin
      PC_src_sel = `PC_BRANCH_TARGET;
    end else if( jal ) begin
      PC_src_sel = `PC_JAL_TARGET;
    end else if( jalr ) begin
      PC_src_sel = `PC_JALR_TARGET;
    end else begin
      PC_src_sel = `PC_PLUS_FOUR;
    end
  end // always @( * )

  // WB stage ctrl =========================================

  always @( posedge clk ) begin
    if( reset ) begin
      prev_killed_WB     <= 0;
      had_ex_WB          <= 0;
      wr_reg_unkilled_WB <= 0;
      prev_ex_code_WB    <= 0;
      store_in_WB        <= 0;
      dmem_en_WB         <= 0;
      uses_as_WB         <= 0;
      uses_md_WB         <= 0;
    end else if( !stall_WB ) begin
      prev_killed_WB     <= killed_EX;
      had_ex_WB          <= ex_EX;
      wr_reg_unkilled_WB <= wr_reg_EX;
      prev_ex_code_WB    <= ex_code_EX;
      wb_src_sel_WB      <= wb_src_sel_EX;
      reg_to_wr_WB       <= reg_to_wr_EX;
      store_in_WB        <= dmem_wen;
      dmem_en_WB         <= dmem_en;
      uses_as_WB         <= uses_as_EX;
      uses_md_WB         <= uses_md_EX;
    end
  end

  assign kill_WB               = stall_WB || ex_WB;
  assign stall_WB              = (dmem_wait && dmem_en_WB)
                                 || (uses_as_WB && !as_resp_valid)
                                 || (uses_md_WB && !md_resp_valid);
  assign dmem_access_exception = dmem_badmem_e && !stall_WB;
  assign ex_WB                 = had_ex_WB || dmem_access_exception;
  assign killed_WB             = prev_killed_WB || kill_WB;

  always @( * ) begin
    ex_code_WB = prev_ex_code_WB;
    if( !had_ex_WB ) begin
      if( dmem_access_exception ) begin
        ex_code_WB = wr_reg_unkilled_WB ?
                     `ECODE_LOAD_ADDR_MISALIGNED :
                     `ECODE_STORE_AMO_ADDR_MISALIGNED;
      end
    end
  end

  assign wr_reg_WB         = wr_reg_unkilled_WB && !kill_WB;
  assign retire_WB         = !(kill_WB || killed_WB);

  assign exception_WB      = ex_WB;
  assign exception_code_WB = ex_code_WB;

  // Hazard logic ==========================================

  assign load_in_WB = dmem_en_WB && !store_in_WB;

  assign raw_rs1    = wr_reg_WB && (rs1_addr_ID == reg_to_wr_WB)
                      && (rs1_addr_ID != 0) && uses_rs1_ID;
  assign bypass_rs1 = raw_rs1;

  assign raw_rs2    = wr_reg_WB && (rs2_addr_ID == reg_to_wr_WB)
                      && (rs2_addr_ID != 0) && uses_rs2_ID;
  assign bypass_rs2 = raw_rs2;

endmodule // vscale_ctrl
