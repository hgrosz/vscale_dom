`define CSR_ADDR_WIDTH     12
`define CSR_COUNTER_WIDTH  64

`define CSR_ADDR_CYCLE     12'hC00  // Cycle counter for RDCYCLE instruction.
`define CSR_ADDR_TIME      12'hC01  // Timer for RDTIME instruction.
`define CSR_ADDR_INSTRET   12'hC02  // Instructions-retired counter for RDINSTRET instruction.
`define CSR_ADDR_CYCLEH    12'hC80  // Upper 32 bits of cycle, RV32I only.
`define CSR_ADDR_TIMEH     12'hC81  // Upper 32 bits of time, RV32I only.
`define CSR_ADDR_INSTRETH  12'hC82  // Upper 32 bits of instret, RV32I only.
`define CSR_ADDR_MCPUID    12'hF00  // CPU description.
`define CSR_ADDR_MIMPID    12'hF01  // Vendor ID and version number.
`define CSR_ADDR_MHARTID   12'hF10  // Hardware thread ID.
`define CSR_ADDR_MSTATUS   12'h300  // Machine status register.
`define CSR_ADDR_MTVEC     12'h301  // Machine trap-handler base address.
`define CSR_ADDR_MTDELEG   12'h302  // Machine trap delegation register.
`define CSR_ADDR_MIE       12'h304  // Machine interrupt-enable register.  <--!!!
`define CSR_ADDR_MTIMECMP  12'h321  // Machine wall-clock timer compare value.
`define CSR_ADDR_MTIME     12'h701  // Machine wall-clock time.
`define CSR_ADDR_MTIMEH    12'h741  // Upper 32 bits of mtime, RV32I only.
`define CSR_ADDR_MSCRATCH  12'h340  // Scratch register for machine trap handlers.
`define CSR_ADDR_MEPC      12'h341  // Machine exception program counter.
`define CSR_ADDR_MCAUSE    12'h342  // Machine trap cause.
`define CSR_ADDR_MBADADDR  12'h343  // Machine bad address.
`define CSR_ADDR_MIP       12'h344  // Machine interrupt pending.
`define CSR_ADDR_CYCLEW    12'h900  // Cycle counter for RDCYCLE instruction.
`define CSR_ADDR_TIMEW     12'h901  // Timer for RDTIME instruction.
`define CSR_ADDR_INSTRETW  12'h902  // Instructions-retired counter for RDINSTRET instruction.
`define CSR_ADDR_CYCLEHW   12'h980  // Upper 32 bits of cycle, RV32I only.
`define CSR_ADDR_TIMEHW    12'h981  // Upper 32 bits of time, RV32I only.
`define CSR_ADDR_INSTRETHW 12'h982  // Upper 32 bits of instret, RV32I only.

`define CSR_ADDR_TRIGGER   12'h784
`define CSR_EN_COUNTER     12'h785
`define CSR_PRNG_CTRL      12'h786

`define CSR_CMD_WIDTH 3
`define CSR_IDLE      0
`define CSR_READ      4
`define CSR_WRITE     5
`define CSR_SET       6
`define CSR_CLEAR     7

`define ECODE_WIDTH                      4
`define ECODE_INST_ADDR_MISALIGNED       0
`define ECODE_INST_ADDR_FAULT            1
`define ECODE_ILLEGAL_INST               2
`define ECODE_BREAKPOINT                 3
`define ECODE_LOAD_ADDR_MISALIGNED       4
`define ECODE_LOAD_ACCESS_FAULT          5
`define ECODE_STORE_AMO_ADDR_MISALIGNED  6
`define ECODE_STORE_AMO_ACCESS_FAULT     7
`define ECODE_ECALL_FROM_U               8
`define ECODE_ECALL_FROM_S               9
`define ECODE_ECALL_FROM_H              10
`define ECODE_ECALL_FROM_M              11

`define ICODE_SOFTWARE 0
`define ICODE_TIMER    1

`define PRV_WIDTH     2
`define PRV_U         0
`define PRV_S         1
`define PRV_H         2
`define PRV_M         3
