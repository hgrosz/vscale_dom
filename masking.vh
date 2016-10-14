`define MERGE_SHARES( VECTOR_DEST, VECTOR_SRC, VECTOR_WIDTH, ELEMENTS )          \
  generate                                                                       \
    begin                                                                        \
      wire [(VECTOR_WIDTH)-1:0] tmp [0:ELEMENTS-1];                              \
      assign tmp[0] = VECTOR_SRC[(VECTOR_WIDTH)-1:0];                            \
      for( genvar i = 1; i < (ELEMENTS); i = i + 1 ) begin                       \
        assign tmp[i] = tmp[i-1] ^ VECTOR_SRC[(VECTOR_WIDTH)*i+:(VECTOR_WIDTH)]; \
      end                                                                        \
      assign VECTOR_DEST = tmp[(ELEMENTS)-1];                                    \
    end                                                                          \
  endgenerate
