/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 0.99. DO NOT MODIFY.
*/
module Sha256_topEntity
    ( // Inputs
      input  clk // clock
    , input  rst // asynchronous reset: active high
    , input [31:0] a_0_0 
    , input [31:0] a_0_1 
    , input [31:0] a_0_2 
    , input [31:0] a_0_3 
    , input [31:0] a_0_4 
    , input [31:0] a_0_5 
    , input [31:0] a_0_6 
    , input [31:0] a_0_7 
    , input [31:0] a_1_0 
    , input [31:0] a_1_1 
    , input [31:0] a_1_2 
    , input [31:0] a_1_3 
    , input [31:0] a_1_4 
    , input [31:0] a_1_5 
    , input [31:0] a_1_6 
    , input [31:0] a_1_7 
    , input [31:0] a_1_8 
    , input [31:0] a_1_9 
    , input [31:0] a_1_10 
    , input [31:0] a_1_11 
    , input [31:0] a_1_12 
    , input [31:0] a_1_13 
    , input [31:0] a_1_14 
    , input [31:0] a_1_15 

      // Outputs
    , output wire [31:0] \#case_alt_0  
    , output wire [31:0] \#case_alt_1  
    , output wire [31:0] \#case_alt_2  
    , output wire [31:0] \#case_alt_3  
    , output wire [31:0] \#case_alt_4  
    , output wire [31:0] \#case_alt_5  
    , output wire [31:0] \#case_alt_6  
    , output wire [31:0] \#case_alt_7  
    );
  wire [255:0] h;
  wire [511:0] w67;
  wire [255:0] \#app_arg ;
  wire [2047:0] \#app_arg_0 ;
  wire [767:0] a;
  wire [255:0] \#case_alt ;

  assign a = {{a_0_0
              ,a_0_1
              ,a_0_2
              ,a_0_3
              ,a_0_4
              ,a_0_5
              ,a_0_6
              ,a_0_7}
             ,{a_1_0
              ,a_1_1
              ,a_1_2
              ,a_1_3
              ,a_1_4
              ,a_1_5
              ,a_1_6
              ,a_1_7
              ,a_1_8
              ,a_1_9
              ,a_1_10
              ,a_1_11
              ,a_1_12
              ,a_1_13
              ,a_1_14
              ,a_1_15}};

  Sha256_addT8 Sha256_addT8_case_alt
    ( .\#case_alt  (\#case_alt )
    , .ds (h)
    , .ds1 (\#app_arg ) );

  assign h = a[767:512];

  assign w67 = a[511:0];

  Sha256_rotate8 Sha256_rotate8_app_arg
    ( .result (\#app_arg )
    , .h (h)
    , .w (\#app_arg_0 ) );

  Sha256_sha256shuffle Sha256_sha256shuffle_app_arg_0
    (.result (\#app_arg_0 ), .w67 (w67));

  assign \#case_alt_0  = \#case_alt [255:224];

  assign \#case_alt_1  = \#case_alt [223:192];

  assign \#case_alt_2  = \#case_alt [191:160];

  assign \#case_alt_3  = \#case_alt [159:128];

  assign \#case_alt_4  = \#case_alt [127:96];

  assign \#case_alt_5  = \#case_alt [95:64];

  assign \#case_alt_6  = \#case_alt [63:32];

  assign \#case_alt_7  = \#case_alt [31:0];
endmodule

