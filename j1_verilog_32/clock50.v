// file: clock50.v
// 
// (c) Copyright 2008 - 2011 Xilinx, Inc. All rights reserved.
// 
// This file contains confidential and proprietary information
// of Xilinx, Inc. and is protected under U.S. and
// international copyright and other intellectual property
// laws.
// 
// DISCLAIMER
// This disclaimer is not a license and does not grant any
// rights to the materials distributed herewith. Except as
// otherwise provided in a valid license issued to you by
// Xilinx, and to the maximum extent permitted by applicable
// law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
// WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
// AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
// BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
// INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
// (2) Xilinx shall not be liable (whether in contract or tort,
// including negligence, or under any other theory of
// liability) for any loss or damage of any kind or nature
// related to, arising under or in connection with these
// materials, including for any direct, or any indirect,
// special, incidental, or consequential loss or damage
// (including loss of data, profits, goodwill, or any type of
// loss or damage suffered as a result of any action brought
// by a third party) even if such damage or loss was
// reasonably foreseeable or Xilinx had been advised of the
// possibility of the same.
// 
// CRITICAL APPLICATIONS
// Xilinx products are not designed or intended to be fail-
// safe, or for use in any application requiring fail-safe
// performance, such as life-support or safety devices or
// systems, Class III medical devices, nuclear facilities,
// applications related to the deployment of airbags, or any
// other applications that could lead to death, personal
// injury, or severe property or environmental damage
// (individually and collectively, "Critical
// Applications"). Customer assumes the sole risk and
// liability of any use of Xilinx products in Critical
// Applications, subject only to applicable laws and
// regulations governing limitations on product liability.
// 
// THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
// PART OF THIS FILE AT ALL TIMES.
// 
//----------------------------------------------------------------------------
// User entered comments
//----------------------------------------------------------------------------
// None
//
//----------------------------------------------------------------------------
// "Output    Output      Phase     Duty      Pk-to-Pk        Phase"
// "Clock    Freq (MHz) (degrees) Cycle (%) Jitter (ps)  Error (ps)"
//----------------------------------------------------------------------------
// CLK_OUT1____50.000______0.000______50.0______247.086____178.771
// CLK_OUT2____60.000______0.000______50.0______235.945____178.771
// CLK_OUT3____69.231______0.000______50.0______227.875____178.771
// CLK_OUT4____81.818______0.000______50.0______219.106____178.771
// CLK_OUT5____90.000______0.000______50.0______214.383____178.771
// CLK_OUT6___100.000______0.000______50.0______209.375____178.771
//
//----------------------------------------------------------------------------
// "Input Clock   Freq (MHz)    Input Jitter (UI)"
//----------------------------------------------------------------------------
// __primary__________50.000____________0.010

`timescale 1ps/1ps

(* CORE_GENERATION_INFO = "clock50,clk_wiz_v3_6,{component_name=clock50,use_phase_alignment=true,use_min_o_jitter=false,use_max_i_jitter=false,use_dyn_phase_shift=false,use_inclk_switchover=false,use_dyn_reconfig=false,feedback_source=FDBK_AUTO,primtype_sel=PLL_BASE,num_out_clk=6,clkin1_period=20.000,clkin2_period=20.000,use_power_down=false,use_reset=false,use_locked=false,use_inclk_stopped=false,use_status=false,use_freeze=false,use_clk_valid=false,feedback_type=SINGLE,clock_mgr_type=AUTO,manual_override=false}" *)
module clock50
 (// Clock in ports
  input         clk_in,
  // Clock out ports
  output        clk_50,
  output        clk_60,
  output        clk_70,
  output        clk_80,
  output        clk_90,
  output        clk_100
 );

  // Input buffering
  //------------------------------------
  IBUFG clkin1_buf
   (.O (clkin1),
    .I (clk_in));


  // Clocking primitive
  //------------------------------------
  // Instantiation of the PLL primitive
  //    * Unused inputs are tied off
  //    * Unused outputs are labeled unused
  wire [15:0] do_unused;
  wire        drdy_unused;
  wire        locked_unused;
  wire        clkfbout;
  wire        clkfbout_buf;

  PLL_BASE
  #(.BANDWIDTH              ("OPTIMIZED"),
    .CLK_FEEDBACK           ("CLKFBOUT"),
    .COMPENSATION           ("SYSTEM_SYNCHRONOUS"),
    .DIVCLK_DIVIDE          (1),
    .CLKFBOUT_MULT          (18),
    .CLKFBOUT_PHASE         (0.000),
    .CLKOUT0_DIVIDE         (18),
    .CLKOUT0_PHASE          (0.000),
    .CLKOUT0_DUTY_CYCLE     (0.500),
    .CLKOUT1_DIVIDE         (15),
    .CLKOUT1_PHASE          (0.000),
    .CLKOUT1_DUTY_CYCLE     (0.500),
    .CLKOUT2_DIVIDE         (13),
    .CLKOUT2_PHASE          (0.000),
    .CLKOUT2_DUTY_CYCLE     (0.500),
    .CLKOUT3_DIVIDE         (11),
    .CLKOUT3_PHASE          (0.000),
    .CLKOUT3_DUTY_CYCLE     (0.500),
    .CLKOUT4_DIVIDE         (10),
    .CLKOUT4_PHASE          (0.000),
    .CLKOUT4_DUTY_CYCLE     (0.500),
    .CLKOUT5_DIVIDE         (9),
    .CLKOUT5_PHASE          (0.000),
    .CLKOUT5_DUTY_CYCLE     (0.500),
    .CLKIN_PERIOD           (20.000),
    .REF_JITTER             (0.010))
  pll_base_inst
    // Output clocks
   (.CLKFBOUT              (clkfbout),
    .CLKOUT0               (clkout0),
    .CLKOUT1               (clkout1),
    .CLKOUT2               (clkout2),
    .CLKOUT3               (clkout3),
    .CLKOUT4               (clkout4),
    .CLKOUT5               (clkout5),
    .LOCKED                (locked_unused),
    .RST                   (1'b0),
     // Input clock control
    .CLKFBIN               (clkfbout_buf),
    .CLKIN                 (clkin1));


  // Output buffering
  //-----------------------------------
  BUFG clkf_buf
   (.O (clkfbout_buf),
    .I (clkfbout));

  BUFG clkout1_buf
   (.O   (clk_50),
    .I   (clkout0));


  BUFG clkout2_buf
   (.O   (clk_60),
    .I   (clkout1));

  BUFG clkout3_buf
   (.O   (clk_70),
    .I   (clkout2));

  BUFG clkout4_buf
   (.O   (clk_80),
    .I   (clkout3));

  BUFG clkout5_buf
   (.O   (clk_90),
    .I   (clkout4));

  BUFG clkout6_buf
   (.O   (clk_100),
    .I   (clkout5));



endmodule
