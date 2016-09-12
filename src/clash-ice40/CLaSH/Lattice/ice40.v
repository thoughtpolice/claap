`define ICE40_PLL_TEMPLATE(name, divr, divf, divq, frange) \
  module name( input wire clk                              \
             , input wire reset                            \
             , output wire clkout                          \
             , output wire locked                          \
             );                                            \
    SB_PLL40_CORE #(                                       \
                    .FEEDBACK_PATH("SIMPLE"),              \
                    .PLLOUT_SELECT("GENCLK"),              \
                    .DIVR(divr),                           \
                    .DIVF(divf),                           \
                    .DIVQ(divq),                           \
                    .FILTER_RANGE(frange)                  \
                    ) uut (                                \
                           .LOCK(locked),                  \
                           .RESETB(reset),                 \
                           .BYPASS(1'b0),                  \
                           .REFERENCECLK(clk),             \
                           .PLLOUTCORE(clkout)             \
                           );                              \
      endmodule

// you can generate a number of macro instantiations for different clock
// frequencies, via icepll in icestorm, with this godawful one-liner:

// export FREQS="16 50 60 100 120 200 270"
// for x in $FREQS; do echo -n \`ICE40_PLL_TEMPLATE\(; echo -n ice40_pll$x; echo -n "mhz, "; icepll -o $x | egrep '(DIV[RFQ]|FILTER_RANGE)' | awk '{print $3}' | cut -c 2- | rev | cut -c 2- | rev | tr '\n' ',' | rev | cut -c 2- | rev | tr '\n' ')'; echo; done

`ICE40_PLL_TEMPLATE(ice40_pll16mhz, 4'b0000,7'b1010100,3'b110,3'b001)
`ICE40_PLL_TEMPLATE(ice40_pll50mhz, 4'b0000,7'b1000010,3'b100,3'b001)
`ICE40_PLL_TEMPLATE(ice40_pll60mhz, 4'b0000,7'b1001111,3'b100,3'b001)
`ICE40_PLL_TEMPLATE(ice40_pll100mhz, 4'b0000,7'b1000010,3'b011,3'b001)
`ICE40_PLL_TEMPLATE(ice40_pll120mhz, 4'b0000,7'b1001111,3'b011,3'b001)
`ICE40_PLL_TEMPLATE(ice40_pll200mhz, 4'b0000,7'b1000010,3'b010,3'b001)
`ICE40_PLL_TEMPLATE(ice40_pll270mhz, 4'b0000,7'b0101100,3'b001,3'b001)
