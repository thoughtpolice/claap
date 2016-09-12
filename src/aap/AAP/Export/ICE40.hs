-- |
-- Module      : AAP.Export.ICE40
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- TODO FIXME
--
module AAP.Export.ICE40
  ( topEntity
  ) where
import CLaSH.Prelude
import CLaSH.Lattice.ICE40

import AAP.Types ()
import AAP.Decoder ()
import AAP.PC ()

--------------------------------------------------------------------------------
-- CLaSH exports: top level entity, and test bench

-- | The output circuit, to be synthesized into (System)Verilog or VHDL, and
-- onto a chip.
topEntity :: ( Signal Bit
             , Signal Bit
             , Signal Bit
             , Signal Bit
             , Signal Bit
             )
topEntity = (led0, led1, led2, led3, led4)
  where
    led0 = pure 1
    led1 = pure 1
    led2 = pure 0
    led3 = pure 1
    led4 = pure 1

{-# ANN topEntity
  (defTop
    { t_name    = "claap"
    , t_inputs  = []
    , t_extraIn = [ ("clk", 1) ]
    , t_outputs = [ "LED0"
                  , "LED1"
                  , "LED2"
                  , "LED3"
                  , "LED4"
                  ]
    , t_clocks  = [ ice40pll "1'b1" PLL_60MHZ "clk" ]
    }) #-}

