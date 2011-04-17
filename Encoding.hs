module Encoding where

import Data.Bits
import qualified Data.ByteString.Lazy as BSL

encode32 i = BSL.pack  [(fromInteger ( toInteger ( shift i 0 ))),
                        (fromInteger ( toInteger  ( shift i 8 ))),
                        (fromInteger ( toInteger  ( shift i 16 ))),
                        (fromInteger ( toInteger  ( shift i 24 )))]
                        
encode16 i = BSL.pack [(fromInteger (toInteger (shift i 0))),
                        (fromInteger (toInteger (shift i 8)))]