module Debugging where

import Debug.Trace

debug :: Show a => a -> a
debug x = trace (show x) x
