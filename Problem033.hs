module Problem033 where

import Fraction

answer :: Fraction
answer = reduce (Fraction.product (filter lessThanOne (filter (not . trivial) (filter curious twoDigitFractions))))
